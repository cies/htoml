{-# LANGUAGE OverloadedStrings #-}

module Text.TOML.Parser
  ( module Text.TOML.Value
  , Token

  , document
  , tableHeader
  , assignment
  , value

  , array
  , bool
  , basicStr
  , multiBasicStr
  , literalStr
  , multiLiteralStr
  , date
  , double
  , integer

  , vString
  , escSeq
  , unicodeHex
  , signed
  , skipBlanks
  ) where


import Control.Applicative
import Data.Text (Text, pack, unpack)
import Data.Attoparsec.Text hiding (signed, double)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Numeric (readHex)

import Text.TOML.Value


-- | A TOML Token, an unstructured part of a TOML document
type Token = Either [Text] (Text, TOMLV)


-- | Parser for TOML documents
document :: Parser [Token]
document = skipBlanks *> many statement <* endOfInput
  where
    statement = (eitherP tableHeader assignment) <* skipBlanks


-- | Parser for a table header (opening a document section)
tableHeader :: Parser [Text]
tableHeader = skipBlanks *> between (string "[") (string "]") tableName
  where
    tableName = namePart `sepBy1` (string ".")
    namePart  = takeWhile1 $ notInClass " \t\n[].#"


-- | Parser for a value-to-key assignment
assignment :: Parser (Text, TOMLV)
assignment = do
    k <- key
    v <- (lexeme $ string "=") *> value
    return (k, v)
  where
    key = lexeme $ takeWhile1 $ notInClass " \t\n=#"


-- | Parser for a value
value :: Parser TOMLV
value = (array           <?> "array")
    <|> (bool            <?> "bool")
    <|> (multiBasicStr   <?> "string")
    <|> (basicStr        <?> "string")
    <|> (multiLiteralStr <?> "string")
    <|> (literalStr      <?> "string")
    <|> (date            <?> "date")
    <|> (double          <?> "number")
    <|> (integer         <?> "number")


--
-- Toml value parsers
--

array, bool, basicStr, multiBasicStr, literalStr, multiLiteralStr,
  date, double, integer :: Parser TOMLV


array = VArray <$> between arrayOpen arrayClose separatedValues
  where
    separatedValues = skipBlanks *> value `sepBy` commaB <* maybeTermCommaB
    commaB          = comma <* skipBlanks
    maybeTermCommaB = (comma <|> return "") <* skipBlanks
    comma           = lexeme $ string ","
    arrayOpen       = lexeme $ string "["
    arrayClose      = lexeme $ string "]"


bool = VBool <$> (true *> return True <|> false *> return False)


basicStr = vString $ between dQuote dQuote (fmap pack $ many strChar)
  where
    strChar = escSeq <|> (satisfy (\c -> c /= '"' && c /= '\\'))


multiBasicStr = vString $ openDQuote3 *> (fmap pack $ manyTill strChar dQuote3)
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = (dQuote3 <* char '\n') <|> dQuote3
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar = many escWhiteSpc                    -- skip escaped white space, if any
              *> ( escSeq <|>                     -- match escape chars OR
                   (satisfy (\c -> c /= '\\')) )  -- match text without escape chars
              <* many escWhiteSpc                 -- skip escaped white space, if any
    -- | Parse escaped white space
    escWhiteSpc = char '\\' *> char '\n' *> many (spc <|> char '\n')


literalStr = vString $ between sQuote sQuote (fmap pack $ many (notChar '\''))


multiLiteralStr = vString $ openSQuote3 *> (fmap pack $ manyTill anyChar sQuote3)
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = (sQuote3 <* char '\n') <|> sQuote3


date = do
  dstr <- takeTill (== 'Z') <* zee
  let mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") (unpack dstr)
  case mt of Just t  -> return $ VDate t
             Nothing -> fail "parse date failed"


-- Attoparsec 'double' parses scientific "e" notation; reimplement according to TOML spec
double = VDouble <$> (lexeme $ signed unsignedDouble)
  where
    unsignedDouble = do
      let numStr = many1 . satisfy $ (\c -> c >= '0' && c <= '9')
      n <- numStr
      char '.'  -- do not use the period lexeme (that allows tailing whitespace)
      d <- numStr
      return (read $ n ++ "." ++ d)


integer = VInteger <$> (lexeme $ signed decimal)



--
-- Utility functions
--

-- | Creates a 'VString' parser from a 'Text' parser (used by string parser)
vString :: Parser Text -> Parser TOMLV
vString p = VString <$> lexeme p


-- | Parser for escape sequences
escSeq :: Parser Char
escSeq = char '\\' *> escSeqChar
  where
    escSeqChar =  char '"'  *> return '"'
              <|> char '\\' *> return '\\'
              <|> char '/'  *> return '/'
              <|> char 'b'  *> return '\b'
              <|> char 't'  *> return '\t'
              <|> char 'n'  *> return '\n'
              <|> char 'f'  *> return '\f'
              <|> char 'r'  *> return '\r'
              <|> char 'u'  *> unicodeHex 4
              <|> char 'U'  *> unicodeHex 8
              <?> "escape character"


-- | Parser for unicode hexadecimal values of length 'n'
unicodeHex :: Int -> Parser Char
unicodeHex n = do
    h <- count n (satisfy isHex)
    let v = fst . head . readHex $ h
    return $ if v <= maxChar then toEnum v else '_'
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
    maxChar = fromEnum (maxBound :: Char)


-- | Parser for negation signs (minusses)
--
-- Attoparsec 'signed' allows a "+" prefix; reimplemented according to TOML spec.
signed :: Num a => Parser a -> Parser a
signed p = (negate <$> (char '-' *> p)) <|> p


-- | Parses the (rest of the) line including an EOF, whitespace and comments
skipBlanks :: Parser ()
skipBlanks = skipMany blank
  where
    blank = (spc >> return ()) <|> comment <|> endOfLine
    comment = ( char '#' *> takeTill (== '\n') ) >> return ()


-- | Adds matching of tailing whitespaces to parser 'p'
lexeme :: Parser a -> Parser a
lexeme p = do x <- p
              many spc
              return x


-- | Parser for a whitespace char, tab or space, according to spec
spc :: Parser Char
spc = char ' ' <|> char '\t'


-- | Prefixes a parser 'a' and suffixes a parser 'b' to parser 'p'
between :: Parser a -> Parser b -> Parser p -> Parser p
between a b p = do { a; e <- p; b; return e }


--
-- Simple literals
--

true    = lexeme $ string "true"
false   = lexeme $ string "false"
zee     = lexeme $ string "Z"

sQuote  = string "'"
dQuote  = string "\""
sQuote3 = string "'''"
dQuote3 = string "\"\"\""
