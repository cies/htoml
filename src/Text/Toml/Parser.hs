{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Parser
  ( module Text.Toml.Types

  , tomlDoc
  , namedSection
  , table
  , tableHeader
  , tableArrayHeader
  , headerValue
  , assignment
  , value

  , array
  , boolean
  , basicStr
  , multiBasicStr
  , literalStr
  , multiLiteralStr
  , datetime
  , float
  , integer

  , vString
  , escSeq
  , unicodeHex
  , signed
  , skipBlanks
  ) where


import Control.Applicative
import qualified Data.Map as M
import Data.Text (Text, pack, unpack)
import Data.Attoparsec.Text hiding (signed, double)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Numeric (readHex)

import Text.Toml.Types


-- | Parses a complete document formatted according to the TOML spec.
tomlDoc :: Parser TomlDoc
tomlDoc = do
    topTable <- skipBlanks *> table
    skipBlanks
    namedSections <- many namedSection
    endOfInput  -- ensures this input is completely consumed
    case foldr sectionToNode (Right []) namedSections of
      Left msg -> fail msg
      Right r  -> return $ TomlDoc topTable r
  where
    sectionToNode section buildNodes = case buildNodes of
        Left msg -> Left msg
        Right bs -> case section of
                      Left  (ns, tbl) -> mayFail $ insertT  ns tbl bs
                      Right (ns, tbl) -> mayFail $ insertTA ns tbl bs
    mayFail e = case e of Left msg -> Left msg
                          Right r  -> Right r


-- | Parses a 'Table' with header ('Left') or a 'TableArray'
-- with header ('Right').
namedSection :: Parser (Either ([Text], Table) ([Text], Table))
namedSection = do
    eitherHdr <- eitherP tableHeader tableArrayHeader
    skipBlanks
    tbl <- table
    return $ case eitherHdr of Left  ns -> Left  (ns, tbl)
                               Right ns -> Right (ns, tbl)


-- | Parses a table of key-value pairs.
table :: Parser Table
table = M.fromList <$> many (assignment <* skipBlanks)


-- | Parses a table header.
tableHeader :: Parser [Text]
tableHeader = skipBlanks *> between (string "[") (string "]") headerValue


-- | Parses a table array header.
tableArrayHeader :: Parser [Text]
tableArrayHeader = skipBlanks *> between (string "[[") (string "]]") headerValue


-- | Parses the value of any header, into a list of 'Text'.
headerValue :: Parser [Text]
headerValue = (takeWhile1 $ notInClass " \t\n[].#") `sepBy1` (string ".")


-- | Parses a value-to-key assignment.
assignment :: Parser (Text, Value)
assignment = do
    k <- takeWhile1 $ notInClass " \t\n=#"
    skipBlanks >> string "=" >> skipBlanks
    v <- value
    return (k, v)


-- | Parser for a value
value :: Parser Value
value = (array    <?> "array")
    <|> (boolean  <?> "boolean")
    <|> (anyStr   <?> "string")
    <|> (datetime <?> "datetime")
    <|> (float    <?> "float")
    <|> (integer  <?> "integer")


--
-- | * Toml value parsers
--


array :: Parser Value
array = VArray <$> between arrayOpen arrayClose separatedValues
  where
    separatedValues = skipBlanks *> value `sepBy` commaB <* maybeTermCommaB
    commaB          = comma <* skipBlanks
    maybeTermCommaB = (comma <|> return "") <* skipBlanks
    comma           = lexeme $ string ","
    arrayOpen       = lexeme $ string "["
    arrayClose      = lexeme $ string "]"


boolean :: Parser Value
boolean = VBoolean <$> (true *> return True <|> false *> return False)
  where
    [true, false] = map (lexeme . string) ["true", "false"]


anyStr :: Parser Value
anyStr = multiBasicStr <|> basicStr <|> multiLiteralStr <|> literalStr


basicStr :: Parser Value
basicStr = vString $ between dQuote dQuote (fmap pack $ many strChar)
  where
    strChar = escSeq <|> (satisfy (\c -> c /= '"' && c /= '\\'))
    dQuote  = string "\""


multiBasicStr :: Parser Value
multiBasicStr = vString $ openDQuote3 *> (fmap pack $ manyTill strChar dQuote3)
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = (dQuote3 <* char '\n') <|> dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = string "\"\"\""
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = escWhiteSpc *> (escSeq <|> (satisfy (/= '\\'))) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' *> char '\n' *> many (spc <|> char '\n')


literalStr :: Parser Value
literalStr = vString $ between sQuote sQuote (fmap pack $ many (notChar '\''))
  where
    sQuote = string "'"


multiLiteralStr :: Parser Value
multiLiteralStr = vString $ openSQuote3 *> (fmap pack $ manyTill anyChar sQuote3)
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = (sQuote3 <* char '\n') <|> sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = string "'''"


datetime :: Parser Value
datetime = do
    d <- takeTill (== 'Z') <* zee
    let  mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") (unpack d)
    case mt of Just t  -> return $ VDatetime t
               Nothing -> fail "parsing datetime failed"
  where
    zee = lexeme $ string "Z"


-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec
float :: Parser Value
float = VFloat <$> (lexeme $ signed unsignedDouble)
  where
    unsignedDouble = do
      let numStr = many1 . satisfy $ (\c -> c >= '0' && c <= '9')
      n <- numStr
      char '.'  -- do not use the period lexeme (that allows tailing whitespace)
      d <- numStr
      return (read $ n ++ "." ++ d)


integer :: Parser Value
integer = VInteger <$> (lexeme $ signed decimal)



--
-- Utility functions
--

-- | Creates a 'VString' parser from a 'Text' parser (used by string parser)
vString :: Parser Text -> Parser Value
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
-- Attoparsec 'signed' allows a "+" prefix; reimplemented according to Toml spec.
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
