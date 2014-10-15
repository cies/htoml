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

  , arrayOf
  , vString
  , escSeq
  , unicodeHex
  , signed
  , skipBlanks
  ) where


import Prelude hiding (takeWhile, concat)
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, pack, unpack, concat)
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
    case sectionsToNodes (reverse namedSections) of
      Left msg -> fail msg
      Right r  -> return $ TomlDoc topTable r
  where
    sectionsToNodes [] = Right []
    sectionsToNodes (x:xs) = case sectionsToNodes xs of Left msg -> Left msg
                                                        Right r  -> insert x r


-- | Parses a 'Table' with header ('Left') or a 'TableArray'
-- with header ('Right').
namedSection :: Parser ([Text], Table, Bool)
namedSection = do
    eitherHdr <- eitherP tableHeader tableArrayHeader
    skipBlanks
    tbl <- table
    return $ case eitherHdr of Left  ns -> (ns, tbl, False)
                               Right ns -> (ns, tbl, True)


-- | Parses a table of key-value pairs.
table :: Parser Table
table = do
    pairs <- many (assignment <* skipBlanks)
    case hasDup (map fst pairs) of
      Just k -> fail $ "Cannot redefine key " ++ (unpack k)
      Nothing -> return $ M.fromList pairs
  where
    hasDup :: Ord a => [a] -> Maybe a
    hasDup xs = dup' xs S.empty
    dup' []     _ = Nothing
    dup' (x:xs) s = if S.member x s then Just x else dup' xs (S.insert x s)


-- | Parses a table header.
tableHeader :: Parser [Text]
tableHeader = skipBlanks *> between (char '[') (char ']') headerValue


-- | Parses a table array header.
tableArrayHeader :: Parser [Text]
tableArrayHeader = skipBlanks *> between (twoChar  '[') (twoChar ']') headerValue
  where
    twoChar c = count 2 (char c)


-- | Parses the value of any header, into a list of 'Text'.
headerValue :: Parser [Text]
headerValue = (takeWhile1 $ notInClass " \t\n[].#") `sepBy1` (char '.')


-- | Parses a value-to-key assignment.
assignment :: Parser (Text, Value)
assignment = do
    k <- takeWhile1 $ notInClass " \t\n=#"
    skipBlanks >> char '=' >> skipBlanks
    v <- value
    return (k, v)


-- | Parser for a value.
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
array = (arrayOf array    <?> "array of arrays")
    <|> (arrayOf boolean  <?> "array of booleans")
    <|> (arrayOf anyStr   <?> "array of strings")
    <|> (arrayOf datetime <?> "array of datetimes")
    <|> (arrayOf float    <?> "array of floats")
    <|> (arrayOf integer  <?> "array of integers")


boolean :: Parser Value
boolean = VBoolean <$> (lexeme "true" *> return True <|> lexeme "false" *> return False)


anyStr :: Parser Value
anyStr = multiBasicStr <|> basicStr <|> multiLiteralStr <|> literalStr


basicStr :: Parser Value
basicStr = vString $ between dQuote dQuote (fmap pack $ many strChar)
  where
    strChar = escSeq <|> (satisfy (\c -> c /= '"' && c /= '\\'))
    dQuote  = char '\"'


multiBasicStr :: Parser Value
multiBasicStr = vString $ openDQuote3 *> (fmap pack $ manyTill strChar dQuote3)
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = (dQuote3 <* char '\n') <|> dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = count 3 $ char '"'
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = escWhiteSpc *> (escSeq <|> (satisfy (/= '\\'))) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' >> char '\n' >> takeWhile (\c -> isSpc c || c == '\n')


literalStr :: Parser Value
literalStr = vString $ between sQuote sQuote (takeWhile (/= '\''))
  where
    sQuote = char '\''


multiLiteralStr :: Parser Value
multiLiteralStr = vString $ openSQuote3 *> (fmap pack $ manyTill anyChar sQuote3)
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = (sQuote3 <* char '\n') <|> sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = count 3 $ char '\''


datetime :: Parser Value
datetime = do
    d <- takeTill (== 'Z') <* zulu
    let  mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") (unpack d)
    case mt of Just t  -> return $ VDatetime t
               Nothing -> fail "parsing datetime failed"
  where
    zulu = lexeme $ char 'Z'


-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec.
float :: Parser Value
float = VFloat <$> (lexeme $ signed unsignedDouble)
  where
    unsignedDouble = do
      let numStr = takeWhile1 (\c -> c >= '0' && c <= '9')
      n <- numStr
      char '.'  -- do not use the period lexeme (that allows tailing whitespace)
      d <- numStr
      return . read . unpack . concat $ [n, ".", d]


integer :: Parser Value
integer = VInteger <$> (lexeme $ signed decimal)



--
-- * Utility functions
--

-- | Parses the elements of an array, while restricting them to a certain type.
arrayOf :: Parser Value -> Parser Value
arrayOf p = VArray <$> between arrayOpen arrayClose separatedValues
  where
    separatedValues = skipBlanks *> p `sepBy` commaB <* maybeTermCommaB
    commaB          = comma <* skipBlanks
    maybeTermCommaB = ((comma >> return ()) <|> return ()) <* skipBlanks
    comma           = lexeme $ char ','
    arrayOpen       = lexeme $ char '['
    arrayClose      = lexeme $ char ']'


-- | Creates a 'VString' parser from a 'Text' parser (used by string parser).
vString :: Parser Text -> Parser Value
vString p = VString <$> lexeme p


-- | Parser for escape sequences.
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


-- | Parser for unicode hexadecimal values of length 'n'.
unicodeHex :: Int -> Parser Char
unicodeHex n = do
    h <- count n (satisfy isHex)
    let v = fst . head . readHex $ h
    return $ if v <= maxChar then toEnum v else '_'
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
    maxChar = fromEnum (maxBound :: Char)


-- | Parser for negation signs (minusses).
--
-- Attoparsec 'signed' allows a "+" prefix; reimplemented according to Toml spec.
signed :: Num a => Parser a -> Parser a
signed p = (negate <$> (char '-' *> p)) <|> p


-- | Parses the (rest of the) line including an EOF, whitespace and comments.
skipBlanks :: Parser ()
skipBlanks = skipMany blank
  where
    blank   = (takeWhile1 isSpc >> return ()) <|> comment <|> endOfLine
    comment = (char '#' >> takeTill (== '\n')) >> return ()


-- | Adds matching of tailing whitespaces to parser 'p'.
lexeme :: Parser a -> Parser a
lexeme p = p <* takeWhile isSpc


-- | Results in 'True' for whitespace chars, tab or space, according to spec.
isSpc :: Char -> Bool
isSpc c = c == ' ' || c == '\t'


-- | Prefixes a parser 'a' and suffixes a parser 'b' to parser 'p'.
between :: Parser a -> Parser b -> Parser p -> Parser p
between a b p = do { a; e <- p; b; return e }
