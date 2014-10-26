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
import Control.Applicative hiding (many, (<|>), optional)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, pack, unpack, concat)
import Text.Parsec
import Text.Parsec.Text
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)
import Numeric (readHex)

import Text.Toml.Types


-- | Parses a complete document formatted according to the TOML spec.
tomlDoc :: Parser TomlDoc
tomlDoc = do
    skipBlanks
    topTable <- table
    namedSections <- many namedSection
    eof  -- ensures this input is completely consumed
    case sectionsToNodes (reverse namedSections) of
      Left msg -> fail msg
      Right r  -> return $ TomlDoc topTable r
  where
    sectionsToNodes [] = Right []
    sectionsToNodes (x:xs) = case sectionsToNodes xs of Left msg -> Left msg
                                                        Right r  -> insert x r


-- | Parses a table of key-value pairs.
table :: Parser Table
table = do
    pairs <- try (many (assignment <* skipBlanks)) <|> (try skipBlanks >> return [])
    case hasDup (map fst pairs) of
      Just k  -> fail $ "Cannot redefine key " ++ (unpack k)
      Nothing -> return $ M.fromList pairs
  where
    hasDup        :: Ord a => [a] -> Maybe a
    hasDup xs     = dup' xs S.empty
    dup' []     _ = Nothing
    dup' (x:xs) s = if S.member x s then Just x else dup' xs (S.insert x s)


-- | Parses a 'Table' with header ('Left') or a 'TableArray'
-- with header ('Right').
namedSection :: Parser ([Text], Table, Bool)
namedSection = do
    eitherHdr <- try (Left <$> tableHeader) <|> try (Right <$> tableArrayHeader)
    skipBlanks
    tbl <- table
    skipBlanks
    return $ case eitherHdr of Left  ns -> (ns, tbl, False)
                               Right ns -> (ns, tbl, True)


-- | Parses a table header.
tableHeader :: Parser [Text]
tableHeader = between (char '[') (char ']') headerValue


-- | Parses a table array header.
tableArrayHeader :: Parser [Text]
tableArrayHeader = between (twoChar  '[') (twoChar ']') headerValue
  where
    twoChar c = count 2 (char c)


-- | Parses the value of any header, into a list of 'Text'.
headerValue :: Parser [Text]
headerValue = (pack <$> many1 headerNameChar) `sepBy1` (char '.')
  where
    headerNameChar = satisfy (\c -> c /= ' ' && c /= '\t' && c /= '\n' &&
                                    c /= '[' && c /= ']'  && c /= '.'  && c /= '#')


-- | Parses a value-to-key assignment.
assignment :: Parser (Text, Value)
assignment = do
    k <- pack <$> many1 keyChar
    skipBlanks >> char '=' >> skipBlanks
    v <- value
    return (k, v)
  where
    -- TODO: Follow the spec, e.g.: only first char cannot be '['.
    keyChar = satisfy (\c -> c /= ' ' && c /= '\t' && c /= '\n' &&
                             c /= '=' && c /= '#'  && c /= '[')


-- | Parser for a value.
value :: Parser Value
value = (try array    <?> "array")
    <|> (try boolean  <?> "boolean")
    <|> (try anyStr   <?> "string")
    <|> (try datetime <?> "datetime")
    <|> (try float    <?> "float")
    <|> (try integer  <?> "integer")


--
-- | * Toml value parsers
--

array :: Parser Value
array = (try (arrayOf array)    <?> "array of arrays")
    <|> (try (arrayOf boolean)  <?> "array of booleans")
    <|> (try (arrayOf anyStr)   <?> "array of strings")
    <|> (try (arrayOf datetime) <?> "array of datetimes")
    <|> (try (arrayOf float)    <?> "array of floats")
    <|> (try (arrayOf integer)  <?> "array of integers")


boolean :: Parser Value
boolean = VBoolean <$> ( (try . lexeme . string $ "true")  *> return True
                     <|> (try . lexeme . string $ "false") *> return False )


anyStr :: Parser Value
anyStr = try multiBasicStr <|> try basicStr <|> try multiLiteralStr <|> try literalStr


basicStr :: Parser Value
basicStr = vString $ between dQuote dQuote (fmap pack $ many strChar)
  where
    strChar = try escSeq <|> try (satisfy (\c -> c /= '"' && c /= '\\'))
    dQuote  = char '\"'


multiBasicStr :: Parser Value
multiBasicStr = vString $ openDQuote3 *> (fmap pack $ manyTill strChar dQuote3)
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = try (dQuote3 <* char '\n') <|> try dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = count 3 $ char '"'
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = escWhiteSpc *> (escSeq <|> (satisfy (/= '\\'))) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' >> char '\n' >> (many $ satisfy (\c -> isSpc c || c == '\n'))


literalStr :: Parser Value
literalStr = vString $ between sQuote sQuote (pack <$> many (satisfy (/= '\'')))
  where
    sQuote = char '\''


multiLiteralStr :: Parser Value
multiLiteralStr = vString $ openSQuote3 *> (fmap pack $ manyTill anyChar sQuote3)
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = try (sQuote3 <* char '\n') <|> try sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = try . count 3 . char $ '\''


datetime :: Parser Value
datetime = do
    d <- manyTill anyChar (try $ char 'Z')
    let  mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") d
    case mt of Just t  -> return $ VDatetime t
               Nothing -> fail "parsing datetime failed"


-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec.
float :: Parser Value
float = VFloat <$> (lexeme $ signed unsignedDouble)
  where
    unsignedDouble = do
      let numStr = pack <$> many1 (satisfy (\c -> c >= '0' && c <= '9'))
      n <- numStr
      char '.'  -- do not use the period lexeme (that allows tailing whitespace)
      d <- numStr
      return . read . unpack . concat $ [n, ".", d]


integer :: Parser Value
integer = VInteger <$> (lexeme . signed $ read <$> (many1 digit))



--
-- * Utility functions
--

-- | Parses the elements of an array, while restricting them to a certain type.
arrayOf :: Parser Value -> Parser Value
arrayOf p = VArray <$> between arrayOpen arrayClose separatedValues
  where
    separatedValues = skipBlanks *> p `sepBy` comma <* (comma <|> skipBlanks)
    comma           = skipBlanks >> char ',' >> skipBlanks >> return ()
    arrayOpen       = lexeme $ char '['
    arrayClose      = lexeme $ char ']'


-- | Creates a 'VString' parser from a 'Text' parser (used by string parser).
vString :: Parser Text -> Parser Value
vString p = VString <$> lexeme p


-- | Parser for escape sequences.
escSeq :: Parser Char
escSeq = char '\\' *> escSeqChar
  where
    escSeqChar =  try (char '"')  *> return '"'
              <|> try (char '\\') *> return '\\'
              <|> try (char '/')  *> return '/'
              <|> try (char 'b')  *> return '\b'
              <|> try (char 't')  *> return '\t'
              <|> try (char 'n')  *> return '\n'
              <|> try (char 'f')  *> return '\f'
              <|> try (char 'r')  *> return '\r'
              <|> try (char 'u')  *> unicodeHex 4
              <|> try (char 'U')  *> unicodeHex 8
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
signed p = try (negate <$> (char '-' *> p)) <|> try p


-- | Parses the (rest of the) line including an EOF, whitespace and comments.
skipBlanks :: Parser ()
skipBlanks = skipMany blank
  where
    blank   = try ((many1 $ satisfy isSpc) >> return ()) <|> try comment <|> try eol
    comment = (char '#' >> (many $ satisfy (/= '\n'))) >> return ()


-- | Adds matching of tailing whitespaces to parser 'p'.
lexeme :: Parser a -> Parser a
lexeme p = p <* (many $ satisfy isSpc)


-- | Results in 'True' for whitespace chars, tab or space, according to spec.
isSpc :: Char -> Bool
isSpc c = c == ' ' || c == '\t'


-- | Parse an EOL, as per TOML spec this is 0x0A a.k.a. '\n'.
eol :: Parser ()
eol = satisfy (== '\n') >> return ()
