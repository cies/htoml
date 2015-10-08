{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Text.Toml.Parser
  ( module Text.Toml.Parser
  , module Text.Toml.Types
  ) where


import           Prelude             hiding (concat, takeWhile)

import           Control.Applicative hiding (many, optional, (<|>))
import           Control.Monad       (when)
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Data.Text           (Text, pack, unpack)

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format    (defaultTimeLocale, iso8601DateFormat,
                                      parseTimeM)
#else
import           Data.Time.Format    (parseTime)
import           System.Locale       (defaultTimeLocale, iso8601DateFormat)
#endif

import           Numeric             (readHex)
import           Text.Parsec
import           Text.Parsec.Text

import           Text.Toml.Types



-- | Convenience function for the test suite and GHCI.
parseOnly :: Parser a -> Text -> Either ParseError a
parseOnly p str = parse (p <* eof) "test" str


-- | Parses a complete document formatted according to the TOML spec.
tomlDoc :: Parser Table
tomlDoc = do
    skipBlanks
    topTable <- table
    namedSections <- many namedSection

    let tables = let f (_, (NTable _)) = True
                     f _               = False
                 in filter f namedSections

    failOnDuplicates (L.intercalate "." . map unpack) tables

    eof  -- ensures input is completely consumed
    case join topTable (reverse namedSections) of
      Left msg -> fail (unpack msg)  -- TODO: allow Text in Parse Errors
      Right r  -> return $ r
  where
    join tbl []     = Right tbl
    join tbl (x:xs) = case join tbl xs of Left msg -> Left msg
                                          Right r  -> insert x r


failOnDuplicates :: Ord a => (a -> String) -> [(a, b)] -> Parser ()
failOnDuplicates show' ks = do
  let duplicates = dupes $ map fst ks
  when (not $ null duplicates) $ fail $ L.concat [ "Overlapping keys: "
                                                 , L.intercalate ", "
                                                   $ map show'
                                                   $ duplicates ]
  where
    dupes :: Ord a => [a] -> [a]
    dupes xs = let xs' = L.sort xs
               in L.concat
                  $ zipWith (\x y -> if x == y then [x] else []) xs' (tail xs')


-- | Parses a table of key-value pairs.
table :: Parser Table
table = do
    pairs <- try (many (assignment <* skipBlanks)) <|> (try skipBlanks >> return [])
    failOnDuplicates (\x -> "\"" ++ unpack x ++ "\"") pairs
    return $ M.fromList (map (\(k, v) -> (k, NTValue v)) pairs)


-- | Parses a 'Table' or 'TableArray' with its header.
-- The resulting tuple has the header's value in the first position, and the
-- 'NTable' or 'NTArray' in the second.
namedSection :: Parser ([Text], Node)
namedSection = do
    eitherHdr <- try (Left <$> tableHeader) <|> try (Right <$> tableArrayHeader)
    skipBlanks
    tbl <- table
    skipBlanks
    return $ case eitherHdr of Left  ns -> (ns, NTable   tbl )
                               Right ns -> (ns, NTArray [tbl])

-- | Parses a table key/name
tableKey :: Parser Text
tableKey = bareKey <|> rawQuotedStr
  where
    bareKey = pack <$> (many1 $ satisfy $ \ch -> ch `elem` keyChars)
    keyChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"


-- | Parses a table header.
tableHeader :: Parser [Text]
tableHeader = between (char '[') (char ']') headerValue


-- | Parses a table array header.
tableArrayHeader :: Parser [Text]
tableArrayHeader = between (twoChar '[') (twoChar ']') headerValue
  where
    twoChar c = count 2 (char c)


-- | Parses the value of any header (names separated by dots), into a list of 'Text'.
headerValue :: Parser [Text]
headerValue = (skipSpaces *> tableKey <* skipSpaces) `sepBy1` (char '.')


-- | Parses a key-value assignment.
assignment :: Parser (Text, TValue)
assignment = do
    k <- skipBlanks *> tableKey
    skipSpaces >> char '=' >> skipSpaces
    v <- value
    return (k, v)


-- | Parses a value.
value :: Parser TValue
value = (try array    <?> "array")
    <|> (try boolean  <?> "boolean")
    <|> (try anyStr   <?> "string")
    <|> (try datetime <?> "datetime")
    <|> (try float    <?> "float")
    <|> (try integer  <?> "integer")


--
-- | * Toml value parsers
--

array :: Parser TValue
array = (try (arrayOf array)    <?> "array of arrays")
    <|> (try (arrayOf boolean)  <?> "array of booleans")
    <|> (try (arrayOf anyStr)   <?> "array of strings")
    <|> (try (arrayOf datetime) <?> "array of datetimes")
    <|> (try (arrayOf float)    <?> "array of floats")
    <|> (try (arrayOf integer)  <?> "array of integers")


boolean :: Parser TValue
boolean = VBoolean <$> ( (try . string $ "true")  *> return True  <|>
                         (try . string $ "false") *> return False )


anyStr :: Parser TValue
anyStr = try multiBasicStr <|> try basicStr <|> try multiLiteralStr <|> try literalStr


rawQuotedStr :: Parser Text
rawQuotedStr = between dQuote dQuote (fmap pack $ many strChar)
  where
    strChar = try escSeq <|> try (satisfy (\c -> c /= '"' && c /= '\\'))
    dQuote  = char '\"'


basicStr :: Parser TValue
basicStr = VString <$> rawQuotedStr


multiBasicStr :: Parser TValue
multiBasicStr = VString <$> (openDQuote3 *> (fmap pack $ manyTill strChar dQuote3))
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = try (dQuote3 <* char '\n') <|> try dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = count 3 $ char '"'
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = escWhiteSpc *> (escSeq <|> (satisfy (/= '\\'))) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' >> char '\n' >> (many $ satisfy (\c -> isSpc c || c == '\n'))


literalStr :: Parser TValue
literalStr = VString <$> between sQuote sQuote (pack <$> many (satisfy (/= '\'')))
  where
    sQuote = char '\''


multiLiteralStr :: Parser TValue
multiLiteralStr = VString <$> (openSQuote3 *> (fmap pack $ manyTill anyChar sQuote3))
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = try (sQuote3 <* char '\n') <|> try sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = try . count 3 . char $ '\''


datetime :: Parser TValue
datetime = do
    d <- try $ manyTill anyChar (char 'Z')
#if MIN_VERSION_time(1,5,0)
    let  mt = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%X") d
#else
    let  mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") d
#endif
    case mt of Just t  -> return $ VDatetime t
               Nothing -> fail "parsing datetime failed"


-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec.
float :: Parser TValue
float = VFloat <$> do
    n <- intStr
    (d, e) <- ((,) <$> (char '.' >> uintStr) <*> (ex <|> return "0"))
              <|>
              ((,)     "0"                   <$> ex)
    return . read . L.concat $ [n, ".", d, "e", e]
  where
    sign    = try (string "-") <|> (try (char '+') >> return "") <|> return ""
    uintStr = many1 digit
    intStr  = do s <- sign
                 u <- uintStr
                 return . L.concat $ [s, u]
    ex      = try (satisfy (\c -> c == 'e' || c == 'E') *> intStr)


integer :: Parser TValue
integer = VInteger <$> (signed $ read <$> (many1 digit))



--
-- * Utility functions
--

-- | Parses the elements of an array, while restricting them to a certain type.
arrayOf :: Parser TValue -> Parser TValue
arrayOf p = VArray <$> between (char '[') (char ']') (skipBlanks *> separatedValues)
  where
    separatedValues = sepEndBy (skipBlanks *> p <* skipBlanks) comma <* skipBlanks
    comma           = skipBlanks >> char ',' >> skipBlanks


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


-- | Parser for unicode hexadecimal values of representation length 'n'.
unicodeHex :: Int -> Parser Char
unicodeHex n = do
    h <- count n (satisfy isHex)
    let v = fst . head . readHex $ h
    return $ if v <= maxChar then toEnum v else '_'
  where
    isHex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
    maxChar = fromEnum (maxBound :: Char)


-- | Parser for signs (a plus or a minus).
signed :: Num a => Parser a -> Parser a
signed p =  try (negate <$> (char '-' *> p))
        <|> try (char '+' *> p)
        <|> try p


-- | Parses the (rest of the) line including an EOF, whitespace and comments.
skipBlanks :: Parser ()
skipBlanks = skipMany blank
  where
    blank   = try ((many1 $ satisfy isSpc) >> return ()) <|> try comment <|> try eol
    comment = char '#' >> (many $ satisfy (/= '\n')) >> return ()

-- | Parses the string that containing whitespaces (but not newline or EOF)
skipSpaces :: Parser ()
skipSpaces = try (many $ satisfy isSpc) *> return ()


-- | Results in 'True' for whitespace chars, tab or space, according to spec.
isSpc :: Char -> Bool
isSpc c = c == ' ' || c == '\t'


-- | Parse an EOL, as per TOML spec this is 0x0A a.k.a. '\n' or 0x0D a.k.a. '\r'.
eol :: Parser ()
eol = satisfy (\c -> c == '\n' || c == '\r') >> return ()
