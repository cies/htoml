{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP               #-}

module Text.Toml.Parser
  ( module Text.Toml.Parser
  , module Text.Toml.Types
  ) where


import           Prelude             hiding (concat, takeWhile)

import           Control.Applicative hiding (many, optional, (<|>))
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import qualified Data.Set            as S
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
tomlDoc = fmap foldTable $ do
    skipBlanks
    topTable <- table
    namedSections <- many namedSection
    eof  -- ensures input is completely consumed
    case join topTable (reverse namedSections) of
      Left msg -> fail (unpack msg)  -- TODO: allow Text in Parse Errors
      Right r  -> return $ r
  where
    join tbl []     = Right tbl
    join tbl (x:xs) = case join tbl xs of Left msg -> Left msg
                                          Right r  -> insert x r


-- | Parses a table of key-value pairs.
table :: Parser Table
table = do
    pairs <- try (many (assignment <* skipBlanks)) <|> (try skipBlanks >> return [])
    case hasDup' (map fst pairs) of
      Just k  -> fail $ "Cannot redefine key " ++ (unpack k)
      Nothing -> return $ M.fromList pairs

inlineTable :: Parser Node
inlineTable = do
    pairs <- between (char '{') (char '}') (skipSpaces *> separatedValues <* skipSpaces)
    case hasDup' (map fst pairs) of
      Just k  -> fail $ "Cannot redefine key " ++ (unpack k)
      Nothing -> return $ VTable $ M.fromList pairs
  where
    skipSpaces      = many (satisfy isSpc)
    separatedValues = sepBy (skipSpaces *> assignment <* skipSpaces) comma
    comma           = skipSpaces >> char ',' >> skipSpaces

hasDup'        :: Ord a => [a] -> Maybe a
hasDup' xx     = dup' xx S.empty
  where
    dup' []     _ = Nothing
    dup' (x:xs) s = if S.member x s then Just x else dup' xs (S.insert x s)


-- | Parses a 'Table' or 'TableArray' with its header.
-- The resulting tuple has the header's value in the first position, and the
-- 'NTable' or 'NTArray' in the second.
namedSection :: Parser ([Text], Node)
namedSection = do
    eitherHdr <- try (Left <$> tableHeader) <|> try (Right <$> tableArrayHeader)
    skipBlanks
    tbl <- table
    skipBlanks
    return $ case eitherHdr of Left  ns -> (ns, VTable   tbl )
                               Right ns -> (ns, VTArray [tbl])


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
headerValue = ((pack <$> many1 keyChar) <|> anyStr') `sepBy1` (char '.')
  where
    keyChar = alphaNum <|> char '_' <|> char '-'

-- | Parses a key-value assignment.
assignment :: Parser (Text, Node)
assignment = do
    k <- (pack <$> many1 keyChar) <|> anyStr'
    many (satisfy isSpc) >> char '=' >> skipBlanks
    v <- value
    return (k, v)
  where
    -- TODO: Follow the spec, e.g.: only first char cannot be '['.
    keyChar = alphaNum <|> char '_' <|> char '-'


-- | Parses a value.
value :: Parser Node
value = (try array       <?> "array")
    <|> (try boolean     <?> "boolean")
    <|> (try anyStr      <?> "string")
    <|> (try datetime    <?> "datetime")
    <|> (try float       <?> "float")
    <|> (try integer     <?> "integer")
    <|> (try inlineTable <?> "inline table")


--
-- | * Toml value parsers
--

array :: Parser Node
array = (try (arrayOf array)    <?> "array of arrays")
    <|> (try (arrayOf boolean)  <?> "array of booleans")
    <|> (try (arrayOf anyStr)   <?> "array of strings")
    <|> (try (arrayOf datetime) <?> "array of datetimes")
    <|> (try (arrayOf float)    <?> "array of floats")
    <|> (try (arrayOf integer)  <?> "array of integers")


boolean :: Parser Node
boolean = VBoolean <$> ( (try . string $ "true")  *> return True  <|>
                         (try . string $ "false") *> return False )


anyStr :: Parser Node
anyStr = VString <$> anyStr'

anyStr' :: Parser Text
anyStr' = try multiBasicStr <|> try basicStr <|> try multiLiteralStr <|> try literalStr


basicStr :: Parser Text
basicStr = between dQuote dQuote (fmap pack $ many strChar)
  where
    strChar = try escSeq <|> try (satisfy (\c -> c /= '"' && c /= '\\'))
    dQuote  = char '\"'


multiBasicStr :: Parser Text
multiBasicStr = (openDQuote3 *> escWhiteSpc *> (pack <$> manyTill strChar (try dQuote3)))
  where
    -- | Parse the a tripple-double quote, with possibly a newline attached
    openDQuote3 = try (dQuote3 <* char '\n') <|> try dQuote3
    -- | Parse tripple-double quotes
    dQuote3     = count 3 $ char '"'
    -- | Parse a string char, accepting escaped codes, ignoring escaped white space
    strChar     = (escSeq <|> (satisfy (/= '\\'))) <* escWhiteSpc
    -- | Parse escaped white space, if any
    escWhiteSpc = many $ char '\\' >> char '\n' >> (many $ satisfy (\c -> isSpc c || c == '\n'))


literalStr :: Parser Text
literalStr = between sQuote sQuote (pack <$> many (satisfy (/= '\'')))
  where
    sQuote = char '\''


multiLiteralStr :: Parser Text
multiLiteralStr = (openSQuote3 *> (fmap pack $ manyTill anyChar sQuote3))
  where
    -- | Parse the a tripple-single quote, with possibly a newline attached
    openSQuote3 = try (sQuote3 <* char '\n') <|> try sQuote3
    -- | Parse tripple-single quotes
    sQuote3     = try . count 3 . char $ '\''


datetime :: Parser Node
datetime = do
    d <- manyTill anyChar (try $ char 'Z')
#if MIN_VERSION_time(1,5,0)
    let  mt = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%X") d
#else
    let  mt = parseTime defaultTimeLocale (iso8601DateFormat $ Just "%X") d
#endif
    case mt of Just t  -> return $ VDatetime t
               Nothing -> fail "parsing datetime failed"


-- | Attoparsec 'double' parses scientific "e" notation; reimplement according to Toml spec.
float :: Parser Node
float = VFloat <$> do
    n <- intStr <* lookAhead (satisfy (\c -> c == '.' || c == 'e' || c == 'E'))
    d <- try (satisfy (== '.') *> uintStr) <|> return "0"
    e <- try (satisfy (\c -> c == 'e' || c == 'E') *> intStr) <|> return "0"
    return . read . L.concat $ [n, ".", d, "e", e]
  where
    sign    = try (string "-") <|> (try (char '+') >> return "") <|> return ""
    uintStr = (:) <$> digit <*> many (optional (char '_') *> digit)
    intStr  = do s <- sign
                 u <- uintStr
                 return . L.concat $ [s, u]


integer :: Parser Node
integer = VInteger <$> (signed $ read <$> uintStr)
  where
    uintStr = (:) <$> digit <*> many (optional (char '_') *> digit)

--
-- * Utility functions
--

-- | Parses the elements of an array, while restricting them to a certain type.
arrayOf :: Parser Node -> Parser Node
arrayOf p = VArray <$> between (char '[') (char ']') (skipBlanks *> separatedValues)
  where
    separatedValues = sepEndBy (skipBlanks *> try p <* skipBlanks) comma <* skipBlanks
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


-- | Results in 'True' for whitespace chars, tab or space, according to spec.
isSpc :: Char -> Bool
isSpc c = c == ' ' || c == '\t'


-- | Parse an EOL, as per TOML spec this is 0x0A a.k.a. '\n' or 0x0D a.k.a. '\r'.
eol :: Parser ()
eol = (string "\n" <|> string "\r\n") >> return ()
