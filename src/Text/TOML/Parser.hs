{-# LANGUAGE OverloadedStrings #-}

module Text.TOML.Parser
  ( module Text.TOML.Value
  , document
  , keygroup
  , keyval
  , value
  , Token
  ) where


import Control.Applicative
import Data.Text (Text, pack, unpack)
import Data.Attoparsec.Text hiding (signed, double)
import Data.Time.Format (parseTime)
import System.Locale (defaultTimeLocale, iso8601DateFormat)

import Text.TOML.Value


-- | A TOML Token, an unstructured part of a TOML document
type Token = Either [Text] (Text, TOMLV)


-- | Parser for a whole document
document :: Parser [Token]
document = smb *> many ekk <* endOfInput
  where
    smb = skipMany blank
    ekk = (eitherP keygroup keyval) <* smb

-- | Parser for a table header
keygroup :: Parser [Text]
keygroup = do
    skipMany blank
    between lbrace rbrace skey
  where
    skey = keyg `sepBy` period
    keyg = lexeme $ takeWhile1 $ notInClass " \t\n]."

-- | Parser for a key-value pair
keyval :: Parser (Text, TOMLV)
keyval = do
    k <- keyv
    v <- equal *> value
    return (k, v)
  where
    keyv = lexeme $ takeWhile1 $ notInClass " \t\n="

-- | Parser for a value
value :: Parser TOMLV
value = (array   <?> "array")
    <|> (bool    <?> "bool")
    <|> (str     <?> "string")
    <|> (date    <?> "date")
    <|> (double  <?> "number")
    <|> (integer <?> "number")


--
-- Toml value types
--

array, bool, str, date, double, integer :: Parser TOMLV

array = VArray <$> between lbrace rbrace (value `sepBy` comma)

bool = VBool <$> (true *> return True <|> false *> return False)

str = VString <$> between quote quote (fmap pack $ many (notChar '"'))

date = do
    dstr <- takeTill (=='Z') <* zee
    let mt = parseTime defaultTimeLocale (iso8601DateFormat (Just "%X")) (unpack dstr)
    case mt of
        Just t  -> return (VDate t)
        Nothing -> fail "parse date failed"

-- Attoparsec 'double' parses scientific "e" notation; reimplement according to TOML spec
double = VDouble <$> (lexeme $ signed unsignedDouble)
unsignedDouble :: Parser Double
unsignedDouble = do
  let numStr = many . satisfy $ (\c -> c >= '0' && c <= '9')
  n <- numStr
  char '.'  -- do not use the period lexeme (that allows tailing whitespace)
  d <- numStr
  return (read $ n ++ "." ++ d)

integer = VInteger <$> (lexeme $ signed decimal)

-- Attoparsec 'signed' allows a "+" prefix; reimplement according to TOML spec
signed :: Num a => Parser a -> Parser a
signed p = (negate <$> (char '-' *> p)) <|> p

-- TODO: The current string parser is very naive, fix it.
-- The strin parser below is from the json package, it uses Parsec.
--
-- p_string         :: CharParser () String
-- p_string          = between (tok (char '"')) (tok (char '"')) (many p_char)
--   where p_char    =  (char '\\' >> p_esc)
--                  <|> (satisfy (\x -> x /= '"' && x /= '\\'))

--         p_esc     =  ('"'   <$ char '"')
--                  <|> ('\\'  <$ char '\\')
--                  <|> ('/'   <$ char '/')
--                  <|> ('\b'  <$ char 'b')
--                  <|> ('\f'  <$ char 'f')
--                  <|> ('\n'  <$ char 'n')
--                  <|> ('\r'  <$ char 'r')
--                  <|> ('\t'  <$ char 't')
--                  <|> (char 'u' *> p_uni)  -- TODO: add \Uxxxxxxxxx as well
--                  <?> "escape character"

--         p_uni     = check =<< count 4 (satisfy isHexDigit)
--           where check x | code <= max_char  = pure (toEnum code)
--                         | otherwise         = empty
--                   where code      = fst $ head $ readHex x
--                         max_char  = fromEnum (maxBound :: Char)


--
-- Utility functions
--

whatever p    = p >> return ()
lexeme p      = do { x <- p; many spc; return x }  -- collapse tailing whitespaces
spc           = char ' ' <|> char '\t'
comment       = whatever $ char '#' *> takeTill (=='\n')
line p        = p *> (lexeme endOfLine)
blank         = line $ lexeme $ (try comment) <|> return ()
between a b p = do { a; e <- p; b; return e }


--
-- Literals
--

zee    = lexeme $ string "Z"
quote  = lexeme $ string "\""
lbrace = lexeme $ string "["
rbrace = lexeme $ string "]"
comma  = lexeme $ string ","
period = lexeme $ string "."
equal  = lexeme $ string "="
true   = lexeme $ string "true"
false  = lexeme $ string "false"
