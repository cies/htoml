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

import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Time.Format

import System.Locale

import Text.TOML.Value


type Token = Either [T.Text] (T.Text, TOMLV)


document :: Parser [Token]
document = smb *> many ekk <* endOfInput
  where
    smb = skipMany blank
    ekk = (eitherP keygroup keyval) <* smb

keygroup :: Parser [T.Text]
keygroup = do
    skipMany blank
    between lbrace rbrace skey
  where skey = keyg `sepBy` period

keyval :: Parser (T.Text, TOMLV)
keyval = do
    k <- keyv
    v <- equal *> value
    return (k, v)

keyg = lexeme $ takeWhile1 $ notInClass " \t\n]."
keyv = lexeme $ takeWhile1 $ notInClass " \t\n="

value :: Parser TOMLV
value = (array <?> "array")
    <|> (bool  <?> "bool")
    <|> (str   <?> "string")
    <|> (date  <?> "date")
    <|> (num   <?> "number")
  where
    array = VArray <$> between lbrace rbrace (value `sepBy` comma)
    bool = VBool <$> (true *> return True <|> false *> return False)
    str = VString <$> between quote quote (fmap T.pack $ many (notChar '"'))
    num = do
        n <- lexeme $ number
        case n of
            I n -> return $ VInteger n
            D d -> return $ VDouble d
    date = do
        dstr <- takeTill (=='Z') <* zee
        let mt = parseTime defaultTimeLocale (iso8601DateFormat (Just "%X")) (T.unpack dstr)
        case mt of
            Just t  -> return (VDate t)
            Nothing -> fail "parse date failed"

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


whatever p = p >> return ()
lexeme p   = do { x <- p; many spc; return x }
spc        = char ' ' <|> char '\t'
comment    = whatever $ char '#' *> takeTill (=='\n')
line p     = p *> (lexeme endOfLine)
blank      = line $ lexeme $ (try comment) <|> return ()

zee    = lexeme $ string "Z"
quote  = lexeme $ string "\""
lbrace = lexeme $ string "["
rbrace = lexeme $ string "]"
comma  = lexeme $ string ","
period = lexeme $ string "."
equal  = lexeme $ string "="
true   = lexeme $ string "true"
false  = lexeme $ string "false"

between a b p = do { a; e <- p; b; return e }
