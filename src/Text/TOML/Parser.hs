{-# LANGUAGE OverloadedStrings #-}
module Text.TOML.Parser where

import Control.Applicative

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.Attoparsec.Types

import Text.TOML.Value

document = comment

value = array <|> bool <|> str <|> num -- <|> number <|> date
  where
    array = VArray <$> between lbrace rbrace (value `sepBy` comma)
    bool  = VBool <$> (true *> return True <|> false *> return False)
    str = VString <$> between quote quote (many (notChar '"'))
    num = lexeme $ do
        n <- number
        case n of
            I n -> return $ VInteger n
            D d -> return $ VDouble d

whatever p = p >> return ()
lexeme p = do { many (many1 spc); p }
spc   = whatever $ char ' ' <|> char '\t'
comment = whatever $ char '#' *> takeTill (=='\n')

quote = lexeme $ string "\""
lbrace = lexeme $ string "["
rbrace = lexeme $ string "]"
comma = lexeme $ string ","
true = lexeme $ string "true"
false = lexeme $ string "false"

between a b p = do { a; e <- p; b; return e }
