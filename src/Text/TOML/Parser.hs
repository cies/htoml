{-# LANGUAGE OverloadedStrings #-}
module Text.TOML.Parser where

import Control.Applicative

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.Attoparsec.Types

import Text.TOML.Value

document = comment

value = array <|> bool <|> str -- <|> number <|> date
  where
    array = VArray <$> between (string "[") (string "]") (value `sepBy` (string ","))
    bool  = VBool <$> (string "true" *> return True <|> string "false" *> return False)
    str = VString <$> between (string "\"") (string "\"") (many (notChar '"'))

whatever p = p >> return ()

space   = whatever $ char ' ' <|> char '\t'
comment = whatever $ char '#' *> takeTill (=='\n')

between a b p = do { a; e <- p; b; return e }
