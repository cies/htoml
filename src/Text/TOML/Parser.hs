{-# LANGUAGE OverloadedStrings #-}
module Text.TOML.Parser where

import Control.Applicative

import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator
import Data.Attoparsec.Types

import Text.TOML.Value

document = comment

value = array <|> bool <|> number -- <|> date
  where
    array = VArray <$> between "[" "]" (value `sepBy` (string ","))
    bool  = VBool <$> (string "true" *> return True <|> string "false" *> return False)
    string = between "\"" "\"" (notChar '"')

whatever p = p >> return ()

space   = whatever $ char ' ' <|> char '\t'
comment = whatever $ char '#' *> takeTill (=='\n')

between a b p = do { string a; e <- p; string b; return e }
