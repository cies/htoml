{-# LANGUAGE OverloadedStrings #-}

module Text.Toml
  ( parseMaybe
  , parseEither
  ) where

import Data.Attoparsec.Text (Result, parse, feed, maybeResult, eitherResult)
import Data.Text (Text)

import Text.Toml.Parser


-- | Parse a 'Text' that 'Maybe' results in an internal representation
-- of the document in the 'Toml' data type.
parseMaybe:: Text -> Maybe TomlDoc
parseMaybe bs = maybeResult . parseTomlDoc $ bs

-- | Parse a 'Text' that results in 'Either' a 'String'
-- containing the error message, or an internal representation
-- of the document in the 'Toml' data type.
parseEither :: Text -> Either String TomlDoc
parseEither bs = eitherResult . parseTomlDoc $ bs


-- | Parse a Toml document as 'Text' to 'Result' a list of 'Token's.
-- The 'Token's need to be processed further in order to be properly structured.
parseTomlDoc :: Text -> Result TomlDoc
parseTomlDoc bs = feed (parse tomlDoc bs) ""
