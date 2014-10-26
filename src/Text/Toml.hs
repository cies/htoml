module Text.Toml where

import Text.Parsec
import Data.Text (Text)

import Text.Toml.Parser


-- | Parse a 'Text' that results in 'Either' a 'String'
-- containing the error message, or an internal representation
-- of the document in the 'Toml' data type.
parseTomlDoc :: String -> Text -> Either ParseError TomlDoc
parseTomlDoc inputName input = parse tomlDoc inputName input
