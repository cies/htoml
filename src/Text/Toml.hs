module Text.Toml where

import           Data.Text        (Text)
import           Data.Set (empty)
import           Text.Parsec

import           Text.Toml.Parser


-- | Parse a 'Text' that results in 'Either' a 'String'
-- containing the error message, or an internal representation
-- of the document in the 'Toml' data type.
parseTomlDoc :: String -> Text -> Either ParseError Table
parseTomlDoc inputName input
 = runParser tomlDoc empty inputName input
