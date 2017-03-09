module Text.Toml where

import           Data.Text        (Text)
import           Data.Set         (empty)
import           Text.Parsec

import           Text.Toml.Parser
import           Text.Toml.Pretty ()


-- | Parse a 'Text' that results in 'Either' a 'String'
-- containing the error message, or an internal representation
-- of the document as a 'Table'.
parseTomlDoc :: String -> Text -> Either ParseError Table
parseTomlDoc inputName input = runParser tomlDoc empty inputName input
