module Main where


import Test.Tasty (defaultMain, testGroup)

import Text.Toml.Parser.Spec


main :: IO ()
main = do
  parserSpec <- tomlParserSpec
  defaultMain $ testGroup ""
    [ parserSpec
      --, quickCheckSuite
      -- A QuickCheck suite for a parser is possible. The internet knows.
    ]
