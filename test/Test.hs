module Main where


import Test.Tasty (defaultMain, testGroup)

import Text.TOML.Parser.Spec


main :: IO ()
main = do
  parserSpec <- tomlParserSpec
  defaultMain $ testGroup "All parser tests"
    [ parserSpec
      --, quickCheckSuite
      -- A QuickCheck suite for a parser is possible. The internet knows.
    ]
