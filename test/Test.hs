module Main where


import Test.Tasty (defaultMain, testGroup)

import Text.Toml.Parser.Spec
import qualified BurntSushi


main :: IO ()
main = do
  parserSpec <- tomlParserSpec
  bsTests <- BurntSushi.tests

  defaultMain $ testGroup "" $
    [ parserSpec
    , bsTests
      --, quickCheckSuite
      -- A QuickCheck suite for a parser is possible. The internet knows.
    ]
