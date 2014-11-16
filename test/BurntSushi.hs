{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module BurntSushi (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed
import Data.List (isPrefixOf, isSuffixOf)
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)

import Text.Toml
import Text.Toml.Types


allFiles :: [(FilePath, B.ByteString)]
allFiles = $(embedDir "test/BurntSushi")


validPairs :: [(String, (B.ByteString, B.ByteString))]
validPairs =
    map (\(tFP, tBS) -> (stripExt tFP, (tBS, jsonCounterpart tFP))) tomlFiles
  where
    validFiles = filter (\(f, _) -> "valid" `isPrefixOf` f) allFiles
    filterOnSuffix sfx = filter (\(f, _) -> sfx `isSuffixOf` f)
    tomlFiles = filterOnSuffix ".toml" validFiles
    jsonFiles = filterOnSuffix ".json" validFiles
    stripExt fp = take (length fp - 5) fp
    jsonCounterpart tFP =
      case filter (\(f, _) -> f == stripExt tFP ++ ".json") jsonFiles of
        []       -> error $ "Could not find a JSON counterpart for: " ++ tFP
        [(_, j)] -> j
        _        -> error $ "Expected one, but found several \
                            \JSON counterparts for: " ++ tFP


invalidTomlFiles :: [(FilePath, B.ByteString)]
invalidTomlFiles = filter (\(f, _) -> "invalid" `isPrefixOf` f) allFiles


tests :: IO TestTree
tests = return $ testGroup "BurntSushi's test suite"
          [ testGroup "test equality of resulting JSON (valid)" $
              map (\(fp, (tBS, jBS)) -> testCase fp $ assertIsValid fp tBS jBS) validPairs
          , testGroup "test parse failures of malformed TOML files (invalid)" $
              map (\(fp, tBS) -> testCase fp $ assertParseFailure fp tBS) invalidTomlFiles
          ]
  where
    assertIsValid f tomlBS jsonBS =
      case parseTomlDoc "test" (decodeUtf8 tomlBS) of
        Left e -> assertFailure $ "Could not parse TOML file: " ++ f ++ ".toml\n" ++ (show e)
        Right tomlTry -> case eitherDecode (fromStrict jsonBS) of
          Left _ -> assertFailure $ "Could not parse JSON file: " ++ f ++ ".json"
          Right jsonCorrect -> assertEqual "" jsonCorrect (toBsJSON tomlTry)
    assertParseFailure f tomlBS =
      case parseTomlDoc "test" (decodeUtf8 tomlBS) of
        Left _ -> return ()
        Right _ -> assertFailure $ "Parser accepted invalid TOML file: " ++ f ++ ".toml"
