{-# LANGUAGE OverloadedStrings #-}

module Text.TOML.Parser.Spec (tomlParserSpec) where


import Test.Tasty (TestTree)
import Test.Tasty.Hspec

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Text.TOML.Parser


tomlParserSpec :: IO TestTree
tomlParserSpec = testSpec "parser tests" tomlParserSpec'

tomlParserSpec' :: Spec
tomlParserSpec' = do

  describe "Parser.document" $ do

    it "should parse empty input" $
      testParser document "" []

    it "should parse the simple 'bio' block from the example" $
      testParser document "[owner]\n\
                          \bio = \"GitHub Cofounder & CEO\"\n\
                          \dob = 1979-05-27T07:32:00Z # First class dates? Why not?\n" $
                          [ Left ["owner"]
                          , Right ("bio", VString "GitHub Cofounder & CEO")
                          , Right ("dob", VDate $ UTCTime (ModifiedJulianDay 44020) 27120) ]

    it "should parse the simple 'types' block from the example" $
      testParser document "[database]\n\
                          \server = \"192.168.1.1\"\n\
                          \ports = [ 8001, 8001, 8002 ]\n\
                          \connection_max = 5000\n\
                          \enabled = true\n" $
                          [ Left ["database"]
                          , Right ("server", VString "192.168.1.1")
                          , Right ("ports", VArray [ VInteger 8001
                                                   , VInteger 8001
                                                   , VInteger 8002 ])
                          , Right ("connection_max", VInteger 5000)
                          , Right ("enabled",VBool True) ]

    it "should parse the simple 'unicode' value from the example" $
      testParser document "country = \"中国\" # This should be parsed as UTF-8" $
                          [ Right ("country", VString "中国") ]

    it "should parse the nested 'array' from the example" $
      testParser document "[clients]\n\
                          \data = [ [\"gamma\", \"delta\"], [1, 2] ]\n" $
                          [ Left ["clients"]
                          , Right ("data", VArray [ VArray [ VString "gamma"
                                                           , VString "delta" ]
                                                  , VArray [ VInteger 1
                                                           , VInteger 2 ] ]) ]

    it "should allow linebreaks in an array as shown in the example" $
      testParser document "hosts = [\n\
                          \  \"alpha\",\n\
                          \  \"omega\"\n\
                          \]\n" $
                          [ Right ("hosts", VArray [VString "alpha", VString "omega"]) ]

    it "should parse non-empty documents that do not end with a newline" $
      testParser document "number = 123" [ Right ("number", VInteger 123) ]

    it "should not parse rubbish" $
      testParserFails document "{"

  where testParser p str success = case parseOnly p str of Left  _ -> False
                                                           Right x -> x == success
        testParserFails p str    = case parseOnly p str of Left  _ -> True
                                                           Right _ -> False