{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Text.TOML.Parser.Spec (tomlParserSpec) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec

import Data.Attoparsec.Text (parseOnly)
import NeatInterpolation
import Data.Text (pack)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Text.TOML.Parser


tomlParserSpec :: IO TestTree
tomlParserSpec = testSpec "parser tests" tomlParserSpec'

tomlParserSpec' :: Spec
tomlParserSpec' = do

  describe "Parser.document generic" $ do

    it "should parse empty input" $
      testParser document "" []

    it "should parse the simple 'bio' block from the example" $
      testParser document
        (pack [string|
          [owner]
            bio = "GitHub Cofounder & CEO"
            dob = 1979-05-27T07:32:00Z  # First class dates? Why not?
        |])
        [ Left ["owner"]
        , Right ("bio", VString "GitHub Cofounder & CEO")
        , Right ("dob", VDate $ UTCTime (ModifiedJulianDay 44020) 27120) ]

    it "should parse the simple 'types' block from the example" $
      testParser document
        (pack [string|
          [database]
            server = "192.168.1.1"
            ports = [ 8001, 8001, 8002 ]
            connection_max = 5000
            enabled = true
        |])
        [ Left ["database"]
        , Right ("server", VString "192.168.1.1")
        , Right ("ports", VArray [ VInteger 8001
                                 , VInteger 8001
                                 , VInteger 8002 ])
        , Right ("connection_max", VInteger 5000)
        , Right ("enabled",VBool True) ]

    it "should parse non-empty documents that do not end with a newline" $
      testParser document "number = 123" [ Right ("number", VInteger 123) ]

    it "should parse when document ends in a comment" $
      testParser document "q = 42  # understood?" [ Right ("q", VInteger 42) ]

    it "should not parse rubbish" $
      testParserFails document "{"


  describe "Parser.document strings" $ do

    it "should parse the common escape sequences in basic strings" $
      testParser document "escaped = \"123\\b\\t\\n\\f\\r\\\"\\/\\\\\""
                          [ Right ("escaped", VString "123\b\t\n\f\r\"/\\") ]

    it "should parse the simple unicode value from the example" $
      testParser document "country = \"中国\" # This should be parsed as UTF-8\n"
                          [ Right ("country", VString "中国") ]

    it "should parse escaped 4 digit unicode values" $
      testParser document "special_k = \"\\u0416\""
                          [ Right ("special_k", VString "Ж") ]

    it "should parse escaped 8 digit unicode values" $
      testParser document "g_clef = \"\\U0001D11e\""
                          [ Right ("g_clef", VString "𝄞") ]
    it "should not parse escaped unicode values with missing digits" $
      testParserFails document "g_clef = \"\\U1D11e\""

    it "should parse multi-line basic strings" $
      testParser document "s = \"\"\"thorrough\"\"\"" [ Right ("s", VString "thorrough") ]

    it "should parse multi-line basic strings, with escaped newlines" $
      testParser document "s = \"\"\"One\nTwo\"\"\"" [ Right ("s", VString "One\nTwo") ]

    it "should parse multi-line basic strings, with newlines" $
      testParser document
        (pack [string|
          s = """One
          Two"""|])
        [ Right ("s", VString "One\nTwo") ]

    it "should parse multi-line basic strings, with newlines, ignoring 1 leading newline" $
      testParser document
        (pack [string|
          s = """
          One
          Two"""|])
        [ Right ("s", VString "One\nTwo") ]

    it "should parse multi-line basic strings, with espaced whitespace" $
      testParser document
        (pack [string|
          s = """\
          Quick \

          Jumped \
             Lazy\
          """|])
        [ Right ("s", VString "Quick Jumped Lazy") ]

    it "should parse literal strings literally" $
      testParser document "s = '\"Your\" folder: \\\\User\\new\\tmp\\'"
                          [ Right ("s", VString "\"Your\" folder: \\\\User\\new\\tmp\\") ]

    it "has no notion of 'escaped single quotes' in literal strings" $
      testParserFails document "s = 'I don\\'t know.'"  -- string terminates before the "t"

    it "should parse multi-line literal strings literally" $
      testParser document
        "s = '''\nFirst newline is dropped.\n   Other whitespace,\n  is preserved -- isn't it?'''"
        [ Right ("s", VString
          "First newline is dropped.\n   Other whitespace,\n  is preserved -- isn't it?") ]


  describe "Parser.document numbers" $ do

    it "should distinguish between integers and floats (doubles)" $
      testParser document "data = [[42, -17], [3.14, -0.01]]"
        [ Right ("data", VArray [ VArray [ VInteger   42
                                         , VInteger (-17)    ]
                                , VArray [ VDouble     3.14
                                         , VDouble   (-0.01) ] ]) ]

    it "should not accept floats starting with a dot" $
      testParserFails document "n = .5"

    it "should not accept floats without any decimals" $
      testParserFails document "n = 5."

    it "should not accept 'scientific notation' ('e'-notation) of numbers" $
      testParserFails document "one_and_a_half_million = 1.5e6"

    it "should not allow integers prefixed with a plus" $
      testParserFails document "n = +42"

    it "should not allow floats prefixed with a plus" $
      testParserFails document "n = +2.1828"



  describe "Parser.document arrays" $ do

    it "should parse nested arrays" $
      testParser document
        (pack [string|
          [clients]
            d = [ ["gamma", "delta"], [1, 2] ]
        |])
        [ Left ["clients"]
        , Right ("d", VArray [ VArray [ VString "gamma"
                                      , VString "delta" ]
                             , VArray [ VInteger 1
                                      , VInteger 2 ] ]) ]

    it "should allow linebreaks in an array" $
      testParser document
        (pack [string|
          hosts = [
            "alpha",
            "omega"
          ]
        |])
        [ Right ("hosts", VArray [VString "alpha", VString "omega"]) ]

    it "should allow some linebreaks in an array" $
      testParser document
        (pack [string|
          hosts = ["alpha" ,
                   "omega"]
        |])
        [ Right ("hosts", VArray [VString "alpha", VString "omega"]) ]

    it "should allow linebreaks in an array, with comments" $
      testParser document
        (pack [string|
          hosts = [
            "alpha",  # the first
            "omega"   # the last
          ]
        |])
        [ Right ("hosts", VArray [VString "alpha", VString "omega"]) ]

    it "should allow linebreaks in an array, with comments, and terminating comma" $
      testParser document
        (pack [string|
          hosts = [
            "alpha",  # the first
            "omega",  # the last
          ]
        |])
        [ Right ("hosts", VArray [VString "alpha", VString "omega"]) ]

    it "inside an array, all element should be of the same type" $
      testParserFails document "data = [1, 2.0]"

    it "should parse terminating commas in arrays" $
      testParser document "a = [1, 2, ]"
                          [ Right ("a", VArray [ VInteger 1, VInteger 2 ]) ]

    it "should parse terminating commas in arrays(2)" $
      testParser document "a = [1,2,]"
                          [ Right ("a", VArray [ VInteger 1, VInteger 2 ]) ]



    -- TODO: The "Table" and "Array of Tables" sections form the TOML spec


  where testParser p str success = case parseOnly p str of Left  _ -> False
                                                           Right x -> x == success
        testParserFails p str    = case parseOnly p str of Left  _ -> True
                                                           Right _ -> False
