{-# LANGUAGE OverloadedStrings #-}

import           Prelude        hiding (readFile)

import           Criterion.Main
import           Data.Text.IO   (readFile)

import           Text.Toml


main :: IO ()
main = do
    exampleToml  <- readFile "./benchmarks/example.toml"
    repeatedToml <- readFile "./benchmarks/repeated.toml"
    defaultMain

      [ bgroup "string"
        [ bench "assignment"  $ whnf (parseTomlDoc "") "q=42\nqa=[4,2,]\nqb=true\nqf=23.23\n\
                                                       \qd=1979-05-27T07:32:00Z\nqs='forty-two'"
        , bench "headers"     $ whnf (parseTomlDoc "") "[Q]\n[A]\n[[QA]]\n[[QA]]\n[[QA]]\n[[QA]]"
        , bench "mixed"       $ whnf (parseTomlDoc "") "q=42\nqa=[4,2,]\n[Q]qq=42\n[[QA]]\n[[QA]]"
        ]

      , bgroup "file"
        [ bench "example"     $ whnf (parseTomlDoc "") exampleToml
        , bench "repeated-4x" $ whnf (parseTomlDoc "") repeatedToml
        ]

      ]
