{-# LANGUAGE OverloadedStrings #-}

module Text.TOML
  ( parseMaybe
  , parseEither
  ) where

import Data.Attoparsec.Text (Result, parse, feed, maybeResult, eitherResult)
import Data.Text (Text)
import Data.List (foldl', groupBy)
import Data.Either (rights)
import qualified Data.Map as M

import Text.TOML.Parser


-- | Parse a 'Text' that 'Maybe' results in an internal representation
-- of the document in the 'TOML' data type.
parseMaybe:: Text -> Maybe TOML
parseMaybe bs = process `fmap` (maybeResult . parseTomlDoc) bs

-- | Parse a 'Text' that results in 'Either' a 'String'
-- containing the error message, or an internal representation
-- of the document in the 'TOML' data type.
parseEither :: Text -> Either String TOML
parseEither bs = case eitherResult . parseTomlDoc $ bs of
                   Left  e  -> Left e
                   Right ts -> Right $ process ts


-- | Parse a TOML document as 'Text' to 'Result' a list of 'Token's.
-- The 'Token's need to be processed further in order to be properly structured.
parseTomlDoc :: Text -> Result [Token]
parseTomlDoc bs = feed (parse document bs) ""

-- | Process a list of 'Token's to a properly structured 'TOML'.
process :: [Token] -> TOML
process ts = go (group ts) tempty
  where
    go []             m = m
    go ((ks, kvs):gs) m = go gs (okalter ks kvs m)

    okalter :: [Text] -> [(Text, TOMLV)] -> TOML -> TOML
    okalter []     kvs t = insertMany kvs t
    okalter (k:ks) kvs t = liftT (M.alter (Just . f) k) t
      where f Nothing   = liftTV (okalter ks kvs) (Left tempty)
            f (Just t') = liftTV (okalter ks kvs) t'

    insertMany :: [(Text, TOMLV)] -> TOML -> TOML
    insertMany kvs m = foldl' (flip $ uncurry tinsert) m kvs'
      where kvs' = [(k, Right v) | (k, v) <- kvs]

-- NB: groupBy will never produce an empty group.
group :: [Either [t1] t] -> [([t1], [t])]
group ts = alternate $ (map omg) $ (groupBy right ts)
  where
    omg    ((Left l):_)  = Left l
    omg rs@((Right _):_) = Right (rights rs)
    -- Only key-value pairs are grouped together
    right (Right _) (Right _) = True
    right _         _         = False

    -- If the token list starts with a Right, then there are key-value pairs that
    -- don't belong to a keygroup. Assign that one the 'empty' keygroup, and match
    -- pairs. If the token list starts with a right, then there are no "global"
    -- key-value pairs, and it's ok to straight zip the partition.
    alternate                          []  = []
    alternate ((Left l)              : []) = (l , []) : []
    alternate ((Right r)             : gs) = ([], r ) : (alternate gs)
    alternate ((Left l ) : (Right r) : gs) = (l , r ) : (alternate gs)
    alternate ((Left l1) : (Left l2) : gs) = (l1, []) : (alternate $ (Left l2) : gs)
