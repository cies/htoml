{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Json.BurntSushi where

import Text.Toml.Types
import Data.Aeson.Types


-- | 'ToJSON' instances for the 'Node' type that produce Aeson (JSON)
-- in line with BurntSushi's language agnostic TOML test suite.
instance ToJSON Node where
  toJSON (NTValue v) = toJSON v
  toJSON (NTable v)  = toJSON v
  toJSON (NTArray v) = toJSON v


-- | 'ToJSON' instances for the 'TValue' type that produce Aeson (JSON)
-- in line with BurntSushi's language agnostic TOML test suite.
--
-- BurntSushi's JSON encoding explicitly specifies the types of the values.
instance ToJSON TValue where
  toJSON (VString v)   = object [ "type"  .= toJSON ("string" :: String)
                                , "value" .= toJSON v ]
  toJSON (VInteger v)  = object [ "type"  .= toJSON ("integer" :: String)
                                , "value" .= toJSON (show v) ]
  toJSON (VFloat v)    = object [ "type"  .= toJSON ("float" :: String)
                                , "value" .= toJSON (show v) ]
  toJSON (VBoolean v)  = object [ "type"  .= toJSON ("bool" :: String)
                                , "value" .= toJSON (if v then "true" else "false" :: String) ]
  toJSON (VDatetime v) = object [ "type"  .= toJSON ("datetime" :: String)
                                , "value" .= toJSON (let s = show v
                                                         z = take (length s - 4) s  ++ "Z"
                                                         d = take (length z - 10) z
                                                         t = drop (length z - 9) z
                                                     in  d ++ "T" ++ t)]
  toJSON (VArray v)    = object [ "type"  .= toJSON ("array" :: String)
                                , "value" .= toJSON v ]
