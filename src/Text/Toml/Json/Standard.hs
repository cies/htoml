{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Toml.Json.Standard where

import Text.Toml.Types
import Data.Aeson.Types


-- | 'ToJSON' instances for the 'Node' type that produce Aeson (JSON)
-- in line with the TOML specification.
instance ToJSON Node where
  toJSON (NTValue v) = toJSON v
  toJSON (NTable v)  = toJSON v
  toJSON (NTArray v) = toJSON v


-- | 'ToJSON' instances for the 'TValue' type that produce Aeson (JSON)
-- in line with the TOML specification.
instance ToJSON TValue where
  toJSON (VString v)   = toJSON v
  toJSON (VInteger v)  = toJSON v
  toJSON (VFloat v)    = toJSON v
  toJSON (VBoolean v)  = toJSON v
  toJSON (VDatetime v) = toJSON v
  toJSON (VArray v)    = toJSON v
