module Text.TOML.Value
  ( TOML (..)
  , TOMLV (..)
  )where

import Data.Map ( Map )


newtype TOML = TOML (Map String (Either TOML TOMLV))
    deriving ( Eq, Ord, Show )

data TOMLV
    = VString String
    | VInteger Integer
    | VDouble Double
    | VBool Bool
    | VArray [TOMLV]
    | VDocument TOML
    | VDate -- TODO
    deriving ( Eq, Ord, Show )

