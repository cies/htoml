module Text.TOML.Value
  ( TOML (..)
  , TOMLV (..)
  , Value
  , tempty
  , tinsert
  , liftT
  , liftTV
  ) where


import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format()


type Value = Either TOML TOMLV

newtype TOML = TOML (Map T.Text Value)
  deriving ( Eq, Ord, Show )

data TOMLV = VString T.Text
           | VInteger Integer
           | VDouble Double
           | VBool Bool
           | VArray [TOMLV]
           | VDocument TOML
           | VDate UTCTime
  deriving ( Eq, Ord, Show )

tempty :: TOML
tempty = TOML M.empty

liftT :: (Map T.Text Value -> Map T.Text Value) -> TOML -> TOML
liftT f (TOML m) = (TOML $ f m)

liftTV :: (TOML -> TOML) -> Value -> Value
liftTV f (Left  t) = Left $ f t
liftTV f (Right _) = Left $ f tempty

tinsert :: T.Text -> Value -> TOML -> TOML
tinsert k v t = liftT (M.insert k v) t
