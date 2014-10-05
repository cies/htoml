module Text.TOML.Value
  ( TOML (..)
  , TOMLV (..)
  , Value
  , tempty
  , tinsert
  , liftT
  , liftTV
  ) where


import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Format()


type Value = Either TOML TOMLV

newtype TOML = TOML (Map Text Value)
  deriving ( Eq, Ord, Show )

data TOMLV = VString Text
           | VInteger Integer
           | VDouble Double
           | VBool Bool
           | VArray [TOMLV]
           | VDocument TOML
           | VDate UTCTime
  deriving ( Eq, Ord, Show )

tempty :: TOML
tempty = TOML M.empty

liftT :: (Map Text Value -> Map Text Value) -> TOML -> TOML
liftT f (TOML m) = (TOML $ f m)

liftTV :: (TOML -> TOML) -> Value -> Value
liftTV f (Left  t) = Left $ f t
liftTV f (Right _) = Left $ f tempty

tinsert :: Text -> Value -> TOML -> TOML
tinsert k v t = liftT (M.insert k v) t
