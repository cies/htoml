{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Text.Toml.Types (
    Table
  , Node (..)
  , ToBsJSON (..)
  , ExplicitNess (..)
  , emptyTable
  , insert
  ) where

import           Control.Monad.State

import           Data.Aeson.Types
import qualified Data.HashMap.Strict as M
import           Data.Int            (Int64)
import           Data.List           (intersect)
import           Data.Set (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    ()
import qualified Data.Vector         as V

import           Text.Parsec hiding (State)

-- | The 'Table' is a mapping ('HashMap') of 'Text' keys to 'Node' values.
type Table = M.HashMap Text Node

-- | A 'Node' may contain any type of value that can put in a 'VArray'.
data Node = VTable    Table
          | VTArray   [Table]
          | VString   !Text
          | VInteger  !Int64
          | VFloat    !Double
          | VBoolean  !Bool
          | VDatetime !UTCTime
          | VArray    [Node]
  deriving (Eq, Show)

data ExplicitNess = Explicit | Implicit

-- | Contruct an empty 'Table'.
emptyTable :: Table
emptyTable = M.empty

-- | Inserts a table ('Table') with name ('[Text]') which may be part of
-- a table array into a 'Table'.
-- It may result in an error in the ParsecT Monad for redefinitions.
insert :: ExplicitNess -> ([Text], Node) -> Table -> ParsecT Text () (State (Set [Text])) Table
insert _ ([], _)         _ = parserFail "FATAL: Cannot call 'insert' without a name."
insert explicit ([name], node) ttbl =
    -- In case 'name' is final
    case M.lookup name ttbl of
      Nothing           -> do
        updateExplicts explicit [name] node
        return $ M.insert name node ttbl
      Just (VTable t)   -> case node of
        (VTable nt) -> case merge t nt of
          Left ds -> nameInsertError ds name
          Right r -> do
            testAndUpdateExplicts explicit [name] node
            return $ M.insert name (VTable r) ttbl
        _         -> commonInsertError node [name]
      Just (VTArray a)  -> case node of
        (VTArray na) -> return $ M.insert name (VTArray $ a ++ na) ttbl
        _            -> commonInsertError node [name]
      Just _            -> commonInsertError node [name]
insert explicit (fullName@(name:ns), node) ttbl =
    -- In case 'name' is not final, but a sub-name
    case M.lookup name ttbl of
      Nothing           -> do
        r <- insert Implicit (ns, node) emptyTable
        updateExplicts explicit fullName node
        return $ M.insert name (VTable r) ttbl
      Just (VTable t)   -> do
        r <- insert Implicit (ns, node) t
        testAndUpdateExplicts explicit fullName node
        return $ M.insert name (VTable r) ttbl
      Just (VTArray []) -> parserFail "FATAL: Call to 'insert' found impossibly empty VArray."
      Just (VTArray a)  -> do
        r <- insert Implicit (ns, node) (last a)
        return $ M.insert name (VTArray $ (init a) ++ [r]) ttbl
      Just _            -> commonInsertError node fullName

-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contian those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: Table -> Table -> Either [Text] Table
merge existing new = case M.keys existing `intersect` M.keys new of
                       [] -> Right $ M.union existing new
                       ds -> Left  $ ds

nameInsertError :: [Text] -> Text -> ParsecT Text () m a
nameInsertError ns name = parserFail . T.unpack $ T.concat
    [ "Cannot redefine key(s) (", T.intercalate ", " ns
    , "), from table named '", name, "'." ]

commonInsertError :: Monad m => Node -> [Text] -> ParsecT Text () m a
commonInsertError what name = parserFail . concat $ case what of
    _         -> ["Cannot insert ", w, " '", n, "' as key already exists."]
  where
    n = T.unpack $ T.intercalate "." name
    w = case what of (VTable _)  -> "tables"
                     _           -> "array of tables"

testAndUpdateExplicts :: ExplicitNess -> [Text] -> Node -> ParsecT Text () (State (Set [Text])) ()
testAndUpdateExplicts Explicit name node@(VTable _) = do
  alreadyDefinedExplicity <- lift get
  if S.member name alreadyDefinedExplicity
    then commonInsertError node name
    else return ()
  lift $ put $ S.insert name alreadyDefinedExplicity
testAndUpdateExplicts _ _ _ = return ()

updateExplicts :: ExplicitNess -> [Text] -> Node -> ParsecT Text () (State (Set [Text])) ()
updateExplicts Explicit name (VTable _) = lift . modify $ S.insert name
updateExplicts _ _ _ = return ()


-- * Regular ToJSON instances

-- | 'ToJSON' instances for the 'Node' type that produce Aeson (JSON)
-- in line with the TOML specification.
instance ToJSON Node where
  toJSON (VTable v)    = toJSON v
  toJSON (VTArray v)   = toJSON v
  toJSON (VString v)   = toJSON v
  toJSON (VInteger v)  = toJSON v
  toJSON (VFloat v)    = toJSON v
  toJSON (VBoolean v)  = toJSON v
  toJSON (VDatetime v) = toJSON v
  toJSON (VArray v)    = toJSON v



-- * Special BurntSushi ToJSON type class and instances

-- | Type class for conversion to BurntSushi-style JSON.
--
-- BurntSushi has made a language agnostic test suite available that
-- this library uses. This test suit expects that values are encoded
-- as JSON objects with a 'type' and a 'value' member.
class ToBsJSON a where
  toBsJSON :: a -> Value


-- | Provide a 'toBsJSON' instance to the 'NTArray'.
instance (ToBsJSON a) => ToBsJSON [a] where
  toBsJSON = Array . V.fromList . map toBsJSON
  {-# INLINE toBsJSON #-}


-- | Provide a 'toBsJSON' instance to the 'NTable'.
instance (ToBsJSON v) => ToBsJSON (M.HashMap Text v) where
  toBsJSON = Object . M.map toBsJSON
  {-# INLINE toBsJSON #-}

-- | 'ToBsJSON' instances for the 'TValue' type that produce Aeson (JSON)
-- in line with BurntSushi's language agnostic TOML test suite.
--
-- As seen in this function, BurntSushi's JSON encoding explicitly
-- specifies the types of the values.
instance ToBsJSON Node where
  toBsJSON (VTable v)    = toBsJSON v
  toBsJSON (VTArray v)   = toBsJSON v
  toBsJSON (VString v)   = object [ "type"  .= toJSON ("string" :: String)
                                  , "value" .= toJSON v ]
  toBsJSON (VInteger v)  = object [ "type"  .= toJSON ("integer" :: String)
                                  , "value" .= toJSON (show v) ]
  toBsJSON (VFloat v)    = object [ "type"  .= toJSON ("float" :: String)
                                  , "value" .= toJSON (show v) ]
  toBsJSON (VBoolean v)  = object [ "type"  .= toJSON ("bool" :: String)
                                  , "value" .= toJSON (if v then "true" else "false" :: String) ]
  toBsJSON (VDatetime v) = object [ "type"  .= toJSON ("datetime" :: String)
                                  , "value" .= toJSON (let s = show v
                                                           z = take (length s - 4)  s ++ "Z"
                                                           d = take (length z - 10) z
                                                           t = drop (length z - 9)  z
                                                       in  d ++ "T" ++ t) ]
  toBsJSON (VArray v)    = object [ "type"  .= toJSON ("array" :: String)
                                  , "value" .= toBsJSON v ]
