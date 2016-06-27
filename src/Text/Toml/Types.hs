{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Types
  ( Table
  , emptyTable
  , VTArray
  , VArray
  , Node (..)
  , Explicitness (..)
  , isExplicit
  , insert
  , ToJSON (..)
  , ToBsJSON (..)
  ) where

import           Control.Monad       (when)
import           Text.Parsec
import           Data.Aeson.Types
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Int            (Int64)
import           Data.List           (intersect)
import           Data.Set (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    ()
import           Data.Vector         (Vector)
import qualified Data.Vector         as V


-- | The TOML 'Table' is a mapping ('HashMap') of 'Text' keys to 'Node' values.
type Table = HashMap Text Node

-- | Contruct an empty 'Table'.
emptyTable :: Table
emptyTable = M.empty

-- | An array of 'Table's, implemented using a 'Vector'.
type VTArray = Vector Table

-- | A \"value\" array that may contain zero or more 'Node's, implemented using a 'Vector'.
type VArray = Vector Node

-- | A 'Node' may contain any type of value that may be put in a 'VArray'.
data Node = VTable    !Table
          | VTArray   !VTArray
          | VString   !Text
          | VInteger  !Int64
          | VFloat    !Double
          | VBoolean  !Bool
          | VDatetime !UTCTime
          | VArray    !VArray
  deriving (Eq, Show)

-- | To mark whether or not a 'Table' has been explicitly defined.
-- See: https://github.com/toml-lang/toml/issues/376
data Explicitness = Explicit | Implicit
  deriving (Eq, Show)

-- | Convenience function to get a boolean value.
isExplicit :: Explicitness -> Bool
isExplicit Explicit = True
isExplicit Implicit = False


-- | Inserts a table, 'Table', with the namespaced name, '[Text]', (which
-- may be part of a table array) into a 'Table'.
-- It may result in an error in the 'ParsecT' monad for redefinitions.
insert :: Explicitness -> ([Text], Node) -> Table -> Parsec Text (Set [Text]) Table
insert _ ([], _) _ = parserFail "FATAL: Cannot call 'insert' without a name."
insert ex ([name], node) ttbl =
    -- In case 'name' is final (a top-level name)
    case M.lookup name ttbl of
      Nothing -> do when (isExplicit ex) $ updateExState [name] node
                    return $ M.insert name node ttbl
      Just (VTable t) -> case node of
          (VTable nt) -> case merge t nt of
                  Left ds -> nameInsertError ds name
                  Right r -> do when (isExplicit ex) $
                                  updateExStateOrError [name] node
                                return $ M.insert name (VTable r) ttbl
          _ -> commonInsertError node [name]
      Just (VTArray a) -> case node of
          (VTArray na) -> return $ M.insert name (VTArray $ a V.++ na) ttbl
          _ -> commonInsertError node [name]
      Just _ -> commonInsertError node [name]
insert ex (fullName@(name:ns), node) ttbl =
    -- In case 'name' is not final (not a top-level name)
    case M.lookup name ttbl of
      Nothing -> do
          r <- insert Implicit (ns, node) emptyTable
          when (isExplicit ex) $ updateExState fullName node
          return $ M.insert name (VTable r) ttbl
      Just (VTable t) -> do
          r <- insert Implicit (ns, node) t
          when (isExplicit ex) $ updateExStateOrError fullName node
          return $ M.insert name (VTable r) ttbl
      Just (VTArray a) ->
          if V.null a
          then parserFail "FATAL: Call to 'insert' found impossibly empty VArray."
          else do r <- insert Implicit (ns, node) (V.last a)
                  return $ M.insert name (VTArray $ (V.init a) `V.snoc` r) ttbl
      Just _ -> commonInsertError node fullName


-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contain those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: Table -> Table -> Either [Text] Table
merge existing new = case M.keys existing `intersect` M.keys new of
                       [] -> Right $ M.union existing new
                       ds -> Left  $ ds

-- TOML tables maybe redefined when first definition was implicit.
-- For instance a top-level table `a` can implicitly defined by defining a non top-level
-- table `b` under it (namely with `[a.b]`). Once the table `a` is subsequently defined
-- explicitly (namely with `[a]`), it is then not possible to (re-)define it again.
-- A parser state of all explicitly defined tables is maintained, which allows
-- raising errors for illegal redefinitions of such.
updateExStateOrError :: [Text] -> Node -> Parsec Text (Set [Text]) ()
updateExStateOrError name node@(VTable _) = do
    explicitlyDefinedNames <- getState
    when (S.member name explicitlyDefinedNames) $ tableClashError name
    updateExState name node
updateExStateOrError _ _ = return ()

-- | Like 'updateExStateOrError' but does not raise errors. Only use this when sure
-- that redefinitions cannot occur.
updateExState :: [Text] -> Node -> Parsec Text (S.Set [Text]) ()
updateExState name (VTable _) = modifyState $ S.insert name
updateExState _ _ = return ()


-- * Parse errors resulting from invalid TOML

-- | Key(s) redefintion error.
nameInsertError :: [Text] -> Text -> Parsec Text (Set [Text]) a
nameInsertError ns name = parserFail . T.unpack $ T.concat
    [ "Cannot redefine key(s) (", T.intercalate ", " ns
    , "), from table named '", name, "'." ]

-- | Table redefinition error.
tableClashError :: [Text] -> Parsec Text (Set [Text]) a
tableClashError name = parserFail . T.unpack $ T.concat
    [ "Cannot redefine table named: '", T.intercalate "." name, "'." ]

-- | Common redefinition error.
commonInsertError :: Node -> [Text] -> Parsec Text (Set [Text]) a
commonInsertError what name = parserFail . concat $
    [ "Cannot insert ", w, " as '", n, "' since key already exists." ]
  where
    n = T.unpack $ T.intercalate "." name
    w = case what of (VTable _) -> "tables"
                     _          -> "array of tables"


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

-- | Provide a 'toBsJSON' instance to the 'VTArray'.
instance (ToBsJSON a) => ToBsJSON (Vector a) where
  toBsJSON = Array . V.map toBsJSON
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
