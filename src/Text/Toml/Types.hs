{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Types where

import qualified Data.HashMap.Strict as M
-- TODO: measure the improvement of Vector over List
-- import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format()
import Data.List (intersect)


-- | The 'Table' is a mapping ('HashMap') of 'Text' keys to 'Node' values.
type Table = M.HashMap Text Node


-- | A 'Node' may contain a 'TValue', a 'Table' or a table array '[Table]'.
data Node = NTValue TValue
          | NTable  Table
          | NTArray [Table]
  deriving (Eq, Show)


-- | A 'TValue' may contain any type of value that can put in a 'VArray'.
data TValue = VString   Text
            | VInteger  Integer
            | VFloat    Double
            | VBoolean  Bool
            | VDatetime UTCTime
            | VArray    [TValue]
  deriving (Eq, Show)


-- | Contruct an empty 'Table'.
emptyTable :: Table
emptyTable = M.empty


-- | Contruct an empty 'NTable'.
emptyNTable :: Node
emptyNTable = NTable M.empty


-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contian those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: Table -> Table -> Either [Text] Table
merge existing new = case intersect (M.keys existing) (M.keys new) of
                       [] -> Right $ M.union existing new
                       ds -> Left  $ ds


-- | Inserts a table ('Table') with name ('[Text]') which may be part of
-- a table array (when 'Bool' is 'True') into a 'Table'.
-- It may result in an error ('Text') on the 'Left' or a modified table
-- on the 'Right'.
insert :: ([Text], Node) -> Table -> Either Text Table
insert ([], _)         _ = error "FATAL: Cannot call 'insert' without a name."
insert (_ , NTValue _) _ = error "FATAL: Cannot call 'insert' with a TValue."
insert ([name], node) ttbl =
    -- ^ In case 'name' is final
    case M.lookup name ttbl of
      Nothing           -> Right $ M.insert name node ttbl
      Just (NTable t)   -> case node of
        (NTable nt) -> case merge t nt of
          Left ds -> Left $ T.concat [ "Cannot redefine key(s) (", (T.intercalate ", " ds)
                                     , "), from table named '", name, "'." ]
          Right r -> Right $ M.insert name (NTable r) ttbl
        (NTArray _) -> commonInsertError node [name]
      Just (NTArray a)  -> case node of
        (NTable _) -> commonInsertError node [name]
        (NTArray na) -> Right $ M.insert name (NTArray $ a ++ na) ttbl
      Just _            -> commonInsertError node [name]
insert (fullName@(name:ns), node) ttbl =
    -- ^ In case 'name' is not final, but a sub-name
    case M.lookup name ttbl of
      Nothing           -> case insert (ns, node) emptyTable of
                             Left msg -> Left msg
                             Right r  -> Right $ M.insert name (NTable r) ttbl
      Just (NTable t)   -> case insert (ns, node) t of
                             Left msg -> Left msg
                             Right tt -> Right $ M.insert name (NTable tt) ttbl
      Just (NTArray []) -> error "FATAL: Call to 'insert' found impossibly empty NTArray."
      Just (NTArray a)  -> case insert (ns, node) (last a) of
                             Left msg -> Left msg
                             Right t  -> Right $ M.insert name (NTArray $ (init a) ++ [t]) ttbl
      Just _            -> commonInsertError node fullName


-- | Convenience function to construct a common error message for the 'insert' function.
commonInsertError :: Node -> [Text] -> Either Text Table
commonInsertError what name =
  let w = case what of (NTable _) -> "tables"
                       _          -> "array of tables"
      n = T.intercalate "." name
  in  Left $ T.concat ["Cannot insert ", w, " '", n, "' as key already exists."]
