{-# LANGUAGE OverloadedStrings #-}

module Text.Toml.Types where

import Data.Map (Map)
import Data.List (findIndex)
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format()


-- | Type of the Toml document.
-- Differs from a 'TableNode' in that it is not "named".
-- A 'TableArray' is not possible at top level.
-- When no key-value pairs are defined on the top level,
-- it simply contains an empty 'Table'.
data TomlDoc = TomlDoc Table [TableNode]
  deriving (Eq, Ord, Show)


-- | A node in the namespace tree.
-- It is named by the 'Text' value and may contain
-- a 'Table', a 'TableArray' or 'Nothing'.
-- 'Nothing' designates that the particular name has
-- not yet been taken.
data TableNode = TableNode
    Text
    (Maybe (Either Table TableArray))
    [TableNode]
  deriving (Eq, Ord, Show)


-- | A 'Table' of key-value pairs.
type Table = Map Text Value


-- | A 'TableArray' is simply a list of 'Table's.
type TableArray = [Table]


-- | Insert a 'Table' with the name '[Text]' into a 'TablNode', either as
-- a 'Table' or as a 'TableArray'.
insert :: ([Text], Table, Bool) -> [TableNode] -> Either String [TableNode]
insert ([], _, _) _ = Left $ "Cannot call 'insert' without a name."
insert ([name], tbl, isArray) nodes =
    let content = Just $ if isArray then Right [tbl] else Left tbl
    in case idxAndNodeWithName name nodes of
      -- Nothing with the same name at this level: append new node here
      Nothing -> Right $ nodes ++ [TableNode name content []]
      Just (idx, TableNode _ c branches) -> case c of
        -- Node exists, but not explicitly defined: insert content
        Nothing -> return $ replaceItem idx (TableNode name content branches) nodes
        Just cc -> if not isArray
          -- Node explicitly defined and inserting a 'Table': error out
          then Left $ "Cannot insert " ++ unpack name ++ ", as it is already defined."
          else case cc of
            -- Node explicitly defined and of type 'Table': error out
            Left _ -> Left $ "Cannot insert " ++ unpack name ++ ", as it is already defined."
            -- Node explicitly defined and of type 'TableArray': append to array
            Right tArray ->
              let newNode = TableNode name (Just . Right $ tArray ++ [tbl]) branches
              in  Right $ replaceItem idx newNode nodes
insert ((name:ns), tbl, isArray) nodes =
    case idxAndNodeWithName name nodes of
      -- Nothing with the same name at this level: append implicit node and recurse
      Nothing -> case insert (ns, tbl, isArray) [] of
                   Left msg -> Left msg
                   Right r  -> Right $ nodes ++ [TableNode name Nothing r]
      Just (idx, TableNode _ c branches) -> case c of
        -- Node exists, but not explicitly defined: recurse into existing node
        Nothing -> case insert (ns, tbl, isArray) branches of
                     Left msg -> Left msg
                     Right r  -> Right $ replaceItem idx (TableNode name c r) nodes
        -- Node has already been explicitly defined: error out
        Just _ -> Left $ "Cannot insert " ++ unpack name ++ ", as it is already defined."


-- | Maybe get a tuple of the index and the node ('TableNode') from a 'TableNode' list.
idxAndNodeWithName :: Text -> [TableNode] -> Maybe (Int, TableNode)
idxAndNodeWithName name nodes = fmap (\i -> (i, nodes !! i)) (idxOfName name nodes)
  where
    idxOfName n = findIndex (\(TableNode nn _ _) -> n == nn)


-- | Replace the 'TableNode' from a list pointed by the index.
replaceItem :: Int -> a -> [a] -> [a]
replaceItem idx x xs = concat [take idx xs, [x], drop (idx + 1) xs]


-- | The 'Value' of a key-value pair.
data Value = VString   Text
           | VInteger  Integer
           | VFloat    Double
           | VBoolean  Bool
           | VDatetime UTCTime
           | VArray    [Value]
  deriving (Eq, Ord, Show)
