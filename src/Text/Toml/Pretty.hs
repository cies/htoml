{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.Toml.Pretty
--
-- Display TOML values using pretty printing combinators.

module Text.Toml.Pretty (
    module Text.Toml.Pretty,
    module Text.PrettyPrint.HughesPJ,
    ) where

import Text.Toml.Types
import Text.PrettyPrint.HughesPJ

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Vector         as V
import           Data.Text           (Text)
import qualified Data.Text           as T

ppNode :: Node -> Doc
ppNode n = case n of
    (VTable v)    -> ppTable v
    (VTArray v)   -> undefined
    (VString v)   -> ppTomlString v
    (VInteger v)  -> undefined
    (VFloat v)    -> ppFloat v
    (VBoolean v)  -> ppBoolean v
    (VDatetime v) -> undefined
    (VArray v)    -> ppArray v

ppTomlString :: T.Text -> Doc
ppTomlString str = doubleQuotes $ hcat $ Prelude.map pp_char (T.unpack str)
    where pp_char '\\' = text "\\\\"
          pp_char '\"' = text "\\\""
          pp_char c    = char c

ppFloat :: Double -> Doc
ppFloat f = double f

ppBoolean :: Bool -> Doc
ppBoolean True = text "true"
ppBoolean False = text "false"

-- Unclear with fsep, vcat
ppArray :: Vector Node -> Doc
ppArray va = brackets $ fsep $ punctuate comma $ Prelude.map ppNode (V.toList va)

ppTable :: Table -> Doc
ppTable t = vcat $ tableToList $ M.toList t

tableToList :: [(Text,Node)] -> [Doc]
tableToList t = Prelude.map fsep (Prelude.map f t)
    where f (x,y) = punctuate equals [text $ T.unpack x,ppNode y]