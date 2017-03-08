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

import           Data.Text           (Text)
import qualified Data.Text           as T

ppNode :: Node -> Doc
ppNode n = case n of
    (VTable v)    -> undefined
    (VTArray v)   -> undefined
    (VString v)   -> ppTomlString v
    (VInteger v)  -> undefined
    (VFloat v)    -> undefined
    (VBoolean v)  -> undefined
    (VDatetime v) -> undefined
    (VArray v)    -> undefined

ppTomlString :: T.Text -> Doc
ppTomlString str = doubleQuotes $ hcat $ map pp_char (T.unpack str)
    where pp_char '\\' = text "\\\\"
          pp_char '\"' = text "\\\""
          pp_char c    = char c

