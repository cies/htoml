{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------
-- |
-- Module    : Text.Toml.Pretty
-- Authors   : Johan Backman      <johback@student.chalmers.se>
--             Hampus Ramström    <hampusr@student.chalmers.se>
--
-- Display TOML values using pretty printing combinators.
-- -----------------------------------------------------------------

module Text.Toml.Pretty (
    module Text.Toml.Pretty,
    module Text.PrettyPrint.HughesPJ,
    ) where

import Text.Toml.Types
import Text.PrettyPrint.HughesPJ

import qualified Data.HashMap.Strict as M
import qualified Data.Vector         as V
import           Data.Time.Clock     (UTCTime)
import           Data.Time.Format    (formatTime, defaultTimeLocale)
import qualified Data.Text           as T

ppNode :: Node -> Doc
ppNode n = case n of
    (VTable v)    -> ppTable v
    (VTArray v)   -> ppTArray v
    (VString v)   -> ppTomlString v
    (VInteger v)  -> ppInteger $ fromIntegral v
    (VFloat v)    -> ppFloat v
    (VBoolean v)  -> ppBoolean v
    (VDatetime v) -> ppDateTime v
    (VArray v)    -> ppArray v

ppTomlString :: T.Text -> Doc
ppTomlString str = doubleQuotes $ hcat $ map ppChar (T.unpack str)
    where ppChar '\\' = text "\\\\"
          ppChar '\"' = text "\\\""
          ppChar c    = char c

ppDateTime :: UTCTime -> Doc
ppDateTime t = hcat $ map ppDate (show f_date)
    where f_date      = formatTime defaultTimeLocale "%FT%TZ" t
          ppDate '\"' = text ""
          ppDate c    = char c

ppInteger :: Integer -> Doc
ppInteger = integer

ppFloat :: Double -> Doc
ppFloat = double

ppBoolean :: Bool -> Doc
ppBoolean True = text "true"
ppBoolean False = text "false"

-- Unclear with fsep, vcat
ppArray :: V.Vector Node -> Doc
ppArray va = brackets $ fsep $ punctuate comma $ map ppNode (V.toList va)

ppTable :: Table -> Doc
ppTable t = vcat $ tableToList $ M.toList t

tableToList :: [(T.Text, Node)] -> [Doc]
tableToList = map (fsep . f)
    where f (x, y) = punctuate equals [text $ T.unpack x,ppNode y]

ppTArray :: V.Vector Table -> Doc
ppTArray vt = brackets $ fsep $ punctuate comma $ map ppTable (V.toList vt)
