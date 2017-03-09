{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------
-- |
-- Module    : Text.Toml.Pretty
-- Authors   : Johan Backman <johback@student.chalmers.se>
--             Hampus Ramström <hampusr@student.chalmers.se>
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
import           Data.Vector         as V
import           Data.Text           (Text)
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
ppTomlString str = doubleQuotes $ hcat $ Prelude.map ppChar (T.unpack str)
    where ppChar '\\' = text "\\\\"
          ppChar '\"' = text "\\\""
          ppChar c    = char c

ppInteger :: Integer -> Doc
ppInteger = integer

ppDateTime :: UTCTime -> Doc
ppDateTime t = text $ show f_date
    where f_date = formatTime defaultTimeLocale "%FT%TZ" t

ppFloat :: Double -> Doc
ppFloat = double

ppBoolean :: Bool -> Doc
ppBoolean True = text "true"
ppBoolean False = text "false"

-- Unclear with fsep, vcat
ppArray :: Vector Node -> Doc
ppArray va = brackets $ fsep $ punctuate comma $ Prelude.map ppNode (V.toList va)

ppTable :: Table -> Doc
ppTable t = vcat $ tableToList $ M.toList t

{-tableToList :: [(Text,Node)] -> Doc
tableToList (x:xs) = (title x) $$ (vcat $ (Prelude.map (fsep . f) xs))
    where f (x,y) = punctuate (space <> equals) [text $ T.unpack x,ppNode y]
          title (x,_) = brackets $ text $ T.unpack x-}

tableToList :: [(T.Text, Node)] -> [Doc]
tableToList = Prelude.map (fsep . f)
    where f (x, y) = punctuate (space <> equals) [text $ T.unpack x,ppNode y]

ppTArray :: Vector Table -> Doc
ppTArray vt = brackets $ fsep $ punctuate comma $ Prelude.map ppTable (V.toList vt)
