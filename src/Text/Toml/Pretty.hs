{-# LANGUAGE OverloadedStrings #-}
-- -----------------------------------------------------------------
-- |
-- Module    : Text.Toml.Pretty
-- Authors   : Johan Backman      <johback@student.chalmers.se>
--             Hampus Ramström    <hampusr@student.chalmers.se>
--
-- Display TOML nodes using pretty printing combinators.
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
    (VTArray v)   -> ppTArray v ""
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
ppBoolean True  = text "true"
ppBoolean False = text "false"

ppArray :: V.Vector Node -> Doc
ppArray va = brackets $ fsep $ punctuate comma $ map ppNode (V.toList va)

ppTable :: Table -> Doc
ppTable tb = findTTitle (M.toList tb) [text ""]
    where
        findTTitle []                    ti = brackets $ hcat ti
        -- findTTitle ((t, VTArray v) : xs) ti = brackets (brackets $ text $ T.unpack t) $$ (hcat $ map (\x -> findTTitle x [text $ T.unpack t]) (map M.toList $ V.toList v)) $$ findTTitle xs [text $ T.unpack t]
        findTTitle ((t, VTArray v) : xs) ti = ppTArray v t
        findTTitle [(t, VTable v)]       ti =
            findTTitle (M.toList v) $ ti ++ [text $ T.unpack t]
        findTTitle ((t, VTable v) : xs)  ti =
            (findTTitle (M.toList v) $ ti ++ [text $ T.unpack t]) $$ findTTitle xs ti
        findTTitle v                    [_] = vcat (tableToList v)
        findTTitle v                     ti =
            (brackets (hcat $ punctuate (char '.') (tail ti))) $$ vcat (tableToList v)

tableToList :: [(T.Text, Node)] -> [Doc]
tableToList = map (fsep . f)
    where f (x, y) = punctuate (space <> equals) [text $ T.unpack x, ppNode y]

ppTArray :: V.Vector Table -> T.Text -> Doc
ppTArray v t = vcat $ map (\x -> doubleBracket pt $$ ppTable x) (V.toList v)
    where pt              = text $ T.unpack t
          doubleBracket x = brackets $ brackets x







