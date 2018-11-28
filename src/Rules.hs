{-# LANGUAGE OverloadedStrings #-}

module Rules where

import           Control.Exception.Safe
import           Data.List              (elemIndex)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T

import           Types

type Require = [(Group, CreditNum)]

-----------
-- Rules --
-----------
require :: Require
require = [ ("基礎科目", 1.0)
          , ("専攻共通 必修", 1.0)
          , ("専攻共通 選択", 4.0)
          , ("教科教育（数学教育）", 6.0)
          , ("教科専門（数学）", 12.0)
          , ("教科選択（研究）", 6.0)
          , ("その他", 0.0)
          ]

require' :: Require
require' = [ ("基礎科目", 1.0)
           , ("専攻共通 必修", 1.0)
           , ("専攻共通 選択", 4.0)
           , ("教科教育（数学教育）", 6.0)
           , ("教科専門（数学）", 12.0)
           , ("教科選択（研究）", 0.0)
           , ("その他", 0.0)
           ]

mkGroup :: Code -> Group
mkGroup cd
  | f /= "01B"  = "その他"
  | a == "1001" = "基礎科目"
  | a == "1011" = "専攻共通 必修"
  | b == '2'    = "専攻共通 選択"
  | b /= '6'    = "その他"
  | c == '1'    = "教科教育（数学教育）"
  | c == '5'    = "教科選択（研究）"
  | otherwise   = "教科専門（数学）"
  where
    (f, a) = T.splitAt 3 cd
    b      = T.head a
    cs     = T.tail a
    c      = T.head cs

--------------------------
-- Parse CSV -> Credits --
--------------------------
mkCredit :: [T.Text] -> [T.Text] -> Credit
mkCredit ts ds = let cd = findData "科目番号"
                 in Credit { code   = cd
                           , title  = findData "科目名"
                           , number = case reads (T.unpack $ findData "単位数") of
                                        [(sd, "")] -> sd
                                        _          -> 0
                           , grade  = findData "総合評価"
                           , group  = mkGroup cd
                           }
  where
    findData :: T.Text -> T.Text
    findData term = let idx = elemIndex term ts
                    in case idx of
                         Just i -> ds !! i
                         _      -> ""
