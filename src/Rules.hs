{-# LANGUAGE OverloadedStrings #-}

module Rules where

import           Control.Exception.Safe
import           Data.List              (elemIndex)
import           Data.Maybe             (fromMaybe)
import qualified Data.Text              as T

import           Types


-----------
-- Rules --
-----------
require :: CreditMap
require = [ ("基礎科目", 1.0)
          , ("専攻共通 必修", 1.0)
          , ("専攻共通 選択", 4.0)
          , ("教科教育（数学教育）", 6.0)
          , ("教科専門（数学）", 12.0)
          , ("教科選択（研究）", 6.0)
          , ("その他", 0.0)
          ]

require' :: CreditMap
require' = [ ("基礎科目", 1.0)
           , ("専攻共通 必修", 1.0)
           , ("専攻共通 選択", 4.0)
           , ("教科教育（数学教育）", 6.0)
           , ("教科専門（数学）", 12.0)
           , ("教科選択（研究）", 0.0)
           , ("その他", 0.0)
           ]

parseCode :: Code -> Group
parseCode cd
  | f   /= "01B"  = "その他"
  | s   == "1001" = "基礎科目"
  | s   == "1011" = "専攻共通 必修"
  | hs  == "2"    = "専攻共通 選択"
  | hs  /= "6"    = "その他"
  | hts == '1'    = "教科教育（数学教育）"
  | hts == '5'    = "教科選択（研究）"
  | otherwise     = "教科専門（数学）"
  where
    (f, s )  = T.splitAt 3 cd
    (hs, ts) = T.splitAt 1 s
    hts      = T.head ts

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
                           , group  = parseCode cd
                           }
  where
    findData :: T.Text -> T.Text
    findData term = let idx = elemIndex term ts
                    in case idx of
                         Just i -> ds !! i
                         _      -> ""

