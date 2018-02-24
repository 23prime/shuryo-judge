{-# LANGUAGE OverloadedStrings #-}

module Csv where

import qualified Data.Text as T

type Csv = T.Text

-- CSV -> 配列
csvReader :: Csv -> [[T.Text]]
csvReader = map (T.split (== ',')) . T.lines

-- CSV の各要素に "" がついてる場合
csvReader' :: Csv -> [[T.Text]]
csvReader' = map (T.split (== ',')) . T.lines . killQuot

killQuot :: T.Text -> T.Text
killQuot = T.replace  "\"" ""

-- 配列 -> CSV
csvWriter :: [[T.Text]] -> Csv
csvWriter = T.unlines . map concatByCommas

-- [T.Text] にカンマを入れて Csv に
concatByCommas :: [T.Text] -> Csv
concatByCommas []       = ""
concatByCommas (x : xs) = T.concat $ x : map (',' `T.cons`) xs
