module Csv where

import           Data.List

type Csv = String

split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split p xs
  | null s    = (f, s)
  | otherwise = (f, tail s)
  where
    (f, s) = break p xs

splitAll :: (a -> Bool) -> [a] -> [[a]]
splitAll _ [] = []
splitAll p xs = h : splitAll p ts
  where
    (h, ts) = split p xs

-- CSV -> 配列
csvReader :: Csv -> [[String]]
csvReader = map (splitAll (== ',')) . lines

-- CSV の各要素に "" がついてる場合
csvReader' :: Csv -> [[String]]
csvReader' = map (splitAll (== ',')) . lines . killQuot

killQuot :: String -> String
killQuot = filter (/= '\"')

-- 配列 -> CSV
csvWriter :: [[String]] -> Csv
csvWriter = unlines . map concatByCommas

-- [String] にカンマを入れて Csv に
concatByCommas :: [String] -> Csv
concatByCommas []       = []
concatByCommas (x : xs) = concat $ x : map (',' :) xs
