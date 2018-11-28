{-# LANGUAGE OverloadedStrings #-}

module Csv where

import qualified Data.Text as T

import           Rules
import           Types

type Csv = T.Text

csvReader :: Csv -> [[T.Text]]
csvReader = map (T.split (== ',')) . T.lines . T.replace  "\"" ""

csvWriter :: [[T.Text]] -> Csv
csvWriter = T.unlines . map (T.intercalate ", ")

parseCsv :: Csv -> Maybe Credits
parseCsv csv
  | isCorrectCsv readed = let terms : datas = csvReader csv
                          in Just $ map (mkCredit terms) datas
  | otherwise = Nothing
    where
      readed = csvReader csv

isCorrectCsv :: [[T.Text]] -> Bool
isCorrectCsv [] = False
isCorrectCsv txtss
 | l <= 5 || 15 <= l = False
 | otherwise         = all (== l) ls
  where
    l : ls = map length txtss
