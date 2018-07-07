{-# LANGUAGE OverloadedStrings #-}

module Csv where

import qualified Data.Text as T

type Csv = T.Text

-- CSV -> List
csvReader :: Csv -> [[T.Text]]
csvReader = map (T.split (== ',')) . T.lines

-- For the case of each factor of CSV have ""
csvReader' :: Csv -> [[T.Text]]
csvReader' = map (T.split (== ',')) . T.lines . killQuot

killQuot :: T.Text -> T.Text
killQuot = T.replace  "\"" ""

-- List -> CSV
csvWriter :: [[T.Text]] -> Csv
csvWriter = T.unlines . map (T.intercalate ", ")
