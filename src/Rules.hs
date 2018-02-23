module Rules where

import           Csv
import           Types

-----------
-- Rules --
-----------
require :: [(Group, CreditNum)]
require = [ ("基礎科目", 1.0)
          , ("専攻共通 必修", 1.0)
          , ("専攻共通 選択", 4.0)
          , ("教科教育（数学教育）", 6.0)
          , ("教科専門（数学）", 12.0)
          , ("教科選択（研究）", 6.0)
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
    (f, a@(b : c : xs)) = splitAt 3 cd

--------------------------
-- Parse CSV -> Credits --
--------------------------
mkCredit :: [String] -> Credit
mkCredit xs = Credit { code  = cd
                     , title = xs !! 3
                     , num   = read (xs !! 4) :: Float
                     , grade = xs !! 7
                     , group = mkGroup cd
                     }
  where cd = xs !! 2
