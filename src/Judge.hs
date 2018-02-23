-- CSV について
-- 0  ID                     学籍番号
-- 1  Name                   氏名
-- 2  Course Code            科目番号
-- 3  Course Title           科目名
-- 4  Number of Credits      単位数
-- 5  Spring Semester        春学期
-- 6  Fall Semester          秋学期
-- 7  Overall grade          評価
-- 8  Course Category        科目区分
-- 9  Semester Academic Year 年度
-- 10 Semester status        開講区分（通常・集中）

module Judge where

import           Control.Monad
import           Csv
import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Maybe
import qualified Data.Text     as T

type Code      = String -- 科目番号
type Title     = String -- 科目名
type CreditNum = Float  -- 単位数
type Grade     = String -- 評価
type Group     = String -- 科目群

data Credit = Credit { code  :: Code
                     , title :: Title
                     , num   :: CreditNum
                     , grade :: Grade
                     , group :: Group
                     } deriving (Show, Eq)

type Credits = [Credit]


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

parseCsv :: Csv -> Credits
parseCsv = map mkCredit . tail . csvReader'


--------------------------------------
-- Count available number of Credit --
--------------------------------------
isAvailable :: Grade -> Bool
isAvailable g = case g of
  "A+" -> True
  "A"  -> True
  "B"  -> True
  "C"  -> True
  "P"  -> True
  _    -> False

availableCreditNum :: Credit -> CreditNum
availableCreditNum credit
  | isAvailable $ grade credit = num credit
  | otherwise                  = 0

countCreditNum :: Credits -> CreditNum
countCreditNum = sum . map availableCreditNum


-----------
-- Judge --
-----------
filterGroup :: Credits -> Group -> Credits
filterGroup credits grp = filter (\x -> group x == grp) credits

-- Make all Group list.
groupList = map fst require

-- Difference between Yours and Required in a Gruop.
-- ; (Required Number of Credits) - (Yours)
difference :: Credits -> Group -> (Group, CreditNum)
difference credits grp = (grp, req - number)
  where
    credits' = filterGroup credits grp
    req    = fromJust $ M.lookup grp $ M.fromList require
    number = countCreditNum credits'

-- Check difference in a Group.
judgeGroup :: Credits -> Group -> (Group, Bool, CreditNum)
judgeGroup credits grp
  | dif <= 0   = (grp, True,  dif)
  | otherwise  = (grp, False, dif) -- Then your Credits is short.
  where
    dif = snd $ difference credits grp

-- Take judgeGroup to each Gruops.
-- And make list which have result of judgeGroup.
judgeList :: Credits -> [(Group, Bool, CreditNum)]
judgeList credits = map (judgeGroup credits) groupList

fst3 (x, _, _) = x
snd3 (_, y, _) = y
thr3 (_, _, z) = z
rmSnd3 (x, y, z) = (x, z)

-- judgeList have only True; You have enough Credits in all Groups.
-- Then True.
judge :: Credits -> Bool
judge = all snd3 . judgeList

-- Result whether shuryo or not.
result :: Credits -> String
result credits = "結果: " ++ if judge credits
                             then "修了です"
                             else "留年！ｗ"

-- If exist Groups which have short of Credits, make there's list.
shortList :: [(Group, Bool, CreditNum)] -> [(Group, CreditNum)]
shortList = map rmSnd3 . filter (not . snd3)

