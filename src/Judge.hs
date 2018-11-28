{-# LANGUAGE OverloadedStrings #-}

module Judge where

import           Control.Lens  ((^.), _2)
import           Control.Monad (when)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

import           Csv
import           Rules
import           Types


showResult :: T.Text -> Require -> T.Text
showResult csv req = let results = mkResult csv req
                     in T.intercalate "\n" results

mkResult :: T.Text -> Require -> [T.Text]
mkResult csv req =
  let cdts' = parseCsv csv
  in case cdts' of
       Just cdts -> [ "[修了要件]"
                    , showGroupNums req
                    , "計: " +.+  showT (sum $ map snd req) +.+ " 単位"
                    , "\n[あなたの修得した単位]"
                    , showGroupNums' cdts $ groupList req
                    , "計: " +.+ showT (countCreditNum cdts) +.+ " 単位"
                    , '\n' `T.cons` getResult cdts req
                    , "\n不足: "
                    , showGroupNums $ shortList $ judgeList cdts req
                    ]
       _ -> ["Error: Invalid file!"]


showGroupNum' :: Credits -> Group -> T.Text
showGroupNum' cdts grp =
  let count = countCreditNum $ filterGroup cdts grp
  in  grp +.+ ": " +.+ showT count +.+ " 単位"

--maxLen :: Int
--maxLen = maximum $ map (T.length . fst) require

showGroupNums' :: Credits -> [Group] -> T.Text
showGroupNums' cdts = T.intercalate "\n" . map (showGroupNum' cdts)

printGroupNums' :: Credits -> [Group] -> IO ()
printGroupNums' cdts grps = do
  let gns = showGroupNums' cdts grps
  T.putStrLn gns

showGroupNum :: (Group, CreditNum) -> T.Text
showGroupNum (grp, num) = grp +.+ ": " +.+ showT num +.+ " 単位"

showGroupNums :: [(Group, CreditNum)] -> T.Text
showGroupNums = T.intercalate "\n" . map showGroupNum

printGroupNum :: (Group, CreditNum) -> IO ()
printGroupNum (grp, num) = T.putStrLn $ grp +.+ ": " +.+ showT num +.+ " 単位"


------------------------
-- Functions for Text --
------------------------
showT = T.pack . show

(+.+) = T.append

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
  | isAvailable $ grade credit = number credit
  | otherwise                  = 0

countCreditNum :: Credits -> CreditNum
countCreditNum = sum . map availableCreditNum


-----------
-- Judge --
-----------
filterGroup :: Credits -> Group -> Credits
filterGroup cdts grp = filter (\x -> group x == grp) cdts

-- Difference between Yours and Required in a Gruop.
-- ; (Required Number of Credits) - (Yours)
difference :: Credits -> Group -> Require -> (Group, CreditNum)
difference cdts grp req = (grp, req' - number)
  where
    cdts' = filterGroup cdts grp
    req'     = fromJust $ M.lookup grp $ M.fromList req
    number   = countCreditNum cdts'

-- Make all Group list.
groupList = map fst

-- Check difference in a Group.
judgeGroup :: Credits -> Group -> Require -> (Group, Bool, CreditNum)
judgeGroup cdts grp req
  | dif <= 0  = (grp, True,  dif)
  | otherwise = (grp, False, dif) -- Then your Credits is short.
  where
    dif = snd $ difference cdts grp req

-- Take judgeGroup to each Gruops.
-- And make list which have result of judgeGroup.
judgeList :: Credits -> Require -> [(Group, Bool, CreditNum)]
judgeList cdts req = map (flip (judgeGroup cdts) req) $ groupList req

-- judgeList have only True; You have enough Credits in all Groups.
-- Then True.
judge :: Credits -> Require -> Bool
judge cdts = all (^. _2) . judgeList cdts

-- Result whether shuryo or not.
getResult :: Credits -> Require -> T.Text
getResult cdts req = "結果: " +.+
  if judge cdts req
  then "修了です"
  else "留年！ｗ"

-- If exist Groups which have short of Credits, make there's list.
shortList :: [(Group, Bool, CreditNum)] -> [(Group, CreditNum)]
shortList = map (\(x, y, z) -> (x, z)) . filter (not . (^. _2))
