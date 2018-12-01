{-# LANGUAGE OverloadedStrings #-}

module Judge where

import           Control.Lens  ((^.), _2)
import           Control.Monad (when)
import qualified Data.Map      as M
import           Data.Maybe    (fromJust, fromMaybe)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

import           Csv
import           Rules
import           Types


-----------
-- Utils --
-----------
showT = T.pack . show

(+.+) = T.append

mapTuple f (x, y) = (f x, f y)


-----------
-- Result --
-----------
showResult :: T.Text -> CreditMap -> T.Text
showResult csv req = let results = mkResult csv req
                     in T.intercalate "\n" results

mkResult :: T.Text -> CreditMap -> [T.Text]
mkResult csv req =
  let cdts' = parseCsv csv
  in case cdts' of
       Just cdts ->
         let (got, onCourse) = showGroupNums' cdts $ groupList req
             shorts = shortList $ judgeList False cdts req
         in  [ "結果: " +.+ getResult False cdts req
             , "履修中を含む → " +.+ getResult True cdts shorts
             , "\n[修了要件]"
             , showGroupNums req
             , "計: " +.+  showT (sum $ map snd req) +.+ " 単位"
             , "\n[あなたの修得した単位]"
             , got
             , "計: " +.+ showT (fst $ countCreditNum cdts) +.+ " 単位"
             , "\n不足: "
             , showGroupNums shorts
             , "\n履修中: "
             , onCourse
             ]
       _ -> ["Error: Invalid file!"]

showGroupNum :: (Group, CreditNum) -> T.Text
showGroupNum (grp, num) = grp +.+ ": " +.+ showT num +.+ " 単位"

showGroupNumOn :: (Group, CreditNum) -> Maybe T.Text
showGroupNumOn (grp, num)
  = case num of
      0 -> Nothing
      _ -> Just $ grp +.+ ": " +.+ showT num +.+ " 単位"

showGroupNums :: CreditMap -> T.Text
showGroupNums = T.intercalate "\n" . map showGroupNum

showGroupNum' :: Credits -> Group -> (T.Text, Maybe T.Text)
showGroupNum' cdts grp =
  let (num0, num1) = countCreditNum $ filterGroup cdts grp
      got          = showGroupNum (grp, num0)
      onCourse     = showGroupNumOn (grp, num1)
  in (got, onCourse)

showGroupNums' :: Credits -> [Group] -> (T.Text, T.Text)
showGroupNums' cdts grps = let s = map (showGroupNum' cdts) grps
                               fsts = map fst s
                               snds = map fromJust $ filter (/= Nothing) $ map snd s
                           in mapTuple (T.intercalate "\n") (fsts, snds)


--------------------------------------
-- Count available number of Credit --
--------------------------------------
isAvailable :: Grade -> Maybe Bool
isAvailable g = case g of
  "A+"  -> Just True
  "A"   -> Just True
  "B"   -> Just True
  "C"   -> Just True
  "P"   -> Just True
  "履修中" -> Nothing
  _     -> Just False

availableCreditNum :: Credit -> Either CreditNum CreditNum
availableCreditNum cdt = case isAvailable $ grade cdt of
  Just True -> Right $ number cdt
  Just _    -> Right 0
  _         -> Left $ number cdt

tupleAdd :: Num a => (a, a) -> (a, a) -> (a, a)
tupleAdd (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

countCreditNum :: Credits -> (CreditNum, CreditNum)
countCreditNum [] = (0, 0)
countCreditNum (cdt : cdts)
  = let num' = availableCreditNum cdt
    in case num' of
         Right num -> (num, 0) `tupleAdd` countCreditNum cdts
         Left num  -> (0, num) `tupleAdd` countCreditNum cdts


-----------
-- Judge --
-----------
filterGroup :: Credits -> Group -> Credits
filterGroup cdts grp = filter (\x -> group x == grp) cdts

-- Difference between Yours and Require in a Gruop.
--   ; (Require Number of Credits) - (Yours)
-- If include on cource  -> True
--   else                -> False
difference :: Bool -> Credits -> Group -> CreditMap -> (Group, CreditNum)
difference bool cdts grp req = (grp, req' - num)
  where
    cdts'    = filterGroup cdts grp
    req'     = fromJust $ M.lookup grp $ M.fromList req -- Possibly unsafe.
    (n0, n1) = countCreditNum cdts'
    num      = n0 + if bool then n1 else 0

-- Make all Group list.
groupList = map fst

-- Check diffference in a Group.
judgeGroup :: Bool -> Credits -> CreditMap -> Group -> (Group, Bool, CreditNum)
judgeGroup bool cdts req grp
  | diff <= 0 = (grp, True,  diff)
  | otherwise = (grp, False, diff) -- Then your Credits is short.
  where
    diff = snd $ difference bool cdts grp req

-- Take judgeGroup to each Gruops.
-- And make list which have result of judgeGroup.
judgeList :: Bool -> Credits -> CreditMap -> [(Group, Bool, CreditNum)]
judgeList bool cdts req = map (judgeGroup bool cdts req) $ groupList req

-- judgeList have only True; You have enough Credits in all Groups.
-- Then True.
judge :: Bool -> Credits -> CreditMap -> Bool
judge bool cdts = all (^. _2) . judgeList bool cdts

-- Result whether shuryo or not.
getResult :: Bool -> Credits -> CreditMap -> T.Text
getResult bool cdts req
  | judge bool cdts req = "修了です🎉🎉🎉"
  | otherwise           = "留年！ｗ"

-- If exist Groups which have short of Credits, make there's list.
shortList :: [(Group, Bool, CreditNum)] -> CreditMap
shortList = map (\(x, y, z) -> (x, z)) . filter (not . (^. _2))
