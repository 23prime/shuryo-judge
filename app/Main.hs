{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens       ((^.), _2)
import           Control.Monad      (when)
import qualified Data.Map           as M
import           Data.Maybe         (fromJust)
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import           System.Environment (getArgs)
import           System.IO

import           Csv
import           Rules
import           Types

main :: IO ()
main = do
  args <- getArgs
  let (csvPath, req) = if head args == "s"
                       then (args !! 1, require')
                       else (head args, require)
  withFile csvPath ReadMode $ \csvHandle -> do
    csv <- T.hGetContents csvHandle
    let credits = parseCsv csv
    T.putStrLn "[修了要件]"
    mapM_ printGroupNum req
    T.putStrLn $ "計: " +.+ showT (sum $ map snd req) +.+ " 単位"
    T.putStrLn "\n[あなたの修得した単位]"
    mapM_ (printGroupNum' credits) $ groupList req
    T.putStrLn $ "計: " +.+ showT (countCreditNum credits) +.+ " 単位"
    T.putStrLn $ '\n' `T.cons` result credits req
    T.putStrLn "\n不足: "
    mapM_ printGroupNum $ shortList $ judgeList credits req

printGroupNum' :: Credits -> Group -> IO ()
printGroupNum' credits grp = do
  let count = countCreditNum $ filterGroup credits grp
  T.putStrLn $ grp +.+ ": " +.+ showT count +.+ " 単位"

printGroupNum :: (Group, CreditNum) -> IO ()
printGroupNum (grp, number) = T.putStrLn $ grp +.+ ": " +.+ showT number +.+ " 単位"

parseCsv :: Csv -> Credits
parseCsv csv = let terms : datas = csvReader' csv
               in map (mkCredit terms) datas

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
  | isAvailable $ grade credit = num credit
  | otherwise                  = 0

countCreditNum :: Credits -> CreditNum
countCreditNum = sum . map availableCreditNum


-----------
-- Judge --
-----------
filterGroup :: Credits -> Group -> Credits
filterGroup credits grp = filter (\x -> group x == grp) credits

-- Difference between Yours and Required in a Gruop.
-- ; (Required Number of Credits) - (Yours)
difference :: Credits -> Group -> Require -> (Group, CreditNum)
difference credits grp req = (grp, req' - number)
  where
    credits' = filterGroup credits grp
    req'     = fromJust $ M.lookup grp $ M.fromList req
    number   = countCreditNum credits'

-- Make all Group list.
groupList = map fst

-- Check difference in a Group.
judgeGroup :: Credits -> Group -> Require -> (Group, Bool, CreditNum)
judgeGroup credits grp req
  | dif <= 0  = (grp, True,  dif)
  | otherwise = (grp, False, dif) -- Then your Credits is short.
  where
    dif = snd $ difference credits grp req

-- Take judgeGroup to each Gruops.
-- And make list which have result of judgeGroup.
judgeList :: Credits -> Require -> [(Group, Bool, CreditNum)]
judgeList credits req = map (flip (judgeGroup credits) req) $ groupList req

-- judgeList have only True; You have enough Credits in all Groups.
-- Then True.
judge :: Credits -> Require -> Bool
judge credits = all (^. _2) . judgeList credits

-- Result whether shuryo or not.
result :: Credits -> Require -> T.Text
result credits req = "結果: " +.+
  if judge credits req
  then "修了です"
  else "留年！ｗ"

-- If exist Groups which have short of Credits, make there's list.
shortList :: [(Group, Bool, CreditNum)] -> [(Group, CreditNum)]
shortList = map (\(x, y, z) -> (x, z)) . filter (not . (^. _2))
