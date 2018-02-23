{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad      (when)
import qualified Data.Map           as M
import           Data.Maybe
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
  let csvPath = head args
  withFile csvPath ReadMode $ \csvHandle -> do
    csv <- hGetContents csvHandle
    let credits = parseCsv csv
    putStrLn "[修了要件]"
    mapM_ printGroupNum require
    putStrLn $ "計: " ++ show (sum $ map snd require) ++ " 単位"
    putStrLn "\n[あなたの修得した単位]"
    mapM_ (printGroupNum' credits) groupList
    putStrLn $ "計: " ++ show (countCreditNum credits) ++ " 単位"
    putStrLn $ '\n' : result credits
    putStrLn "\n不足: "
    mapM_ printGroupNum $ shortList $ judgeList credits
    return ()

printGroupNum' :: Credits -> Group -> IO ()
printGroupNum' credits grp = do
  let count = countCreditNum $ filterGroup credits grp
  putStrLn $ grp ++ ": " ++ show count ++ " 単位"

printGroupNum :: (Group, CreditNum) -> IO ()
printGroupNum (grp, number) = putStrLn $ grp ++ ": " ++ show number ++ " 単位"

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


hoge :: T.Text
hoge = T.pack "a"
