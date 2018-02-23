module Main where

import           Control.Monad      (when)
import           Judge
import           System.Environment (getArgs)
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  let csvPath = head args
  withFile csvPath ReadMode $ \csvHandle -> do
    csv <- hGetContents csvHandle
    let credits = parseCsv csv
    putStrLn "[修了要件]"
    mapM_ printRequire require
    putStrLn $ "計: " ++ show (sum $ map snd require) ++ " 単位"
    putStrLn "\n[あなたの修得した単位]"
    mapM_ (printGroupNum credits) groupList
    putStrLn $ "計: " ++ show (countCreditNum credits) ++ " 単位"
    putStrLn $ '\n' : result credits
    putStrLn "\n不足: "
    mapM_ printShort $ shortList $ judgeList credits
    return ()

printGroupNum :: Credits -> Group -> IO ()
printGroupNum credits grp = do
  let count = countCreditNum $ filterGroup credits grp
  putStrLn $ grp ++ ": " ++ show count ++ " 単位"

printRequire :: (Group, CreditNum) -> IO ()
printRequire (grp, number) = putStrLn $ grp ++ ": " ++ show number ++ " 単位"

printShort :: (Group, CreditNum) -> IO ()
printShort (grp, number) = putStrLn $ grp ++ ": " ++ show number ++ " 単位"
