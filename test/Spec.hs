{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO       as T
import           System.Environment (getArgs)
import           System.IO

import           Csv
import           Judge
import           Rules
import           Types

main :: IO ()
main = do
{-
  args <- getArgs
  print args
  let (csvPath, req) = if head args == "-s"
                       then (args !! 1, require')
                       else (head args, require)
-}
  let csvPath0 = "./test/samples/seiseki0.csv"
      csvPath1 = "./test/samples/seiseki1.csv"
      req = require
  putStrLn "\n---------- test0 ----------"
  test csvPath0
  putStrLn "\n---------- test1 ----------"
  test csvPath1

test :: String -> IO ()
test csvPath = do
  let req = require
  withFile csvPath ReadMode $ \handle -> do
    csv <- T.hGetContents handle
    let cdts = parseCsv csv
    T.putStrLn $ showResult csv req
