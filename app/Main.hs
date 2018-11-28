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
  args <- getArgs
  let (csvPath, req) = if head args == "s"
                       then (args !! 1, require')
                       else (head args, require)
  withFile csvPath ReadMode $ \handle -> do
    csv <- T.hGetContents handle
    let cdts = parseCsv csv
    T.putStrLn $ showResult csv req
