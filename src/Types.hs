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
{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
{-
type Code      = String -- 科目番号
type Title     = String -- 科目名
type CreditNum = Float  -- 単位数
type Grade     = String -- 評価
type Group     = String -- 科目群
-}
type Code      = T.Text -- 科目番号
type Title     = T.Text -- 科目名
type CreditNum = Float  -- 単位数
type Grade     = T.Text -- 評価
type Group     = T.Text -- 科目群

data Credit = Credit { code  :: Code
                     , title :: Title
                     , num   :: CreditNum
                     , grade :: Grade
                     , group :: Group
                     } deriving (Show, Eq)

type Credits = [Credit]
