module Types where

import qualified Data.Text as T

type Code      = T.Text -- 科目番号
type Title     = T.Text -- 科目名
type CreditNum = Float  -- 単位数
type Grade     = T.Text -- 評価
type Group     = T.Text -- 科目群

data Credit = Credit { code   :: Code
                     , title  :: Title
                     , number :: CreditNum
                     , grade  :: Grade
                     , group  :: Group
                     } deriving (Show, Eq)

type Credits = [Credit]

type CreditMap = [(Group, CreditNum)]
