{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString     as B
import           Data.Maybe          (fromJust, fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Text.Lazy      (fromStrict)
import           System.Environment  (lookupEnv)
import           Yesod

import           Csv
import           Judge
import           Rules
import           Types


data File = File

instance Yesod File where
  approot = ApprootStatic ""

instance RenderMessage File FormMessage where
  renderMessage _ _ MsgValueRequired = ""
  renderMessage _ _ msg              = defaultFormMessage msg


mkYesod "File" [parseRoutes|
/favicon.ico FaviconR GET
/main.css    CSSR     GET
/            RootR    GET
/result      ResultR  GET POST
|]

defaultHeader :: Widget
defaultHeader = do
  setTitle "Completion Judgment"
  addStylesheet CSSR
  toWidgetHead [hamlet|$newline never
<link rel="icon" href=@{FaviconR}>
<meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">
|]

defaultH1 :: Widget
defaultH1 = [whamlet|$newline never
<h1>
  修了判定機
|]

form = identifyForm "form" $ renderDivs $ areq fileField "" Nothing

select = identifyForm "form" $ renderDivs af
  where
    af   = areq (selectFieldList list) "" Nothing :: AForm Handler Bool
    list = [("修得済みの単位のみ", False),("履修中の単位を含む", True)] :: [(T.Text, Bool)]


defaultForm :: (Widget, Enctype) -> Widget -> Widget
defaultForm (wgt, enc) wgt2 = [whamlet|$newline never
<form id="upload" method=post enctype=#{enc} action=@{ResultR}>
  ^{wgt}
  <p>
  <input id="submit_button" type=submit value="判定！">
|]


defaultWidgets :: (Widget, Enctype) -> Widget -> Widget
defaultWidgets we1 wgt2 = do
  defaultHeader
  defaultH1
  defaultForm we1 wgt2

usage :: Widget
usage = [whamlet|$newline never
<h2 id="usage"> Usage
<ol>
  <li><a href="https://twins.tsukuba.ac.jp/" target="_blank">Twins</a>へアクセスします．
  <li>スマートフォンから利用している場合，＜PC 版＞を選択してください．
  <li>＜成績＞ページ下に＜ダウンロード＞があるので進みます．
  <li>ファイル形式は "CSV"，文字コードは "Unicode" のまま＜出力＞を押します．
  <li>ダウンロードされた CSV ファイルをアップロードし，＜判定！＞を押します．
  <li>結果が出ます．
|]

invalidFilePage :: (Widget, Enctype) -> Widget -> FileInfo -> Widget
invalidFilePage (wgt1, enc) wgt2 res = do
  defaultHeader
  defaultH1
  [whamlet|$newline never
<p class="error">
  Error: "#{fileName res}" は不正なファイルです．
<br>
<p class="retry">
  ↓リトライ↓
|]
  defaultForm (wgt1, enc) wgt2
  usage

successPage :: Credits -> FormResult Bool -> Handler Html
successPage cdts bool = do
  let (got, onCourse) = showGroupNums' cdts $ groupList require
      shorts = shortList $ judgeList False cdts require
      sum1   = "計: " +.+  showT (sum $ map snd require)    +.+ " 単位"
      sum2   = "計: " +.+ showT (fst $ countCreditNum cdts) +.+ " 単位"
  defaultLayout $ do
    defaultHeader
    defaultH1
    [whamlet|$newline never
<h3>
  結果
<p class="result">
  #{getResult False cdts require}
<h3>
  履修中の単位を含む結果
<p class="result">
  #{getResult True cdts shorts}
<h3>
  詳細
<ul>
  <li>
    修了要件
  <pre id="pre1">
    #{showGroupNums require}
  #{sum1}
  <br>
  <br>
  <br>
  <li>
    あなたの修得した単位
  <pre id="pre1">
    #{got}
  #{sum2}
  <br>
  <br>
  <br>
  <li>
    不足
  <pre id="pre2">
    #{showGroupNums shorts}
  <br>
  <li>
    履修中
  <pre id="pre2">
    #{onCourse}
  <br>
  <input class="back" type="button" onClick="location.href='@{RootR}'" value="戻る">
|]


getFaviconR :: Handler ()
getFaviconR = sendFile "image/ico" "favicon.ico"

getCSSR :: Handler ()
getCSSR = sendFile "text/css" "./main.css"

getRootR :: Handler Html
getRootR = do
    ((_, wgt), enc) <- runFormPost form
    ((_, wgt2), _)  <- runFormPost select
    defaultLayout $ do
      defaultWidgets (wgt, enc) wgt2
      usage


getResultR :: Handler Html
getResultR = redirect RootR

postResultR :: Handler Html
postResultR = do
    ((file, wgt1), enc) <- runFormPost form
    ((slt, wgt2), _)    <- runFormPost select
    let msubmission = case file of
          FormSuccess res ->
            let (name, ext) = T.breakOnEnd "." $ fileName res
            in case T.toLower ext of
              "csv" -> if name `elem` ["", "."]
                       then Left (Just res) -- When non CSV
                       else Right res       -- When CSV
              _     -> Left (Just res)      -- When non CSV
          _               -> Left Nothing   -- When no Success (No file?)
    case msubmission of
      Left Nothing ->
        defaultLayout $ do
          defaultWidgets (wgt1, enc) wgt2
          usage
      Left (Just res) ->
        defaultLayout $ invalidFilePage (wgt1, enc) wgt2 res
      Right res -> do
        sourceBS <- fileSourceByteString res
        let source = T.decodeUtf8 sourceBS
            cdts' = parseCsv source
        case cdts' of
          Nothing ->
            defaultLayout $ invalidFilePage (wgt1, enc) wgt2 res
          Just cdts -> do
            --let FormSuccess bool = slt
            successPage cdts slt

main :: IO ()
main = do
  port <- fromMaybe (error "PORT number not set")
              <$> lookupEnv "PORT"
  let portInt = read port :: Int
  warp portInt File
