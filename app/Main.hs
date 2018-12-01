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
import           Yesod.Alert

import           Csv
import           Judge
import           Rules
import           Types

data File = File

instance Yesod File where
  approot = ApprootStatic ""
instance RenderMessage File FormMessage where
  renderMessage _ _ MsgValueRequired = "↑ファイルを選択してください．"
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

form = renderDivs $ fileAFormReq ""

defaultForm :: Widget -> Enctype -> Widget
defaultForm widget enctype = [whamlet|$newline never
<form id="upload" method=post enctype=#{enctype} action=@{ResultR}>
  ^{widget}
  <p>
  <input id="submit_button" type=submit value="判定！">
|]

defaultWidgets :: Widget -> Enctype -> Widget
defaultWidgets widget enctype = do
  defaultHeader
  defaultH1
  defaultForm widget enctype

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

invalidFilePage :: Widget -> Enctype -> FileInfo -> Widget
invalidFilePage widget enctype res = do
  defaultHeader
  defaultH1
  [whamlet|$newline never
<p class="error">
  Error: "#{fileName res}" は不正なファイルです．
<br>
<p class="retry">
  ↓リトライ↓
|]
  defaultForm widget enctype
  usage

successPage :: Credits -> Handler Html
successPage cdts = do
  let (got, onCourse) = showGroupNums' cdts $ groupList require
      shorts = shortList $ judgeList False cdts require
      r0  = "修了要件" :: T.Text
      r1  = showGroupNums require
      r2  = "計: " +.+  showT (sum $ map snd require) +.+ " 単位"
      r3  = "あなたの修得した単位" :: T.Text
      r4  = got
      r5  = "計:" +.+ showT (fst $ countCreditNum cdts) +.+ " 単位"
      r6  = getResult False cdts require
      r7  = "不足" :: T.Text
      r8  = showGroupNums shorts
      r9  = "履修中" :: T.Text
      r10 = onCourse
      r11 = getResult True cdts shorts
  defaultLayout $ do
    defaultHeader
    defaultH1
    [whamlet|$newline never
<h3>
  結果
<p class="result">
  #{r6}
<h3>
  履修中の単位を含む結果
<p class="result">
  #{r11}
<h3>
  詳細
<ul>
  <li>
    #{r0}
  <pre id="pre1">
    #{r1}
  #{r2}
  <br>
  <br>
  <br>
  <li>
    #{r3}
  <pre id="pre1">
    #{r4}
  #{r5}
  <br>
  <br>
  <br>
  <li>
    #{r7}
  <pre id="pre2">
    #{r8}
  <br>
  <li>
    #{r9}
  <pre id="pre2">
    #{r10}
  <br>
  <input class="back" type="button" onClick="location.href='@{RootR}'" value="戻る">
|]


getFaviconR :: Handler ()
getFaviconR = sendFile "image/ico" "favicon.ico"

getCSSR :: Handler ()
getCSSR = sendFile "text/css" "./main.css"

getRootR :: Handler Html
getRootR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout $ do
      defaultWidgets widget enctype
      usage

getResultR :: Handler Html
getResultR = redirect RootR

postResultR :: Handler Html
postResultR = do
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
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
          defaultWidgets widget enctype
          usage
      Left (Just res) ->
        defaultLayout $ invalidFilePage widget enctype res
      Right res -> do
        sourceBS <- fileSourceByteString res
        let source = T.decodeUtf8 sourceBS
            cdts' = parseCsv source
        case cdts' of
          Nothing ->
            defaultLayout $ invalidFilePage widget enctype res
          Just cdts -> successPage cdts

main :: IO ()
main = do
  port <- fromMaybe (error "PORT number not set")
              <$> lookupEnv "PORT"
  let portInt = read port :: Int
  warp portInt File
