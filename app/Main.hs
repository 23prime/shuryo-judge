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

data File = File

instance Yesod File where
    approot = ApprootStatic ""
instance RenderMessage File FormMessage where
    renderMessage _ _ = defaultFormMessage


mkYesod "File" [parseRoutes|
/ RootR GET
/result ResultR POST
/favicon.ico FaviconR GET
/main.css CSSR GET
|]


form = renderDivs $ fileAFormReq "Upload your file: "


getRootR :: Handler Html
getRootR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout $ do
      setTitle "Completion Judgment"
      [whamlet|$newline never
<head>
  <link rel="icon" href=@{FaviconR}>
  <link rel="stylesheet" href=@{CSSR}>
  <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">
<body>
  <h1>修了判定機
  <form id="upload" value="ファイルを選択" method=post enctype=#{enctype} action=@{ResultR}>
    ^{widget}
    <p>
    <input id="submit_button" type=submit value="判定！">
  <h2 id="usage"> Usage
  <ol>
    <li><a href="https://twins.tsukuba.ac.jp/" target="_blank">Twins</a>へアクセスします．
    <li>スマートフォンから利用している場合，＜PC 版＞を選択してください．
    <li>＜成績＞ページ下に＜ダウンロード＞があるので進みます．
    <li>ファイル形式は "CSV"，文字コードは "Unicode" のまま＜出力＞を押します．
    <li>ダウンロードされた CSV ファイルをアップロードし，＜判定！＞を押します．
    <li>結果が出ます．
    <li>もう一度判定しなおしたい場合，ブラウザの戻るボタンで戻ってください．
|]


postResultR :: Handler Html
postResultR = do
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _               -> Nothing
    case msubmission of
      Nothing  ->
        defaultLayout $ do
          setTitle "Completion Judgment"
          [whamlet|$newline never
$maybe file <- msubmission
<head>
  <link rel="stylesheet" href=@{CSSR}>
<body>
  <h1>修了判定機
  <form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input id="submit_button" type=submit value="判定！">
|]

      Just sub -> do
        sourceBS <- fileSourceByteString sub
        let source = T.decodeUtf8 sourceBS
            cdts' = parseCsv source
        case cdts' of
          Nothing ->
            defaultLayout $ do
              setTitle "Completion Judgment"
              [whamlet|$newline never
$maybe file <- msubmission
<head>
  <link rel="stylesheet" href=@{CSSR}>
<body>
  <h1>修了判定機
  <p class="error">Error: 不正なファイルです．
  <p>↓もう一度試す↓
  <form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input id="submit_button" type=submit value="判定！">
|]

          Just cdts -> do
            let r0 = "修了要件" :: T.Text
                r1 = showGroupNums require
                r2 = "計: " +.+  showT (sum $ map snd require) +.+ " 単位"
                r3 = "あなたの修得した単位" :: T.Text
                r4 = showGroupNums' cdts $ groupList require
                r5 = "計: " +.+ showT (countCreditNum cdts) +.+ " 単位"
                r6 = getResult cdts require
                r7 = "不足 " :: T.Text
                r8 = showGroupNums $ shortList $ judgeList cdts require
            defaultLayout $ do
              setTitle "Completion Judgment"
              [whamlet|$newline never
$maybe file <- msubmission
<head>
  <link rel="icon" href=@{FaviconR}>
  <link rel="stylesheet" href=@{CSSR}>
  <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1">
<body>
  <h1>修了判定機</h1>
  <h3>結果</h3>
  <font size="7vw" color=red>#{r6}</font>
  <h3>詳細</h3>
  <ul>
  <li>#{r0}</li>
  <pre id="pre1">#{r1}</pre></div>
  <br>
  #{r2}
  <br>
  <br>
  <br>
  <li>#{r3}</li>
  <pre id="pre1">#{r4}</pre></div>
  <br>
  #{r5}
  <br>
  <br>
  <br>
  <li>#{r7}</li>
  <pre id="pre2">#{r8}</pre></div>
  <br>
  <input class="back" type="button" onClick="location.href='@{RootR}'" value="戻る">
|]


getFaviconR :: Handler ()
getFaviconR = sendFile "image/ico" "favicon.ico"


getCSSR :: Handler ()
getCSSR = sendFile "text/css" "./main.css"


main :: IO ()
main = do
  port <- fromMaybe (error "PORT number not set")
              <$> lookupEnv "PORT"
  let portInt = read port :: Int
  warp portInt File
