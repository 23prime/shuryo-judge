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
/ RootR GET POST
/favicon.ico FaviconR GET
|]

form = renderDivs $ fileAFormReq "Upload your file: "

getRootR :: Handler Html
getRootR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout $ do
      setTitle "Completation Judgment"
      [whamlet|$newline never
<head>
  <link rel="icon" href=@{FaviconR}>
<body>
<h1> 修了判定機
<form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input type=submit value="判定！">
<h2> Usage
<ol>
  <li><a href="https://twins.tsukuba.ac.jp/">Twins</a>へアクセスします．
  <li>＜成績＞ページ下に＜ダウンロード＞があるので進みます．
  <li>ファイル形式は "CSV"，文字コードは "Unicode" のまま＜出力＞を押します．
  <li>ダウンロードされた CSV ファイルをアップロードし，＜判定！＞を押します．
  <li>結果が出ます．
  <li>もう一度判定しなおしたい場合，ブラウザの戻るボタンで戻ってください．
|]


postRootR :: Handler Html
postRootR = do
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _               -> Nothing
    case msubmission of
      Nothing  ->
        defaultLayout $ do
          setTitle "Completation Judgment"
          [whamlet|$newline never
$maybe file <- msubmission
<h1>修了判定機
<form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input type=submit value="判定！">
|]

      Just sub -> do
        sourceBS <- fileSourceByteString sub
        let source = T.decodeUtf8 sourceBS
            cdts' = parseCsv source
        case cdts' of
          Nothing ->
            defaultLayout $ do
              setTitle "Completation Judgment"
              [whamlet|$newline never
$maybe file <- msubmission
<h1>修了判定機
<p>Error: 不正なファイルです．
<p>↓もう一度試す↓
<form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input type=submit value="判定！">
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
              setTitle "Completation Judgment"
              [whamlet|$newline never
$maybe file <- msubmission
<h1>修了判定機</h1>
<h2>結果：</h2>
    <ul>
    <li>#{r0}</li>
    <div style="background-color: gainsboro; width: 300px; font-size: 12pt">
      <pre>#{r1}</pre></div>
    <br>
    #{r2}
    <br>
    <br>
    <br>
    <li>#{r3}</li>
    <div style="background-color: gainsboro; width: 300px; font-size: 12pt">
      <pre>#{r4}</pre></div>
    <br>
    #{r5}
    <br>
    <br>
    <br>
    <li>#{r7}</li>
    <div style="background-color: gainsboro; width: 300px; font-size: 12pt; color: red">
      <pre>#{r8}</pre></div>
    <br>
    <font size="5">判定結果 → </font>
    <font size="7" color=red>#{r6}</font>
    |]


getFaviconR :: Handler ()
getFaviconR = sendFile "image/ico" "favicon.ico"


main :: IO ()
main = do
  port <- fromMaybe (error "PORT number not set")
              <$> lookupEnv "PORT"
  let portInt = read port :: Int
  warp portInt File
