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
|]

--form :: Html -> MForm File File (FormResult (FileInfo, Maybe FileInfo), Widget)
form = renderDivs $ fileAFormReq "Upload your file: "

getRootR :: Handler Html
getRootR = do
    ((_, widget), enctype) <- runFormPost form
    defaultLayout
      [whamlet|$newline never
<form method=post enctype=#{enctype}>
    ^{widget}
    <p>
    <input type=submit>
|]

postRootR :: Handler Html
postRootR = do
    ((result, widget), enctype) <- runFormPost form
    let msubmission = case result of
            FormSuccess res -> Just res
            _               -> Nothing
    case msubmission of
      Nothing  ->
        defaultLayout
          [whamlet|$newline never
                  $maybe file <- msubmission
                  <form method=post enctype=#{enctype}>
                  ^{widget}
                  <p>
                  <input type=submit>
                  |]
      Just sub -> do
        sourceBS <- fileSourceByteString sub
        let source = T.decodeUtf8 sourceBS
            cdts = fromJust $ parseCsv source
            --result = mkResult source require
            r0 = "修了要件" :: T.Text
            r1 = showGroupNums require
            r2 = "計: " +.+  showT (sum $ map snd require) +.+ " 単位"
            r3 = "あなたの修得した単位" :: T.Text
            r4 = showGroupNums' cdts $ groupList require
            r5 = "計: " +.+ showT (countCreditNum cdts) +.+ " 単位"
            r6 = getResult cdts require
            r7 = "不足 " :: T.Text
            r8 = showGroupNums $ shortList $ judgeList cdts require
        --mapM_ (setSuccessAlert . fromStrict) r8
        defaultLayout $ do
          setTitle "Completation Judgment"
          [whamlet|$newline never
                  $maybe file <- msubmission
                  <form method=post enctype=#{enctype}>
                  ^{widget}
                  <p>
                  <input type=submit>
                  <ol>
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
                    <pre>
                      #{r8}</pre></div>
                  <br>
                  <font size="7" color=red>#{r6}</font>
                  |]

main :: IO ()
main = do
  port <- fromMaybe (error "PORT number not set")
              <$> lookupEnv "PORT"
  let portInt = read port :: Int
  warp portInt File
