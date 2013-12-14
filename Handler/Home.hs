{-# LANGUAGE OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "Trass - the rigorous assignment submission system"
    $(widgetFile "homepage")
