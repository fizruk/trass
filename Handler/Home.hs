{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "trass - The Rigorous Assignment Submission System"
    $(widgetFile "homepage")

