{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "trass - The Rigorous Assignment Submission System"
        $(widgetFile "homepage")

