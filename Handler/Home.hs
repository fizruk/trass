{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    ma <- maybeAuth
    let maybeLogin = userIdent . entityVal <$> ma
    defaultLayout $ do
        setTitle "trass - The Rigorous Assignment Submission System"
        $(widgetFile "header")
        $(widgetFile "homepage")

