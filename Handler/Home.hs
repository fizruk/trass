{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Data.Maybe

getHomeR :: Handler Html
getHomeR = do
  loggedIn <- isJust <$> maybeAuthId
  defaultLayout $ do
    setTitle "trass - The Rigorous Assignment Submission System"
    $(widgetFile "homepage")

