module Handler.Submits where

import Import

getSubmitsR :: Handler Html
getSubmitsR = do
  defaultLayout $ do
    $(widgetFile "submits")
