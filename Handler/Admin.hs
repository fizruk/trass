module Handler.Admin where

import Import

getAdminR :: Handler Html
getAdminR = do
  defaultLayout $ do
    $(widgetFile "admin")
