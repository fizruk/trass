module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = do
  defaultLayout $ do
    $(widgetFile "about")
