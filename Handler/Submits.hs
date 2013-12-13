module Handler.Submits where

import Import
import Yesod.Auth

getSubmitsR :: Handler Html
getSubmitsR = do
  userId <- requireAuthId
  ss <- runDB $ selectList [SubmissionUser ==. userId] [Desc SubmissionId]
  let submits = map entityVal ss
  defaultLayout $ do
    $(widgetFile "submits")
