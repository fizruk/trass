module Handler.CourseProblem where

import Import
import CourseRepository

getCourseProblemR :: CourseId -> [Text] -> Handler Html
getCourseProblemR courseId ppath = do
  course <- runDB $ get404 courseId
  let name = courseName course
  cp <- problemContents404 name ppath
  mtitle    <- liftIO $ cpTitle cp
  mdesc     <- liftIO $ cpDescription cp
  msnippet  <- liftIO $ cpSnippet cp
  defaultLayout $ do
    $(widgetFile "problem")
