module Handler.Course where

import Import
import CourseRepository

getCourseR :: CourseId -> Handler Html
getCourseR courseId = do
  course <- runDB $ get404 courseId
  let name = courseName course
  cr <- courseContents name
  mtitle <- liftIO $ csTitle cr
  mdesc  <- liftIO $ csDescription cr
  sections <- liftIO $ csSubsections cr
  problems <- liftIO $ csProblems cr
  defaultLayout $ do
    $(widgetFile "section")
