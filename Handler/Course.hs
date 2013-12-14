module Handler.Course where

import Import
import CourseRepository
import Control.Monad

getCourseR :: CourseId -> Handler Html
getCourseR courseId = do
  course <- runDB $ get404 courseId
  let name = courseName course
  cr <- courseContents name
  mtitle <- liftIO $ csTitle cr
  mdesc  <- liftIO $ csDescription cr
  ss     <- liftIO $ csSubsections cr
  ps     <- liftIO $ csProblems cr
  sections <- forM ss $ \section -> do
    mtitle <- liftIO $ csTitle section
    return (mtitle)
  problems <- forM ps $ \problem -> do
    mtitle <- liftIO $ cpTitle problem
    return (mtitle)
  defaultLayout $ do
    $(widgetFile "section")
