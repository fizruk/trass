module Handler.Courses where

import Import
import CourseRepository
import Control.Monad

getCoursesR :: Handler Html
getCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId course) -> do
    let name = courseName course
    cr <- courseContents name
    title    <- liftIO $ csTitle cr
    summary  <- liftIO $ csSummary cr
    sections <- liftIO $ csSubsections cr
    problems <- liftIO $ csProblems cr
    return (courseId, title, summary, sections, problems)
  defaultLayout $ do
    $(widgetFile "courses")
