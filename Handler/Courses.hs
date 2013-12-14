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
    title <- liftIO $ csTitle cr
    desc  <- liftIO $ csDescription cr
    sections <- liftIO $ csSubsections cr
    problems <- liftIO $ csProblems cr
    return (courseId, title, desc, sections, problems)
  defaultLayout $ do
    $(widgetFile "courses")
