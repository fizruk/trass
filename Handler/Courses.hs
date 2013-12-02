module Handler.Courses where

import Import
import CourseRepository
import Control.Monad

getCoursesR :: Handler Html
getCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId (Course _ repo)) -> do
    cr <- readCourseRepository repo
    return (courseId, cr)
  defaultLayout $ do
    $(widgetFile "courses")
