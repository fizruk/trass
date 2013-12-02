module Handler.Courses where

import Import
import CourseRepository
import Control.Monad

getCoursesR :: Handler Html
getCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId (Course name repo)) -> do
    cr <- readCourseRepository repo
    return (courseId, name, cr)
  defaultLayout $ do
    $(widgetFile "courses")
