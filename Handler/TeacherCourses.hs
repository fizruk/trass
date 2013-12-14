module Handler.TeacherCourses where

import Import
import Control.Monad

getTeacherCoursesR :: Handler Html
getTeacherCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId course) -> do
    return ("title" :: Text, courseName course, "author" :: Text)
  defaultLayout $ do
    setTitle "Teacher's corner: courses"
    $(widgetFile "teacher-courses")
