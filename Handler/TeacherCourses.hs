module Handler.TeacherCourses where

import Import
import CourseRepository
import Control.Monad

getTeacherCoursesR :: Handler Html
getTeacherCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId course) -> do
    let name = courseName course
    cr <- courseContents name
    title <- liftIO $ csTitle cr
    return (courseId, title, name, "author" :: Text)
  defaultLayout $ do
    setTitle "Teacher's corner: courses"
    $(widgetFile "teacher-courses")
