module Handler.TeacherCourses where

import Import
import CourseRepository
import Control.Monad
import System.FilePath
import qualified Data.Text as T

getTeacherCoursesR :: Handler Html
getTeacherCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId course) -> do
    let name = courseName course
    title <- liftIO . csTitle . section $ csContentsPath </> T.unpack name
    return (title, name, "author" :: Text)
  defaultLayout $ do
    setTitle "Teacher's corner: courses"
    $(widgetFile "teacher-courses")
