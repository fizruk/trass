module Handler.TeacherNewCourse where

import Import
import Yesod.Auth
import System.Process
import System.Exit

getTeacherNewCourseR :: Handler Html
getTeacherNewCourseR = do
  defaultLayout $ do
    setTitle "Teacher's corner: starting a new course"
    $(widgetFile "teacher-new-course")

postTeacherNewCourseR :: Handler Html
postTeacherNewCourseR = do
  userId <- requireAuthId
  name   <- runInputPost $ ireq textField "course-repo"
  exitCode <- liftIO . system $ "./courses/new_course.sh " ++ show name
  case exitCode of
    ExitSuccess -> do
      courseId <- runDB $ insert (Course userId name)
      redirect TeacherCoursesR
    _ -> do
      error "Failed to create new course."
