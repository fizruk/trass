module Handler.TeacherNewCourse where

import Import
import Yesod.Auth

getTeacherNewCourseR :: Handler Html
getTeacherNewCourseR = do
  defaultLayout $ do
    setTitle "Teacher's corner: starting a new course"
    $(widgetFile "teacher-new-course")

postTeacherNewCourseR :: Handler Html
postTeacherNewCourseR = do
  userId <- requireAuthId
  name   <- runInputPost $ ireq textField "course-repo"
  courseId <- runDB $ insert (Course userId name)
  redirect TeacherCoursesR
