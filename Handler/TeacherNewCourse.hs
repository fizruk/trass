module Handler.TeacherNewCourse where

import Import

getTeacherNewCourseR :: Handler Html
getTeacherNewCourseR = do
  defaultLayout $ do
    setTitle "Teacher's corner: starting a new course"
    $(widgetFile "teacher-new-course")

postTeacherNewCourseR :: Handler Html
postTeacherNewCourseR = error "Not yet implemented: postTeacherNewCourseR"
