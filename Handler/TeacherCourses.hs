module Handler.TeacherCourses where

import Import

getTeacherCoursesR :: Handler Html
getTeacherCoursesR = do
  defaultLayout $ do
    setTitle "Teacher's corner: courses"
    $(widgetFile "teacher-courses")
