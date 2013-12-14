module Handler.TeacherCourses where

import Import

getTeacherCoursesR :: Handler Html
getTeacherCoursesR = do
  courses <- runDB $ selectList [] [Asc CourseId]
  defaultLayout $ do
    setTitle "Teacher's corner: courses"
    $(widgetFile "teacher-courses")
