module Handler.TeacherCorner where

import Import

getTeacherCornerR :: Handler Html
getTeacherCornerR = redirect TeacherCoursesR
