module Handler.AdminCourses where

import Import

getAdminCoursesR :: Handler Html
getAdminCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  let courses = zip [1 :: Int ..] cs
  defaultLayout $ do
    $(widgetFile "admin-courses")
