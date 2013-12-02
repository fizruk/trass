module Handler.AdminCourse where

import Import

getAdminCourseR :: CourseId -> Handler Html
getAdminCourseR courseId = do
  course <- runDB $ get404 courseId
  defaultLayout $ do
    $(widgetFile "admin-course")
