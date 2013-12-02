module Handler.AdminNewCourse where

import Import

getAdminNewCourseR :: Handler Html
getAdminNewCourseR = do
  defaultLayout $ do
    $(widgetFile "admin-new-course")

postAdminNewCourseR :: Handler Html
postAdminNewCourseR = do
  name <- runInputPost $ ireq textField "courseName"
  repo <- runInputPost $ ireq textField "courseRepo"
  courseId <- runDB $ insert (Course name repo)
  redirect (AdminCourseR courseId)

