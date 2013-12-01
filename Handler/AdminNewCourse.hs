module Handler.AdminNewCourse where

import Import

getAdminNewCourseR :: Handler Html
getAdminNewCourseR = do
  defaultLayout $ do
    $(widgetFile "admin-new-course")

postAdminNewCourseR :: Handler Html
postAdminNewCourseR = error "Not yet implemented: postAdminNewCourseR"

