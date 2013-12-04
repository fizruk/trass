module Handler.AdminNewCourse where

import Import
import System.Process
import System.Exit
import Data.Text

getAdminNewCourseR :: Handler Html
getAdminNewCourseR = do
  defaultLayout $ do
    $(widgetFile "admin-new-course")

postAdminNewCourseR :: Handler Html
postAdminNewCourseR = do
  name <- runInputPost $ ireq textField "courseName"
  repo <- runInputPost $ ireq textField "courseRepo"
  exitCode <- liftIO . system $ "./new-course-repo " ++ show (unpack repo)
  case exitCode of
    ExitSuccess -> do
      courseId <- runDB $ insert (Course name repo)
      redirect (AdminCourseR courseId)
    _ -> do
      defaultLayout $ [whamlet| Internal server error. |]

