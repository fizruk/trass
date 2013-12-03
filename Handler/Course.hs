module Handler.Course where

import Import
import CourseRepository

getCourseR :: CourseId -> Handler Html
getCourseR courseId = do
  c <- runDB $ get404 courseId
  cr <- readCourseRepository (courseRepo c)
  case cr of
    Nothing -> notFound
    Just course -> do
      defaultLayout $ do
        $(widgetFile "course")
