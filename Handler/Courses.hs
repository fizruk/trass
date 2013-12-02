module Handler.Courses where

import Import
import Control.Monad

getCoursesR :: Handler Html
getCoursesR = do
  cs <- runDB $ selectList [] [Asc CourseId]
  courses <- forM cs $ \(Entity courseId (Course name repo)) -> do
    return (courseId, name, "No description" :: Text)
  defaultLayout $ do
    $(widgetFile "courses")
