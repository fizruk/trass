module Handler.Courses where

import Import
import CourseRepository
import Control.Monad

getCoursesR :: Handler Html
getCoursesR = do
  cs  <- runDB $ selectList [] [Asc CourseId]
  cs' <- forM cs $ \(Entity courseId (Course _ repo)) -> do
    cr <- readCourseRepository repo
    return $ case cr of
      Nothing  -> []
      Just cr' -> [(courseId, cr')]
  let courses = concat cs'
  defaultLayout $ do
    $(widgetFile "courses")
