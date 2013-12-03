module Handler.CourseProblem where

import Import
import CourseRepository

getCourseProblemR :: CourseId -> FilePath -> Handler Html
getCourseProblemR courseId prob = do
  c <- runDB $ get404 courseId
  let repo = courseRepo c
  p <- readCourseRepositoryProblem repo prob
  case p of
    Nothing -> notFound
    Just problem -> do
      defaultLayout $ do
        $(widgetFile "course-problem")
