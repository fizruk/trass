module Handler.CourseProblem where

import Import
import Data.List
import CourseRepository
import CodeMirror

getCourseProblemR :: CourseId -> FilePath -> Handler Html
getCourseProblemR courseId prob = do
  c <- runDB $ get404 courseId
  let repo = courseRepo c
  p <- readCourseRepositoryProblem repo prob
  case p of
    Nothing -> notFound
    Just problem -> do
      defaultLayout $ do
        addStylesheet $ StaticR codemirror_lib_codemirror_css
        addScript     $ StaticR codemirror_lib_codemirror_js
        let languages = [CM_Haskell]
        case lookup (head languages) codeMirrorModes of
          Just mode_js -> addScript mode_js
          Nothing      -> return ()
        $(widgetFile "course-problem")

postCourseProblemR :: CourseId -> FilePath -> Handler Html
postCourseProblemR courseId prob = do
  c <- runDB $ get404 courseId
  let repo = courseRepo c
  p <- readCourseRepositoryProblem repo prob
  case p of
    Nothing -> notFound
    Just problem -> do
      file <- lookupFile "solutionFile"
      -- TODO: add to submit branch in course repo
      redirect HomeR
