module Handler.CourseSection where

import Import
import CourseRepository
import Control.Monad
import qualified Data.Text as T
import System.FilePath
import Data.List (last, init)

getCourseSectionR :: CourseId -> [Text] -> Handler Html
getCourseSectionR courseId spath = do
  course <- runDB $ get404 courseId
  let name = courseName course
  cr <- sectionContents404 name spath
  courseTitle <- courseContents name >>= liftIO . csTitle
  breadcrumbs <- contentsBreadcrumbs name (init spath)
  mtitle <- liftIO $ csTitle cr
  mdesc  <- liftIO $ csDescription cr
  ss     <- liftIO $ csSubsections cr
  ps     <- liftIO $ csProblems cr
  sections <- forM ss $ \s -> do
    let path = T.pack . last . splitPath $ csPath s
    title <- liftIO $ csTitle s
    return ([path], title)
  problems <- forM ps $ \p -> do
    let path = T.pack . last . splitPath $ cpPath p
    title <- liftIO $ cpTitle p
    return ([path], title)
  defaultLayout $ do
    $(widgetFile "course-breadcrumbs")
    $(widgetFile "section")
