module Handler.Course where

import Import
import CourseRepository
import Control.Monad
import qualified Data.Text as T
import System.FilePath
import Data.List (last)

getCourseR :: CourseId -> Handler Html
getCourseR courseId = do
  let spath = []
  course <- runDB $ get404 courseId
  let name = courseName course
  cr <- courseContents404 name
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
    $(widgetFile "section")
