module Handler.CourseProblem where

import Import hiding (parseTime)
import CodeMirror
import CourseRepository
import Control.Monad
import qualified Data.Yaml.Config as Y
import Data.List

getCourseProblemR :: CourseId -> [Text] -> Handler Html
getCourseProblemR courseId ppath = do
  course <- runDB $ get404 courseId
  let name = courseName course
  cp <- problemContents404 name ppath
  mtitle    <- liftIO $ cpTitle cp
  mdesc     <- liftIO $ cpDescription cp
  msnippet  <- liftIO $ cpSnippet cp
  mc <- liftIO $ cpConfig cp
  let mdeadline = mc >>= Y.lookup "deadline" :: Maybe Text
      languages = maybe []   (Y.lookupDefault "languages" []) mc
      webEditor = maybe True (Y.lookupDefault "web-editor" True) mc
  defaultLayout $ do
    addStylesheet $ StaticR codemirror_lib_codemirror_css
    addScript     $ StaticR codemirror_lib_codemirror_js
    addScript     $ StaticR codemirror_addon_mode_loadmode_js
    case (msum $ map codeMirrorMode languages) of
      Just mode_js -> addScript mode_js
      Nothing      -> return ()
    $(widgetFile "problem")
