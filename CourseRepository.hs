module CourseRepository where

import Import
import Control.Monad
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Data.Maybe

data CourseRepository = CourseRepository
  { crName        :: Text
  , crCloneUrl    :: Text
  , crDescription :: Widget
  , crMaterials   :: [CourseMaterial]
  , crExercises   :: [CourseExercise]
  , crProblems    :: [CourseProblem]
  , crSubmissions :: [CourseSubmission]
  }

data CourseMaterial   = CourseMaterial
data CourseExercise   = CourseExercise

data CourseProblem = CourseProblem
  { crProblemName   :: Text
  , crProblemDesc   :: Widget
  }

data CourseSubmission = CourseSubmission

readCourseRepository :: Text -> Handler (Maybe CourseRepository)
readCourseRepository name = do
  lang <- fromMaybe "en" <$> lookupSession "_LANG"
  let path = "courses" </> T.unpack name </> T.unpack lang
  exists <- liftIO $ doesDirectoryExist path
  if not exists
    then return Nothing
    else fmap Just $ CourseRepository
      <$> pure (T.pack path)
      <*> pure ""
      <*> readDesc path
      <*> pure []
      <*> pure []
      <*> readProblems path
      <*> pure []

readDesc :: FilePath -> Handler Widget
readDesc path = pure [whamlet| _{MsgNoDescription} |]
  where
    filename = path ++ "/desc.html"

readProblems :: FilePath -> Handler [CourseProblem]
readProblems path = do
  ps <- liftIO $ getDirectoryContents (path ++ "/problems") `mplus` pure []
  mapM readProblem ps

readProblem :: FilePath -> Handler CourseProblem
readProblem path = CourseProblem
  <$> pure "noname"
  <*> readDesc path

