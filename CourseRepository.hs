module CourseRepository where

import Import
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath

import Data.Maybe

data CourseRepository = CourseRepository
  { crName        :: Maybe Text
  , crCloneUrl    :: Text
  , crDescription :: Maybe Text
  , crMaterials   :: [CourseMaterial]
  , crExercises   :: [CourseExercise]
  , crProblems    :: [CourseProblem]
  , crSubmissions :: [CourseSubmission]
  }

data CourseMaterial   = CourseMaterial
data CourseExercise   = CourseExercise

data CourseProblem = CourseProblem
  { crProblemName   :: Maybe Text
  , crProblemDesc   :: Maybe Text
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
      <$> readTitle path
      <*> pure ""
      <*> readDesc path
      <*> pure []
      <*> pure []
      <*> readProblems path
      <*> pure []

readDesc :: FilePath -> Handler (Maybe Text)
readDesc path = liftIO $ tryReadFile (path </> "desc.txt")
  where
    filename = path ++ "/desc.html"

readTitle :: FilePath -> Handler (Maybe Text)
readTitle path = liftIO $ tryReadFile (path </> "title.txt")

tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile file = (Just <$> T.readFile file) `mplus` return Nothing

readProblems :: FilePath -> Handler [CourseProblem]
readProblems path = do
  ps <- liftIO $ getDirectoryContents (path </> "problems") `mplus` pure []
  mapM readProblem ps

readProblem :: FilePath -> Handler CourseProblem
readProblem path = CourseProblem
  <$> readTitle path
  <*> readDesc path

