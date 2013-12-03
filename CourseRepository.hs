module CourseRepository where

import Import
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import Text.Markdown

import Data.Maybe
import Data.List

data CourseRepository = CourseRepository
  { crName        :: Maybe Text
  , crCloneUrl    :: Text
  , crDescription :: Maybe Html
  , crMaterials   :: [CourseMaterial]
  , crExercises   :: [CourseExercise]
  , crProblems    :: [CourseProblem]
  , crSubmissions :: [CourseSubmission]
  }

data CourseMaterial   = CourseMaterial
data CourseExercise   = CourseExercise

data CourseProblem = CourseProblem
  { crProblemDir    :: FilePath
  , crProblemName   :: Maybe Text
  , crProblemDesc   :: Maybe Html
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

readProblems :: FilePath -> Handler [CourseProblem]
readProblems path = do
  ps <- liftIO $ getDirectoryContents (path </> "problems") `mplus` pure []
  let ps' = filter (not . isPrefixOf ".") ps
  mapM (readProblem (path </> "problems")) ps'

readProblem :: FilePath -> FilePath -> Handler CourseProblem
readProblem parent prob = CourseProblem
  <$> pure prob
  <*> readTitle path
  <*> readDesc path
  where
    path = parent </> prob

readTitle :: FilePath -> Handler (Maybe Text)
readTitle path = maybeReadText (path </> "title") [".txt"]

readDesc :: FilePath -> Handler (Maybe Html)
readDesc path = maybeReadMarkdown (path </> "desc") [".md", ".markdown"]

maybeReadText :: FilePath -> [String] -> Handler (Maybe Text)
maybeReadText file exts = liftIO $ msum (map (fmap Just . T.readFile . (file <>)) exts) `mplus` return Nothing

maybeReadMarkdown :: FilePath -> [String] -> Handler (Maybe Html)
maybeReadMarkdown path exts = do
  contents <- maybeReadText path exts
  return $ markdown def . TL.pack . T.unpack <$> contents

