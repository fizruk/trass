module CourseRepository where

import Import
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import Control.Concurrent
import System.Process
import System.Directory
import System.FilePath
import Text.Markdown
import Text.Highlighting.Kate

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
  { crProblemDir      :: FilePath
  , crProblemName     :: Maybe Text
  , crProblemDesc     :: Maybe Html
  , crProblemSnippet  :: Maybe Text
  }

data CourseSubmission = CourseSubmission

coursePath :: Text -> Handler FilePath
coursePath name = do
  lang <- fromMaybe "en" <$> lookupSession "_LANG"
  return $ "courses" </> T.unpack name </> T.unpack lang

courseProblemPath :: Text -> FilePath -> Handler FilePath
courseProblemPath name prob = do
  path <- coursePath name
  return $ path </> "problems" </> prob

withExistingDir :: (MonadIO m) => FilePath -> m a -> m (Maybe a)
withExistingDir path m = do
  exists <- liftIO $ doesDirectoryExist path
  if not exists
    then return Nothing
    else Just `liftM` m

readCourseRepository :: Text -> Handler (Maybe CourseRepository)
readCourseRepository name = do
  path <- coursePath name
  withExistingDir path $ CourseRepository
    <$> readTitle path
    <*> pure ""
    <*> readDesc path
    <*> pure []
    <*> pure []
    <*> readProblems path
    <*> pure []

readCourseRepositoryProblem :: Text -> FilePath -> Handler (Maybe CourseProblem)
readCourseRepositoryProblem name prob = do
  path <- courseProblemPath name prob
  withExistingDir path $ readProblem (path </> "..") prob -- FIXME: fix that .. dirty hack

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
  <*> readSnippet path
  where
    path = parent </> prob

readTitle :: FilePath -> Handler (Maybe Text)
readTitle path = maybeReadText (path </> "title") [".txt"]

readDesc :: FilePath -> Handler (Maybe Html)
readDesc path = maybeReadMarkdown (path </> "desc") [".md", ".markdown"]

readSnippet :: FilePath -> Handler (Maybe Text)
readSnippet path = maybeReadText (path </> "snippet") [".hs", ".lhs"]

maybeReadText :: FilePath -> [String] -> Handler (Maybe Text)
maybeReadText file exts = liftIO $ msum (map (fmap Just . T.readFile . (file <>)) exts) `mplus` return Nothing

maybeReadMarkdown :: FilePath -> [String] -> Handler (Maybe Html)
maybeReadMarkdown path exts = do
  contents <- maybeReadText path exts
  return $ markdown def {msBlockCodeRenderer = kateBlockCodeRenderer} . TL.pack . T.unpack <$> contents

kateBlockCodeRenderer :: Maybe Text -> (Text, Html) -> Html
kateBlockCodeRenderer lang (src, _) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" T.unpack lang) $ T.unpack src

updateRepositoriesDaemon :: IO ()
updateRepositoriesDaemon = forever $ do
  _ <- system "./courses/update-repos >temp.log 2>temp.err"
  threadDelay 5000000

