{-# LANGUAGE ScopedTypeVariables #-}
module CourseRepository where

import Import
import qualified Control.Exception as E

import Data.Char
import Data.List (sort, isPrefixOf)
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import qualified Data.Yaml.Config as Y

import Text.Markdown
import Text.Highlighting.Kate

import System.Directory
import System.FilePath

type CourseRepository = CourseSection

data CourseSection = CourseSection
  { csPath        :: FilePath
  , csTitle       :: IO (Maybe Text)
  , csSummary     :: IO (Maybe Html)
  , csDescription :: IO (Maybe Html)
  , csSubsections :: IO [CourseSection]
  , csProblems    :: IO [CourseProblem]
  }

data CourseProblem = CourseProblem
  { cpPath        :: FilePath
  , cpTitle       :: IO (Maybe Text)
  , cpDescription :: IO (Maybe Html)
  , cpSnippet     :: IO (Maybe Text)
  , cpConfig      :: IO (Maybe Y.Config)
  }

courseContents404 :: Text -> Handler CourseRepository
courseContents404 name = sectionContents404 name []

courseContents :: Text -> Handler CourseRepository
courseContents name = sectionContents name []

sectionContents404 :: Text -> [Text] -> Handler CourseSection
sectionContents404 course path = section <$> getCoursePath404 course path

sectionContents :: Text -> [Text] -> Handler CourseSection
sectionContents course path = section <$> getCoursePath course path

problemContents404 :: Text -> [Text] -> Handler CourseProblem
problemContents404 course path = problem <$> getCoursePath404 course path

problemContents :: Text -> [Text] -> Handler CourseProblem
problemContents course path = problem <$> getCoursePath course path

getCoursePath404 :: Text -> [Text] -> Handler FilePath
getCoursePath404 course tpath = do
  path <- getCoursePath course tpath
  exists <- liftIO $ doesDirectoryExist path
  if exists then return path else notFound

getCoursePath :: Text -> [Text] -> Handler FilePath
getCoursePath course tpath = do
  lang <- fromMaybe "en" <$> lookupSession "_LANG"
  let path = System.FilePath.joinPath $ map T.unpack (course : lang : tpath)
  return $ "courses" </> "contents" </> path

tryExts :: [FilePath] -> FilePath -> (FilePath -> IO a) -> IO (Maybe a)
tryExts [] _ _ = return Nothing
tryExts (ext:exts) path act = (Just <$> act (path ++ ext)) `E.catch` \(_ :: E.SomeException) -> tryExts exts path act

readMarkdown :: FilePath -> IO Html
readMarkdown path = markdown def {msBlockCodeRenderer = kateBlockCodeRenderer} <$> TL.readFile path

kateBlockCodeRenderer :: Maybe Text -> (Text, Html) -> Html
kateBlockCodeRenderer lang (src, _) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" T.unpack lang) $ T.unpack src

section :: FilePath -> CourseSection
section = CourseSection
  <$> id
  <*> readTitle
  <*> readSummary
  <*> readDesc
  <*> readSubsections
  <*> readProblems

problem :: FilePath -> CourseProblem
problem = CourseProblem
  <$> id
  <*> readTitle
  <*> readDesc
  <*> readSnippet
  <*> readConfig

readTitle :: FilePath -> IO (Maybe Text)
readTitle path = tryExts [".txt"] (path </> "title") T.readFile

readSnippet :: FilePath -> IO (Maybe Text)
readSnippet path = do
  exts <- map takeExtension . filter ("snippet." `isPrefixOf`) <$> getDirectoryContents path
  tryExts exts (path </> "snippet") T.readFile

readSummary :: FilePath -> IO (Maybe Html)
readSummary path = tryExts [".md", ".markdown"] (path </> "summary") readMarkdown

readDesc :: FilePath -> IO (Maybe Html)
readDesc path = tryExts [".md", ".markdown"] (path </> "desc") readMarkdown

readSubsections :: FilePath -> IO [CourseSection]
readSubsections path = map section . map (path </>) . sort . filter isSectionDir <$> getDirectoryContents path
  where
    isSectionDir (x:_) = isDigit x
    isSectionDir _ = False

readConfig :: FilePath -> IO (Maybe Y.Config)
readConfig path = tryExts [".yml", ".yaml"] (path </> "trass") Y.load

readProblems :: FilePath -> IO [CourseProblem]
readProblems path = map problem . map (path </>) . sort . filter isProblemDir <$> getDirectoryContents path
  where
    isProblemDir (x:y:_) = x == 'p' && isDigit y
    isProblemDir _ = False

