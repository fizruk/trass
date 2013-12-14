module CourseRepository where

import Import
import Control.Monad

import Data.Char
import Data.List (sort)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import Text.Markdown
import Text.Highlighting.Kate

import System.Directory
import System.FilePath

type CourseRepository = CourseSection

data CourseSection = CourseSection
  { csPath        :: FilePath
  , csTitle       :: IO (Maybe Text)
  , csDescription :: IO (Maybe Html)
  , csSubsections :: IO [CourseSection]
  , csProblems    :: IO [CourseProblem]
  }

data CourseProblem = CourseProblem
  { cpPath        :: FilePath
  , cpTitle       :: IO (Maybe Text)
  , cpDescription :: IO (Maybe Html)
  , cpSnippet     :: IO (Maybe Text)
  }

csContentsPath :: FilePath
csContentsPath = "courses/contents"

tryExts :: [FilePath] -> FilePath -> (FilePath -> IO a) -> IO (Maybe a)
tryExts [] _ _ = return Nothing
tryExts (ext:exts) path act = (Just <$> act (path ++ ext)) `mplus` tryExts exts path act

readMarkdown :: FilePath -> IO Html
readMarkdown path = markdown def {msBlockCodeRenderer = kateBlockCodeRenderer} <$> TL.readFile path

kateBlockCodeRenderer :: Maybe Text -> (Text, Html) -> Html
kateBlockCodeRenderer lang (src, _) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" T.unpack lang) $ T.unpack src

section :: FilePath -> CourseSection
section = CourseSection
  <$> id
  <*> readTitle
  <*> readDesc
  <*> readSubsections
  <*> readProblems

problem :: FilePath -> CourseProblem
problem = CourseProblem
  <$> id
  <*> readTitle
  <*> readDesc
  <*> readSnippet

readTitle :: FilePath -> IO (Maybe Text)
readTitle path = tryExts [".txt"] (path </> "title") T.readFile

readSnippet :: FilePath -> IO (Maybe Text)
readSnippet path = tryExts [".hs"] (path </> "snippet") T.readFile

readDesc :: FilePath -> IO (Maybe Html)
readDesc path = tryExts [".md", ".markdown"] (path </> "desc") readMarkdown

readSubsections :: FilePath -> IO [CourseSection]
readSubsections path = map section . sort . filter isSectionDir <$> getDirectoryContents path
  where
    isSectionDir (x:_) = isDigit x
    isSectionDir _ = False

readProblems :: FilePath -> IO [CourseProblem]
readProblems path = map problem . sort . filter isProblemDir <$> getDirectoryContents path
  where
    isProblemDir (x:y:_) = x == 'p' && isDigit y
    isProblemDir _ = False

