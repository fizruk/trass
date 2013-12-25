{-# LANGUAGE ScopedTypeVariables #-}
module CourseRepository where

import Import
import qualified Control.Exception as E

import Data.Char
import Data.List
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import qualified Data.Yaml.Config as Y

import Text.Markdown
import Text.Highlighting.Kate

import System.Directory
import System.FilePath

-- | A course is just a root section.
type CourseRepository = CourseSection

-- | A course (sub)section.
data CourseSection = CourseSection
  { csPath        :: FilePath             -- ^ Full path to the section directory.
  , csTitle       :: IO (Maybe Text)      -- ^ IO action to get title (if present).
  , csSummary     :: IO (Maybe Html)      -- ^ IO action to get summary (if present).
  , csDescription :: IO (Maybe Html)      -- ^ IO action to get description (if present).
  , csSubsections :: IO [CourseSection]   -- ^ IO action to get subsections.
  , csProblems    :: IO [CourseProblem]   -- ^ IO action to get section problems.
  }

-- | A course problem.
data CourseProblem = CourseProblem
  { cpPath        :: FilePath             -- ^ Full path to the problem directory.
  , cpTitle       :: IO (Maybe Text)      -- ^ IO action to get title (if present).
  , cpDescription :: IO (Maybe Html)      -- ^ IO action to get description (if present).
  , cpSnippet     :: IO (Maybe Text)      -- ^ IO action to get problem solution snippet (if present).
  , cpConfig      :: IO (Maybe Y.Config)  -- ^ IO action to get YAML config for a problem (if present).
  }

-- | Make breadcrumbs with titles of sections.
contentsBreadcrumbs :: Text -> [Text] -> Handler [([Text], Maybe Text)]
contentsBreadcrumbs course path = do
  let paths = tail (inits path)
  sections <- mapM (sectionContents course) paths
  titles   <- liftIO $ mapM csTitle sections
  return $ zip paths titles

-- | Get course by given path.
-- If course does not exist user is given 404 page (Not Found).
courseContents404 :: Text -> Handler CourseRepository
courseContents404 name = sectionContents404 name []

-- | Get course root section.
-- NOTE: Existence of a course is not checked.
courseContents :: Text -> Handler CourseRepository
courseContents name = sectionContents name []

-- | Get course section by given path.
-- If section does not exist user is given 404 page (Not Found).
sectionContents404 :: Text -> [Text] -> Handler CourseSection
sectionContents404 course path = section <$> getCoursePath404 course path

-- | Get course section by given path.
-- NOTE: Existence of a course is not checked.
sectionContents :: Text -> [Text] -> Handler CourseSection
sectionContents course path = section <$> getCoursePath course path

-- | Get course problem by given path.
-- If problem does not exist user is given 404 page (Not Found).
problemContents404 :: Text -> [Text] -> Handler CourseProblem
problemContents404 course path = problem <$> getCoursePath404 course path

-- | Get course problem by given path.
-- NOTE: Existence of a course is not checked.
problemContents :: Text -> [Text] -> Handler CourseProblem
problemContents course path = problem <$> getCoursePath course path

-- | Get course object filepath by given path.
-- If filepath does not exist user is given 404 page (Not Found).
getCoursePath404 :: Text -> [Text] -> Handler FilePath
getCoursePath404 course tpath = do
  path <- getCoursePath course tpath
  exists <- liftIO $ doesDirectoryExist path
  if exists then return path else notFound

-- | Get course object filepath by given path.
-- NOTE: Existence of a filepath is not checked.
getCoursePath :: Text -> [Text] -> Handler FilePath
getCoursePath course tpath = do
  lang <- fromMaybe "en" <$> lookupSession "_LANG"
  let path = System.FilePath.joinPath $ map T.unpack (course : lang : tpath)
  return $ "courses" </> "contents" </> path

-- | Try an IO action with different file extensions until it gives the
-- result. If none does - return Nothing.
tryExts :: [FilePath] -> FilePath -> (FilePath -> IO a) -> IO (Maybe a)
tryExts [] _ _ = return Nothing
tryExts (ext:exts) path act = (Just <$> act (path ++ ext)) `E.catch` \(_ :: E.SomeException) -> tryExts exts path act

-- | Read and render markdown from a file.
readMarkdown :: FilePath -> IO Html
readMarkdown path = markdown def {msBlockCodeRenderer = kateBlockCodeRenderer} <$> TL.readFile path

-- | Highlighting code with Kate Highlighter.
kateBlockCodeRenderer :: Maybe Text -> (Text, Html) -> Html
kateBlockCodeRenderer lang (src, _) = formatHtmlBlock defaultFormatOpts $ highlightAs (maybe "text" T.unpack lang) $ T.unpack src

-- | Read a section from a directory.
section :: FilePath -> CourseSection
section = CourseSection
  <$> id
  <*> readTitle
  <*> readSummary
  <*> readDesc
  <*> readSubsections
  <*> readProblems

-- | Read a problem from a directory.
problem :: FilePath -> CourseProblem
problem = CourseProblem
  <$> id
  <*> readTitle
  <*> readDesc
  <*> readSnippet
  <*> readConfig

-- | Read title file from directory.
readTitle :: FilePath -> IO (Maybe Text)
readTitle path = tryExts [".txt"] (path </> "title") T.readFile

-- | Read snippet file from directory.
readSnippet :: FilePath -> IO (Maybe Text)
readSnippet path = do
  exts <- map takeExtension . filter ("snippet." `isPrefixOf`) <$> getDirectoryContents path
  tryExts exts (path </> "snippet") T.readFile

-- | Read summary file from directory.
readSummary :: FilePath -> IO (Maybe Html)
readSummary path = tryExts [".md", ".markdown"] (path </> "summary") readMarkdown

-- | Read description file (Markdown) from directory.
readDesc :: FilePath -> IO (Maybe Html)
readDesc path = tryExts [".md", ".markdown"] (path </> "desc") readMarkdown

-- | Read subsections from a section directory.
readSubsections :: FilePath -> IO [CourseSection]
readSubsections path = map section . map (path </>) . sort . filter isSectionDir <$> getDirectoryContents path
  where
    isSectionDir (x:_) = isDigit x
    isSectionDir _ = False

-- | Read YAML configuration file from a directory.
readConfig :: FilePath -> IO (Maybe Y.Config)
readConfig path = tryExts [".yml", ".yaml"] (path </> "trass") Y.load

-- | Read problems from a section directory.
readProblems :: FilePath -> IO [CourseProblem]
readProblems path = map problem . map (path </>) . sort . filter isProblemDir <$> getDirectoryContents path
  where
    isProblemDir (x:y:_) = x == 'p' && isDigit y
    isProblemDir _ = False

