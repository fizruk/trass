module CourseRepository where

import Import

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
data CourseProblem    = CourseProblem
data CourseSubmission = CourseSubmission

readCourseRepository :: Text -> Handler CourseRepository
readCourseRepository name = CourseRepository
  <$> pure name
  <*> pure ""
  <*> pure [whamlet| _{MsgCourseNoDescription} |]
  <*> pure []
  <*> pure []
  <*> pure []
  <*> pure []
