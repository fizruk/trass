import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

import CourseRepository (updateRepositoriesDaemon)
import Control.Concurrent (forkIO)

main :: IO ()
main = do
  forkIO updateRepositoriesDaemon
  defaultMain (fromArgs parseExtra) makeApplication
