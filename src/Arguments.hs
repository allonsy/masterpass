module Arguments (processCmdLine) where

import System.Environment
import System.Console.GetOpt
import Control.Monad
import System.Exit


data Argument = SiteNameArgument String | FileArgument String
  deriving (Show, Eq)

isFileArgument :: Argument -> Bool
isFileArgument (FileArgument _) = True
isFileArgument _ = False

isSiteNameArgument :: Argument -> Bool
isSiteNameArgument (SiteNameArgument _) = True
isSiteNameArgument _ = False

extract :: Argument -> String
extract (FileArgument val) = val
extract (SiteNameArgument val) = val

-- if the user provides arguments, it decides what control flow to take
processCmdLine :: IO (Maybe String)
processCmdLine = do
  args <- getArgs
  let filenameArg = Option ['f'] ["filename"] (ReqArg (FileArgument) "FILENAME") "use password database at FILENAME"
  let sitenameArg = Option ['s'] ["sitename"] (ReqArg (SiteNameArgument) "SITENAME") "retrieve information for site SITENAME"
  let (opts, rems, errs) = getOpt Permute [filenameArg, sitenameArg] args
  if errs != [] then
    mapM_ putStrLn errs >> exitFailure 1 >> return Nothing
  else do
    if opts == [] then
      return Nothing
    else do
      let filenames = map extract (filter isFileArgument opts)
      case filenames of
        
