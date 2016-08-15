module State where

import Cryptography.MasterPassword.Types
import Cryptography.MasterPassword.Encode
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types
import Data.Maybe
import System.FilePath
import System.Directory
import System.IO

data ManageState = ManageState {
  name :: String,
  version :: AlgorithmVersion,
  passwords :: [Password]
} deriving (Show, Read)

data Password = Password {
  sitename :: String,
  passVersion :: AlgorithmVersion,
  passType :: SiteType,
  passVariant :: SiteVariant,
  counter :: CounterType,
  context :: Maybe String
} deriving (Show, Read)

-- encodePassword takes in a master key and a password struct and returns
-- the password for that Password
-- Uses to the IO monad to convert to and from CStrings and free allocated resources
encodePassword :: Ptr CUChar -> Password -> IO String
encodePassword mkey pass = do
  siteNamePtr <- newCString $ sitename pass
  contextPtr <- if context pass == Nothing then return nullPtr else newCString $ fromJust (context pass)
  let cpassword = passwordForSite mkey siteNamePtr (passType pass) (counter pass) (passVariant pass) contextPtr (passVersion pass)
  hpassword <- peekCString cpassword
  freePassword cpassword
  return hpassword


-- commitDB commits the given DB to the given filename on disk
commitDB :: String -> ManageState -> IO ()
commitDB toSave db = do
  let dir = takeDirectory toSave
  let template = "mpass.db"
  (tempPath, hand) <- openTempFile dir template
  hPutStr hand (show db)
  hClose hand
  renamePath tempPath toSave

-- reads in a db from a given file
readDb :: String -> IO ManageState
readDb filename = do
  contents <- readFile filename
  return $ read contents
