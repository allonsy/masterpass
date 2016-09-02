{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Cryptography.MasterPassword.Types as T
import Cryptography.MasterPassword.Encode
import Foreign.C.String
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import State
import Generate
import System.Directory
import System.Exit

main :: IO ()
main = do
  doesExist <- doesFileExist "mpass.db"
  runDB <- if doesExist then readDb "mpass.db"
    else do
      putStrLn "I can't find a db file, creating a blank one"
      nam <- promptNonEmpty "Your Name: "
      ver <- promptVersion defaultVersion
      let newDB = ManageState nam ver []
      commitDB "mpass.db" newDB
      return newDB
  newDB <- runCLI runDB
  commitDB "mpass.db" newDB

runCLI :: ManageState -> IO (ManageState)
runCLI st = do
  mpass <- promptOncePassword "Please enter your master password: "
  username <- newCString (name st)
  mpassCString <- newCString mpass
  let mkey = masterKeyForUser username mpassCString (version st)
  let ident = identicon username mpassCString
  peekCString ident >>= putStrLn
  freePassword mpassCString
  free username
  free ident
  newST <- runMenu st mkey
  freeMasterKey mkey
  return newST

runMenu :: ManageState -> Ptr CUChar -> IO (ManageState)
runMenu st mkey = do
  putStrLn "What would you like to do? (enter ':h' for help)? "
  commandMaybe <- promptOnce "> "
  case commandMaybe of
    Nothing -> putStrLn "Quitting" >> return st
    Just cmd -> do
      case strip cmd of
        ":c" -> do
          newST <- createPwd st mkey
          commitDB "mpass.db" newST
          runMenu newST mkey
        ":q" -> putStrLn "Quitting" >> return st
        _ -> putStrLn "command not found" >> runMenu st mkey

createPwd :: ManageState -> Ptr CUChar -> IO (ManageState)
createPwd st mkey = do
  sname <- promptSiteName
  lname <- promptLoginName
  ver <- promptVersion (version st)
  typ <- promptSiteType defaultSiteType
  var <- promptSiteVariant defaultSiteVariant
  count <- promptSiteCounter 1
  let newPassword = State.Password sname ver typ var count Nothing lname
  pwd <- encodePassword mkey newPassword
  putStrLn $ "Your new password is: " ++ pwd
  let newST = st { passwords = newPassword : (passwords st)}
  return newST
