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
import Text.Read
import Data.List
import Data.Maybe
import Data.Char

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
  print $ name st
  print mpass
  let mkey = masterKeyForUser username mpassCString (version st)
  let ident = identicon username mpassCString
  peekCString ident >>= putStrLn
  free ident
  newST <- runMenu st mkey
  freePassword mpassCString
  free username
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
        ":l" -> listPwd st >> putStrLn "" >> runMenu st mkey
        ":s" -> showPwd st mkey >> putStrLn "" >> runMenu st mkey
        ":d" -> do
          newST <- deletePwd st
          commitDB "mpass.db" newST
          putStrLn ""
          runMenu newST mkey
        ":q" -> putStrLn "Quitting" >> return st
        _ -> putStrLn "command not found" >> runMenu st mkey

listPwd :: ManageState -> IO ()
listPwd st = do
  enumeratePwds (passwords st) 1 where
    enumeratePwds [] _ =  return ()
    enumeratePwds (x:xs) k = putStrLn ((show k) ++ ") " ++ sitename x ) >> enumeratePwds xs (k+1)

showPwd :: ManageState -> Ptr CUChar -> IO ()
showPwd st mkey = do
  putStrLn "Which password would you like to show?"
  listPwd st
  answer <- promptNonEmpty "Please enter the number or site name (or ':b' to return to the menu): "
  if strip answer == ":b" then return ()
  else do
    case readMaybe answer :: Maybe Int of
      Nothing -> do
        let matchPassMaybe = find (\pass -> sitename pass == strip answer) (passwords st)
        case matchPassMaybe of
          Nothing -> putStrLn "Site not found" >> showPwd st mkey
          Just pass -> showGivenPwd pass
      Just k -> do
        if k <= length (passwords st) then do
          let pass = (passwords st) !! (k-1)
          showGivenPwd pass
        else putStrLn "index out of range" >> showPwd st mkey
  where
    showGivenPwd pass = do
      if isJust (loginName pass) then
        putStrLn $ "Your username is: " ++ (fromJust (loginName pass))
      else
        return ()
      sitepass <- encodePassword mkey pass
      putStrLn $ "Your password is: " ++ sitepass


deletePwd :: ManageState -> IO ManageState
deletePwd st = do
  putStrLn "Which password would you like to delete"
  listPwd st
  answer <- promptNonEmpty "Please enter the number or site name (or ':b' to return to the menu): "
  if strip answer == ":b" then return st
  else do
    case readMaybe answer :: Maybe Int of
      Nothing -> do
        let matchPassMaybe = find (\pass -> sitename pass == strip answer) (passwords st)
        case matchPassMaybe of
          Nothing -> putStrLn "Site not found" >> deletePwd st
          Just pass -> delGivenPwd pass
      Just k -> do
        if k <= length (passwords st) then do
          let pass = (passwords st) !! (k-1)
          delGivenPwd pass
        else putStrLn "index out of range" >> deletePwd st
  where
    delGivenPwd pass = do
      putStrLn $ "Are you sure you want to delete the password for site: " ++ (sitename pass)
      answer <- promptNonEmpty $ "y/n: "
      case map toLower (strip answer) of
        "y" -> putStrLn "Deleted" >> return $ st {passwords = filter (/= pass) (passwords st)}
        "n" -> return st
        _   -> putStrLn "Input not understood. Please enter 'y' or 'n'" >> delGivenPwd pass

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
