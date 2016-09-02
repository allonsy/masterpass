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
import Data.Maybe
import Data.List
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
        ":cmpw" -> do
          newPwd <- promptOncePassword "Please enter a new password: "
          newPwdCString <- newCString newPwd
          username <- newCString (name st)
          let ident = identicon username newPwdCString
          peekCString ident >>= putStrLn
          free ident
          let newMKey = masterKeyForUser username newPwdCString (version st)
          ret <- runMenu st newMKey
          freePassword newPwdCString
          free username
          freeMasterKey newMKey
          return ret
        ":cver" -> do
          newVer <- promptVersion (version st)
          let newST = st {version = newVer}
          runMenu newST mkey
        ":cname" -> do
          newName <- promptNonEmpty "Please enter a new name: "
          let newST = st {name = newName}
          commitDB "mpass.db" newST
          runMenu newST mkey
        ":l" -> listPwd st >> putStrLn "" >> runMenu st mkey
        ":s" -> showPwd st mkey >> putStrLn "" >> runMenu st mkey
        ":i" -> do
          newST <- incrementPwd st mkey
          commitDB "mpass.db" newST
          putStrLn ""
          runMenu st mkey
        ":d" -> do
          newST <- deletePwd st
          commitDB "mpass.db" newST
          putStrLn ""
          runMenu newST mkey
        ":q" -> putStrLn "Quitting" >> return st
        _ -> putStrLn "command not found" >> runMenu st mkey

showPwd :: ManageState -> Ptr CUChar -> IO ()
showPwd st mkey = do
  passMaybe <- promptPassword "Which password would you like to show?" st
  case passMaybe of
    Nothing -> return ()
    Just pass -> showGivenPwd pass
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
  passMaybe <- promptPassword "Which password would you like to delete" st
  case passMaybe of
    Nothing -> return st
    Just pass -> delGivenPwd pass
  where
    delGivenPwd pass = do
      putStrLn $ "Are you sure you want to delete the password for site: " ++ (sitename pass)
      answer <- promptNonEmpty $ "y/n: "
      case map toLower (strip answer) of
        "y" -> putStrLn "Deleted" >> (return  (st {passwords = delete pass (passwords st)}) )
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

incrementPwd :: ManageState -> Ptr CUChar -> IO (ManageState)
incrementPwd st mkey = do
  passMaybe <- promptPassword "Which password would you like to increment" st
  case passMaybe of
    Nothing -> return st
    Just pass -> do
      confirm <- promptConfirm $ "Are you sure you want to increment site: " ++ sitename pass ++ "?"
      if confirm then do
        let newPass = pass {counter = (counter pass) + 1}
        newPassword <- encodePassword mkey newPass
        putStrLn $ "Your new password is: " ++ newPassword
        let newPasswords = replace (passwords st) pass newPass
        let newST = st {passwords = newPasswords}
        return newST
      else return st
  where
    replace [] _ new = [new]
    replace (x:xs) old new
      | x == old = new : xs
      | otherwise = x: replace xs old new
