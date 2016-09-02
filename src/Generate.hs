module Generate where

import System.Console.Haskeline
import Cryptography.MasterPassword.Types
import System.Exit
import Data.Char
import Text.Read

dontUnderstand :: String
dontUnderstand = "I didn't understand your answer, please try again!"

promptOnce :: String -> IO (Maybe String)
promptOnce prompt = runInputT defaultSettings $ getInputLine prompt

promptOncePassword :: String -> IO (String)
promptOncePassword prompt = do
  pass <- runInputT defaultSettings $ getPassword (Just '*') prompt
  case pass of
    Nothing -> putStrLn dontUnderstand >> promptOncePassword prompt
    Just "" -> putStrLn "Your password cannot be empty, please try again" >> promptOncePassword prompt
    Just p  -> return p

promptNonEmpty :: String -> IO String
promptNonEmpty p = do
  maybeResponse <- promptOnce p
  case maybeResponse of
    Nothing -> putStrLn dontUnderstand >> promptNonEmpty p
    Just "" -> putStrLn "Your answer cannot be empty!" >> promptNonEmpty p
    Just val -> return val

promptVersion :: AlgorithmVersion -> IO AlgorithmVersion
promptVersion def = do
  putStrLn "Please enter the version number"
  putStrLn "0) Version 0"
  putStrLn "1) Version 1"
  putStrLn "2) Version 2"
  putStrLn "3) Version 3"
  loop where
    loop = do
      maybeResponse <- promptOnce $ "Please enter the number (eg '3' (default " ++ (show def) ++ " )): "
      case maybeResponse of
        Nothing -> putStrLn dontUnderstand >> loop
        Just val -> do
          case strip val of
            "0" -> return Version0
            "1" -> return Version1
            "2" -> return Version2
            "3" -> return Version3
            ""  -> putStrLn "using version 3" >> return def
            _ -> putStrLn dontUnderstand >> loop

promptSiteCounter :: CounterType -> IO CounterType
promptSiteCounter def = do
  countMaybe <- promptOnce $ "please enter the site counter (please enter a number (default " ++ show def ++ " )): "
  case countMaybe of
    Nothing -> exitSuccess >> return def
    Just "" -> return def
    Just numberString -> do
      let numberStrNoSpace = strip numberString
      case readMaybe numberStrNoSpace of
        Nothing -> putStrLn "please enter a valid number" >> promptSiteCounter def
        Just n -> return n

promptSiteType :: SiteType -> IO SiteType
promptSiteType def = do
  putStrLn $ "Please enter a site type, the default is: " ++ show def
  putStrLn "1) Maximum"
  putStrLn "2) Long"
  putStrLn "3) Medium"
  putStrLn "4) Basic"
  putStrLn "5) Short"
  putStrLn "6) PIN"
  putStrLn "7) Name"
  putStrLn "8) Phrase"
  putStrLn "9) Stored Personal"
  putStrLn "10) Stored Device"
  typeMaybe <- promptOnce $ "please enter the entry number (eg '3'): "
  case typeMaybe of
    Nothing -> exitSuccess >> return def
    Just "" -> return def
    Just numberString -> do
      case strip numberString of
        "1" -> return Maximum
        "2" -> return Long
        "3" -> return Medium
        "4" -> return Basic
        "5" -> return Short
        "6" -> return PIN
        "7" -> return Name
        "8" -> return Phrase
        "9" -> return StoredPersonal
        "10" -> return StoredDevice
        _ -> putStrLn dontUnderstand >> promptSiteType def


promptSiteVariant :: SiteVariant -> IO SiteVariant
promptSiteVariant def = do
  putStrLn $ "Please enter a site variant. The default is: " ++ show def
  putStrLn "1) Password"
  putStrLn "2) Login"
  putStrLn "3) Answer"
  variantMaybe <- promptOnce $ "Please enter the entry number (eg '3'): "
  case variantMaybe of
    Nothing -> exitSuccess >> return def
    Just "" -> return def
    Just variantString -> do
      case strip variantString of
        "1" -> return Password
        "2" -> return Login
        "3" -> return Answer
        _ -> putStrLn dontUnderstand >> promptSiteVariant def

promptLoginName :: IO (Maybe String)
promptLoginName = do
  lnameMaybe <- promptOnce "Please enter in a username (leave blank to not store the username): "
  case lnameMaybe of
    Nothing -> exitSuccess >> return Nothing
    Just "" -> return Nothing
    Just lname -> return $ Just lname


promptSiteName :: IO String
promptSiteName = do
  snameMaybe <- promptOnce "Please enter in a sitename: "
  case snameMaybe of
    Nothing -> exitSuccess >> return ""
    Just "" -> putStrLn "Site name cannot be empty" >> promptSiteName
    Just sname -> return sname

strip :: String -> String
strip = filter (\c -> not (isSpace c))
