{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Cryptography.MasterPassword.Types as T
import Cryptography.MasterPassword.Encode
import Foreign.C.String
import Foreign.Ptr



main :: IO ()
main = do
  let v3 = T.Version3
  let plong = T.Long
  let ppass = T.Password
  name <- newCString "Alec S"
  masterPassword <- newCString "hellopwd"
  let ident = identicon name masterPassword
  identString <- peekCString ident
  siteName <- newCString "google.com"
  let mkey = masterKeyForUser name masterPassword v3
  let pwd = passwordForSite mkey siteName plong 1 ppass nullPtr v3
  pwdString <- peekCString pwd
  freeMasterKey mkey
  freePassword pwd
  freePassword ident
  putStrLn identString
  putStrLn pwdString
