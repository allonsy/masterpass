{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Types as T

foreign import ccall "mpw_masterKeyForUser" c_mpw_masterKeyForUser :: CString -> CString -> CUInt -> Ptr CUChar
foreign import ccall "mpw_passwordForSite"
  c_mpw_passwordForSite :: Ptr CUChar -> CString -> CUInt -> CUInt -> CUInt -> CString -> CUInt -> CString


main :: IO ()
main = do
  let v3 = T.algorithmToEnum T.Version3
  let plong = T.siteTypeToEnum T.Long
  let ppass = T.siteVariantToEnum T.Password
  name <- newCString "Alec S"
  masterPassword <- newCString "hellopwd"
  siteName <- newCString "google.com"
  let mkey = c_mpw_masterKeyForUser name masterPassword v3
  let pwd = c_mpw_passwordForSite mkey siteName plong (fromIntegral 1) ppass nullPtr v3
  pwdString <- peekCString pwd
  putStrLn pwdString
