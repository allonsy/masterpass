{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

foreign import ccall "getVersion3" c_getVersion3 :: CInt
foreign import ccall "mpw_masterKeyForUser" c_mpw_masterKeyForUser :: CString -> CString -> CInt -> Ptr CUChar
foreign import ccall "getLongSiteType" c_getLongSiteType :: CInt
foreign import ccall "getPasswordType" c_getPasswordType :: CInt
foreign import ccall "mpw_passwordForSite"
  c_mpw_passwordForSite :: Ptr CUChar -> CString -> CInt -> CUInt -> CInt -> CString -> CInt -> CString


main :: IO ()
main = do
  let v3 = c_getVersion3
  let plong = c_getLongSiteType
  let ppass = c_getPasswordType
  name <- newCString "Alec S"
  masterPassword <- newCString "hellopwd"
  siteName <- newCString "google.com"
  let mkey = c_mpw_masterKeyForUser name masterPassword v3
  let pwd = c_mpw_passwordForSite mkey siteName plong (fromIntegral 1) ppass nullPtr v3
  pwdString <- peekCString pwd
  putStrLn pwdString
