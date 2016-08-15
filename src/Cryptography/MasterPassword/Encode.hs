module Cryptography.MasterPassword.Encode where

import Cryptography.MasterPassword.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Data.Word

foreign import ccall "mpw_masterKeyForUser" c_mpw_masterKeyForUser :: CString -> CString -> CUInt -> Ptr CUChar
foreign import ccall "mpw_passwordForSite"
  c_mpw_passwordForSite :: Ptr CUChar -> CString -> CUInt -> CUInt -> CUInt -> CString -> CUInt -> CString
foreign import ccall "mpw_free" c_mpw_free :: Ptr CUChar ->CUInt -> IO ()
foreign import ccall "getMPdkLen" c_getMPdkLen :: CUInt
foreign import ccall "mpw_freeString" c_mpw_freeString :: CString -> IO ()
foreign import ccall "mpw_identicon" c_mpw_identicon :: CString -> CString -> CString

type CounterType = Word32

-- Generates Master key for a given user. It returns an array of unsigned chars for use
-- in the encoding functions
masterKeyForUser :: CString -> CString -> AlgorithmVersion -> Ptr CUChar
masterKeyForUser name mpassword ver = c_mpw_masterKeyForUser name mpassword verEnum where
  verEnum = algorithmToEnum ver


passwordForSite :: Ptr CUChar -> CString -> SiteType -> Word32 -> SiteVariant -> CString -> AlgorithmVersion -> CString
passwordForSite mkey siteName typ counter variant context ver = c_mpw_passwordForSite mkey siteName typeEnum countNum variantEnum context verEnum where
  typeEnum = siteTypeToEnum typ
  countNum = fromIntegral counter
  variantEnum = siteVariantToEnum variant
  verEnum = algorithmToEnum ver


freeMasterKey :: Ptr CUChar -> IO ()
freeMasterKey mkey = c_mpw_free mkey c_getMPdkLen

freePassword :: CString -> IO ()
freePassword pwd = c_mpw_freeString pwd

identicon :: CString -> CString -> CString
identicon = c_mpw_identicon
