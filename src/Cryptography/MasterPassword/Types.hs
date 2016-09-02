module Cryptography.MasterPassword.Types where

import Foreign.C.Types
import Data.Word

foreign import ccall "getVersion3" c_getVersion3 :: CUInt
foreign import ccall "getVersion2" c_getVersion2 :: CUInt
foreign import ccall "getVersion1" c_getVersion1 :: CUInt
foreign import ccall "getVersion0" c_getVersion0 :: CUInt

foreign import ccall "getMaximumSiteType" c_getMaximumSiteType :: CUInt
foreign import ccall "getLongSiteType" c_getLongSiteType :: CUInt
foreign import ccall "getMediumSiteType" c_getMediumSiteType :: CUInt
foreign import ccall "getBasicSiteType" c_getBasicSiteType :: CUInt
foreign import ccall "getShortSiteType" c_getShortSiteType :: CUInt
foreign import ccall "getPinSiteType" c_getPinSiteType :: CUInt
foreign import ccall "getNameSiteType" c_getNameSiteType :: CUInt
foreign import ccall "getPhraseSiteType" c_getPhraseSiteType :: CUInt
foreign import ccall "getStoredPersonalSiteType" c_getStoredPersonalSiteType :: CUInt
foreign import ccall "getStoredDevicePrivateSiteType" c_getStoredDevicePrivateSiteType :: CUInt

foreign import ccall "getPasswordVariant" c_getPasswordVariant :: CUInt
foreign import ccall "getLoginVariant" c_getLoginVariant :: CUInt
foreign import ccall "getAnswerVariant" c_getAnswerVariant :: CUInt

type CounterType = Word32

data AlgorithmVersion =
  Version0 |
  Version1 |
  Version2 |
  Version3
  deriving (Show, Read)

algorithmToEnum :: AlgorithmVersion -> CUInt
algorithmToEnum Version3 = c_getVersion3
algorithmToEnum Version2 = c_getVersion2
algorithmToEnum Version1 = c_getVersion1
algorithmToEnum Version0 = c_getVersion0

defaultVersion :: AlgorithmVersion
defaultVersion = Version3


data SiteType =
  Maximum |
  Long    |
  Medium  |
  Basic   |
  Short   |
  PIN     |
  Name    |
  Phrase  |
  StoredPersonal |
  StoredDevice
  deriving (Show, Read)

defaultSiteType :: SiteType
defaultSiteType = Long

siteTypeToEnum :: SiteType -> CUInt
siteTypeToEnum Maximum = c_getMaximumSiteType
siteTypeToEnum Long = c_getLongSiteType
siteTypeToEnum Medium = c_getMediumSiteType
siteTypeToEnum Basic = c_getBasicSiteType
siteTypeToEnum Short = c_getShortSiteType
siteTypeToEnum PIN = c_getShortSiteType
siteTypeToEnum Name = c_getNameSiteType
siteTypeToEnum Phrase = c_getPhraseSiteType
siteTypeToEnum StoredPersonal = c_getStoredPersonalSiteType
siteTypeToEnum StoredDevice = c_getStoredDevicePrivateSiteType


data SiteVariant =
  Password |
  Login    |
  Answer
  deriving (Show, Read)

defaultSiteVariant :: SiteVariant
defaultSiteVariant = Password

siteVariantToEnum :: SiteVariant -> CUInt
siteVariantToEnum Password = c_getPasswordVariant
siteVariantToEnum Login = c_getLoginVariant
siteVariantToEnum Answer = c_getAnswerVariant
