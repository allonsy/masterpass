/* bridge.c by allonsy
 * bridging file between C constants and Haskell */
#include "MasterPassword/mpw-algorithm.h"
#include "MasterPassword/mpw-types.h"


// Algorithm enums
unsigned int getVersion3() {
  return MPAlgorithmVersion3;
}

unsigned int getVersion2() {
  return MPAlgorithmVersion2;
}

unsigned int getVersion1() {
  return MPAlgorithmVersion1;
}

unsigned int getVersion0() {
  return MPAlgorithmVersion0;
}

// Site Variant enums
unsigned int getPasswordVariant() {
  return MPSiteVariantPassword;
}

unsigned int getLoginVariant() {
  return MPSiteVariantLogin;
}

unsigned int getAnswerVariant() {
  return MPSiteVariantAnswer;
}

// Site Type enums
unsigned int getMaximumSiteType() {
  return MPSiteTypeGeneratedMaximum;
}

unsigned int getLongSiteType() {
  return MPSiteTypeGeneratedLong;
}

unsigned int getMediumSiteType() {
  return MPSiteTypeGeneratedMedium;
}

unsigned int getBasicSiteType() {
  return MPSiteTypeGeneratedBasic;
}

unsigned int getShortSiteType() {
  return MPSiteTypeGeneratedShort;
}

unsigned int getPinSiteType() {
  return MPSiteTypeGeneratedPIN;
}

unsigned int getNameSiteType() {
  return MPSiteTypeGeneratedName;
}

unsigned int getPhraseSiteType() {
  return MPSiteTypeGeneratedPhrase;
}

unsigned int getStoredPersonalSiteType() {
  return MPSiteTypeStoredPersonal;
}

unsigned int getStoredDevicePrivateSiteType() {
  return MPSiteTypeStoredDevicePrivate;
}
