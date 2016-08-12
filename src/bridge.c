/* bridge.c by allonsy
 * bridging file between C constants and Haskell */
#include "MasterPassword/mpw-algorithm.h"
#include "MasterPassword/mpw-types.h"

uint getVersion3() {
  return MPAlgorithmVersion3;
}

uint getVersion2() {
  return MPAlgorithmVersion2;
}

uint getVersion1() {
  return MPAlgorithmVersion1;
}

uint getVersion0() {
  return MPAlgorithmVersion0;
}

uint getLongSiteType() {
  return MPSiteTypeGeneratedLong;
}

uint getPasswordVariant() {
  return MPSiteVariantPassword;
}

uint getLoginVariant() {
  return MPSiteVariantLogin;
}

uint getAnswerVariant() {
  return MPSiteVariantAnswer;
}
