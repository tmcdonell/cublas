/*
 * Extra bits for CUBLAS bindings
 */

#ifndef C_STUBS_H
#define C_STUBS_H

/*
 * We need to work around some shortcomings in the C parser of c2hs by disabling advanced attributes etc on Apple platforms.
 */
#ifdef __APPLE__
#define _ANSI_SOURCE
#define __AVAILABILITY__
#define __OSX_AVAILABLE_STARTING(_mac, _iphone)
#define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep)
#endif

#include <cublas_v2.h>

#endif /* C_STUBS_H */

