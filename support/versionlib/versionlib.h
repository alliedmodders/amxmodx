#ifndef _INCLUDE_AMXX_VERSIONLIB_H_
#define _INCLUDE_AMXX_VERSIONLIB_H_

#if !defined(AMXX_USE_VERSIONLIB)
// These get defined in amxmodx_version.h since
// versionlib does not use versionlib.
    #undef AMXX_LOCAL_REV
    #undef AMXX_CSET
    #undef AMXX_VERSION
    #undef AMXX_BUILD_TIME
#endif

#ifdef __cplusplus
    #define EXTERN_C extern "C"
#else
    #define EXTERN_C extern
#endif

EXTERN_C const char *AMXX_LOCAL_REV;
EXTERN_C const char *AMXX_SHA;
EXTERN_C const char *AMXX_VERSION;
EXTERN_C const char *AMXX_BUILD_TIME;

#endif // _INCLUDE_AMXX_VERSIONLIB_H_
