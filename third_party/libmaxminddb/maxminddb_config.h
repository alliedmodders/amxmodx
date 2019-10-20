#ifndef MAXMINDDB_CONFIG_H
#define MAXMINDDB_CONFIG_H

#include <osdefs.h> // BYTE_ORDER, LITTLE_ENDIAN

/* This fixes a behavior change in after https://github.com/maxmind/libmaxminddb/pull/123. */
#if defined(BYTE_ORDER) && BYTE_ORDER == LITTLE_ENDIAN
	#define MMDB_LITTLE_ENDIAN 1
#endif

#ifndef MMDB_UINT128_USING_MODE
/* Define as 1 if we we use unsigned int __atribute__ ((__mode__(TI))) for uint128 values */
#define MMDB_UINT128_USING_MODE 0
#endif

#ifndef MMDB_UINT128_IS_BYTE_ARRAY
/* Define as 1 if we don't have an unsigned __int128 type */
#define MMDB_UINT128_IS_BYTE_ARRAY 1
#endif

#endif                          /* MAXMINDDB_CONFIG_H */
