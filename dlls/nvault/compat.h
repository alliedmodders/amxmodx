#ifndef _INCLUDE_COMPAT_H
#define _INCLUDE_COMPAT_H

#ifdef WIN32
typedef	__int16				int16_t;
typedef	unsigned __int16	uint16_t;
typedef	__int8				int8_t;
typedef unsigned __int8		uint8_t;

#else

#include <stdint.h>

#endif

#endif //_INCLUDE_COMPAT_H
