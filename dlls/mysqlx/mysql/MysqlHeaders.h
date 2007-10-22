#ifndef _INCLUDE_SOURCEMOD_MYSQL_HEADERS_H
#define _INCLUDE_SOURCEMOD_MYSQL_HEADERS_H

#if _MSC_VER >= 1400
	/* Disable deprecation warnings concerning unsafe CRT functions */
	#if !defined _CRT_SECURE_NO_DEPRECATE
		#define _CRT_SECURE_NO_DEPRECATE
	#endif

	/* Replace the POSIX function with ISO C++ conformant ones as they are now deprecated */
	#define strnicmp _strnicmp

	/* Disable deprecation warnings because MSVC8 seemingly thinks the ISO C++ conformant 
	 * functions above are deprecated. */
	#pragma warning (disable:4996)
#endif

#include <ISQLDriver.h>
#if defined WIN32 || defined _WIN32
#include <winsock.h>
#endif
typedef unsigned long ulong;
#include <mysql.h>

#endif //_INCLUDE_SOURCEMOD_MYSQL_HEADERS_H
