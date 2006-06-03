#ifndef _INCLUDE_SOURCEMOD_SQLITE_HEADERS_H
#define _INCLUDE_SOURCEMOD_SQLITE_HEADERS_H

#if _MSC_VER >= 1400
	/* disable deprecation warnings */
	#if !defined _CRT_SECURE_NO_DEPRECATE
		#define	_CRT_SECURE_NO_DEPRECATE
	#endif

	#pragma warning (disable:4996)
#endif //_MSC_VER >= 1400

#include <ISQLDriver.h>

#include "sqlite3.h"

#endif //_INCLUDE_SOURCEMOD_SQLITE_HEADERS_H
