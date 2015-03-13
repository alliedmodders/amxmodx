// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// MySQL Module
//

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
#include <WinSock2.h>
#endif
typedef unsigned long ulong;
#include <mysql.h>

#endif //_INCLUDE_SOURCEMOD_MYSQL_HEADERS_H
