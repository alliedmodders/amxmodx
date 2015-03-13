// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SQLite Module
//

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
