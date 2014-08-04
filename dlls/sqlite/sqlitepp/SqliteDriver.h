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

#ifndef _INCLUDE_SOURCEMOD_SQLITE_DRIVER_H
#define _INCLUDE_SOURCEMOD_SQLITE_DRIVER_H

#include "SqliteHeaders.h"

namespace SourceMod
{
	class SqliteDriver : public ISQLDriver
	{
	public:
		IDatabase *Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		IDatabase *Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		const char *NameString();
		bool IsCompatDriver(const char *namestr);
		int QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize);
	};
};

#endif //_INCLUDE_SOURCEMOD_SQLITE_DRIVER_H
