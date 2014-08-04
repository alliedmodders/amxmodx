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

#ifndef _INCLUDE_SOURCEMOD_MYSQL_DRIVER_H
#define _INCLUDE_SOURCEMOD_MYSQL_DRIVER_H

#include "MysqlHeaders.h"

namespace SourceMod
{
	class MysqlDriver : public ISQLDriver
	{
	public:
		IDatabase *Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		IDatabase *Connect2(DatabaseInfo *info, int *errcode, char *error, size_t maxlength);
		const char *NameString();
		bool IsCompatDriver(const char *namestring);
		int QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize);
	public:
		IDatabase *_Connect(DatabaseInfo *info, int *errcode, char *error, size_t maxlength, bool do_timeout);
	};
};

#endif //_INCLUDE_SOURCEMOD_MYSQL_DRIVER_H
