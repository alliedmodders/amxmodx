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

#ifndef _INCLUDE_SQLHEADERS_H
#define _INCLUDE_SQLHEADERS_H

#include "ISQLDriver.h"

#define SQL_DRIVER_FUNC	"GetSqlFuncs"

typedef int (*SQLAFFINITY)(AMX *amx);

struct SqlFunctions
{
	SourceMod::ISQLDriver *driver;
	SQLAFFINITY set_affinity;
	SqlFunctions *prev;
};

#endif //_INCLUDE_SQLHEADERS_H
