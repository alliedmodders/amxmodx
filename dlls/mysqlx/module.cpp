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

#include "amxxmodule.h"
#include "mysql2_header.h"
#include "sqlheaders.h"

static int g_ident = 0;

SqlFunctions g_MysqlFuncs = 
{
	&g_Mysql,
	SetMysqlAffinity,
	NULL
};

int SetMysqlAffinity(AMX *amx)
{
	MF_AmxReRegister(amx, g_BaseSqlNatives, -1);
	MF_AmxReRegister(amx, g_ThreadSqlNatives, -1);

	return 1;
}

void OnAmxxAttach()
{
	MF_AddNatives(g_BaseSqlNatives);
	MF_AddNatives(g_ThreadSqlNatives);
	g_MysqlFuncs.prev = (SqlFunctions *)MF_RegisterFunctionEx(&g_MysqlFuncs, SQL_DRIVER_FUNC);
	if (!MF_RequestFunction("GetDbDriver") 
		&& !MF_FindLibrary("SQLITE", LibType_Library))
	{
		MF_AddNatives(g_OldCompatNatives);
		MF_AddLibraries("dbi", LibType_Class, &g_ident);
	}
}

void OnAmxxDetach()
{
	ShutdownThreading();
	MF_RemoveLibraries(&g_ident);
}

void OnPluginsUnloaded()
{
	FreeAllHandles(Handle_OldResult);
	FreeAllHandles(Handle_OldDb);
	FreeAllHandles(Handle_Connection);
}

extern "C" void __cxa_pure_virtual(void)
{
}

