#include "amxxmodule.h"
#include "sqlite_header.h"
#include "sqlheaders.h"

static g_ident = 0;

SqlFunctions g_SqliteFuncs = 
{
	&g_Sqlite,
	SetMysqlAffinity,
	NULL
};

int SetMysqlAffinity(AMX *amx)
{
	MF_AmxReRegister(amx, g_BaseSqlNatives, -1);
	MF_AmxReRegister(amx, g_ThreadSqlNatives, -1);

	return 0;
}

void OnAmxxAttach()
{
	MF_AddNatives(g_BaseSqlNatives);
	MF_AddNatives(g_ThreadSqlNatives);
	g_SqliteFuncs.prev = (SqlFunctions *)MF_RegisterFunctionEx(&g_SqliteFuncs, SQL_DRIVER_FUNC);

	MF_AddLibraries("dbi", LibType_Class, &g_ident);

	//override any mysqlx old compat stuff
	MF_AddNatives(g_OldCompatNatives);
	MF_OverrideNatives(g_OldCompatNatives);
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

