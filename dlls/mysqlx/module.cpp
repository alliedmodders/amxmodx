#include "amxxmodule.h"
#include "mysql2_header.h"

static g_ident = 0;

void OnAmxxAttach()
{
	MF_AddNatives(g_BaseSqlNatives);
	MF_AddNatives(g_ThreadSqlNatives);
	MF_RegisterFunction(&g_Mysql, "GetSqlDriver");
	if (!MF_RequestFunction("GetDbDriver"))
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

