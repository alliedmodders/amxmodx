#include "amxxmodule.h"
#include "mysql2_header.h"

void OnAmxxAttach()
{
	MF_AddNatives(g_BaseSqlNatives);
	MF_AddNatives(g_ThreadSqlNatives);
	MF_RegisterFunction(&g_Mysql, "GetSqlDriver");
	if (!MF_RequestFunction("GetDbDriver"))
	{
		MF_AddNatives(g_OldCompatNatives);
	}
}

void ServerDeactivate_Post()
{
	FreeAllHandles(Handle_OldResult);
	FreeAllHandles(Handle_OldDb);
	FreeAllHandles(Handle_Connection);

	RETURN_META(MRES_IGNORED);
}

