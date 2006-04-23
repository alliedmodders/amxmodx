#include "amxxmodule.h"
#include "mysql2_header.h"

void OnAmxxAttach()
{
	MF_AddNatives(g_BaseSqlNatives);
	MF_AddNatives(g_ThreadSqlNatives);
}
