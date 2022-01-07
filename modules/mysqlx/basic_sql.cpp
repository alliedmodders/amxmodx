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

#include <stdio.h>
#include "mysql2_header.h"
#include "sqlheaders.h"

using namespace SourceMod;

MysqlDriver g_Mysql;

void FreeConnection(void *p, unsigned int num)
{
	SQL_Connection *cn = (SQL_Connection *)p;

	free(cn->host);
	free(cn->user);
	free(cn->pass);
	free(cn->db);
	free(cn->charset);

	delete cn;
}

void FreeQuery(void *p, unsigned int num)
{
	AmxQueryInfo *qry = (AmxQueryInfo *)p;

	qry->pQuery->FreeHandle();
	delete qry;
}

void FreeDatabase(void *p, unsigned int num)
{
	IDatabase *db = (IDatabase *)p;

	db->FreeHandle();
}

static cell AMX_NATIVE_CALL SQL_MakeDbTuple(AMX *amx, cell *params)
{
	SQL_Connection *sql = new SQL_Connection;
	int len;

	char *host =  strdup(MF_GetAmxString(amx, params[1], 0, &len));

	char *p = strchr(host, ':');
	if (p)
	{
		sql->port = atoi(p+1);
		*p = '\0';
	} else {
		sql->port = 0;
	}

	sql->host = host;
	sql->user = strdup(MF_GetAmxString(amx, params[2], 0, &len));
	sql->pass = strdup(MF_GetAmxString(amx, params[3], 0, &len));
	sql->db = strdup(MF_GetAmxString(amx, params[4], 0, &len));
	if (params[0] / sizeof(cell) >= 5)
	{
		sql->max_timeout = static_cast<unsigned int>(params[5]);
	}
	sql->charset = NULL;

	unsigned int num = MakeHandle(sql, Handle_Connection, FreeConnection);

	return num;
}

static cell AMX_NATIVE_CALL SQL_FreeHandle(AMX *amx, cell *params)
{
	if (!FreeHandle(params[1]))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid handle: %d", params[1]);
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL SQL_Connect(AMX *amx, cell *params)
{
	SQL_Connection *sql = (SQL_Connection *)GetHandle(params[1], Handle_Connection);
	if (!sql)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid info tuple handle: %d", params[1]);
		return 0;
	}

	DatabaseInfo nfo;
	nfo.database = sql->db;
	nfo.user = sql->user;
	nfo.pass = sql->pass;
	nfo.port = sql->port;
	nfo.host = sql->host;
	nfo.max_timeout = sql->max_timeout;
	nfo.charset = sql->charset;

	char buffer[512];
	int errcode;

	IDatabase *pDb = g_Mysql.Connect2(&nfo, &errcode, buffer, sizeof(buffer)-1);

	if (!pDb)
	{
		cell *c_err = MF_GetAmxAddr(amx, params[2]);

		*c_err = errcode;
		MF_SetAmxString(amx, params[3], buffer, params[4]);

		return 0;
	}

	return MakeHandle(pDb, Handle_Database, FreeDatabase);
}

static cell AMX_NATIVE_CALL SQL_PrepareQuery(AMX *amx, cell *params)
{
	IDatabase *pDb = (IDatabase *)GetHandle(params[1], Handle_Database);
	if (!pDb)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid database handle: %d", params[1]);
		return 0;
	}

	int len;
	char *fmt = MF_FormatAmxString(amx, params, 2, &len);

	IQuery *pQuery = pDb->PrepareQuery(fmt);
	if (!pQuery)
		return 0;

	AmxQueryInfo *qinfo = new AmxQueryInfo;
	qinfo->pQuery = pQuery;

	memset(&qinfo->info, 0, sizeof(QueryInfo));

	return MakeHandle(qinfo, Handle_Query, FreeQuery);
}

static cell AMX_NATIVE_CALL SQL_Execute(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	qInfo->error[0] = '\0';

	memset(&qInfo->info, 0, sizeof(QueryInfo));

	if (!qInfo->pQuery->Execute2(&qInfo->info, qInfo->error, 254))
	{
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL SQL_QueryError(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	MF_SetAmxString(amx, params[2], qInfo->error, params[3]);

	return qInfo->info.errorcode;
}

static cell AMX_NATIVE_CALL SQL_MoreResults(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	if (!qInfo->info.rs)
		return 0;

	return (qInfo->info.rs->IsDone() ? 0 : 1);
}

static cell AMX_NATIVE_CALL SQL_IsNull(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs || rs->IsDone())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	unsigned int col = static_cast<unsigned int>(params[2]);
	if (col >= rs->FieldCount())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid column: %d", col);
		return 0;
	}

	IResultRow *rr = rs->GetRow();

	return rr->IsNull(col) ? 1 : 0;
}

static cell AMX_NATIVE_CALL SQL_ReadResult(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs || rs->IsDone())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	IResultRow *row = rs->GetRow();

	unsigned int col = static_cast<unsigned int>(params[2]);
	if (col >= rs->FieldCount())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid column: %d", col);
		return 0;
	}

	cell numparams = params[0] / sizeof(cell);
	switch (numparams)
	{
	case 4:
		{
			const char *str = row->GetString(col);
			if (!str)
				str = "";
			cell *len = MF_GetAmxAddr(amx, params[4]);
			MF_SetAmxString(amx, params[3], str, (int)*len);
			break;
		}
	case 3:
		{
			REAL num = row->GetFloat(col);
			cell *addr = MF_GetAmxAddr(amx, params[3]);
			*addr = amx_ftoc(num);
			break;
		}
	case 2:
		{
			int num = row->GetInt(col);
			return num;
		}
	default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Bad number of arguments passed.");
			break;
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL SQL_NextRow(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs || rs->IsDone())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	rs->NextRow();

	return 1;
}

static cell AMX_NATIVE_CALL SQL_AffectedRows(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	return static_cast<cell>(qInfo->info.affected_rows);
}

static cell AMX_NATIVE_CALL SQL_NumResults(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs)
	{
		return 0;
	}

	return rs->RowCount();
}

static cell AMX_NATIVE_CALL SQL_NumColumns(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	return rs->FieldCount();
}

static cell AMX_NATIVE_CALL SQL_FieldNumToName(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	unsigned int col = static_cast<unsigned int>(params[2]);
	const char *namewa = rs->FieldNumToName(col);

	if (!namewa)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid column: %d", col);
		return 0;
	}

	MF_SetAmxString(amx, params[3], namewa, params[4]);

	return 1;
}

static cell AMX_NATIVE_CALL SQL_GetQueryString(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);

	if (!qInfo || (!qInfo->pQuery && !qInfo->opt_ptr))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	const char *ptr = qInfo->pQuery ? qInfo->pQuery->GetQueryString() : qInfo->opt_ptr;

	return MF_SetAmxString(amx, params[2], ptr, params[3]);
}

static cell AMX_NATIVE_CALL SQL_FieldNameToNum(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	int len;
	char *namewa = MF_GetAmxString(amx, params[2], 0, &len);
	unsigned int columnId;
	if (!rs->FieldNameToNum(namewa, &columnId))
	{
		return -1;
	}

	return columnId;
}

static cell AMX_NATIVE_CALL SQL_GetInsertId(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	return qInfo->info.insert_id;
}

static cell AMX_NATIVE_CALL SQL_GetAffinity(AMX *amx, cell *params)
{
	return MF_SetAmxString(amx, params[1], g_Mysql.NameString(), params[2]);
}

static cell AMX_NATIVE_CALL SQL_SetAffinity(AMX *amx, cell *params)
{
	int len;
	char *str = MF_GetAmxString(amx, params[1], 0, &len);

	if (!str[0])
	{
		return 1;
	}

	if (stricmp(str, g_Mysql.NameString()) == 0)
	{
		return 1;
	}

	SqlFunctions *pFuncs = (SqlFunctions *)MF_RequestFunction(SQL_DRIVER_FUNC);
	while (pFuncs)
	{
		if (pFuncs->driver->IsCompatDriver(str))
		{
			return pFuncs->set_affinity(amx);
		}
		pFuncs = pFuncs->prev;
	}

	return 0;
}

static cell AMX_NATIVE_CALL SQL_Rewind(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	rs->Rewind();

	return 1;
}

static cell AMX_NATIVE_CALL SQL_NextResultSet(AMX *amx, cell *params)
{
	AmxQueryInfo *qInfo = (AmxQueryInfo *)GetHandle(params[1], Handle_Query);
	if (!qInfo)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid query handle: %d", params[1]);
		return 0;
	}

	IResultSet *rs = qInfo->info.rs;

	if (!rs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No result set in this query!");
		return 0;
	}

	if (rs->NextResultSet())
	{
		return 1;
	}
	else
	{
		qInfo->info.rs = NULL;
		return 0;
	}
}

static cell AMX_NATIVE_CALL SQL_QuoteString(AMX *amx, cell *params)
{
	int len;
	char *str = MF_GetAmxString(amx, params[4], 0, &len);
	size_t newsize;
	static char buffer[8192];

	if (params[1] != 0)
	{
		IDatabase *pDb = (IDatabase *)GetHandle(params[1], Handle_Database);
		if (!pDb)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid database handle: %d", params[1]);
			return 0;
		}

		if (pDb->QuoteString(str, buffer, sizeof(buffer)-1, &newsize) == 0)
		{
			MF_SetAmxString(amx, params[2], buffer, params[3]);
			return newsize;
		} else {
			return -1;
		}
	} else {
		if (g_Mysql.QuoteString(str, buffer, sizeof(buffer)-1, &newsize) == 0)
		{
			MF_SetAmxString(amx, params[2], buffer, params[3]);
			return newsize;
		} else {
			return -1;
		}
	}
}

static cell AMX_NATIVE_CALL SQL_QuoteStringFmt(AMX *amx, cell *params)
{
	int len;
	char *str = MF_FormatAmxString(amx, params, 4, &len);
	size_t newsize;
	static char buffer[8192];

	if (params[1] != 0)
	{
		IDatabase *pDb = (IDatabase *)GetHandle(params[1], Handle_Database);
		if (!pDb)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid database handle: %d", params[1]);
			return 0;
		}

		if (pDb->QuoteString(str, buffer, sizeof(buffer)-1, &newsize) == 0)
		{
			MF_SetAmxString(amx, params[2], buffer, params[3]);
			return newsize;
		} else {
			return -1;
		}
	} else {
		if (g_Mysql.QuoteString(str, buffer, sizeof(buffer)-1, &newsize) == 0)
		{
			MF_SetAmxString(amx, params[2], buffer, params[3]);
			return newsize;
		} else {
			return -1;
		}
	}
}

static cell AMX_NATIVE_CALL SQL_SetCharset(AMX *amx, cell *params)
{
	SQL_Connection *sql = (SQL_Connection *)GetHandle(params[1], Handle_Connection);
	if (!sql)
	{
		IDatabase *pDb = (IDatabase *)GetHandle(params[1], Handle_Database);
		if (!pDb)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid info tuple or database handle: %d", params[1]);
			return 0;
		}

		int len;
		return pDb->SetCharacterSet(MF_GetAmxString(amx, params[2], 0, &len));
	}
	else
	{
		int len;
		const char *charset = MF_GetAmxString(amx, params[2], 0, &len);

		if (!sql->charset || stricmp(charset, sql->charset))
		{
			sql->charset = strdup(charset);
		}

		return 1;
	}

	return 0;
}

AMX_NATIVE_INFO g_BaseSqlNatives[] = 
{
	{"SQL_MakeDbTuple",		SQL_MakeDbTuple},
	{"SQL_FreeHandle",		SQL_FreeHandle},
	{"SQL_Connect",			SQL_Connect},
	{"SQL_PrepareQuery",	SQL_PrepareQuery},
	{"SQL_Execute",			SQL_Execute},
	{"SQL_QueryError",		SQL_QueryError},
	{"SQL_MoreResults",		SQL_MoreResults},
	{"SQL_IsNull",			SQL_IsNull},
	{"SQL_ReadResult",		SQL_ReadResult},
	{"SQL_NextRow",			SQL_NextRow},
	{"SQL_AffectedRows",	SQL_AffectedRows},
	{"SQL_NumResults",		SQL_NumResults},
	{"SQL_NumColumns",		SQL_NumColumns},
	{"SQL_FieldNumToName",	SQL_FieldNumToName},
	{"SQL_FieldNameToNum",	SQL_FieldNameToNum},
	{"SQL_GetAffinity",		SQL_GetAffinity},
	{"SQL_SetAffinity",		SQL_SetAffinity},
	{"SQL_GetInsertId",		SQL_GetInsertId},
	{"SQL_GetQueryString",	SQL_GetQueryString},
	{"SQL_Rewind",			SQL_Rewind},
	{"SQL_QuoteString",		SQL_QuoteString},
	{"SQL_QuoteStringFmt",	SQL_QuoteStringFmt},
	{"SQL_NextResultSet",	SQL_NextResultSet},
	{"SQL_SetCharset",		SQL_SetCharset},

	{NULL,					NULL},
};

