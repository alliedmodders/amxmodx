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

using namespace SourceMod;

struct olddb_s
{
	IDatabase *pDatabase;
	char error[255];
	int errcode;
};

struct oldresult_s
{
	IQuery *pQuery;
	QueryInfo info;
	bool firstCall;
};

void FreeOldDb(void *ptr, unsigned int hndl)
{
	olddb_s *old = (olddb_s *)ptr;

	if (old->pDatabase)
	{
		old->pDatabase->FreeHandle();
		old->pDatabase = NULL;
	}

	delete old;
}

void FreeOldResult(void *ptr, unsigned int hndl)
{
	oldresult_s *oldres = (oldresult_s *)ptr;

	if (oldres->pQuery)
	{
		oldres->pQuery->FreeHandle();
		oldres->pQuery = NULL;
	}

	delete oldres;
}

//native Sql:dbi_connect(_host[], _user[], _pass[], _dbname[], _error[]="", _maxlength=0);
static cell AMX_NATIVE_CALL dbi_connect(AMX *amx, cell *params)
{
	int len;
	DatabaseInfo info;
	char *host = MF_GetAmxString(amx, params[1], 0, &len);
	char *user = MF_GetAmxString(amx, params[2], 1, &len);
	char *pass = MF_GetAmxString(amx, params[3], 2, &len);
	char *name = MF_GetAmxString(amx, params[4], 3, &len);

	char *p = strchr(host, ':');
	if (p)
	{
		info.port = atoi(p+1);
		*p = '\0';
	} else {
		info.port = 0;
	}

	info.host = host;
	info.user = user;
	info.pass = pass;
	info.database = name;

	int err;
	char error[512];
	IDatabase *pDatabase = g_Mysql.Connect(&info, &err, error, sizeof(error)-1);
	if (!pDatabase)
	{
		MF_SetAmxString(amx, params[5], error, params[6]);
		return 0;
	}

	olddb_s *old = new olddb_s;
	int hndl;

	old->pDatabase = pDatabase;
	hndl = MakeHandle(old, Handle_OldDb, FreeOldDb);

	return hndl;
}

//native Result:dbi_query(Sql:_sql, _query[], {Float,_}:...);
static cell AMX_NATIVE_CALL dbi_query(AMX *amx, cell *params)
{
	olddb_s *old = (olddb_s *)GetHandle(params[1], Handle_OldDb);
	if (!old)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI handle %d", params[1]);
		return -1;
	}

	int len;
	char *queryString = MF_FormatAmxString(amx, params, 2, &len);

	IQuery *pQuery = old->pDatabase->PrepareQuery(queryString);
	QueryInfo info;

	old->error[0] = '\0';
	old->errcode = 0;

	if (!pQuery->Execute2(&info, old->error, 254))
	{
		old->errcode = info.errorcode;
		return -1;
	} else {
		if (info.rs && info.rs->RowCount())
		{
			oldresult_s *oldrs = new oldresult_s;
			int hndl;

			oldrs->info = info;
			oldrs->pQuery = pQuery;
			oldrs->firstCall = true;
			hndl = MakeHandle(oldrs, Handle_OldResult, FreeOldResult);
			return hndl;
		} else {
			pQuery->FreeHandle();
			return 0;
		}
	}

	/** never reach here */
	return 0;
}

//native Result:dbi_query2(Sql:_sql, &rows, _query[], {Float,_}:...);
static cell AMX_NATIVE_CALL dbi_query2(AMX *amx, cell *params)
{
	olddb_s *old = (olddb_s *)GetHandle(params[1], Handle_OldDb);
	if (!old)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI handle %d", params[1]);
		return -1;
	}

	int len;
	char *queryString = MF_FormatAmxString(amx, params, 3, &len);

	IQuery *pQuery = old->pDatabase->PrepareQuery(queryString);
	QueryInfo info;

	old->error[0] = '\0';
	old->errcode = 0;

	if (!pQuery->Execute2(&info, old->error, 254))
	{
		old->errcode = info.errorcode;
		return -1;
	} else {
		cell *addr = MF_GetAmxAddr(amx, params[2]);
		*addr = static_cast<cell>(info.affected_rows);
		if (info.rs && info.rs->RowCount())
		{
			oldresult_s *oldrs = new oldresult_s;
			int hndl;

			oldrs->info = info;
			oldrs->pQuery = pQuery;
			oldrs->firstCall = true;
			hndl = MakeHandle(oldrs, Handle_OldResult, FreeOldResult);
			return hndl;
		} else {
			pQuery->FreeHandle();
			return 0;
		}
	}

	/** never reach here */
	return 0;
}

//native dbi_nextrow(Result:_result);
static cell AMX_NATIVE_CALL dbi_nextrow(AMX *amx, cell *params)
{
	//special case, fall out!
	if (params[1] == 0)
	{
		return 0;
	}

	oldresult_s *oldrs = (oldresult_s *)GetHandle(params[1], Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", params[1]);
		return 0;
	}

	if (oldrs->firstCall)
	{
		oldrs->firstCall = false;
		return (oldrs->info.rs->IsDone() ? 0 : 1);
	} else {
		oldrs->info.rs->NextRow();
		return (oldrs->info.rs->IsDone() ? 0 : 1);
	}
}

//native dbi_field(Result:_result, _fieldnum, {Float,_}:... );
static cell AMX_NATIVE_CALL dbi_field(AMX *amx, cell *params)
{
	oldresult_s *oldrs = (oldresult_s *)GetHandle(params[1], Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", params[1]);
		return 0;
	}

	IResultSet *rs = oldrs->info.rs;
	if (rs->IsDone())
	{
		return 0;
	}
	IResultRow *rr = rs->GetRow();
	unsigned int num = (unsigned int)params[2] - 1;
	if (num >= rs->FieldCount())
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid column %d", params[2]);
		return 0;
	}

	cell stype = params[0] / sizeof(cell);
	const char *data = rr->GetString(num);
	if (!data)
		data = "";

	switch (stype)
	{
	case 2:
		{
			return atoi(data);
		}
	case 3:
		{
			cell *destaddr = MF_GetAmxAddr(amx, params[3]);
			REAL fdata = atof(data);
			*destaddr = amx_ftoc(fdata);
			return 1;
		}
	case 4:
		{
			return MF_SetAmxString(amx, params[3], data, params[4]);
		}
	}

	/** never reach here */
	return 0;
}

//native dbi_result(Result:_result, _field[], {Float,_}:... );
static cell AMX_NATIVE_CALL dbi_result(AMX *amx, cell *params)
{
	oldresult_s *oldrs = (oldresult_s *)GetHandle(params[1], Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", params[1]);
		return 0;
	}

	IResultSet *rs = oldrs->info.rs;
	if (rs->IsDone())
	{
		return 0;
	}
	IResultRow *rr = rs->GetRow();
	unsigned int num;
	bool found = false;
	unsigned int fields = rs->FieldCount();
	int len;
	char *field = MF_GetAmxString(amx, params[2], 0, &len);
	for (unsigned int i=0; i<fields; i++)
	{
		if (strcmp(field, rs->FieldNumToName(i)) == 0)
		{
			num = i;
			found = true;
			break;
		}
	}

	if (!found)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Unknown column \"%s\"", field);
		return 0;
	}

	cell stype = params[0] / sizeof(cell);
	const char *data = rr->GetString(num);
	if (!data)
		data = "";

	switch (stype)
	{
	case 2:
		{
			return atoi(data);
		}
	case 3:
		{
			cell *destaddr = MF_GetAmxAddr(amx, params[3]);
			REAL fdata = atof(data);
			*destaddr = amx_ftoc(fdata);
			return 1;
		}
	case 4:
		{
			return MF_SetAmxString(amx, params[3], data, params[4]);
		}
	}

	/** never reach here */
	return 0;
}

//native dbi_num_rows(Result:_result);
static cell AMX_NATIVE_CALL dbi_num_rows(AMX *amx, cell *params)
{
	oldresult_s *oldrs = (oldresult_s *)GetHandle(params[1], Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", params[1]);
		return 0;
	}

	return oldrs->info.rs->RowCount();
}

//native dbi_free_result(&Result:result);
static cell AMX_NATIVE_CALL dbi_free_result(AMX *amx, cell *params)
{
	cell *_r = MF_GetAmxAddr(amx, params[1]);
	cell num = *_r;

	if (!num)
	{
		return 1;
	}

	oldresult_s *oldrs = (oldresult_s *)GetHandle(num, Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", num);
		return 0;
	}

	FreeHandle(num);

	*_r = 0;

	return 1;
}

//native dbi_close(&Sql:_sql);
static cell AMX_NATIVE_CALL dbi_close(AMX *amx, cell *params)
{
	cell *_r = MF_GetAmxAddr(amx, params[1]);
	cell num = *_r;
	olddb_s *old = (olddb_s *)GetHandle(num, Handle_OldDb);
	if (!old)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI handle %d", num);
		return 0;
	}

	FreeHandle(num);

	*_r = 0;

	return 1;
}

//native dbi_error(Sql:_sql, _error[], _len);
static cell AMX_NATIVE_CALL dbi_error(AMX *amx, cell *params)
{
	olddb_s *old = (olddb_s *)GetHandle(params[1], Handle_OldDb);
	if (!old)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI handle %d", params[1]);
		return -1;
	}

	MF_SetAmxString(amx, params[2], old->error, params[3]);
	return old->errcode;
}

//native dbi_type(_type[], _len);
static cell AMX_NATIVE_CALL dbi_type(AMX *amx, cell *params)
{
	return MF_SetAmxString(amx, params[1], "mysql", params[2]);
}

//native dbi_num_fields(Result:result);
static cell AMX_NATIVE_CALL dbi_num_fields(AMX *amx, cell *params)
{
	oldresult_s *oldrs = (oldresult_s *)GetHandle(params[1], Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", params[1]);
		return 0;
	}

	return oldrs->info.rs->FieldCount();
}

//native dbi_field_name(Result:result, field, name[], maxLength);
static cell AMX_NATIVE_CALL dbi_field_name(AMX *amx, cell *params)
{
	oldresult_s *oldrs = (oldresult_s *)GetHandle(params[1], Handle_OldResult);
	if (!oldrs)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid DBI result handle %d", params[1]);
		return 0;
	}

	const char *name = oldrs->info.rs->FieldNumToName(static_cast<unsigned int>(params[2]-1));
	if (!name)
		return 0;

	MF_SetAmxString(amx, params[3], name, params[4]);

	return 1;
}

AMX_NATIVE_INFO g_OldCompatNatives[] = 
{
	{ "dbi_connect",		dbi_connect },
	{ "dbi_query",			dbi_query },
	{ "dbi_query2",			dbi_query2 },
	{ "dbi_field",			dbi_field },	
	{ "dbi_nextrow",		dbi_nextrow },	
	{ "dbi_close",			dbi_close },	
	{ "dbi_error",			dbi_error },
	{ "dbi_type",			dbi_type },
	{ "dbi_free_result",	dbi_free_result },
	{ "dbi_num_rows",		dbi_num_rows },
	{ "dbi_result",			dbi_result },
	{ "dbi_num_fields",		dbi_num_fields },
	{ "dbi_field_name",		dbi_field_name },

	{ NULL, NULL }
};
