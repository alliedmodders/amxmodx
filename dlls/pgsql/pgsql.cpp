/* AMX Mod X
*	 PGSQL Module
*
* by BAILOPAN
*
* This file is part of AMX Mod X.
*
*
*	This program is free software; you can redistribute it and/or modify it
*	under the terms of the GNU General Public License as published by the
*	Free Software Foundation; either version 2 of the License, or (at
*	your option) any later version.
*
*	This program is distributed in the hope that it will be useful, but
*	WITHOUT ANY WARRANTY; without even the implied warranty of
*	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*	General Public License for more details.
*
*	You should have received a copy of the GNU General Public License
*	along with this program; if not, write to the Free Software Foundation,
*	Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*	In addition, as a special exception, the author gives permission to
*	link the code of this program with the Half-Life Game Engine ("HL
*	Engine") and Modified Game Libraries ("MODs") developed by Valve,
*	L.L.C ("Valve"). You must obey the GNU General Public License in all
*	respects for all of the code used other than the HL Engine and MODs
*	from Valve. If you modify this file, you may extend this exception
*	to your version of the file, but you are not obligated to do so. If
*	you do not wish to do so, delete this exception statement from your
*	version.
*/

#include "pgsql_amx.h"
#include "amxxmodule.h"

CVector<SQLResult*> Results;
CVector<SQL*> DBList;

int sql_exists(const char* host,const char* user,const char* pass,const char* dbase) {
	unsigned int i = 0;
	int id = 0;
	for (i=0; i<DBList.size(); i++) {
		id++;
		if ((DBList[i]->Host.compare(host) == 0) &&
			(DBList[i]->Username.compare(user) == 0) &&
			(DBList[i]->Password.compare(pass) == 0) &&
			(DBList[i]->Database.compare(dbase) == 0) &&
			(!DBList[i]->isFree)) {
				return id;
		}
	}
	return -1;
}

// ///////////////////////////////
// pgsql natives for AMX scripting
// ///////////////////////////////

// sql = pgsql_connect(host[],user[],pass[],dbname[],error[],maxlength) :
// - open connection
static cell AMX_NATIVE_CALL sql_connect(AMX *amx, cell *params) //	6 param
{
	int i;
	char *host = MF_GetAmxString(amx,params[1], 0, &i);
	char *user = MF_GetAmxString(amx,params[2], 1, &i);
	char *pass = MF_GetAmxString(amx,params[3], 2, &i);
	char *dbname = MF_GetAmxString(amx,params[4], 3, &i);
	i = 0;

	if (!strlen(host) || !strlen(user) || !strlen(dbname)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return -1;
	}
	
	int id = sql_exists(host,user,pass,dbname);
	
	if (id > 0)
		return id;

	SQL *c=NULL;
	
	for (i=0; (unsigned int)i<DBList.size(); i++) {
		if (DBList[i]->isFree) {
			id = i;
			break;
		}
	}

	if (id>=0) {
		c = DBList[id];
	} else {
		c = new SQL;
		DBList.push_back(c);
		id = (unsigned int)(DBList.size() - 1);
	}
	
	if (!c->Connect(host, user, pass, dbname))
	{
		MF_SetAmxString(amx, params[5], c->ErrorStr.c_str(), params[6]);
		return -1;
	}

    MF_SetAmxString(amx,params[5],"",params[6]);
	
	return id+1;
}

// pgsql_error(sql,dest[],maxlength)
// - store maxlength characters from pgsql error in current row to dest
static cell AMX_NATIVE_CALL sql_error(AMX *amx, cell *params) // 3 params
{
	unsigned int id = params[1]-1;
	if (id >= DBList.size() || DBList[id]->isFree)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQL *sql = DBList[id];
	
	if (sql->ErrorStr.size() > 1)
	{
		MF_SetAmxString(amx, params[2], sql->ErrorStr.c_str(), params[3]);
		sql->ErrorStr.assign("");
		return 1;
	} else {
		if (sql->ErrorStr.size() > 1)
		{
			MF_SetAmxString(amx, params[2], sql->ErrorStr.c_str(), params[3]);
			sql->ErrorStr.assign("");
			return 1;
		}
	}
	MF_SetAmxString(amx, params[2], "", params[3]);

	return 0;
}

// pgsql_query(sql,query[]) - returns 0 on success, <0 on failure, >0 on result set
static cell AMX_NATIVE_CALL sql_query(AMX *amx, cell *params) //	2 params
{
	unsigned int id = params[1]-1;

	if (id >= DBList.size() || DBList[id]->isFree) {
		MF_Log("Invalid Database Handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return -1;
	}

	int len = 0;
	const char *query = MF_FormatAmxString(amx, params, 2, &len);

	SQL *sql = DBList[id];

	return sql->Query(query);	//Return the result set handle, if any
}

// pgsql_nextrow(sql) :
// - read next row
// - return :
//	 . number of line
//	 . 0 at end
static cell AMX_NATIVE_CALL sql_nextrow(AMX *amx, cell *params) //	1 param
{
	unsigned int id = params[1]-1;

	if (id >= Results.size() || Results[id]->isFree)
	{
		MF_Log("Invalid result handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQLResult *Result = Results[id];

	return Result->Nextrow();
}


// pgsql_close(sql) :
// - free result
// - close connection
static cell AMX_NATIVE_CALL sql_close(AMX *amx, cell *params) // 1 param
{
	unsigned int id = params[1]-1;
	if (id >= DBList.size() || DBList[id]->isFree) {
		MF_Log("Invalid Database Handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQL *sql = DBList[id];
	
	sql->Disconnect();

	return 1;
}

//Returns a field from a query result handle.
// 2 param - returns integer
// 3 param - stores float in cell byref
// 4 param - stores string
static cell AMX_NATIVE_CALL sql_getfield(AMX *amx, cell *params) // 2-4 params
{
	unsigned int id = params[1]-1;

	if (id >= Results.size() || Results[id]->isFree)
	{
		MF_Log("Invalid result handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQLResult *Result = Results[id];
	int numParams = (*params)/sizeof(cell);
	cell *fAddr = NULL;
	const char *field = Result->GetField(params[2]-1);
	if (field == NULL)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	switch (numParams)
	{
	case 2:
		return atoi(field);
		break;
	case 3:
		fAddr = MF_GetAmxAddr(amx, params[3]);
		*fAddr = amx_ftoc((REAL)atof(field));
		return 1;
		break;
	case 4:
		return MF_SetAmxString(amx, params[3], field?field:"", params[4]);
		break;
	default:
		break;
	}

	return 0;
}

//Returns a field from a query result handle.
// 2 param - returns integer
// 3 param - stores float in cell byref
// 4 param - stores string
static cell AMX_NATIVE_CALL sql_getresult(AMX *amx, cell *params) // 4 params
{
	unsigned int id = params[1]-1;

	if (id >= Results.size() || Results[id]->isFree)
	{
		MF_Log("Invalid result handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQLResult *Result = Results[id];
	int numParams = (*params)/sizeof(cell);
	cell *fAddr = NULL;
	int len = 0;
	const char *column = MF_GetAmxString(amx, params[2], 0, &len);
	const char *field = Result->GetField(column);
	if (field == NULL)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	switch (numParams)
	{
	case 2:
		return atoi(field);
		break;
	case 3:
		fAddr = MF_GetAmxAddr(amx, params[3]);
		*fAddr = amx_ftoc((REAL)atof(field));
		return 1;
		break;
	case 4:
		return MF_SetAmxString(amx, params[3], field?field:"", params[4]);
		break;
	default:
		break;
	}

	return 0;
}

static cell AMX_NATIVE_CALL sql_free_result(AMX *amx, cell *params)
{
	unsigned int id = params[1]-1;

	if (id >= Results.size() || Results[id]->isFree)
	{
		MF_Log("Invalid result handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQLResult *Result = Results[id];

	Result->FreeResult();

	return 1;
}

static cell AMX_NATIVE_CALL sql_num_rows(AMX *amx, cell *params)
{
	unsigned int id = params[1]-1;

	if (id >= Results.size() || Results[id]->isFree)
	{
		MF_Log("Invalid result handle %d", id);
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SQLResult *Result = Results[id];

	return (cell)Result->NumRows();
}

static cell AMX_NATIVE_CALL sql_type(AMX *amx, cell *params)
{
	return MF_SetAmxString(amx, params[1], "pgsql", params[2]);
}

AMX_NATIVE_INFO pgsql_Natives[] = {
	{ "dbi_connect",		sql_connect },
	{ "dbi_query",			sql_query },	
	{ "dbi_field",			sql_getfield },	
	{ "dbi_nextrow",		sql_nextrow },	
	{ "dbi_close",			sql_close },	
	{ "dbi_error",			sql_error },
	{ "dbi_type",			sql_type },
	{ "dbi_free_result",	sql_free_result },
	{ "dbi_num_rows",		sql_num_rows },
	{ "dbi_result",			sql_getresult },
	{ NULL, NULL }
};

void OnAmxxAttach()
{
	MF_AddNatives(pgsql_Natives);
}

void OnAmxxDetach()
{
	Results.clear();
	DBList.clear();
}
