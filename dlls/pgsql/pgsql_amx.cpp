/* AMX Mod X
*   PostgreSQL Module
*
* by David "BAILOPAN" Anderson
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "pgsql_amx.h"

std::string error;
std::vector<pgdb*> dblist;

int sql_exists(const char* host,const char* user,const char* pass,const char* dbase) {
	std::vector<pgdb*>::iterator i;
	int id = 0;
	for (i=dblist.begin(); i!=dblist.end(); i++) {
		id++;
		if (((*i)->host.compare(host) == 0) &&
			((*i)->user.compare(user) == 0) &&
			((*i)->pass.compare(pass) == 0) &&
			((*i)->name.compare(dbase) == 0) &&
			(!(*i)->free)) {
				return id;
		}
	}
	return -1;
}

bool is_ipaddr(const char *IP)
{
	do {
		if ((int)(*(IP++)) > 0x37) {
			return false;
		}
	} while (*IP);

	return true;
}

void pgdb::Kill()
{
	if (free)
		return;
	PQfinish(cn);
	host.clear();
	user.clear();
	pass.clear();
	name.clear();
	row = 0;
	free = true;
}

int pgdb::Connect(const char *hh, const char *uu, const char *pp, const char *dd)
{
	host.assign(hh);
	user.assign(uu);
	pass.assign(pp);
	name.assign(dd);

	if (is_ipaddr(host.c_str())) {
		cstr.assign("hostaddr = '");
	} else {
		cstr.assign("host = '");
	}
	
	cstr.append(host);
	cstr.append("' user = '");
	cstr.append(user);
	cstr.append("' pass = '");
	cstr.append(pass);
	cstr.append("' name = '");
	cstr.append(name);
	cstr.append("'");

	cn = PQconnectdb(cstr.c_str());

	if (PQstatus(cn) != CONNECTION_OK) {
		err.assign(PQerrorMessage(cn));
		free = true;
		lastError = PQstatus(cn);
		return 0;
	}

	free = false;
	return true;
}

static cell AMX_NATIVE_CALL pgsql_connect(AMX  *amx, cell *params)
{
	int len;
	unsigned int i;
	pgdb *c = NULL;

	const char *host = MF_GetAmxString(amx,params[1],0,&len);
	const char *user = MF_GetAmxString(amx,params[2],1,&len);
	const char *pass = MF_GetAmxString(amx,params[3],2,&len);
	const char *name = MF_GetAmxString(amx,params[4],3,&len);

	int id = sql_exists(host, user, pass, name);
	
	if (id >= 0)
		return id;
	
	id = -1;
	for (i=0; i<dblist.size(); i++) {
		if (dblist[i]->free) {
			id = i;
			break;
		}
	}

	if (id < 0) {
		c = new pgdb;
		dblist.push_back(c);
		id = dblist.size() - 1;
	} else {
		c = dblist[id];
	}

	if (!c->Connect(host, user, pass, name)) {
		MF_SetAmxString(amx, params[5], c->err.c_str(), params[6]);
		return 0;
	}

	return id+1;
}


static cell AMX_NATIVE_CALL pgsql_error(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;

	if (id >= dblist.size() || dblist[id]->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	dblist[id]->err.assign(PQerrorMessage(dblist[id]->cn));
	MF_SetAmxString(amx, params[2], dblist[id]->err.c_str(), params[3]);
	return dblist[id]->lastError;
}

static cell AMX_NATIVE_CALL pgsql_query(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;

	if (id >= dblist.size() || dblist[id]->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pgdb *c = dblist[id];

	if (c->res)
		c->reset();

	int i;
	const char *query = MF_FormatAmxString(amx, params, 2, &i);

	c->res = PQexec(c->cn, query);
	c->row = 0;
	
	if (PQresultStatus(c->res) != PGRES_COMMAND_OK) {
		c->lastError = PQresultStatus(c->res);
		return -1;
	}

	return PQntuples(c->res);
}

static cell AMX_NATIVE_CALL pgsql_nextrow(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;

	if (id >= dblist.size() || dblist[id]->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pgdb *c = dblist[id];

	if (c->row > PQntuples(c->res))
		return 0;
	
	c->row++;

	return 1;
}

static cell AMX_NATIVE_CALL pgsql_getfield(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;
	int col = params[2];

	if (id >= dblist.size() || dblist[id]->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pgdb *c = dblist[id];

	if (col-1 > PQnfields(c->res))
		return 0;

	char *field = PQgetvalue(c->res, c->row, col);
	return MF_SetAmxString(amx, params[3], field?field:"", params[4]);
}

static cell AMX_NATIVE_CALL pgsql_close(AMX *amx, cell *params)
{
	unsigned int id = params[1] - 1;

	if (id >= dblist.size() || dblist[id]->free) {
		error.assign("Invalid handle.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pgdb *c = dblist[id];

	c->Kill();

	return 1;
}

void OnAmxxAttach()
{
	MF_AddNatives(pgsql_exports);
}

AMX_NATIVE_INFO pgsql_exports[] = {
	{"dbi_connect",		pgsql_connect},
	{"dbi_error",		pgsql_error},
	{"dbi_query",		pgsql_query},
	{"dbi_nextrow",		pgsql_nextrow},
	{"dbi_close",		pgsql_close},
	{"dbi_getfield",	pgsql_getfield},
	{"pgsql_connect",	pgsql_connect},
	{"pgsql_error",		pgsql_error},
	{"pgsql_query",		pgsql_query},
	{"pgsql_nextrow",	pgsql_nextrow},
	{"pgsql_close",		pgsql_close},
	{"pgsql_getfield",	pgsql_getfield},

	{NULL,			NULL},
};
