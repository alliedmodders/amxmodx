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

pgs *cns = NULL;

bool is_ipaddr(const char *IP)
{
	do {
		if ((int)(*(IP++)) > 0x37) {
			return false;
		}
	} while (*IP);

	return true;
}


char *make_connstring(const char *host, const char *user, const char *pass, const char *name)
{
	int len = 46 + strlen(host) + strlen(user) + strlen(pass) + strlen(name) + 2;
	char *c_info = new char[len];

	if (is_ipaddr(host)) {
		sprintf(c_info, "hostaddr = '%s' user = '%s' pass = '%s' name = '%s'", host, user, pass, name);
	} else {
		sprintf(c_info, "host = '%s' user = '%s' pass = '%s' name = '%s'", host, user, pass, name);
	}

	return c_info;
}

PGconn* make_connection(const char *h, const char *u, const char *ps, const char *n)
{
	pgs *p = cns;
	int last = 0;
	
	while (p) {
		last = p->ii();
		if (p->v.host==h && p->v.user==u && p->v.pass==ps && p->v.name==n) {
			return p->v.cn;
		}
	}
	char *c_info = make_connstring(h, u, ps, n);
	if (cns == NULL) {
		cns = new pgs;
		PGconn *cn = PQconnectdb(c_info);
		cns->set(h, u, ps, n, 1);
		cns->scn(cn);
		return cn;
	} else {
		p = new pgs(h, u, ps, n, last+1);
		cns->sln(p);
		PGconn *cn = PQconnectdb(c_info);
		cns->scn(cn);
		return cn;
	}
}

pgs* get_conn_i(int n=1)
{
	pgs *p = cns;
	int i=0;
	while (p) {
		if (++i==n) {
			return p;
		} else {
			p = p->link();
		}
	}

	return NULL;
}

static cell AMX_NATIVE_CALL pgsql_connect(AMX  *amx, cell *params)
{
	int i;
	const char *host = GET_AMXSTRING(amx,params[1],0,i);
	const char *user = GET_AMXSTRING(amx,params[2],1,i);
	const char *pass = GET_AMXSTRING(amx,params[3],2,i);
	const char *name = GET_AMXSTRING(amx,params[4],3,i);

	PGconn *cn = make_connection(host, user, pass, name);

	if (PQstatus(cn) != CONNECTION_OK) {
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL pgsql_error(AMX *amx, cell *params)
{
	int c = params[1];
	pgs *p = get_conn_i(c);
	char *error = PQerrorMessage(p->v.cn);
	SET_AMXSTRING(amx, params[2], (error==NULL?"":error), params[3]);
	return 1;
}

static cell AMX_NATIVE_CALL pgsql_query(AMX *amx, cell *params)
{
	pgs *p = get_conn_i(params[1]);
	if (p == NULL) {
		return 0;
	}

	if (p->v.res) {
		p->reset();
	}

	int i;
	const char *query = FORMAT_AMXSTRING(amx, params, 2, i);

	p->v.res = PQexec(p->v.cn, query);
	
	if (PQresultStatus(p->v.res) != PGRES_COMMAND_OK) {
		return -1;
	}

	return PQntuples(p->v.res);
}

static cell AMX_NATIVE_CALL pgsql_nextrow(AMX *amx, cell *params)
{
	pgs *p = get_conn_i(params[1]);
	if (p == NULL) {
		return 0;
	}

	if (p->v.row > PQntuples(p->v.res)) {
		return 0;
	}
	
	p->v.row++;

	return 1;
}

static cell AMX_NATIVE_CALL pgsql_getfield(AMX *amx, cell *params)
{
	pgs *p = get_conn_i(params[1]);
	int col = params[2] + 1;
	if (p == NULL) {
		return 0;
	}
	if (col-1 > PQnfields(p->v.res)) {
		return 0;
	}

	char *field = PQgetvalue(p->v.res, p->v.row, col);
	return SET_AMXSTRING(amx, params[3], field?field:"", params[4]);
}

static cell AMX_NATIVE_CALL pgsql_close(AMX *amx, cell *params)
{
	pgs *p = get_conn_i(params[1]);
	
	p->close();

	return 1;
}

AMX_NATIVE_INFO pgsql_exports[] = {
	{"pgsql_connect",		pgsql_connect},
	{"pgsql_error",			pgsql_error},
	{"pgsql_query",			pgsql_query},
	{"pgsql_nextrow",		pgsql_nextrow},
	{"pgsql_close",			pgsql_close},
	{"pgsql_getfield",		pgsql_getfield},

	{NULL,			NULL},
};

C_DLLEXPORT int AMX_Query(module_info_s** info) {
	*info = &module_info;
	return 1;
}

C_DLLEXPORT int AMX_Attach(pfnamx_engine_g* amxeng,pfnmodule_engine_g* meng) {
	g_engAmxFunc = amxeng;
	g_engModuleFunc = meng;

	ADD_AMXNATIVES(&module_info, pgsql_exports);

	return(1);
}

C_DLLEXPORT int AMX_Detach() {
	delete cns;
	return(1);
}
