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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libpq-fe.h>
#include <modules.h>

#define destroy(x) if(x){delete(x);x=0;}
#define destarr(x) if(x){delete[]x;x=0;}

pfnamx_engine_g* g_engAmxFunc;
pfnmodule_engine_g* g_engModuleFunc;

#define NAME "PgSQL"
#define AUTHOR "BAILOPAN"
#define VERSION "1.00"
#define URL "http://www.bailopan.com/"
#define LOGTAG "PGSQL"
#define DATE __DATE__

module_info_s module_info = {
  NAME,
  AUTHOR,
  VERSION,
  AMX_INTERFACE_VERSION,
  RELOAD_MODULE,
};

class pgs
{
public:
	pgs()
	{
	}

	pgs(const char *h, const char *u, const char *p, const char *n, int i=1)
	{
		set(h, u, p, n, i);
	}

	void set(const char *h, const char *u, const char *p, const char *n, int i=1)
	{
		v.host = h;
		v.user = u;
		v.pass = p;
		v.name = n;
		v.cn = NULL;
		v.row = 0;
		next = NULL;
		id = i;
	}

	pgs* link()
	{
		return next;
	}

	int ii()
	{
		return id;
	}

	void scn(PGconn *cn)
	{
		v.cn = cn;
	}

	void sln(pgs *p)
	{
		next = p;
	}

	void reset()
	{
		PQclear(v.res);
		v.row = 0;
	}

	void close()
	{
		PQfinish(v.cn);
		destroy(v.host);
		destroy(v.user);
		destroy(v.pass);
		destroy(v.name);
	}

	struct pgsql {
		const char *host;
		const char *user;
		const char *pass;
		const char *name;
		PGconn *cn;
		PGresult *res;
		int row;
	} v;

	~pgs()
	{
		close();
	}

private:
	pgs *next;
	int id;
};
