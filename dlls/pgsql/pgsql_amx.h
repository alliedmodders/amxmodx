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
#include <string>
#include <vector>
#include "amxxmodule.h"

class pgdb
{
public:
	void reset() { PQclear(res); row = 0; }
	pgdb() { free = true; }
	int Connect(const char *hh, const char *uu, const char *pp, const char *dd);
	void Kill();
	~pgdb() { Kill(); }

	bool free;
	PGconn *cn;
	PGresult *res;
	int row;
	std::string host;
	std::string user;
	std::string pass;
	std::string name;
	std::string cstr;
	std::string err;
	int lastError;
};

extern std::string error;
extern std::vector<pgdb*> dblist;
extern AMX_NATIVE_INFO pgsql_exports[];
