/*  by David "BAILOPAN" Anderson
*   http://www.bailopan.com
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
*/
#ifndef _INCLUDE_MSSQL_H
#define _INCLUDE_MSSQL_H

#import "c:\Program Files\Common Files\System\ADO\msado15.dll" rename("EOF", "ADOEOF")
#include <string>
#include <vector>
#include <oledb.h>
#include "amxxmodule.h"

class msdb
{
public:
	ADODB::_ConnectionPtr cn;
	ADODB::_RecordsetPtr res;
	msdb() { free = true; res=NULL; resStart = false; }
	~msdb() { Kill(); }
	std::string user;
	std::string pass;
	std::string dbname;
	std::string host;
	std::string cstr;
	std::string error;
	bool Connect();
	bool Kill();
	bool free;
	bool resStart;
};

extern std::vector<msdb*> DBList;
extern AMX_NATIVE_INFO mssql_Natives[];

#endif //_INCLUDE_MSSQL_H
