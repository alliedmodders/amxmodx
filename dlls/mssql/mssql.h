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
