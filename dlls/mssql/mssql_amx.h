#ifndef _INCLUDE_MSSQL_H
#define _INCLUDE_MSSQL_H

#import "c:\Program Files\Common Files\System\ADO\msado15.dll" rename("EOF", "ADOEOF")

#include <vector>
#include <string>
#include <oledb.h>
#include "amxxmodule.h"

class SQL
{
public:
	SQL();
	~SQL();
	int Connect(const char *host, const char *user, const char *pass, const char *base);
	int Query(const char *query);
	void Disconnect();
	int Error(_com_error &e);

	ADODB::_ConnectionPtr cn;

	std::string ErrorStr;
	int ErrorCode;

	std::string Host;
	std::string Password;
	std::string Username;
	std::string Database;
	std::string cstr;

	bool isFree;
};

class SQLResult
{
public:
	SQLResult();
	~SQLResult();
	int Query(SQL *cn, const char *query);
	bool Nextrow();
	void FreeResult();
	const char *GetField(unsigned int field);
	const char *GetField(const char *field);
	unsigned int NumRows();

	int FieldCount;
	int RowCount;
	bool isFree;
	bool isStart;

	SQL *sql;

	ADODB::_RecordsetPtr res;
	std::string LastResult;
};

char *amx_string(AMX *amx, cell &param, int &len);

extern std::vector<SQLResult*> Results;
extern std::vector<SQL*> DBList;

#endif //MSSQL