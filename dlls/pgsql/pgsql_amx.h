#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libpq-fe.h"
#include "CVector.h"
#include "CString.h"
#include "amxxmodule.h"

class SQL
{
public:
	SQL();
	~SQL();
	int Connect(const char *host, const char *user, const char *pass, const char *base);
	int Query(const char *query);
	void Disconnect();
	int Error(int code);

	PGconn *cn;

	CString ErrorStr;
	int ErrorCode;

	CString Host;
	CString Password;
	CString Username;
	CString Database;
	CString cstr;

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

	PGresult *res;
	int row;
	int RowCount;
	bool isFree;
	SQL *sql;
	CVector<const char *> Fields;
};

char *amx_string(AMX *amx, cell &param, int &len);

extern CVector<SQLResult*> Results;
extern CVector<SQL*> DBList;
