

#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef __linux__
#define WINDOWS_LEAN_AND_MEAN
#include <winsock.h>
#endif
#include "amxxmodule.h"
#include "CVector.h"
#include "CString.h"
#include "sqlite3.h"

#define MEM_ALLOC_FAILED -20
#define CONNECT_FAILED   -10
#define QUERY_FAILED     -5

class SQL
{
public:
	SQL();
	~SQL();
	int Connect(/*const char *host, const char *user, const char *pass,*/ const char *base);
	int Query(const char *query);
	void Disconnect();
	int Error();

	sqlite3 *sqlite;

	String ErrorStr;
	int ErrorCode;

	String Database;

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
	
	String *m_fieldNames;
	bool isFree;
	int m_currentRow;
	bool m_hasData;

	char **m_data;
	char *m_errorMsg;
	unsigned int m_rowCount, m_columnCount;
#if defined _DEBUG
	static unsigned int latestStoredResultId;
#endif
};

char *amx_string(AMX *amx, cell &param, int &len);

extern CVector<SQLResult*> Results;
extern CVector<SQL*> DBList;

