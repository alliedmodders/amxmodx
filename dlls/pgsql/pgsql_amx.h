#include <vector>
#include <string>
#include <map>
#include <libpq-fe.h>
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

	PGresult *res;
	int row;
	int RowCount;
	bool isFree;
	SQL *sql;
	std::vector<const char *> Fields;
};

char *amx_string(AMX *amx, cell &param, int &len);

extern std::vector<SQLResult*> Results;
extern std::vector<SQL*> DBList;
