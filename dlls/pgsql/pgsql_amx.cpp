#include "pgsql_amx.h"

bool is_ipaddr(const char *IP)
{
	do {
		if ((int)(*(IP++)) > 0x37) {
			return false;
		}
	} while (*IP);

	return true;
}

SQL::SQL()
{
	isFree = true;
}

SQL::~SQL()
{
	if (!isFree)
		Disconnect();
}

SQLResult::SQLResult()
{
	isFree = true;
	RowCount = 0;
	Fields.clear();
	res = NULL;
	row = 0;
}

SQLResult::~SQLResult()
{
	if (!isFree)
		FreeResult();
}

int SQL::Error(int code)
{
	if (isFree)
		return 0;

	ErrorStr.assign(PQerrorMessage(cn));
	ErrorCode = code;
	return code;
}

int SQL::Connect(const char *host, const char *user, const char *pass, const char *base)
{
	Username.assign(user);
	Password.assign(pass);
	Database.assign(base);
	Host.assign(host);

	isFree = false;
	int err = 0;

	if (is_ipaddr(Host.c_str())) {
		cstr.assign("hostaddr = '");
	} else {
		cstr.assign("host = '");
	}
	
	cstr.append(Host);
	cstr.append("' user = '");
	cstr.append(Username);
	cstr.append("' pass = '");
	cstr.append(Password);
	cstr.append("' name = '");
	cstr.append(Database);
	cstr.append("'");

	cn = PQconnectdb(cstr.c_str());

	if (PQstatus(cn) != CONNECTION_OK) {
		Error(PQstatus(cn));
		PQfinish(cn);
		isFree = true;
		return 0;
	}

	isFree = false;

	return 1;
}

void SQL::Disconnect()
{
	if (isFree)
		return;

	Host.assign("");
	Username.assign("");
	Password.assign("");
	Database.assign("");

	PQfinish(cn);

	isFree = true;
}

int SQL::Query(const char *query)
{
	if (isFree)
	{
		ErrorCode = -1;
		return -1;
	}


	unsigned int i = 0;
	int id = -1;
	for (i=0; i < Results.size(); i++)
	{
		if (Results[i]->isFree) {
			id = i;
			break;
		}
	}

	if (id < 0) {

		SQLResult *p = new SQLResult;
		int ret = p->Query(this, query);

		if (ret != 0)
		{
			delete p;
			if (ret == -1)
				return 0;
			else
				return -1;
		} else {
			Results.push_back(p);
			return Results.size();
		}
	} else {
		SQLResult *r = Results[id];
		int ret = Results[id]->Query(this, query);
		if (ret != 0)
		{
			if (ret == -1)
				return 0;
			else
				return -1;
		} else {
			return (id + 1);
		}
	}
}

int SQLResult::Query(SQL *cn, const char *query)
{
	res = PQexec(cn->cn, query);
	row = -1;

	sql = cn;

	int queryResult = PQresultStatus(res);

	if (queryResult != PGRES_COMMAND_OK)
	{
		cn->Error(queryResult);
		return -1;
	}

	RowCount = PQntuples(res);

	if (RowCount < 1)
		return -1;

	int i = 0;
	const char *fld;
	for (i=0; i < PQnfields(res); i++)
	{
		fld = PQfname(res, i);
		Fields.push_back(fld);
	}

	return 0;
}

bool SQLResult::Nextrow()
{
	if (isFree)
		return false;

	if (row >= RowCount)
	{
		return false;
	}

	row++;

	return true;
}

void SQLResult::FreeResult()
{
	if (isFree)
		return;

	PQclear(res);
	Fields.clear();
	res = 0;
	row = 0;
	isFree = true;
}

const char *SQLResult::GetField(unsigned int field)
{
	if (field > (unsigned int)PQnfields(res))
	{
		sql->Error(-1);
		sql->ErrorStr.assign("Invalid field.");
		return 0;
	}

	return PQgetvalue(res, row, field);
}

const char *SQLResult::GetField(const char *field)
{
	unsigned int fld;
	int id = -1;
	for (fld = 0; fld < Fields.size(); fld++)
	{
		if (strcmp(Fields[fld], field)==0)
		{
			id = fld;
			break;
		}
	}

	if (id == -1)
	{
		sql->Error(-1);
		sql->ErrorStr.assign("Invalid field.");
		return 0;
	}

	return PQgetvalue(res, row, id);
}

unsigned int SQLResult::NumRows()
{
	if (isFree)
		return 0;

	return RowCount;
}
