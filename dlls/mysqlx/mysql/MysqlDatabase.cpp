#include <stdio.h>
#include <string.h>
#include "MysqlDriver.h"
#include "MysqlDatabase.h"
#include "MysqlQuery.h"

#if defined WIN32 && !defined vsnprintf
#define vsnprintf _vsnprintf
#endif

using namespace SourceMod;

MysqlDatabase::MysqlDatabase(MYSQL *mysql, MysqlDriver *drvr) : 
	m_pMysql(mysql), m_pParent(drvr)
{
}

MysqlDatabase::~MysqlDatabase()
{
	Disconnect();
}

void MysqlDatabase::Disconnect()
{
	mysql_close(m_pMysql);
	m_pMysql = NULL;
}

void MysqlDatabase::FreeHandle()
{
	delete this;
}

ISQLDriver *MysqlDatabase::Driver()
{
	return static_cast<ISQLDriver *>(m_pParent);
}

IQuery *MysqlDatabase::PrepareQuery(const char *query)
{
	MysqlQuery *mquery = new MysqlQuery(query, this);

	return static_cast<IQuery *>(mquery);
}

IQuery *MysqlDatabase::PrepareQueryFmt(const char *fmt, va_list ap)
{
	char buffer[4096];

	vsnprintf(buffer, sizeof(buffer)-1, fmt, ap);

	return PrepareQuery(buffer);
}

IQuery *MysqlDatabase::PrepareQueryFmt(const char *fmt, ...)
{
	va_list ap;
	IQuery *qry;

	va_start(ap, fmt);
	qry = PrepareQueryFmt(fmt, ap);
	va_end(ap);

	return qry;
}

int MysqlDatabase::QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize)
{
	unsigned long size = static_cast<unsigned long>(strlen(str));
	unsigned long needed = size*2 + 1;

	if (maxlen < needed)
	{
		return (int)needed;
	}

	needed = mysql_real_escape_string(m_pMysql, buffer, str, size);
	if (newsize)
	{
		*newsize = static_cast<size_t>(needed);
	}

	return 0;
}


