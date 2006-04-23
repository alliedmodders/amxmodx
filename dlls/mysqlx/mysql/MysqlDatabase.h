#ifndef _INCLUDE_SOURCEMOD_MYSQL_DATABASE_H
#define _INCLUDE_SOURCEMOD_MYSQL_DATABASE_H

#include "MysqlHeaders.h"
#include "MysqlDriver.h"

namespace SourceMod
{
	class MysqlDriver;

	class MysqlDatabase : public IDatabase
	{
		friend class MysqlQuery;
	public:
		MysqlDatabase(MYSQL *mysql, MysqlDriver *drvr);
		~MysqlDatabase();
	public:
		void FreeHandle();
		ISQLDriver *Driver();
	public:
		IQuery *PrepareQueryFmt(const char *fmt, ...);
		IQuery *PrepareQueryFmt(const char *fmt, va_list ap);
		IQuery *PrepareQuery(const char *query);
		int QuoteString(const char *str, char buffer[], size_t maxlen, size_t *newsize);
	private:
		void Disconnect();
	private:
		MYSQL *m_pMysql;
        MysqlDriver *m_pParent;
	};
};


#endif //_INCLUDE_SOURCEMOD_MYSQL_DATABASE_H
