#ifndef _INCLUDE_SOURCEMOD_MYSQL_QUERY_H
#define _INCLUDE_SOURCEMOD_MYSQL_QUERY_H

#include "MysqlHeaders.h"

namespace SourceMod
{
	class MysqlDatabase;
	class MysqlResultSet;

	class MysqlQuery : public IQuery
	{
	public:
		MysqlQuery(const char *querystring, MysqlDatabase *db);
		~MysqlQuery();
	public:
		void FreeHandle();
		bool Execute(QueryInfo *info, char *error, size_t maxlength);
		bool ExecuteR(QueryInfo *info, char *error, size_t maxlength);
		bool Execute2(QueryInfo *info, char *error, size_t maxlength);
		const char *GetQueryString();
	private:
		MysqlDatabase *m_pDatabase;
		char *m_QueryString;
		size_t m_QueryLen;
		MysqlResultSet *m_LastRes;
	};
};

#endif //_INCLUDE_SOURCEMOD_MYSQL_QUERY_H
