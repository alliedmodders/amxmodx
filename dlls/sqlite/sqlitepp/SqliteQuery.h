#ifndef _INCLUDE_SOURCEMOD_SQLITE_QUERY_H
#define _INCLUDE_SOURCEMOD_SQLITE_QUERY_H

#include "SqliteHeaders.h"

namespace SourceMod
{
	class SqliteDatabase;;
	class SqliteResultSet;

	class SqliteQuery : public IQuery
	{
	public:
		struct SqliteResults
		{
			char **results;
			int rows;
			int cols;
		};
	public:
		SqliteQuery(SqliteDatabase *db, const char *query);
		~SqliteQuery();
	public:
		void FreeHandle();
		bool Execute(QueryInfo *info, char *error, size_t maxlength);
		bool ExecuteR(QueryInfo *info, char *error, size_t maxlength);
		bool Execute2(QueryInfo *info, char *error, size_t maxlength);
		const char *GetQueryString();
	private:
		SqliteDatabase *m_pDatabase;
		SqliteResultSet *m_LastRes;
		char *m_QueryString;
	};
};

#endif //_INCLUDE_SOURCEMOD_SQLITE_QUERY_H
