#include <stdio.h>
#include <string.h>
#include "SqliteQuery.h"
#include "SqliteDatabase.h"
#include "SqliteResultSet.h"

#if defined WIN32
#define snprintf _snprintf
#endif

using namespace SourceMod;

SqliteQuery::SqliteQuery(SqliteDatabase *db, const char *query) : 
	m_pDatabase(db), m_LastRes(NULL)
{
	m_QueryString = new char[strlen(query)+1];
	strcpy(m_QueryString, query);
}

SqliteQuery::~SqliteQuery()
{
	if (m_LastRes)
	{
		m_LastRes->FreeHandle();
		m_LastRes = NULL;
	}

	delete [] m_QueryString;
}

void SqliteQuery::FreeHandle()
{
	delete this;
}

bool SqliteQuery::Execute(QueryInfo *info, char *error, size_t maxlength)
{
	bool res = ExecuteR(info, error, maxlength);
	
	if (m_LastRes)
	{
		m_LastRes->FreeHandle();
	}

	m_LastRes = (SqliteResultSet *)info->rs;

	return res;
}

bool SqliteQuery::Execute2(QueryInfo *info, char *error, size_t maxlength)
{
	bool res = ExecuteR(info, error, maxlength);

	if (m_LastRes)
		m_LastRes->FreeHandle();

	m_LastRes = (SqliteResultSet *)info->rs;

	if (info->success)
	{
		info->insert_id = sqlite3_last_insert_rowid(m_pDatabase->m_pSql);
	} else {
		info->insert_id = 0;
	}

	return res;
}

const char *SqliteQuery::GetQueryString()
{
	return m_QueryString;
}

bool SqliteQuery::ExecuteR(QueryInfo *info, char *error, size_t maxlength)
{
	int err;
	char *errmsg;
	char **results;
	int rows, cols;

	err = sqlite3_get_table(m_pDatabase->m_pSql, m_QueryString, &results, &rows, &cols, &errmsg);

	if (err != SQLITE_OK)
	{
		if (error && maxlength && errmsg)
		{
			snprintf(error, maxlength, "%s", errmsg);
		}
		info->affected_rows = 0;
		info->errorcode = err;
		info->rs = NULL;
		info->success = false;
	} else {
		info->affected_rows = sqlite3_changes(m_pDatabase->m_pSql);
		info->errorcode = 0;
		info->success = true;
		if (cols)
		{
			SqliteResults data;
			data.cols = cols;
			data.rows = rows;
			data.results = results;

			SqliteResultSet *pRes = new SqliteResultSet(data);
			info->rs = static_cast<IResultSet *>(pRes);
		} else {
			info->rs = NULL;
			if (results)
			{
				sqlite3_free_table(results);
			}
		}
	}

	return info->success;
}
