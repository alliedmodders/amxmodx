// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// MySQL Module
//

#include "amxxmodule.h"
#include "MysqlQuery.h"
#include "MysqlDatabase.h"
#include "MysqlResultSet.h"
#include <amtl/am-string.h>

using namespace SourceMod;

MysqlQuery::MysqlQuery(const char *querystring, MysqlDatabase *db) :
	m_pDatabase(db)
{
	m_QueryLen = strlen(querystring);
	m_QueryString = new char[m_QueryLen + 1];
	m_LastRes = NULL;
	strcpy(m_QueryString, querystring);
}

MysqlQuery::~MysqlQuery()
{
	if (m_LastRes)
	{
		m_LastRes->FreeHandle();
	}

	delete [] m_QueryString;
}

void MysqlQuery::FreeHandle()
{
	delete this;
}

bool MysqlQuery::Execute(QueryInfo *info, char *error, size_t maxlength)
{
	bool res = ExecuteR(info, error, maxlength);

	if (m_LastRes)
		m_LastRes->FreeHandle();
	
	m_LastRes = (MysqlResultSet *)info->rs;

	return res;
}

bool MysqlQuery::Execute2(QueryInfo *info, char *error, size_t maxlength)
{
	bool res = ExecuteR(info, error, maxlength);

	if (m_LastRes)
		m_LastRes->FreeHandle();

	m_LastRes = (MysqlResultSet *)info->rs;

	if (info->success)
	{
		info->insert_id = mysql_insert_id(m_pDatabase->m_pMysql);
	} else {
		info->insert_id = 0;
	}

	return res;
}

const char *MysqlQuery::GetQueryString()
{
	return m_QueryString;
}

bool MysqlQuery::ExecuteR(QueryInfo *info, char *error, size_t maxlength)
{
	int err;

	if ( (err=mysql_real_query(m_pDatabase->m_pMysql, m_QueryString, (unsigned long)m_QueryLen)) )
	{
		info->errorcode = mysql_errno(m_pDatabase->m_pMysql);
		info->success = false;
		info->affected_rows = 0;
		info->rs = NULL;
		if (error && maxlength)
		{
			ke::SafeSprintf(error, maxlength, "%s", mysql_error(m_pDatabase->m_pMysql));
		}
	}
	else
	{
		MYSQL_RES *res = mysql_store_result(m_pDatabase->m_pMysql);
		if (!res)
		{
			if (mysql_field_count(m_pDatabase->m_pMysql) > 0)
			{
				//error !111!!11
				info->errorcode = mysql_errno(m_pDatabase->m_pMysql);
				info->success = false;
				info->affected_rows = 0;
				info->rs = NULL;
			} else {
				info->errorcode = 0;
				info->success = true;
				info->affected_rows = mysql_affected_rows(m_pDatabase->m_pMysql);
				info->rs = NULL;
			}
		} else {
			info->errorcode = 0;
			info->success = true;
			info->affected_rows = mysql_affected_rows(m_pDatabase->m_pMysql);
			MysqlResultSet *rs = new MysqlResultSet(res, m_pDatabase->m_pMysql);
			info->rs = rs;
		}
	}

	return info->success;
}

