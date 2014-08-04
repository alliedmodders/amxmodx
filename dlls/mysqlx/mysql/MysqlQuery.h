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
