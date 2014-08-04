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

#ifndef _INCLUDE_SOURCEMOD_MYSQL_RESULTSET_H
#define _INCLUDE_SOURCEMOD_MYSQL_RESULTSET_H

#include "MysqlHeaders.h"

namespace SourceMod
{
	class MysqlResultSet;

	class MysqlResultRow : public IResultRow
	{
		friend class MysqlResultSet;
	public:
		MysqlResultRow();
	public:
		const char *GetString(unsigned int columnId);
		double GetDouble(unsigned int columnId);
		float GetFloat(unsigned int columnId);
		int GetInt(unsigned int columnId);
		bool IsNull(unsigned int columnId);
		const char *GetRaw(unsigned int columnId, size_t *length);
	private:
		const char *GetStringSafe(unsigned int columnId);
	private:
		MYSQL_ROW m_CurRow;
		unsigned long *m_Lengths;
		unsigned int m_Columns;
	};

	class MysqlResultSet : public IResultSet
	{
	public:
		MysqlResultSet(MYSQL_RES *res, MYSQL *mysql);
		~MysqlResultSet();
	public:
		void FreeHandle();
	public: 
		unsigned int RowCount();
		unsigned int FieldCount();
		const char *FieldNumToName(unsigned int num);
		bool FieldNameToNum(const char *name, unsigned int *columnId);
	public:
		bool IsDone();
		IResultRow *GetRow();
		void NextRow();
		void Rewind();
		bool NextResultSet();
	private:
		MYSQL *m_pMySQL;
		MYSQL_RES *m_pRes;
		MysqlResultRow m_kRow;
		unsigned int m_Columns;
		unsigned int m_Rows;
	};
};

#endif //_INCLUDE_SOURCEMOD_MYSQL_RESULTSET_H
