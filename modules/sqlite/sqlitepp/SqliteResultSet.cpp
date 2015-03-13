// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SQLite Module
//

#include <string.h>
#include <stdlib.h>
#include "SqliteResultSet.h"

using namespace SourceMod;

SqliteResultSet::SqliteResultSet(SqliteQuery::SqliteResults &res)
{
	m_pResults = res.results;
	m_Columns = res.cols;
	m_Rows = res.rows;

	m_CurRow = 1;
	m_CurIndex = (m_CurRow * m_Columns);
}

SqliteResultSet::~SqliteResultSet()
{
	if (m_pResults)
	{
		sqlite3_free_table(m_pResults);
		m_pResults = NULL;
	}
}

const char *SqliteResultSet::GetStringSafe(unsigned int columnId)
{
	if (columnId > m_Columns)
	{
		return "";
	}

	const char *data = m_pResults[m_CurIndex + columnId];

	return data ? data : "";
}

const char *SqliteResultSet::GetString(unsigned int columnId)
{
	if (columnId > m_Columns)
	{
		return NULL;
	}

	return m_pResults[m_CurIndex + columnId];
}

bool SqliteResultSet::IsNull(unsigned int columnId)
{
	return (GetString(columnId) == NULL);
}

double SqliteResultSet::GetDouble(unsigned int columnId)
{
	return atof(GetStringSafe(columnId));
}

float SqliteResultSet::GetFloat(unsigned int columnId)
{
	return (float)atof(GetStringSafe(columnId));
}

int SqliteResultSet::GetInt(unsigned int columnId)
{
	return atoi(GetStringSafe(columnId));
}

/**
 * :TODO: - convert this whole beast to sqlite3_prepare/step
 * that way we get finer control and actual raw/null data.
 */
const char *SqliteResultSet::GetRaw(unsigned int columnId, size_t *length)
{
	if (columnId >= m_Columns)
	{
		if (length)
		{
			*length = 0;
		}
		return NULL;
	}

	const char *str = GetString(columnId);
	if (!str)
	{
		if (length)
		{
			*length = 0;
		}
		return NULL;
	} else {
		if (length)
		{
			*length = strlen(str);
		}
		return str;
	}
}

void SqliteResultSet::FreeHandle()
{
	delete this;
}

IResultRow *SqliteResultSet::GetRow()
{
	return static_cast<IResultRow *>(this);
}

unsigned int SqliteResultSet::RowCount()
{
	return m_Rows;
}

const char *SqliteResultSet::FieldNumToName(unsigned int num)
{
	if (num >= m_Columns)
	{
		return NULL;
	}

	return m_pResults[num];
}

bool SqliteResultSet::FieldNameToNum(const char *name, unsigned int *columnId)
{
	for (unsigned int i=0; i<m_Columns; i++)
	{
		if (strcmp(m_pResults[i], name) == 0)
		{
			if (columnId)
			{
				*columnId = i;
			}
			return true;
		}
	}

	if (columnId)
	{
		*reinterpret_cast<int *>(columnId) = -1;
	}

	return false;
}

unsigned int SqliteResultSet::FieldCount()
{
	return m_Columns;
}

bool SqliteResultSet::IsDone()
{
	return (m_CurRow > m_Rows);
}

void SqliteResultSet::NextRow()
{
	m_CurIndex = (++m_CurRow * m_Columns);
}

void SqliteResultSet::Rewind()
{
	m_CurRow = 1;
	m_CurIndex = (m_CurRow * m_Columns);
}

bool SqliteResultSet::NextResultSet()
{
	return false;
}
