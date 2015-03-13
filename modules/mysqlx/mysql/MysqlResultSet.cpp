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

#include <string.h>
#include <stdlib.h>
#include "MysqlResultSet.h"

using namespace SourceMod;

MysqlResultRow::MysqlResultRow() : 
	m_CurRow(NULL), m_Columns(0)
{
}

const char *MysqlResultRow::GetRaw(unsigned int columnId, size_t *length)
{
	if (columnId >= m_Columns)
	{
		if (length)
			*length = 0;
		return NULL;
	}

	*length = static_cast<size_t>(m_Lengths[columnId]);
	return m_CurRow[columnId];
}

bool MysqlResultRow::IsNull(unsigned int columnId)
{
	if (columnId >= m_Columns)
		return true;

	return (m_CurRow[columnId] == NULL);
}

const char *MysqlResultRow::GetStringSafe(unsigned int columnId)
{
	if (columnId >= m_Columns)
		return "";

	return (m_CurRow[columnId] ? m_CurRow[columnId] : "");
}

const char *MysqlResultRow::GetString(unsigned int columnId)
{
	if (columnId >= m_Columns)
		return NULL;

	return m_CurRow[columnId];
}

double MysqlResultRow::GetDouble(unsigned int columnId)
{
	return atof(GetStringSafe(columnId));
}

float MysqlResultRow::GetFloat(unsigned int columnId)
{
	return (float)atof(GetStringSafe(columnId));
}

int MysqlResultRow::GetInt(unsigned int columnId)
{
	return atoi(GetStringSafe(columnId));
}

MysqlResultSet::MysqlResultSet(MYSQL_RES *res, MYSQL *mysql) : 
	m_pRes(res)
{
	m_Rows = (unsigned int)mysql_num_rows(res);
	m_Columns = (unsigned int)mysql_num_fields(res);
	m_pMySQL = mysql;

	if (m_Rows > 0)
	{
		NextRow();
	}

	m_kRow.m_Columns = m_Columns;
}

MysqlResultSet::~MysqlResultSet()
{
	if (m_pRes == NULL)
	{
		return;
	}

	mysql_free_result(m_pRes);
	while (mysql_next_result(m_pMySQL) == 0)
	{
		m_pRes = mysql_store_result(m_pMySQL);
		if (m_pRes != NULL)
		{
			mysql_free_result(m_pRes);
		}
	}
}

bool MysqlResultSet::NextResultSet()
{
	if (!mysql_more_results(m_pMySQL))
	{
		return false;
	}

	mysql_free_result(m_pRes);
	if (mysql_next_result(m_pMySQL) != 0
		|| (m_pRes = mysql_store_result(m_pMySQL)) == NULL)
	{
		m_Rows = 0;
		m_pRes = NULL;
		m_Columns = 0;
		m_kRow.m_CurRow = NULL;
		return false;
	}

	m_Rows = (unsigned int)mysql_num_rows(m_pRes);
	m_Columns = (unsigned int)mysql_num_fields(m_pRes);

	if (m_Rows > 0)
	{
		NextRow();
	}

	m_kRow.m_Columns = m_Columns;

	return true;
}

void MysqlResultSet::FreeHandle()
{
	delete this;
}

IResultRow *MysqlResultSet::GetRow()
{
	return static_cast<IResultRow *>(&m_kRow);
}

unsigned int MysqlResultSet::RowCount()
{
	return m_Rows;
}

const char *MysqlResultSet::FieldNumToName(unsigned int num)
{
	if (num >= m_Columns)
		return NULL;

	MYSQL_FIELD *field = mysql_fetch_field_direct(m_pRes, num);
	if (!field || !field->name)
		return "";

	return field->name;
}

bool MysqlResultSet::FieldNameToNum(const char *name, unsigned int *columnId)
{
	MYSQL_FIELD *field;
	for (unsigned int i=0; i<m_Columns; i++)
	{
		field = mysql_fetch_field_direct(m_pRes, i);
		if (!field || !field->name)
			continue;
		if (strcmp(name, field->name) == 0)
		{
			if (columnId)
				*columnId = i;
			return true;
		}
	}

	return false;
}

unsigned int MysqlResultSet::FieldCount()
{
	return m_Columns;
}

bool MysqlResultSet::IsDone()
{
	return (m_kRow.m_CurRow == NULL);
}

void MysqlResultSet::NextRow()
{
	MYSQL_ROW row = mysql_fetch_row(m_pRes);
	m_kRow.m_CurRow = row;

	if (row)
	{
		unsigned long *lengths = mysql_fetch_lengths(m_pRes);
		m_kRow.m_Lengths = lengths;
	}
}

void MysqlResultSet::Rewind()
{
	mysql_data_seek(m_pRes, 0);
	NextRow();
}
