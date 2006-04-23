#include <stdlib.h>
#include "MysqlResultSet.h"

using namespace SourceMod;

MysqlResultRow::MysqlResultRow() : 
	m_Columns(0), m_CurRow(NULL)
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

MysqlResultSet::MysqlResultSet(MYSQL_RES *res) : 
	m_pRes(res)
{
	m_Rows = (unsigned int)mysql_num_rows(res);
	m_Columns = (unsigned int)mysql_num_fields(res);

	if (m_Rows > 0)
	{
		NextRow();
	}

	m_kRow.m_Columns = m_Columns;
}

MysqlResultSet::~MysqlResultSet()
{
	mysql_free_result(m_pRes);
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
