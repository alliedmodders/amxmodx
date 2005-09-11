#include "sqlite_amx.h"

SQL::SQL()
{
	isFree = true;
	sqlite = NULL;
}

SQL::~SQL()
{
	if (!isFree)
		Disconnect();
}
int SQL::Error()
{
	if (sqlite == NULL)
		return 0;

	ErrorCode = sqlite3_errcode(sqlite);
	ErrorStr.assign(sqlite3_errmsg(sqlite));

	return ErrorCode;
}

int SQL::Connect(const char *base)
{
	Database.assign(base);

	isFree = false;
	int err = 0;
	
	this->ErrorCode = sqlite3_open(Database.c_str(), &sqlite);
	if (ErrorCode != SQLITE_OK) {
		err = Error();
		if (err)
		{
			MF_Log("DB Connection failed(%d): %s", err, sqlite3_errmsg(sqlite));
			sqlite3_close(sqlite);
			isFree = true;
			return 0;
		}
	}

	isFree = false;

	return 1;
}

void SQL::Disconnect()
{
	Database.clear();

	if (sqlite != NULL)
		sqlite3_close(sqlite);

	sqlite = NULL;

	isFree = true;
}

int SQL::Query(const char *query)
{
	if (sqlite == NULL || isFree)
	{
		ErrorCode = -1;
		return -1;
	}

	unsigned int i = 0;
	int id = -1;
	for (i=0; i < Results.size(); i++)
	{
		if (Results[i]->isFree) {
			id = i;
			break;
		}
	}

	if (id < 0) {

		SQLResult *p = new SQLResult;

		int ret = p->Query(this, query);

		if (ret != 0)
		{
			delete p;

			if (ret == -1)
				return 0;
			else
				return -1;
		} else {
			Results.push_back(p);
#if defined _DEBUG
			MF_PrintSrvConsole("***STORE: %d (push_back)\n", Results.size());
			SQLResult::latestStoredResultId = Results.size();
#endif

			return Results.size();
		}
	} else {
		SQLResult *r = Results[id];
		int ret = Results[id]->Query(this, query);
		if (ret != 0)
		{
			if (ret == -1)
				return 0;
			else
				return -1;
		} else {
#if defined _DEBUG
			MF_PrintSrvConsole("***STORE: %d\n", id + 1);
			SQLResult::latestStoredResultId = id + 1;
#endif
			return (id + 1);
		}
	}
}

SQLResult::SQLResult()
{
	isFree = true;
	m_fieldNames = 0;
	m_hasData = false;
	m_currentRow = -1;
	m_data = NULL;
	m_errorMsg = NULL;
	m_rowCount = 0;
	m_columnCount = 0;
}

SQLResult::~SQLResult()
{
	if (!isFree)
		FreeResult();
}

int SQLResult::Query(SQL *cn, const char *query)
{
	/*
	int sqlite3_get_table(
	sqlite3*,               // An open database 
	const char *sql,       // SQL to be executed 
	char ***resultp,       // Result written to a char *[]  that this points to 
	int *nrow,             // Number of result rows written here 
	int *ncolumn,          // Number of result columns written here 
	char **errmsg          // Error msg written here 
	);
	*/
	int rowCount, columnCount;
	int result = sqlite3_get_table(cn->sqlite, query, &m_data, &rowCount, &columnCount, &m_errorMsg);
	m_rowCount = rowCount;
	m_columnCount = columnCount;
	if (result != SQLITE_OK)
	{
		MF_Log("Query error: %s", m_errorMsg);
		return 1;
	}
	else {
		if (!m_rowCount)
			return -1;
		m_hasData = true;
		this->m_fieldNames = new String[m_columnCount];
		for (unsigned int i = 0; i < m_columnCount; i++)
			m_fieldNames[i].assign(m_data[i]);

#if defined _DEBUG
		MF_PrintSrvConsole("SQLite: Select query returned %d rows in %d columns.\n", m_rowCount, m_columnCount);
		for (unsigned int i = 0; i < m_columnCount; i++) {
			MF_PrintSrvConsole("%15s", m_fieldNames[i].c_str());
		}
		MF_PrintSrvConsole("\n");
		for (unsigned int i = 0; i < m_rowCount; i++) {
			for (unsigned int j = 0; j < m_columnCount; j++) {
				MF_PrintSrvConsole("%15s", m_data[(1 + i) * m_columnCount + j]);
			}
			MF_PrintSrvConsole("\n");
		}
#endif
	}

	isFree = false;
	return 0; // Return 0 here? and 1 on error... 0's get stored and 1's get deleted
}

bool SQLResult::Nextrow()
{
	if (isFree)
		return false;

	if (++m_currentRow >= (int)this->m_rowCount) {
		//m_currentRow = -1; <-- this is probably bad and inconsistent...
		//FreeResult(); <-- this is probably bad and inconsistent... freeing should be the responsibility of the scripter
		return false;
	}

	return true;
}

void SQLResult::FreeResult()
{
	if (isFree)
		return;
	/*
#if defined _DEBUG
	MF_PrintSrvConsole("FREEING a result!\n");
#endif
	*/
	isFree = true;

	if (m_hasData) {
		sqlite3_free_table(m_data);

		delete [] this->m_fieldNames;

		m_hasData = false;
	}

	m_currentRow = -1;
	m_columnCount = 0;
	m_rowCount = 0;
}

const char *SQLResult::GetField(unsigned int field)
{
	if (isFree || field >= m_columnCount || m_currentRow < 0 || m_currentRow >= (int)m_rowCount)
	{
		return NULL;
	}

	return m_data[(m_currentRow + 1) * m_columnCount + field];
}

const char *SQLResult::GetField(const char *field)
{
	unsigned int i = 0;
	int id = -1;
	if (field == NULL)
		return NULL;
	for (i=0; i < m_columnCount; i++)
	{
		if (strcmp(m_fieldNames[i].c_str(), field) == 0)
		{
			id = i;
			break;
		}
	}

	if (id<0 || id>=(int)m_columnCount)
	{
		return NULL;
	}

	return GetField(id);
}

unsigned int SQLResult::NumRows()
{
	if (isFree)
		return 0;

	return m_rowCount;
}
#if defined _DEBUG
unsigned int SQLResult::latestStoredResultId = 0;
#endif

