#ifndef _INCLUDE_MYSQL_THREADING_H
#define _INCLUDE_MYSQL_THREADING_H

#include "IThreader.h"
#include "ISQLDriver.h"
#include "sh_string.h"
#include "CVector.h"
#include "sh_stack.h"

struct QueuedResultInfo
{
	AmxQueryInfo amxinfo;
	bool connect_success;
	bool query_success;
	float queue_time;
};

class AtomicResult : 
	public IResultSet,
	public IResultRow
{
	friend class MysqlThread;
public:
	AtomicResult();
	~AtomicResult();
public:
	//free the handle if necessary (see IQuery).
	virtual void FreeHandle();
	virtual unsigned int RowCount();
	virtual unsigned int FieldCount();
	virtual const char *FieldNumToName(unsigned int num);
	virtual bool FieldNameToNum(const char *name, unsigned int *columnId);
	virtual bool IsDone();
	virtual IResultRow *GetRow();
	virtual void NextRow();
	virtual void Rewind();
	virtual bool NextResultSet();
public:
	virtual const char *GetString(unsigned int columnId);
	virtual const char *GetStringSafe(unsigned int columnId);
	virtual double GetDouble(unsigned int columnId);
	virtual float GetFloat(unsigned int columnId);
	virtual int GetInt(unsigned int columnId);
	virtual bool IsNull(unsigned int columnId);
	virtual const char *GetRaw(unsigned int columnId, size_t *length);
public:
	void CopyFrom(IResultSet *rs);
private:
	void _InternalClear();
private:
	unsigned int m_RowCount;
	unsigned int m_FieldCount;
	size_t m_AllocSize;
	SourceHook::String **m_Table;
	unsigned int m_CurRow;
	bool m_IsFree;
};

class MysqlThread : public IThread
{
public:
	MysqlThread();
	~MysqlThread();
public:
	void SetInfo(const char *host, const char *user, const char *pass, const char *db, int port, unsigned int max_timeout);
	void SetQuery(const char *query);
	void SetCellData(cell data[], ucell len);
	void SetForward(int forward);
	void Invalidate();
	void Execute();
public:
	void RunThread(IThreadHandle *pHandle);
	void OnTerminate(IThreadHandle *pHandle, bool cancel);
private:
	SourceHook::String m_query;
	SourceHook::String m_host;
	SourceHook::String m_user;
	SourceHook::String m_pass;
	SourceHook::String m_db;
	unsigned int m_max_timeout;
	int m_port;
	cell *m_data;
	ucell m_datalen;
	size_t m_maxdatalen;
	int m_fwd;
	QueuedResultInfo m_qrInfo;
	AtomicResult m_atomicResult;
};

#endif //_INCLUDE_MYSQL_THREADING_H

