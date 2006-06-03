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
};

typedef int stridx_t;

class StringPool
{
public:
	StringPool();
	~StringPool();
	void SetMutex(IMutex *m);
	void UnsetMutex();
	bool IsThreadable();
public:
	stridx_t MakeString(const char *str);
	void FreeString(stridx_t idx);
	const char *GetString(stridx_t idx);
	void StartHardLock();
	void StopHardLock();
public:
	static const int NullString = -1;
private:
	CStack<stridx_t> m_FreeStrings;
	CVector<stridx_t> m_UseTable;
	CVector<SourceHook::String *> m_Strings;
	IMutex *m_mutex;
	bool m_stoplock;
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
	unsigned int m_AllocFields;
	unsigned int m_AllocRows;
	stridx_t *m_Fields;
	stridx_t **m_Rows;
	unsigned int m_CurRow;
	bool m_IsFree;
};

class MysqlThread : public IThread
{
public:
	MysqlThread();
	~MysqlThread();
public:
	void SetInfo(const char *db);
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
	SourceHook::String m_db;
	cell *m_data;
	ucell m_datalen;
	size_t m_maxdatalen;
	int m_fwd;
	QueuedResultInfo m_qrInfo;
	AtomicResult m_atomicResult;
};

#endif //_INCLUDE_MYSQL_THREADING_H
