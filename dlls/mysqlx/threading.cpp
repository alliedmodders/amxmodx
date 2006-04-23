#include "amxxmodule.h"
#include "mysql2_header.h"
#include "threading.h"

using namespace SourceMod;
using namespace SourceHook;

MainThreader g_Threader;
ThreadWorker *g_pWorker = NULL;
extern DLL_FUNCTIONS *g_pFunctionTable;
StringPool g_StringPool;
IMutex *g_QueueLock = NULL;
CStack<MysqlThread *> g_ThreadQueue;
CStack<MysqlThread *> g_FreeThreads;
float g_lasttime = 0.0f;

void OnAmxxDetach()
{
	if (g_pWorker)
	{
		g_pWorker->Stop(true);
		delete g_pWorker;
		g_pWorker = NULL;
	}

	g_QueueLock->Lock();
	while (!g_ThreadQueue.empty())
	{
		delete g_ThreadQueue.front();
		g_ThreadQueue.pop();
	}
	while (!g_FreeThreads.empty())
	{
		delete g_FreeThreads.front();
		g_FreeThreads.pop();
	}
	g_QueueLock->Unlock();
	g_QueueLock->DestroyThis();
}

//public QueryHandler(state, Handle:query, error[], errnum, data[], size)
//native SQL_ThreadQuery(Handle:cn_tuple, const handler[], const query[], const data[]="", dataSize=0);
static cell AMX_NATIVE_CALL SQL_ThreadQuery(AMX *amx, cell *params)
{
	if (!g_pWorker)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Thread worker was unable to start.");
		return 0;
	}

	SQL_Connection *cn = (SQL_Connection *)GetHandle(params[1], Handle_Connection);
	if (!cn)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid handle: %d", params[1]);
		return 0;
	}

	int len;
	const char *handler = MF_GetAmxString(amx, params[2], 0, &len);
	int fwd = MF_RegisterSPForwardByName(amx, handler, FP_CELL, FP_CELL, FP_STRING, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
	if (fwd < 1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Function not found: %s", handler);
		return 0;
	}

	MysqlThread *kmThread;
	g_QueueLock->Lock();
	if (g_FreeThreads.empty())
	{
		kmThread = new MysqlThread();
	} else {
		kmThread = g_FreeThreads.front();
		g_FreeThreads.pop();
	}
	g_QueueLock->Unlock();

	kmThread->SetInfo(cn->host, cn->user, cn->pass, cn->db, cn->port);
	kmThread->SetForward(fwd);
	kmThread->SetQuery(MF_GetAmxString(amx, params[3], 1, &len));
	kmThread->SetCellData(MF_GetAmxAddr(amx, params[4]), (ucell)params[5]);

	g_pWorker->MakeThread(kmThread);

	return 1;
}

MysqlThread::MysqlThread()
{
	m_fwd = 0;
	m_data = NULL;
	m_datalen = 0;
	m_maxdatalen = 0;
}

MysqlThread::~MysqlThread()
{
	if (m_fwd)
	{
		MF_UnregisterSPForward(m_fwd);
		m_fwd = 0;
	}
	
	delete [] m_data;
	m_data = NULL;
}

void MysqlThread::SetCellData(cell data[], ucell len)
{
	if (len > m_maxdatalen)
	{
		delete [] m_data;
		m_data = new cell[len];
		m_maxdatalen = len;
	}
	if (len)
	{
		m_datalen = len;
		memcpy(m_data, data, len*sizeof(cell));
	}
}

void MysqlThread::SetForward(int forward)
{
	m_fwd = forward;
}

void MysqlThread::SetInfo(const char *host, const char *user, const char *pass, const char *db, int port)
{
	m_host.assign(host);
	m_user.assign(user);
	m_pass.assign(pass);
	m_db.assign(db);
	m_port = port;
}

void MysqlThread::SetQuery(const char *query)
{
	m_query.assign(query);
}

void MysqlThread::RunThread(IThreadHandle *pHandle)
{
	DatabaseInfo info;

	info.database = m_db.c_str();
	info.pass = m_pass.c_str();
	info.user = m_user.c_str();
	info.host = m_host.c_str();
	info.port = m_port;

	memset(&m_qrInfo, 0, sizeof(m_qrInfo));

	IDatabase *pDatabase = g_Mysql.Connect(&info, &m_qrInfo.amxinfo.info.errorcode, m_qrInfo.amxinfo.error, 254);
	IQuery *pQuery = NULL;
	if (!pDatabase)
	{
		m_qrInfo.connect_success = false;
		m_qrInfo.query_success = false;
	} else {
		m_qrInfo.connect_success = true;
		pQuery = pDatabase->PrepareQuery(m_query.c_str());
		if (!pQuery->Execute(&m_qrInfo.amxinfo.info, m_qrInfo.amxinfo.error, 254))
		{
			m_qrInfo.query_success = false;
		} else {
			m_qrInfo.query_success = true;
		}
	}

	if (m_qrInfo.query_success && m_qrInfo.amxinfo.info.rs)
	{
		m_atomicResult.CopyFrom(m_qrInfo.amxinfo.info.rs);
		m_qrInfo.amxinfo.pQuery = NULL;
		m_qrInfo.amxinfo.info.rs = &m_atomicResult;
	}

	if (pQuery)
	{
		pQuery->FreeHandle();
		pQuery = NULL;
	}
	if (pDatabase)
	{
		pDatabase->FreeHandle();
		pDatabase = NULL;
	}
}

void MysqlThread::Invalidate()
{
	m_atomicResult.FreeHandle();
}

void MysqlThread::OnTerminate(IThreadHandle *pHandle, bool cancel)
{
	if (cancel)
	{
		Invalidate();
		g_QueueLock->Lock();
		g_FreeThreads.push(this);
		g_QueueLock->Unlock();
	} else {
		g_QueueLock->Lock();
		g_ThreadQueue.push(this);
		g_QueueLock->Unlock();
	}
}

void NullFunc(void *ptr, unsigned int num)
{
}

//public QueryHandler(state, Handle:query, error[], errnum, data[], size)
void MysqlThread::Execute()
{
	cell data_addr;
	if (m_datalen)
	{
		data_addr = MF_PrepareCellArray(m_data, m_datalen);
	} else {
		static cell tmpdata[1] = {0};
		data_addr = MF_PrepareCellArray(tmpdata, 1);
	}
	int state = 0;
	if (!m_qrInfo.connect_success)
	{
		state = -2;
	} else if (!m_qrInfo.query_success) {
		state = -1;
	}
	if (state != 0)
	{
		MF_ExecuteForward(m_fwd, 
			(cell)state, 
			(cell)0, 
			m_qrInfo.amxinfo.error, 
			m_qrInfo.amxinfo.info.errorcode,
			data_addr,
			m_datalen);
	} else {
		unsigned int hndl = MakeHandle(&m_qrInfo.amxinfo, Handle_Query, NullFunc);
		MF_ExecuteForward(m_fwd,
			(cell)0,
			(cell)hndl,
			"",
			(cell)0,
			data_addr,
			m_datalen);
		FreeHandle(hndl);
	}
}

/*****************
 * METAMOD STUFF *
 *****************/

int DispatchSpawn(edict_t *pEnt)
{
	if (g_pWorker)
	{
		RETURN_META_VALUE(MRES_IGNORED, 0);
	}

	if (!g_StringPool.IsThreadable())
	{
		g_StringPool.SetMutex(g_Threader.MakeMutex());
		g_QueueLock = g_Threader.MakeMutex();
	}
	g_pWorker = new ThreadWorker(&g_Threader, 250);
	if (!g_pWorker->Start())
	{
		delete g_pWorker;
		g_pWorker = NULL;
	}
	g_pFunctionTable->pfnSpawn = NULL;

	g_lasttime = 0.0f;

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void StartFrame()
{
	if (g_lasttime < gpGlobals->time)
	{
        g_lasttime = gpGlobals->time + 0.3f;
		g_QueueLock->Lock();
		size_t remaining = g_ThreadQueue.size();
		if (remaining)
		{
			MysqlThread *kmThread;
			do 
			{
				kmThread = g_ThreadQueue.front();
				g_ThreadQueue.pop();
				g_QueueLock->Unlock();
				kmThread->Execute();
				kmThread->Invalidate();
				g_FreeThreads.push(kmThread);
				g_QueueLock->Lock();
			} while (!g_ThreadQueue.empty());
		}

		g_QueueLock->Unlock();
	}

	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate()
{
	g_pFunctionTable->pfnSpawn = DispatchSpawn;

	if (!g_pWorker)
		RETURN_META(MRES_IGNORED);

	g_pWorker->Stop(false);
	delete g_pWorker;
	g_pWorker = NULL;

	RETURN_META(MRES_IGNORED);
}

/***********************
 * ATOMIC RESULT STUFF *
 ***********************/

AtomicResult::AtomicResult()
{
	m_IsFree = true;
	m_CurRow = 0;
	m_AllocFields = 0;
	m_AllocRows = 0;
	m_Rows = NULL;
	m_Fields = NULL;
}

AtomicResult::~AtomicResult()
{
	if (!m_IsFree)
	{
		FreeHandle();
	}

	if (m_AllocFields)
	{
		delete [] m_Fields;
		m_AllocFields = 0;
		m_Fields = NULL;
	}
	if (m_AllocRows)
	{
		for (unsigned int i=0; i<m_AllocRows; i++)
			delete [] m_Rows[i];
		delete [] m_Rows;
		m_Rows = NULL;
		m_AllocRows = NULL;
	}
}

unsigned int AtomicResult::RowCount()
{
	return m_RowCount;
}

bool AtomicResult::IsNull(unsigned int columnId)
{
	return (GetString(columnId) == NULL);
}

unsigned int AtomicResult::FieldCount()
{
	return m_FieldCount;
}

bool AtomicResult::FieldNameToNum(const char *name, unsigned int *columnId)
{
	for (unsigned int i=0; i<m_FieldCount; i++)
	{
		if (strcmp(g_StringPool.GetString(m_Fields[i]), name) == 0)
		{
			if (*columnId)
				*columnId = i;
			return true;
		}
	}

	return false;
}

const char *AtomicResult::FieldNumToName(unsigned int num)
{
	if (num >= m_FieldCount)
		return NULL;

	return g_StringPool.GetString(m_Fields[num]);
}

double AtomicResult::GetDouble(unsigned int columnId)
{
	return atof(GetStringSafe(columnId));
}

float AtomicResult::GetFloat(unsigned int columnId)
{
	return atof(GetStringSafe(columnId));
}

int AtomicResult::GetInt(unsigned int columnId)
{
	return atoi(GetStringSafe(columnId));
}

const char *AtomicResult::GetRaw(unsigned int columnId, size_t *length)
{
	//we don't support this yet...
	*length = 0;
	return "";
}

const char *AtomicResult::GetStringSafe(unsigned int columnId)
{
	const char *str = GetString(columnId);
	
	return str ? str : "";
}

const char *AtomicResult::GetString(unsigned int columnId)
{
	if (columnId >= m_FieldCount)
		return NULL;

	return g_StringPool.GetString(m_Rows[m_CurRow][columnId]);
}

IResultRow *AtomicResult::GetRow()
{
	return static_cast<IResultRow *>(this);
}

bool AtomicResult::IsDone()
{
	if (m_CurRow >= m_RowCount)
		return true;

	return false;
}

void AtomicResult::NextRow()
{
	m_CurRow++;
}

void AtomicResult::_InternalClear()
{
	if (m_IsFree)
		return;

	m_IsFree = true;

	g_StringPool.StartHardLock();

	for (unsigned int i=0; i<m_FieldCount; i++)
		g_StringPool.FreeString(m_Fields[i]);

	for (unsigned int i=0; i<m_RowCount; i++)
	{
		for (unsigned int j=0; j<m_FieldCount; j++)
			g_StringPool.FreeString(m_Rows[i][j]);
	}

	g_StringPool.StopHardLock();
}

void AtomicResult::FreeHandle()
{
	_InternalClear();
}

void AtomicResult::CopyFrom(IResultSet *rs)
{
	if (!m_IsFree)
	{
		_InternalClear();
	}

	m_IsFree = false;

	m_FieldCount = rs->FieldCount();
	m_RowCount = rs->RowCount();
	if (m_RowCount > m_AllocRows)
	{
		/** allocate new array, zero it */
		stridx_t **newRows = new stridx_t *[m_RowCount];
		memset(newRows, 0, m_RowCount * sizeof(stridx_t *));
		/** if we have a new field count, just delete all the old stuff. */
		if (m_FieldCount > m_AllocFields)
		{
			for (unsigned int i=0; i<m_AllocRows; i++)
			{
				delete [] m_Rows[i];
				newRows[i] = new stridx_t[m_FieldCount];
			}
			for (unsigned int i=m_AllocRows; i<m_RowCount; i++)
				newRows[i] = new stridx_t[m_FieldCount];
		} else {
			/** copy the old pointers */
			memcpy(newRows, m_Rows, m_AllocRows * sizeof(stridx_t *));
			for (unsigned int i=m_AllocRows; i<m_RowCount; i++)
				newRows[i] = new stridx_t[m_AllocFields];
		}
		delete [] m_Rows;
		m_Rows = newRows;
		m_AllocRows = m_RowCount;
	}
	if (m_FieldCount > m_AllocFields)
	{
		delete [] m_Fields;
		m_Fields = new stridx_t[m_FieldCount];
		m_AllocFields = m_FieldCount;
	}
	m_CurRow = 0;

	g_StringPool.StartHardLock();

	IResultRow *row;
	unsigned int idx = 0;
	while (!rs->IsDone())
	{
		row = rs->GetRow();
		for (size_t i=0; i<m_FieldCount; i++)
			m_Rows[idx][i] = g_StringPool.MakeString(row->GetString(i));
		rs->NextRow();
		idx++;
	}

	for (unsigned int i=0; i<m_FieldCount; i++)
		m_Fields[i] = g_StringPool.MakeString(rs->FieldNumToName(i));

	g_StringPool.StopHardLock();
}

/*********************
 * STRING POOL STUFF *
 *********************/

StringPool::StringPool()
{
	m_mutex = NULL;
	m_stoplock = false;
}

StringPool::~StringPool()
{
	if (m_stoplock)
		StopHardLock();

	if (m_mutex)
		UnsetMutex();

	for (size_t i=0; i<m_Strings.size(); i++)
		delete m_Strings[i];

	m_Strings.clear();
	m_UseTable.clear();
	while (!m_FreeStrings.empty())
		m_FreeStrings.pop();
}

bool StringPool::IsThreadable()
{
	return (m_mutex != NULL);
}

const char *StringPool::GetString(stridx_t idx)
{
	if (idx < 0 || idx >= (int)m_Strings.size() || !m_UseTable[idx])
		return NULL;

	return m_Strings[idx]->c_str();
}

void StringPool::FreeString(stridx_t idx)
{
	if (idx < 0 || idx >= (int)m_Strings.size())
		return;

	if (!m_stoplock && m_mutex)
		m_mutex->Lock();

	if (m_UseTable[idx])
	{
		m_FreeStrings.push(idx);
		m_UseTable[idx] = 0;
	}

	if (!m_stoplock && m_mutex)
		m_mutex->Unlock();
}

stridx_t StringPool::MakeString(const char *str)
{
	if (!str)
		return StringPool::NullString;

	if (!m_stoplock && m_mutex)
		m_mutex->Lock();

	stridx_t idx;

	if (m_FreeStrings.empty())
	{
		idx = static_cast<stridx_t>(m_Strings.size());
		SourceHook::String *shString = new SourceHook::String(str);
		m_Strings.push_back(shString);
		m_UseTable.push_back(1);
	} else {
		idx = m_FreeStrings.front();
		m_FreeStrings.pop();
		m_UseTable[idx] = 1;
		m_Strings[idx]->assign(str);
	}

	if (!m_stoplock && m_mutex)
		m_mutex->Unlock();

	return idx;
}

void StringPool::SetMutex(IMutex *m)
{
	m_mutex = m;
}

void StringPool::UnsetMutex()
{
	if (m_mutex)
	{
		m_mutex->DestroyThis();
		m_mutex = NULL;
	}
}

void StringPool::StartHardLock()
{
	if (m_stoplock)
		return;

	m_mutex->Lock();
	m_stoplock = true;
}

void StringPool::StopHardLock()
{
	if (!m_stoplock)
		return;

	m_mutex->Unlock();
	m_stoplock = false;
}

AMX_NATIVE_INFO g_ThreadSqlNatives[] =
{
	{"SQL_ThreadQuery",			SQL_ThreadQuery},
	{NULL,						NULL},
};
