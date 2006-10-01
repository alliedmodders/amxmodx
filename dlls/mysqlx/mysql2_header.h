#ifndef _INCLUDE_AMXMODX_MYSQL2_HEADER_H
#define _INCLUDE_AMXMODX_MYSQL2_HEADER_H

#include "MysqlDriver.h"
#include "amxxmodule.h"
#include "ThreadSupport.h"
#include "ThreadWorker.h"

#define MYSQL2_THREADED

struct AmxQueryInfo
{
	AmxQueryInfo() : opt_ptr(NULL) { };
	IQuery *pQuery;
	QueryInfo info;
	char error[255];
	char *opt_ptr;
};

enum HandleType
{
	Handle_Invalid = -1,
	Handle_Connection = 0,
	Handle_Database,
	Handle_Query,
	Handle_OldDb,
	Handle_OldResult,
};

struct SQL_Connection
{
	char *host;
	char *user;
	char *pass;
	char *db;
	int port;
	unsigned int max_timeout;
};

typedef void (*FREEHANDLE)(void *, unsigned int);

unsigned int MakeHandle(void *ptr, HandleType type, FREEHANDLE f);
void *GetHandle(unsigned int num, HandleType type);
bool FreeHandle(unsigned int num);
void FreeAllHandles(HandleType type);
void FreeHandleTable();
void ShutdownThreading();
int SetMysqlAffinity(AMX *amx);

extern AMX_NATIVE_INFO g_BaseSqlNatives[];
extern AMX_NATIVE_INFO g_ThreadSqlNatives[];
extern AMX_NATIVE_INFO g_OldCompatNatives[];
extern MainThreader g_Threader;
extern ThreadWorker *g_pWorker;
extern SourceMod::MysqlDriver g_Mysql;

#endif //_INCLUDE_AMXMODX_MYSQL2_HEADER_H
