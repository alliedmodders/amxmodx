#ifndef _INCLUDE_AMXMODX_MYSQL2_HEADER_H
#define _INCLUDE_AMXMODX_MYSQL2_HEADER_H

#include "MysqlDriver.h"
#include "amxxmodule.h"
#include "ThreadSupport.h"
#include "ThreadWorker.h"

#define MYSQL2_THREADED

struct AmxQueryInfo
{
	IQuery *pQuery;
	QueryInfo info;
	char error[255];
};

enum HandleType
{
	Handle_Invalid = -1,
	Handle_Connection = 0,
	Handle_Database,
	Handle_Query,
	Handle_ThreadQuery,
};

struct SQL_Connection
{
	char *host;
	char *user;
	char *pass;
	char *db;
	int port;
};

typedef void (*FREEHANDLE)(void *, unsigned int);

unsigned int MakeHandle(void *ptr, HandleType type, FREEHANDLE f);
void *GetHandle(unsigned int num, HandleType type);
bool FreeHandle(unsigned int num);

extern AMX_NATIVE_INFO g_BaseSqlNatives[];
extern AMX_NATIVE_INFO g_ThreadSqlNatives[];
extern MainThreader g_Threader;
extern ThreadWorker *g_pWorker;
extern SourceMod::MysqlDriver g_Mysql;

#endif //_INCLUDE_AMXMODX_MYSQL2_HEADER_H

