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

#ifndef _INCLUDE_AMXMODX_MYSQL2_HEADER_H
#define _INCLUDE_AMXMODX_MYSQL2_HEADER_H

#include "SqliteDriver.h"
#include "amxxmodule.h"
#include "ThreadSupport.h"
#include "ThreadWorker.h"

#define SQLITE_THREADED

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
extern SourceMod::SqliteDriver g_Sqlite;

#endif //_INCLUDE_AMXMODX_MYSQL2_HEADER_H
