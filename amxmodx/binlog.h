#ifndef _INCLUDE_BINLOG_H
#define _INCLUDE_BINLOG_H

#if defined BINLOG_ENABLED

#include "CString.h"

#define BINLOG_MAGIC	0x414D424C
#define BINLOG_VERSION	0x0100
#define BINDB_MAGIC		0x414D4244
#define BINDB_VERSION	0x0100

/**
 * Format of binlog:
 * int32_t		magic
 * int16_t		version
 * int8_t		sizeof(time_t)
 * [
 *  time_t		realtime
 *  float		gametime
 *  int8_t		operation code
 *  int16_t		plugin id
 *  <extra info>
 * ]
 * Format of bindb:
 * int32_t		magic
 * int16_t		version
 * int32_t		num plugins
 * [
 *  int8_t		status codes
 *  str[int8_t]	filename
 *  int32_t		num natives
 *  int32_t		num publics
 *  [
 *   str[int8_t] native name
 *  ]
 *  [
 *   str[int8_t] public name
 *  ]
 */

enum BinLogOp
{
	BinLog_Start=1,
	BinLog_End,
	BinLog_NativeCall,	//<int16_t native id>
	BinLog_CallPubFunc,	//<int16_t public id>
	BinLog_SetLine,		//<int16_t line no#>
	BinLog_Registered,	//<string title> <string version>
};

class BinLog
{
public:
	bool Open();
	void Close();
	void CacheAllPlugins();
	void WriteOp(BinLogOp op, int plug, ...);
private:
	String m_dbfile;
	String m_logfile;
};

#endif //BINLOG_ENABLED

extern BinLog g_BinLog;

#endif //_INCLUDE_BINLOG_H
