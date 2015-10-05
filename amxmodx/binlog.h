// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_BINLOG_H
#define _INCLUDE_BINLOG_H

#if defined BINLOG_ENABLED

#include <amtl/am-string.h>

#define BINLOG_MAGIC	0x414D424C
#define BINLOG_VERSION	0x0300

/**
 * Format of binlog:
 * uint32		magic
 * uint16		version
 * uint8		sizeof(time_t)
 * uint32		num plugins
 * [
 *  uint8		status codes
 *  str[int8]	filename
 *  if(status==2)
 *    uint32	num filenames
 *  uint32		num natives
 *  uint32		num publics
 *  if (status==2)
 *    [
 *     str[uint8] file name
 *    ]
 *  [
 *   str[uint8] native name
 *  ]
 *  [
 *   str[uint8] public name
 *  ]
 * ]
 * [
 *  uint8		operation code
 *  time_t		realtime
 *  float		gametime
 *  int32		plugin id
 *  <extra info>
 * ]
 * If filename id is 0 skip as plugin was not in debug mode, if -1 there was an error.
 */

enum BinLogOp
{
	BinLog_Start=1,
	BinLog_End,
	BinLog_NativeCall,	//<int32 native id> <int32_t num_params> <int32_t filename id>
	BinLog_NativeError, //<int32 errornum> <str[int16] string>
	BinLog_NativeRet,	//<cell value>
	BinLog_CallPubFunc,	//<int32 public id> <int32_t filename id>
	BinLog_SetLine,		//<int32 line no#> <int32_t filename id>
	BinLog_Registered,	//<string title> <string version>
	BinLog_FormatString, //<int32 param#> <int32 maxlen> <str[int16] string>
	BinLog_NativeParams, //<int32 num> <cell ...>
	BinLog_GetString,	//<cell addr> <string[int16]>
	BinLog_SetString,	//<cell addr> <int maxlen> <string[int16]>
};

class BinLog
{
public:
	BinLog() : m_state(false)
	{
	};
public:
	bool Open();
	void Close();
	void WriteOp(BinLogOp op, int plug, ...);
private:
	void WritePluginDB(FILE *fp);
private:
	ke::AString m_logfile;
	bool m_state;
};

extern BinLog g_BinLog;
extern int g_binlog_level;
extern int g_binlog_maxsize;

#endif //BINLOG_ENABLED

#endif //_INCLUDE_BINLOG_H
