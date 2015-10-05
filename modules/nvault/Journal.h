// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// NVault Module
//

#ifndef _INCLUDE_JOURNAL_H
#define _INCLUDE_JOURNAL_H

#include "Binary.h"
#include <amtl/am-linkedlist.h>
#include <sm_stringhashmap.h>
#include <amtl/am-string.h>

enum JOp
{
	Journal_Nop=0,		//no operation
	Journal_Clear,		//clears, no parameters
	Journal_Prune,		//prunes, two params (start, end, 32bit both)
	Journal_Insert,		//inserts stamp (32), key (8+[]), val (16+[])
	Journal_Remove,		//removes key(8+[])
	Journal_TotalOps,
};

enum Encode
{
	Encode_Small,
	Encode_Medium,
};

struct ArrayInfo
{
	ke::AString value;
	time_t stamp;
};

typedef StringHashMap<ArrayInfo> VaultMap;

class Journal
{
public:
	Journal(const char *file);
public:
	bool Begin();
	bool End();
	int Replay(VaultMap *pMap);
	bool Erase();
public:
	bool Write_Clear();
	bool Write_Prune(time_t start, time_t end);
	bool Write_Insert(const char *key, const char *val, time_t stamp);
	bool Write_Remove(const char *key);
private:
	bool WriteOp(JOp op);
	bool WriteInt32(int num);
	bool WriteString(const char *str, Encode enc);
private:
	ke::AString m_File;
	FILE *m_fp;
	BinaryWriter m_Bw;
};

#endif //_INCLUDE_JOURNAL_H

