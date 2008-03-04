#ifndef _INCLUDE_JOURNAL_H
#define _INCLUDE_JOURNAL_H

#include "Binary.h"
#include "sh_list.h"
#include "sh_tinyhash.h"
#include "CString.h"

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

typedef THash<String,String> VaultMap;

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
	String m_File;
	FILE *m_fp;
	BinaryWriter m_Bw;
};

#endif //_INCLUDE_JOURNAL_H

