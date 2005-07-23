#ifndef _INCLUDE_JOURNAL_H
#define _INCLUDE_JOURNAL_H

#include "nvault.h"

/**
 * (C)2005 David "BAILOPAN" Anderson
 * Licensed under the GNU General Public License, version 2
 * 
 * Adds Journaling capabilities for an nVault
 */

#define JOURNAL_MAGIC	0x6E564A4C

class Journal
{
public:
	enum JournalOp
	{
		Journal_Nop,		//Nothing
		Journal_Name,		//Maps name to Id (id,len,[])
		Journal_Store,		//Stores key/val (id,time,klen,vlen,[],[])
		Journal_Erase,		//Erases key (id,klen,[])
		Journal_Clear,		//Clears (id)
		Journal_Prune		//Prunes (id,t1,t2,all)
	};
public:
	Journal(const char *file);
public:
	bool Replay(size_t &files, size_t &ops);
	void ClearJournal();
	bool StartJournal();
	void EndJournal();
public:
	void Store(const char *name, const char *key, const char *val, time_t stamp);
	void Erase(const char *name, const char *key);
	void Clear(const char *name);
	void Prune(const char  *name, time_t begin, time_t end, bool all);
private:
	void BeginOp(const char *name, JournalOp jop);
	void WriteByte(uint8_t num);
	void WriteShort(uint16_t num);
	void WriteInt(uint32_t num);
	void WriteString(const char *str);
	size_t EndOp();
private:
	String m_File;
	FILE *m_Fp;
	HashTable m_Names;
	uint32_t m_LastId;
};

#endif //_INCLUDE_JOURNAL_H
