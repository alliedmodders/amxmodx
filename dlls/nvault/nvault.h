#ifndef _INCLUDE_NVAULT_H
#define _INCLUDE_NVAULT_H

#include "sdk/CString.h"
#include "hash.h"

class Journal
{
public:
	enum JournalOp
	{
		Journal_Store,
		Journal_Erase,
		Journal_Clear,
		Journal_Prune
	};
public:
	Journal(const char *file);
public:
	bool Replay(size_t &files, size_t &ops);
	void Clear();
public:
	void Begin(const char *name, JournalOp jop);
	void WriteByte(uint8_t num);
	void WriteInt(uint32_t num);
	void WriteTime(time_t n);
	void WriteString(const char *str);
	size_t End();
private:
	String m_File;
	FILE *m_Fp;
	size_t m_WriteSize;
};

class nVault
{
public:
	nVault(const char *name);
public:
	bool WriteToFile();
	bool ReadFromFile();
public:
	void Store(const char *key, const char *value, bool temporary=true);
	size_t Prune(time_t begin, time_t end, bool all=false);
	HashTable::htNode *Find(const char *key);
	bool KeyExists(const char *key);
	void Clear();
private:
	String m_File;
	HashTable *m_Vault;
};

#endif //_INCLUDE_NVAULT_H
