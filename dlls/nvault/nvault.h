#ifndef _INCLUDE_NVAULT_H
#define _INCLUDE_NVAULT_H

#include "sdk/CString.h"
#include "hash.h"

/**
 * Vault file format:
 *  Headers
 *   uint32_t - nVLT
 *   uint8_t  - sizeof(time_t)
 *   uint32_t - key size (will be used in future maybe)
 *   uint32_t - number of hashes stored
 *  Data
 *   uint32_t - key hash
 *   uint32_t - # of keys in this hash
 *   Data
 *    uint32_t - Time
 *    uint8_t  - Characters in key
 *    uint16_t - Characters in value
 *    char[]   - Key
 *    char[]   - Value
 */

#define VAULT_MAGIC		0x6E564C54

class Vault
{
public:
	Vault(const char *name);
	~Vault();
	enum VaultError
	{
		Vault_Ok=0,
		Vault_ReadFail,
		Vault_BadMagic,
	};
public:
	bool WriteToFile();
	VaultError ReadFromFile();
public:
	void Store(const char *key, const char *value, bool temporary=true);
	void Store(const char *key, const char *value, time_t stamp);
	size_t Prune(time_t begin, time_t end, bool all=false);
	HashTable::htNode *Find(const char *key);
	bool KeyExists(const char *key);
	void Clear();
	void EraseKey(const char *key);
	const char  *GetFileName();
private:
	void _WriteHeaders(FILE *fp, uint32_t hashes);
private:
	String m_File;
	HashTable *m_Vault;
};

#endif //_INCLUDE_NVAULT_H
