#include "nvault.h"

Vault::Vault(const char *name)
{
	m_File.assign(name);
	m_Vault = NULL;
}

Vault::~Vault()
{
	if (m_Vault)
	{
		delete m_Vault;
		m_Vault = NULL;
	}
}

const char *Vault::GetFileName()
{
	return m_File.c_str();
}

void Vault::Clear()
{
	if (m_Vault)
		m_Vault->Clear();
}

void Vault::EraseKey(const char *key)
{
	if (m_Vault)
		m_Vault->EraseKey(key);
}

HashTable::htNode *Vault::Find(const char *key)
{
	if (m_Vault)
		return m_Vault->Retrieve(key);

	return NULL;
}

bool Vault::KeyExists(const char *key)
{
	if (m_Vault)
		return m_Vault->KeyExists(key);

	return NULL;
}

size_t Vault::Prune(time_t begin, time_t end, bool all)
{
	if (m_Vault)
		return m_Vault->Prune(begin, end, all);

	return 0;
}

void Vault::Store(const char *key, const char *value, bool temporary)
{
	if (m_Vault)
		m_Vault->Store(key, value, temporary);
}

void Vault::Store(const char *key, const char *value, time_t stamp)
{
	if (m_Vault)
		m_Vault->Store(key, value, stamp);
}

#define wr(v,s) fwrite(&v, sizeof(s), 1, fp)

bool Vault::WriteToFile()
{
	FILE *fp = fopen(m_File.c_str(), "wb");

	if (!fp)
		return false;

	uint32_t hashes = m_Vault->UsedHashes();

	_WriteHeaders(fp, hashes);

	HashTable::htNode *node;
	uint32_t keys, stamp;
	uint16_t vChars;
	uint8_t kChars;
	for (uint32_t i=0; i<HT_SIZE; i++)
	{
		if (m_Vault->m_Table[i])
		{
			keys = 0;
			node = m_Vault->m_Table[i]->head;;
			while (node != NULL)
			{
				keys++;
				node = node->next;
			}
			wr(i, uint32_t);
			wr(keys, uint32_t);
			node = m_Vault->m_Table[i]->head;
			while (node != NULL)
			{
				stamp = (uint32_t)(node->stamp);
				wr(stamp, uint32_t);
				kChars = (uint8_t)(node->key.size());
				vChars = (uint16_t)(node->val.size());
				wr(kChars, uint8_t);
				wr(vChars, uint16_t);
				fwrite(node->key.c_str(), sizeof(char), kChars, fp);
				fwrite(node->val.c_str(), sizeof(char), vChars, fp);
				node = node->next;
			}
		}
	}

	fclose(fp);

	return true;
}

#define rd(v,s) if (fread(&v, sizeof(s), 1, fp) != 1) { \
	fclose(fp); \
	return Vault_ReadFail; }

Vault::VaultError Vault::ReadFromFile()
{
	FILE *fp = fopen(m_File.c_str(), "rb");

	if (!fp)
	{
		fp = fopen(m_File.c_str(), "wb");
		if (!fp)
			return Vault_ReadFail;
		_WriteHeaders(fp, 0);
		fclose(fp);
		m_Vault = new HashTable();
		return Vault_Ok;
	}

	uint32_t magic, keysize, hashes;
	uint8_t timesize;

	rd(magic, uint32_t);
	if (magic != VAULT_MAGIC)
	{
		fclose(fp);
		return Vault_BadMagic;
	}
	rd(timesize, uint8_t);
	rd(keysize, uint32_t);
	rd(hashes, uint32_t);
	
	m_Vault = new HashTable();

	uint32_t hash, keys, stamp;
	uint16_t vChars;
	uint8_t kChars;
	char *key, *value;
	for (uint32_t i=0; i<hashes; i++)
	{
		rd(hash, uint32_t);
		rd(keys, uint32_t);
		for (uint32_t d=0; d<keys; d++)
		{
			rd(stamp, uint32_t);
			rd(kChars, uint8_t);
			rd(vChars, uint16_t);
			key = new char[kChars+1];
			if (fread(key, sizeof(char), kChars, fp) != kChars)
			{
				delete [] key;
				fclose(fp);
				return Vault_ReadFail;
			}
			value = new char[vChars+1];
			if (fread(value, sizeof(char), vChars, fp) != vChars)
			{
				delete [] key;
				delete [] value;
				return Vault_ReadFail;
			}
			key[kChars] = '\0';
			value[vChars] = '\0';
			m_Vault->Store(key, value, (time_t)stamp);
			delete [] key;
			delete [] value;
		}
	}

	fclose(fp);

	return Vault_Ok;
}

///////////////////
// Private stuff //
///////////////////

void Vault::_WriteHeaders(FILE *fp, uint32_t keys)
{
	uint32_t magic = VAULT_MAGIC;
	uint32_t keysize = (1<<11);
	uint8_t timesize = sizeof(time_t);

	fwrite(&magic, sizeof(uint32_t), 1, fp);
	fwrite(&timesize, sizeof(uint8_t), 1, fp);
	fwrite(&keysize, sizeof(uint32_t), 1, fp);
	fwrite(&keys, sizeof(uint32_t), 1, fp);
}

