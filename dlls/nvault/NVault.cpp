#include <stdio.h>
#include "amxxmodule.h"
#include "NVault.h"
#include "Binary.h"
#include "CString.h"

#if defined(__linux__) || defined(__APPLE__)
#define _snprintf snprintf
#endif

/** 
 * :TODO: This beast calls strcpy()/new() way too much by creating new strings on the stack.
 *		  That's easily remedied and it should be fixed?
 *         ---bail
 */

template <>
int HashFunction<String>(const String & k)
{
	unsigned long hash = 5381;
	const char *str = k.c_str();
	char c;
	while ((c = *str++))
	{
		hash = ((hash << 5) + hash) + c; // hash*33 + c
	}
	return hash;
}

template <>
int Compare<String>(const String & k1, const String & k2)
{
	return strcmp(k1.c_str(),k2.c_str());
}

NVault::NVault(const char *file)
{
	m_File.assign(file);
	m_Journal = NULL;
	m_Open = false;

	FILE *fp = fopen(m_File.c_str(), "rb");
	if (!fp)
	{
		fp = fopen(m_File.c_str(), "wb");
		if (!fp)
		{
			this->m_Valid = false;
			return;
		}
	}

	this->m_Valid = true;
	fclose(fp);
}

NVault::~NVault()
{
	Close();
}

VaultError NVault::_ReadFromFile()
{
	FILE *fp = fopen(m_File.c_str(), "rb");

	if (!fp)
	{
		return Vault_NoFile;
	}
	//this is a little more optimized than the other version in the journal <_<
	//I could optimize this more by embedding the position in the hash table but...
	// the hash function can be changed.  this could be fixed by storing a string and its
	// resulting hash, and the entries could be ignored therein, but not right now.
	//Also the string class could have embedded binary reading (like it does for text files)
	BinaryReader br(fp);

	uint8_t oldkeylen=0;
	uint16_t oldvallen=0;
	uint8_t keylen;
	uint16_t vallen;
	time_t stamp;
	char *key = NULL;
	char *val = NULL;
	String sKey;
	String sVal;

//	try
//	{
		uint32_t magic;
		if (!br.ReadUInt32(magic)) goto fail;
			
		if (magic != VAULT_MAGIC)
			return Vault_BadFile;
			
			
		uint16_t version;
		if (!br.ReadUInt16(version)) goto fail;
		
		if (version != VAULT_VERSION)
			return Vault_OldFile;
			
		int32_t entries;

		if (!br.ReadInt32(entries)) goto fail;
		
		
		int32_t temp;
		for (int32_t i=0; i<entries; i++)
		{
			if (!br.ReadInt32(temp)) goto fail;
			
			stamp = static_cast<time_t>(temp);
			
			if (!br.ReadUInt8(keylen)) goto fail;
			if (!br.ReadUInt16(vallen)) goto fail;

			if (keylen > oldkeylen)
			{
				if (key)
					delete [] key;
				key = new char[keylen + 1];
				oldkeylen = keylen;
			}
			if (vallen > oldvallen)
			{
				if (val)
					delete [] val;
				val = new char[vallen + 1];
				oldvallen = vallen;
			}
			
			if (!br.ReadChars(key, keylen)) goto fail;
			if (!br.ReadChars(val, vallen)) goto fail;
			
			key[keylen] = '\0';
			val[vallen] = '\0';
			sKey.assign(key);
			sVal.assign(val);
			m_Hash.Insert(sKey, sVal, stamp);
		}

//	} catch (...) {

	goto success;
fail:
	if (key)
	{
		delete [] key;
		key = NULL;
	}
	if (val)
	{
		delete [] val;
		val = NULL;
	}
	fclose(fp);
	return Vault_Read;
//	}

success:
	fclose(fp);

	return Vault_Ok;
}

bool NVault::_SaveToFile()
{
	FILE *fp = fopen(m_File.c_str(), "wb");

	if (!fp)
	{
		return false;
	}

	BinaryWriter bw(fp);

//	try
//	{
	uint32_t magic = VAULT_MAGIC;
	uint16_t version = VAULT_VERSION;

	time_t stamp;
	String key;
	String val;

	THash<String,String>::iterator iter = m_Hash.begin();

	if (!bw.WriteUInt32(magic)) goto fail;
	if (!bw.WriteUInt16(version)) goto fail;

	if (!bw.WriteUInt32( m_Hash.Size() )) goto fail;
	
	while (iter != m_Hash.end())
	{
		key = (*iter).key;
		val = (*iter).val;
		stamp = (*iter).stamp;
		
		if (!bw.WriteInt32(static_cast<int32_t>(stamp))) goto fail;;
		if (!bw.WriteUInt8( key.size() )) goto fail;
		if (!bw.WriteUInt16( val.size() )) goto fail;
		if (!bw.WriteChars( key.c_str(), key.size() )) goto fail;
		if (!bw.WriteChars( val.c_str(), val.size() )) goto fail;
		iter++;
	}
		
	goto success;
//	} catch (...) {

fail:
	fclose(fp);
	return false;
//	}
success:

	fclose(fp);

	return true;
}

const char *NVault::GetValue(const char *key)
{
	String sKey(key);
	if (!m_Hash.Exists(sKey))
	{
		return "";
	} else {
		return m_Hash.Retrieve(sKey).c_str();
	}
}

bool NVault::Open()
{
	_ReadFromFile();

	char *journal_name = new char[m_File.size() + 10];
	strcpy(journal_name, m_File.c_str());

	char *pos = strstr(journal_name, ".vault");
	if (pos)
	{
		strcpy(pos, ".journal");
	} else {
		strcat(journal_name, ".journal");
	}

	m_Journal = new Journal(journal_name);
	delete [] journal_name;

	m_Journal->Replay(&m_Hash);
	m_Journal->Erase();
	if (!m_Journal->Begin())
	{
		delete m_Journal;
		m_Journal = NULL;
	}
	
	m_Open = true;

	return true;
}

bool NVault::Close()
{
	if (!m_Open)
		return false;

	_SaveToFile();
	m_Journal->End();
	m_Journal->Erase();
	m_Open = false;

	return true;
}

void NVault::SetValue(const char *key, const char *val)
{
	if (m_Journal)
		m_Journal->Write_Insert(key, val, time(NULL));
	String sKey;
	String sVal;
	sKey.assign(key);
	sVal.assign(val);
	m_Hash.Insert(sKey, sVal);
}

void NVault::SetValue(const char *key, const char *val, time_t stamp)
{
	if (m_Journal)
		m_Journal->Write_Insert(key, val, stamp);
	String sKey;
	String sVal;
	sKey.assign(key);
	sVal.assign(val);
	m_Hash.Insert(sKey, sVal, stamp);
}

void NVault::Remove(const char *key)
{
	if (m_Journal)
		m_Journal->Write_Remove(key);
	String sKey(key);
	m_Hash.Remove(sKey);
}

void NVault::Clear()
{
	if (m_Journal)
		m_Journal->Write_Clear();
	m_Hash.Clear();
}

size_t NVault::Items()
{
	return m_Hash.Size();
}

size_t NVault::Prune(time_t start, time_t end)
{
	if (m_Journal)
		m_Journal->Write_Prune(start, end);
	return m_Hash.Prune(start, end);
}

void NVault::Touch(const char *key, time_t stamp)
{
	String sKey(key);

	if (!m_Hash.Exists(sKey))
	{
		SetValue(key, "", time(NULL));
	}

	m_Hash.Touch(key, stamp);
}

bool NVault::GetValue(const char *key, time_t &stamp, char buffer[], size_t len)
{
	String sKey(key);
	if (!m_Hash.Exists(sKey))
	{
		buffer[0] = '\0';
		return false;
	}

	time_t st;
	String sVal;

	sVal = m_Hash.Retrieve(sKey, st);
	stamp = st;
	_snprintf(buffer, len, "%s", sVal.c_str());

	return true;
}

IVault *VaultMngr::OpenVault(const char *file)
{
	NVault *pVault;
	//try
	//{
	pVault = new NVault(file);
	
//	} catch (...) {
	if (!pVault->isValid())
	{
		delete pVault;
		pVault = NULL;
	}

	return static_cast<IVault *>(pVault);
}

