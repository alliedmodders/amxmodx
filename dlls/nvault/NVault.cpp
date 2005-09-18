#include "NVault.h"
#include "Binary.h"
#include "CString.h"
#include "amxxmodule.h"

#ifdef __linux__
#define _snprintf snprintf
#endif

template <>
int HashFunction<String>(const String & k)
{
	unsigned long hash = 5381;
	const char *str = k.c_str();
	char c;
	while (c = *str++) hash = ((hash << 5) + hash) + c; // hash*33 + c
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
}

NVault::~NVault()
{
	Close();
}

VaultError NVault::_ReadFromFile()
{
	FILE *fp = fopen(m_File.c_str(), "rb");

	if (!fp)
		return Vault_NoFile;
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

	try
	{
		int32_t magic = br.ReadUInt32();
		if (magic != VAULT_MAGIC)
			return Vault_BadFile;
		int16_t version = br.ReadUInt16();
		if (version != VAULT_VERSION)
			return Vault_OldFile;
		int32_t entries = br.ReadUInt32();
		for (int32_t i=0; i<entries; i++)
		{
			stamp = static_cast<time_t>(br.ReadInt32());
			keylen = br.ReadUInt8();
			vallen = br.ReadUInt16();
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
			br.ReadChars(key, keylen);
			br.ReadChars(val, vallen);
			key[keylen] = '\0';
			val[vallen] = '\0';
			sKey.assign(key);
			sVal.assign(val);
			m_Hash.Insert(sKey, sVal, stamp);
		}
	} catch (...) {
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
	}

	fclose(fp);

	return Vault_Ok;
}

bool NVault::_SaveToFile()
{
	FILE *fp = fopen(m_File.c_str(), "wb");

	if (!fp)
		return false;

	BinaryWriter bw(fp);

	try
	{
		int32_t magic = VAULT_MAGIC;
		int16_t version = VAULT_VERSION;

		bw.WriteUInt32(magic);
		bw.WriteUInt16(version);

		bw.WriteUInt32( m_Hash.Size() );
		
		time_t stamp;
		String key;
		String val;
	
		THash<String,String>::iterator iter = m_Hash.begin();
		while (iter != m_Hash.end())
		{
			key = (*iter).key;
			val = (*iter).val;
			stamp = (*iter).stamp;
			bw.WriteInt32(stamp);
			bw.WriteUInt8( key.size() );
			bw.WriteUInt16( val.size() );
			bw.WriteChars( key.c_str(), key.size() );
			bw.WriteChars( val.c_str(), val.size() );
			iter++;
		}
	} catch (...) {
		fclose(fp);
		return false;
	}

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
	m_Journal->Begin();
	
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
	NVault *pVault = new NVault(file);
	
	return static_cast<IVault *>(pVault);
}

