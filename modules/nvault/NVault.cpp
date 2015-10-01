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

#include <stdio.h>
#include "amxxmodule.h"
#include "NVault.h"
#include "Binary.h"
#include <amtl/am-string.h>

/** 
 * :TODO: This beast calls strcpy()/new() way too much by creating new strings on the stack.
 *		  That's easily remedied and it should be fixed?
 *         ---bail
 */

NVault::NVault(const char *file)
{
	m_File = file;
	m_Journal = NULL;
	m_Open = false;

	FILE *fp = fopen(m_File.chars(), "rb");
	if (!fp)
	{
		fp = fopen(m_File.chars(), "wb");
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
	FILE *fp = fopen(m_File.chars(), "rb");

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

			if (!keylen || keylen > oldkeylen)
			{
				if (key)
					delete [] key;
				key = new char[keylen + 1];
				oldkeylen = keylen;
			}
			if (!vallen || vallen > oldvallen)
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

			ArrayInfo info; info.value = val; info.stamp = stamp;
			m_Hash.replace(key, info);
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
	FILE *fp = fopen(m_File.chars(), "wb");

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
	ke::AString key;
	ke::AString val;

	StringHashMap<ArrayInfo>::iterator iter = m_Hash.iter();

	if (!bw.WriteUInt32(magic)) goto fail;
	if (!bw.WriteUInt16(version)) goto fail;

	if (!bw.WriteUInt32( m_Hash.elements() )) goto fail;
	
	while (!iter.empty())
	{
		key = (*iter).key;
		val = (*iter).value.value;
		stamp = (*iter).value.stamp;
		
		if (!bw.WriteInt32(static_cast<int32_t>(stamp))) goto fail;;
		if (!bw.WriteUInt8( key.length() )) goto fail;
		if (!bw.WriteUInt16( val.length() )) goto fail;
		if (!bw.WriteChars( key.chars(), key.length() )) goto fail;
		if (!bw.WriteChars( val.chars(), val.length() )) goto fail;

		iter.next();
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
	StringHashMap<ArrayInfo>::Result r = m_Hash.find(key);
	if (!r.found())
	{
		return "";
	}

	return r->value.value.chars();
}

bool NVault::Open()
{
	_ReadFromFile();

	char *journal_name = new char[m_File.length() + 10];
	strcpy(journal_name, m_File.chars());

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

	if (m_Journal) 
	{
		m_Journal->End();
		m_Journal->Erase();
	}

	m_Open = false;

	return true;
}

void NVault::SetValue(const char *key, const char *val)
{
	if (m_Journal)
		m_Journal->Write_Insert(key, val, time(NULL));

	ArrayInfo info; info.value = val; info.stamp = time(NULL);
	m_Hash.replace(key, info);
}

void NVault::SetValue(const char *key, const char *val, time_t stamp)
{
	if (m_Journal)
		m_Journal->Write_Insert(key, val, stamp);

	ArrayInfo info; info.value = val; info.stamp = stamp;
	m_Hash.replace(key, info);
}

void NVault::Remove(const char *key)
{
	if (m_Journal)
		m_Journal->Write_Remove(key);

	m_Hash.remove(ke::AString(key).chars());
}

void NVault::Clear()
{
	if (m_Journal)
		m_Journal->Write_Clear();

	m_Hash.clear();
}

size_t NVault::Items()
{
	return m_Hash.elements();
}

size_t NVault::Prune(time_t start, time_t end)
{
	if (m_Journal)
		m_Journal->Write_Prune(start, end);

	size_t removed = 0;

	for (StringHashMap<ArrayInfo>::iterator iter = m_Hash.iter(); !iter.empty(); iter.next())
	{
		time_t stamp = iter->value.stamp;
		bool remove = false;

		if (stamp != 0)
		{
			if (start == 0 && end == 0)
				remove = true;
			else if (start == 0 && stamp < end)
				remove = true;
			else if (end == 0 && stamp > start)
				remove = true;
			else if (stamp > start && stamp < end)
				remove = true;

			if (remove)
			{
				iter.erase();
				removed++;
			}
		}
	}

	return removed;
}

void NVault::Touch(const char *key, time_t stamp)
{
	StringHashMap<ArrayInfo>::Insert i = m_Hash.findForAdd(key);
	if (!i.found())
	{
		if (!m_Hash.add(i, key))
		{
			return;
		}

		SetValue(key, "", time(NULL));
	}

	i->value.stamp = stamp;
}

bool NVault::GetValue(const char *key, time_t &stamp, char buffer[], size_t len)
{
	ArrayInfo result;

	if (!m_Hash.retrieve(key, &result))
	{
		buffer[0] = '\0';
		return false;
	}

	stamp = result.stamp;
	ke::SafeSprintf(buffer, len, "%s", result.value.chars());

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

