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

#ifndef _INCLUDE_NVAULT_H
#define _INCLUDE_NVAULT_H

#include <amtl/am-linkedlist.h>
#include <sm_stringhashmap.h>
#include "IVault.h"
#include "Journal.h"

#define VAULT_MAGIC		0x6E564C54			//nVLT
#define	VAULT_VERSION	0x0200				//second version

// File format:
// VAULT_MAGIC (int32)
// VAULT_VERSION (int16)
// ENTRIES (int32)
// [
//  stamp (int32)
//  keylen (int8)
//  vallen (int16)
//  key ([])
//  val ([])
//  ]

enum VaultError
{
	Vault_Ok=0,
	Vault_NoFile,
	Vault_BadFile,
	Vault_OldFile,
	Vault_Read,
};

class NVault : public IVault
{
public:
	NVault(const char *file);
	~NVault();
public:
	bool GetValue(const char *key, time_t &stamp, char buffer[], size_t len);
	const char *GetValue(const char *key);
	void SetValue(const char *key, const char *val);
	void SetValue(const char *key, const char *val, time_t stamp);
	void Touch(const char *key, time_t stamp);
	size_t Prune(time_t start, time_t end);
	void Clear();
	void Remove(const char *key);
	bool Open();
	bool Close();
	size_t Items();
	const char *GetFilename() { return m_File.chars(); }
private:
	VaultError _ReadFromFile();
	bool _SaveToFile();
private:
	ke::AString m_File;
	StringHashMap<ArrayInfo> m_Hash;
	Journal *m_Journal;
	bool m_Open;
	
	bool m_Valid;
	
public:
	bool isValid() { return m_Valid; }
};

class VaultMngr : public IVaultMngr
{
public:
	//when you delete it, it will be closed+saved automatically
	//when you open it, it will read the file automatically, as well as begin/restore journaling
	IVault *OpenVault(const char *file);
};

#endif //_INCLUDE_NVAULT_H
