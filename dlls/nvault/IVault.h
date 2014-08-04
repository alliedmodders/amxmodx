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

#ifndef _INCLUDE_IVAULT_H
#define _INCLUDE_IVAULT_H

#include <time.h>

class IVault
{
public:
	virtual ~IVault() { }
public:
	virtual bool GetValue(const char *key, time_t &stamp, char buffer[], size_t len) =0;
	virtual void SetValue(const char *key, const char *val) =0;
	virtual void SetValue(const char *key, const char *val, time_t stamp) =0;
	virtual size_t Prune(time_t start, time_t end) =0;
	virtual void Clear() =0;
	virtual void Remove(const char *key) =0;
	virtual size_t Items() =0;
	virtual void Touch(const char *key, time_t stamp) =0;
};

class IVaultMngr
{
public:
	virtual ~IVaultMngr() { }
public:
	/**
	 * Note: Will return NULL if the vault failed to create.
	 */
	virtual IVault *OpenVault(const char *name) =0;
};

typedef IVaultMngr *(*GETVAULTMNGR_FUNC)();

#endif //_INCLUDE_IVAULT_H
