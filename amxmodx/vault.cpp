// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "CVault.h"

Vault g_vault;

static cell AMX_NATIVE_CALL set_vaultdata(AMX *amx, cell *params)
{
	int iLen;

	g_vault.put(get_amxstring(amx, params[1], 0, iLen), get_amxstring(amx, params[2], 1, iLen));
	g_vault.saveVault();

	return 1;
}

static cell AMX_NATIVE_CALL get_vaultdata(AMX *amx, cell *params)
{
	int iLen;
	const char* key = get_amxstring(amx, params[1], 0, iLen);

	if (params[3])
		return set_amxstring(amx, params[2], g_vault.get(key), params[3]);

	return g_vault.get_number(key);
}

static cell AMX_NATIVE_CALL remove_vaultdata(AMX *amx, cell *params)
{
	int iLen;

	g_vault.remove(get_amxstring(amx, params[1], 0, iLen));
	g_vault.saveVault();

	return 1;
}

static cell AMX_NATIVE_CALL vaultdata_exists(AMX *amx, cell *params)
{
	int iLen;
	return g_vault.exists(get_amxstring(amx, params[1], 0, iLen)) ? 1 : 0;
}

AMX_NATIVE_INFO vault_Natives[] =
{
	{"set_vaultdata",		set_vaultdata},
	{"get_vaultdata",		get_vaultdata},
	{"remove_vaultdata",	remove_vaultdata},
	{"delete_vaultdata",	remove_vaultdata},
	{"vaultdata_exists",	vaultdata_exists},
	{0,						0}
};
