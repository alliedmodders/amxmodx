// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Vault Functions
//

#if defined _vault_included
  #endinput
#endif
#define _vault_included

/* Reads a data from given key.
* If len is set to zero then get_vaultdata
* returns value as an number. */
native get_vaultdata(const key[], data[] = "", len = 0);

/* Sets a data under given key. */
native set_vaultdata(const key[], const data[] = "");

/* Removes a key from vault.*/
native remove_vaultdata(const key[]);

/* Checks if a key exists in the vault.*/
native vaultdata_exists(const key[]);
