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

 /**
 * Reads data from a given key.
 *
 *
 * @param key           Key to get the value from
 * @param data          Buffer to copy the value to
 * @param len           Buffer size. If len is set to 0 then the function will 
 * 			        	 return the value as a number
 *
 * @return              If len is not 0 the function will return the number of characters written.
					 If len is 0 the function will return the numerical value associated with the key.	
 */
native get_vaultdata(const key[], data[] = "", len = 0);


/**
 * Sets data for a given key.
 *
 * @param key           Key to set the value for
 * @param data          Data to set for the given key
 *
 * @noreturn
 */
native set_vaultdata(const key[], const data[] = "");

/**
 * Removes a key and its data from the vault.
 *
 * @param key           Key to remove
 *
 * @noreturn
 */
native remove_vaultdata(const key[]);

/**
 * Checks if a key exists in the vault.
 *
 * @param key           Key to check
 *
 * @return              1 if an entry was found, 0 otherwise.
 */
native vaultdata_exists(const key[]);
