// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// NVault Functions
//

#if defined _nvault_included
	#endinput
#endif
#define _nvault_included

#pragma reqlib nvault
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib nvault
#endif

/**
 * @global All timestamps are in UNIX epoch form.
 */

/**
 * Opens a vault by name. Creates a vault if it doesn't exist yet.
 *
 * @param name          Name of the vault. The vault will be created in
 *                      ${amxx_datadir}/vault directory.
 *
 * @return              The vault handle to be used in other natives.
 *                      INVALID_HANDLE (-1) if not successfully opened.
 */
native nvault_open(const name[]);

/**
 * Retrieves a value from the given key.
 * 
 * @note An example of retrieving a string:
 *       nvault_get(vaultHandle, "myKey", myString, charsmax(myString));
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param key           A key to get the value from
 * @param ...           If three argument are given, gets a float value and
 *                      puts it in the third argument by reference.
 *                      If four arguments are given, gets a string from the
 *                      vault and copies it to the third argument, up to 
 *                      4th argument characters.
 *
 * @return              Result as integer if only the first two arguments
 *                      of the function are used.
 *                      1 if only the first three arguments are used.
 *                      String length if all four parameters are used.
 * @error               On invalid vault handle.
 */
native nvault_get(vault, const key[], any:...);

/**
 * Retrieves full information about a vault entry.
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param key           A key to get information from
 * @param value         A string where the value should be stored
 * @param maxlen        Maximum length of the @value string
 * @param timestamp     The timestamp of the entry
 *
 * @return              1 if an entry was found, 0 otherwise.
 * @error               On invalid vault handle.
 */
native nvault_lookup(vault, const key[], value[], maxlen, &timestamp);

/**
 * Sets value of a vault entry and updates the timestamp.
 *
 * @note A new entry is created if one with the given key doesn't exist.
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param key           A key to set the value for
 * @param value         A value to set
 *
 * @noreturn
 * @error               On invalid vault handle.
 */
native nvault_set(vault, const key[], const value[]);

/**
 * Sets value of a vault entry and makes it permanent (non-erasable with nvault_prune()).
 *
 * @note A new entry is created if one with the given key doesn't exist.
 * @note Permanent entries have no timestamp.
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param key           A key to set the permanent value for
 * @param value         A permanent value to set
 *
 * @noreturn
 * @error               On invalid vault handle.
 */
native nvault_pset(vault, const key[], const value[]);

/**
 * Prunes the vault for entries that are within the given timestamps.
 * 
 * @note This will not erase values set with nvault_pset().
 * @note An example of pruning all entries that are older than 24 hours:
 *       nvault_prune(vaultHandle, 0, get_systime() - (60 * 60 * 24));
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param start         The timestamp to start erasing from
 * @param end           The timestamp to erase to
 *
 * @return              Number of erased values.
 * @error               On invalid vault handle.
 */
native nvault_prune(vault, start, end);

/**
 * Closes a vault.
 *
 * @param vault         A vault handle returned from nvault_open()
 *
 * @noreturn
 * @error               On invalid vault handle.
 */
native nvault_close(vault);

/**
 * Removes an entry from the vault by its key.
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param key           The key to remove from the vault
 *
 * @noreturn
 * @error               On invalid vault handle.
 */
native nvault_remove(vault, const key[]);

/**
 * "Touches" an entry in the vault, updating its timestamp.
 * 
 * @note If timestamp is equal to -1, it will use the current time.
 * @note An empty entry is created if one with the given key doesn't exist.
 *
 * @param vault         A vault handle returned from nvault_open()
 * @param key           The key to search for
 * @param timestamp     Update an entry's timestamp to this one. Default is -1.
 *
 * @noreturn
 * @error               On invalid vault handle.
 */
native nvault_touch(vault, const key[], timestamp=-1);
