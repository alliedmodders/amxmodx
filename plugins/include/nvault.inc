/* nVault functions
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _nvault_included
  #endinput
#endif
#define _nvault_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib nvault
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib nvault
 #endif
#else
 #pragma library nvault
#endif

/* All timestamps are in UNIX epoch form. */

/* Opens a vault by name (such as "myvault")
 * Returns a vault id, INVALID_HANDLE otherwise (-1)
 */
native nvault_open(const name[]);

/* Gets a vault value by returning an int
 * setting a byref float or setting a string + maxlength
 */
native nvault_get(vault, const key[], {Float,_}:...);

/* Looks up a vault value for full information
 * Returns 0 if the entry is not found
 */
native nvault_lookup(vault, const key[], value[], maxlen, &timestamp);

/* Sets a vault value (with current timestamp) */
native nvault_set(vault, const key[], const value[]);

/* Sets a permanent vault value (no timestamp) */
native nvault_pset(vault, const key[], const value[]);

/* Prunes the vault for entries that are within the given timestamps.
 * This will not erase values set with pset
 */
native nvault_prune(vault, start, end);

/* Closes a vault */
native nvault_close(vault);

/* Removes a key from the vault */
native nvault_remove(vault, const key[]);

/* "Touches" a key to update its timestamp value.  
 * If stamp is -1 (default), it will use the current time.
 * Like the unix command "touch," it will create an empty key
 *  if the value does not exist.
 */
native nvault_touch(vault, const key[], timestamp=-1);
