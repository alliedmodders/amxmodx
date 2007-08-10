/* Vault functions
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

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
