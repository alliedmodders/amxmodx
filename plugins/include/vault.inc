/* Vault
*
* (c) 2002-2004, OLO
*  modified by BAILOPAN, Manip, PM, SniperBeamer
*
* This file is provided as is (no warranties).
*/

/* Reads a data from given key.
* If len is set to zero then get_vaultdata
* returns value as an number. */
native get_vaultdata(const key[], data[] = "", len = 0);

/* Sets a data under given key. */
native set_vaultdata(const key[], const data[] = "" );

/* Removes a key from vault.*/
native remove_vaultdata(const key[]);

/* Checks if a key exists in the vault.*/
native vaultdata_exists(const key[]);
