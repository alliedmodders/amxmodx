/* nVault
 *
 *  A binary (hash table) vault with journaling.
 *  (C)2005 David "BAILOPAN" Anderson
 */

#if defined _nvault_included
  #endinput
#endif
#define _nvault_included

/* Opens a vault and returns an id to it
 *  The vault name is appended with ".nvt" and stored in 
 *  datadir/vault.
 */
native vault_open(const name[]);

/* Reads from a vault.  
 *  No extra params - gets as an int
 *  One extra param - Float byref
 *  Two extra params - gets as a string
 */
native nvault_get(id, const name[], ...);

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