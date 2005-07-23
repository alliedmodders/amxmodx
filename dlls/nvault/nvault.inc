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
native nvault_get(id, const key[], ...);

/* Reads from a vault, includes time
 *  Parameter format same as nvault_get()
 */
native nvault_timeget(id, const key[], &time, ...);

/* Sets a key/value pair
 *  valFmt is formatted according to format()
 */
native nvault_set(id, const key[], const valFmt[], {Float,_}:...);

/* Sets a permanent key/value pair
 *  A permanent key isn't timestamped, so it can be invulnerable to pruning.
 */
native nvault_pset(id, const key[], const valFmt[], {Float,_}:...);


/* Sets a data under given key. */
native set_vaultdata(const key[], const data[] = "" );

/* Removes a key from vault.*/
native remove_vaultdata(const key[]);

/* Checks if a key exists in the vault.*/
native vaultdata_exists(const key[]);