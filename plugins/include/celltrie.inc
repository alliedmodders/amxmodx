#if defined _celltrie_included
	#endinput
#endif
#define _celltrie_included

enum Trie
{
	Invalid_Trie = 0
};

/**
 * Creates a hash map. A hash map is a container that can map strings (called
 * "keys") to arbitrary values (cells, arrays, or strings). Keys in a hash map
 * are unique. That is, there is at most one entry in the map for a given key.
 *
 * Insertion, deletion, and lookup in a hash map are all considered to be fast
 * operations, amortized to O(1), or constant time.
 *
 * The word "Trie" in this API is historical. As of AMX Mod X 1.8.3, tries have
 * been internally replaced with hash tables, which have O(1) insertion time
 * instead of O(n).
 *
 * @return 			New Map Handle, which must be freed via TrieDestroy().
 */
native Trie:TrieCreate();

/**
 * Clears all entries from a Map.
 *
 * @param handle	Map Handle.
 *
 * @error			Invalid Handle.
 */
native TrieClear(Trie:handle);

/**
 * Sets a value in a hash map, either inserting a new entry or replacing an old one.
 *
 * @param handle	Map Handle.
 * @param key		Key string.
 * @param value		Value to store at this key.
 * @param replace	If false, operation will fail if the key is already set.
 *
 * @return			True on success, false on failure.
 * @error			Invalid Handle.
 */
native TrieSetCell(Trie:handle, const key[], any:value, bool:replace = true);

/**
 * Sets a string value in a Map, either inserting a new entry or replacing an old one.
 *
 * @param handle	Map Handle.
 * @param key		Key string.
 * @param value		String to store.
 * @param replace	If false, operation will fail if the key is already set.
 *
 * @return			True on success, false on failure.
 * @error			Invalid Handle.
 */
native TrieSetString(Trie:handle, const key[], const value[], bool:replace = true);

/**
 * Sets an array value in a Map, either inserting a new entry or replacing an old one.
 *
 * @param handle	Map Handle.
 * @param key		Key string.
 * @param buffer	Array to store.
 * @param size		Number of items in the array.
 * @param replace	If false, operation will fail if the key is already set.
 *
 * @return			True on success, false on failure.
 * @error			Invalid Handle.
 */
native TrieSetArray(Trie:handle, const key[], const any:buffer[], size, bool:replace = true);

/**
 * Retrieves a value in a Map.
 *
 * @param handle	Map Handle.
 * @param key		Key string.
 * @param value		Variable to store value.
 * @return			True on success.  False if the key is not set, or the key is set 
 *					as an array or string (not a value).
 * @error			Invalid Handle.
 */
native bool:TrieGetCell(Trie:handle, const key[], &any:value);

/**
 * Retrieves a string in a Map.
 *
 * @param handle		Map Handle.
 * @param key			Key string.
 * @param output		Buffer to store value.
 * @param outputsize	Maximum size of string buffer.
 * @param size			Optional parameter to store the number of bytes written to the buffer.
 *
 * @return				True on success.  False if the key is not set, or the key is set 
 *						as a value or array (not a string).
 * @error				Invalid Handle.
 */
native bool:TrieGetString(Trie:handle, const key[], output[], outputsize, &size = 0);

/**
 * Retrieves an array in a Map.
 *
 * @param handle		Map Handle.
 * @param key			Key string.
 * @param output		Buffer to store array.
 * @param outputsize	Maximum size of array buffer.
 * @param size			Optional parameter to store the number of elements written to the buffer.
 *
 * @return				True on success.  False if the key is not set, or the key is set 
 *						as a value or string (not an array).
 * @error				Invalid Handle.
 */
native bool:TrieGetArray(Trie:handle, const key[], any:output[], outputsize, &size = 0);

/**
 * Removes a key entry from a Map.
 *
 * @param handle	Map Handle.
 * @param key		Key string.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid Handle.
 */
native bool:TrieDeleteKey(Trie:handle, const key[]);

/**
 * Checks a key entry existence from a Map.
 *
 * @param handle	Map Handle.
 * @param key		Key string.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid Handle.
 */
native bool:TrieKeyExists(Trie:handle, const key[]);

/**
 * Destroys a Map.
 *
 * @param handle	Map Handle.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid Handle.
 */
native TrieDestroy(&Trie:handle);

/**
 * Retrieves the number of elements in a map.
 *
 * @param handle	Map Handle.
 *
 * @return			Number of elements in the trie.
 * @error			Invalid Handle.
 */
native TrieGetSize(Trie:handle);
