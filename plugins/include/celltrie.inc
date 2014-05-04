#if defined _celltrie_included
	#endinput
#endif
#define _celltrie_included

enum Trie
{
	Invalid_Trie = 0
};

enum Snapshot
{
	Invalid_Snapshot = 0
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
 * @return 			New Map handle, which must be freed via TrieDestroy().
 */
native Trie:TrieCreate();

/**
 * Clears all entries from a Map.
 *
 * @param handle	Map handle.
 *
 * @error			Invalid handle.
 */
native TrieClear(Trie:handle);

/**
 * Sets a value in a hash map, either inserting a new entry or replacing an old one.
 *
 * @param handle	Map handle.
 * @param key		Key string.
 * @param value		Value to store at this key.
 * @param replace	If false, operation will fail if the key is already set.
 *
 * @return			True on success, false on failure.
 * @error			Invalid handle.
 */
native TrieSetCell(Trie:handle, const key[], any:value, bool:replace = true);

/**
 * Sets a string value in a Map, either inserting a new entry or replacing an old one.
 *
 * @param handle	Map handle.
 * @param key		Key string.
 * @param value		String to store.
 * @param replace	If false, operation will fail if the key is already set.
 *
 * @return			True on success, false on failure.
 * @error			Invalid handle.
 */
native TrieSetString(Trie:handle, const key[], const value[], bool:replace = true);

/**
 * Sets an array value in a Map, either inserting a new entry or replacing an old one.
 *
 * @param handle	Map handle.
 * @param key		Key string.
 * @param buffer	Array to store.
 * @param size		Number of items in the array.
 * @param replace	If false, operation will fail if the key is already set.
 *
 * @return			True on success, false on failure.
 * @error			Invalid handle.
 *					Invalid array size.
 */
native TrieSetArray(Trie:handle, const key[], const any:buffer[], size, bool:replace = true);

/**
 * Retrieves a value in a Map.
 *
 * @param handle	Map handle.
 * @param key		Key string.
 * @param value		Variable to store value.
 * @return			True on success.  False if the key is not set, or the key is set
 *					as an array or string (not a value).
 * @error			Invalid handle.
 */
native bool:TrieGetCell(Trie:handle, const key[], &any:value);

/**
 * Retrieves a string in a Map.
 *
 * @param handle		Map handle.
 * @param key			Key string.
 * @param output		Buffer to store value.
 * @param outputsize	Maximum size of string buffer.
 * @param size			Optional parameter to store the number of bytes written to the buffer.
 *
 * @return				True on success.  False if the key is not set, or the key is set
 *						as a value or array (not a string).
 * @error				Invalid handle.
 *						Invalid buffer size.
 */
native bool:TrieGetString(Trie:handle, const key[], output[], outputsize, &size = 0);

/**
 * Retrieves an array in a Map.
 *
 * @param handle		Map handle.
 * @param key			Key string.
 * @param output		Buffer to store array.
 * @param outputsize	Maximum size of array buffer.
 * @param size			Optional parameter to store the number of elements written to the buffer.
 *
 * @return				True on success.  False if the key is not set, or the key is set
 *						as a value or string (not an array).
 * @error				Invalid handle.
 *						Invalid array size.
 */
native bool:TrieGetArray(Trie:handle, const key[], any:output[], outputsize, &size = 0);

/**
 * Removes a key entry from a Map.
 *
 * @param handle	Map handle.
 * @param key		Key string.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid handle.
 */
native bool:TrieDeleteKey(Trie:handle, const key[]);

/**
 * Checks a key entry existence from a Map.
 *
 * @param handle	Map handle.
 * @param key		Key string.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid handle.
 */
native bool:TrieKeyExists(Trie:handle, const key[]);

/**
 * Destroys a Map.
 *
 * @param handle	Map handle.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid handle.
 */
native TrieDestroy(&Trie:handle);

/**
 * Retrieves the number of elements in a map.
 *
 * @param handle	Map handle.
 *
 * @return			Number of elements in the trie.
 * @error			Invalid handle.
 */
native TrieGetSize(Trie:handle);

/**
 * Creates a snapshot of all keys in the map. If the map is changed after this
 * call, the changes are not reflected in the snapshot. Keys are not sorted.
 *
 * @param handle    Map handle.
 *
 * @return			New map snapshot handle, which must be freed via TrieSnapshotDestroy().
 * @error			Invalid handle.
 */
native Snapshot:TrieSnapshotCreate(Trie:handle);

/**
 * Returns the number of keys in a map snapshot. Note that this may be
 * different from the size of the map, since the map can change after the
 * snapshot of its keys was taken.
 *
 * @param handle	Map snapshot.
 *
 * @return			Number of keys.
 * @error			Invalid handle.
 */
native TrieSnapshotLength(Snapshot:handle);

/**
 * Returns the buffer size required to store a given key. That is, it returns
 * the length of the key plus one.
 *
 * @param handle	Map snapshot.
 * @param index		Key index (starting from 0).
 *
 * @return 			Buffer size required to store the key string.
 * @error			Invalid handle or index out of range.
 */
native TrieSnapshotKeyBufferSize(Snapshot:handle, index);

/**
 * Retrieves the key string of a given key in a map snapshot.
 *
 * @param handle	Map snapshot.
 * @param index		Key index (starting from 0).
 * @param buffer	String buffer.
 * @param maxlength	Maximum buffer length.
 *
 * @return			Number of bytes written to the buffer.
 * @error			Invalid handle or index out of range.
 */
native TrieSnapshotGetKey(Snapshot:handle, index, buffer[], maxlength);

/**
 * Destroys a Map snapshot
 *
 * @param handle	Map snapshot.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid handle.
 */
native TrieSnapshotDestroy(&Snapshot:handle);
