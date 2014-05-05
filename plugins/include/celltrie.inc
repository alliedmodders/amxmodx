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

enum TrieIter
{
	Invalid_Iter = 0
};

enum TrieIterStatus
{
	IterStatus_Valid = 0,
	IterStatus_Outdated = 1,
	IterStatus_Closed = 2,
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
 * @return			Number of elements in the map.
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

/**
 * Creates an iterator for a map. An iterator provides very fast, iterative read-only
 * access to a map.
 *
 * Any mutating change (that is key addition or key removal) to the underlying map
 * will invalidate every iterator operating on it.
 *
 * Updating alreadys existing keys will not invalidate an iterator.
 *
 * Iterators are designed to be short-lived and not be permanently stored or cached.
 * Creating new iterators is very fast, exhibiting virtually no overhead, even for
 * large tries. It is usually recommended to create them on-demand. Value retrieval
 * is as fast as directly reading from the underlying map.
 *
 * @return 				New iterator handle, which must be freed via TrieIterDestroy().
 * @error				Invalid Handle
 */
native TrieIter:TrieIterCreate(Trie:handle);

/**
 * Advances the iterator to the next key if one is available
 *
 * Providing the function with an optional buffer and buffer length will
 * store the next key inside that buffer.
 *
 * @param handle		Iterator handle.
 * @param key			Optional buffer to store the next key in.
 * @param outputsize	Maximum size of string buffer.
 * @param size			Optional parameter to store the number of bytes written to the buffer.
 *
 * @return 				True if the iterator is not empty and a new key was made current.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 *						Iterator is invalid
*/
native bool:TrieIterNext(TrieIter:handle, key[] = "", outputsize = 0, &size = 0);

/**
 * Checks if the iterator currently points to a key and is therefore not empty
 *
 * Providing the function with an optional buffer and buffer length will
 * store the current key inside that buffer.
 *
 * @param handle		Iterator handle.
 * @param key			Optional buffer to store the current key in.
 * @param outputsize	Maximum size of string buffer.
 * @param size			Optional parameter to store the number of bytes written to the buffer.
 *
 * @return 				True if the iterator is not empty  a new
 *						was made current.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 *						Iterator is invalid
 */
native bool:TrieIterGetKey(TrieIter:handle, key[] = "", outputsize = 0, &size = 0);

/**
 * Retrieves the current iterator status
 *
 * @param handle		Iterator handle.
 *
 * @return 				IterStatus_Valid 		if iterator can be used.
 *						IterStatus_Outdated		if iterator has to be refreshed because
 *												the underlying map has been modified.
 *						IterStatus_Closed		if underlying map has been closed. The iterator
 *												can't be refreshed and should be destroyed.
 * @error				Invalid handle
 */
native TrieIterStatus:TrieIterGetStatus(TrieIter:handle);

/**
 * Refreshes the iterator if possible
 *
 * This function serves a dual purpose when working with iterators:
 *
 * 1) If the underlying map was mutated and the iterator invalidated, calling this function
 * will allow the iterator to be revalidated without creating a new one.
 *
 * 2) After this function has returned the iterator will be starting over at its
 * initial position, even if it was valid when this function was called.
 *
 * @param handle		Iterator handle.
 *
 * @return 				True if the iterator has been refreshed.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 */
native bool:TrieIterRefresh(TrieIter:handle);

/**
 * Retrieves the number of elements in the underlying map.
 *
 * When used on a valid iterator this is exactly the same as calling TrieGetSize on the map directly.
 *
 * @param handle		Iterator handle.
 *
 * @return				Number of elements in the map.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 *						Iterator is invalid
 */
native TrieIterGetSize(TrieIter:handle);

/**
 * Retrieves a value at the current position of the iterator
 *
 * @param handle		Iterator handle.
 * @param value			Variable to store value.
 *
 * @return 				True on success, false if the iterator is empty.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 *						Iterator is invalid
 */
native bool:TrieIterGetCell(TrieIter:handle, &any:value);

/**
 * Retrieves a string at the current position of the iterator
 *
 * @param handle		Iterator handle.
 * @param buffer		Buffer to store the string..
 * @param outputsize	Maximum size of string buffer.
 * @param size			Optional parameter to store the number of bytes written to the buffer.
 *
 * @return 				True on success, false if the iterator is empty.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 *						Iterator is invalid
 */
native bool:TrieIterGetString(TrieIter:handle, buffer[], outputsize, &size = 0);

/**
 * Retrieves an array at the current position of the iterator
 *
 * @param handle		Iterator handle.
 * @param buffer		Buffer to store the array.
 * @param outputsize	Maximum size of buffer.
 * @param size			Optional parameter to store the number of bytes written to the buffer.
 *
 * @return 				True on success, false if the iterator is empty.
 * @error				Invalid handle
 *						Iterator has been closed (underlying map destroyed)
 *						Iterator is invalid
 */
native bool:TrieIterGetArray(TrieIter:handle, any:array[], outputsize, &size = 0);

/**
 * Destroys an iterator handle.
 *
 * @param handle	Iterator handle.
 *
 * @return			True on success, false if the value was never set.
 * @error			Invalid handle
 */
native TrieIterDestroy(&TrieIter:handle);
