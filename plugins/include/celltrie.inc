// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _celltrie_included
	#endinput
#endif
#define _celltrie_included

/**
 * Hash map tag declaration
 *
 * @note The word "Trie" in this API is historical. As of AMX Mod X 1.8.3,
 *       tries have been internally replaced with hash tables, which have O(1)
 *       insertion time instead of O(n).
 * @note Plugins are responsible for freeing all Trie handles they acquire.
 *       Failing to free handles will result in the plugin and AMXX leaking
 *       memory.
 */
enum Trie
{
	Invalid_Trie = 0
};

/**
 * Hash map iterator tag declaration
 *
 * @note The word "Trie" in this API is historical. As of AMX Mod X 1.8.3,
 *       tries have been internally replaced with hash tables, which have O(1)
 *       insertion time instead of O(n).
 * @note Plugins are responsible for freeing all TrieIter handles they acquire.
 *       Failing to free handles will result in the plugin and AMXX leaking
 *       memory.
 */
enum TrieIter
{
	Invalid_TrieIter = 0
}

/**
 * Hash map snapshot tag declaration
 *
 * @note Plugins are responsible for freeing all Snapshot handles they acquire.
 *       Failing to free handles will result in the plugin and AMXX leaking
 *       memory.
 */
enum Snapshot
{
	Invalid_Snapshot = 0
};

/**
 * Creates a hash map. A hash map is a container that maps strings (called keys)
 * to arbitrary values (cells, arrays or strings).
 *
 * @note Keys in a hash map are unique so there is no more than one entry in the
 *       map for any given key.
 * @note Insertion, deletion, and lookup in a hash map are all considered to be
 *       fast operations, amortized to O(1), or constant time.
 *
 * @return  New Map handle, which must be freed via TrieDestroy()
 */
native Trie:TrieCreate();

/**
 * Clears all entries from a Map.
 *
 * @param handle    Map handle
 *
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 * @noreturn
 */
native TrieClear(Trie:handle);

/**
 * Sets a cell value in a hash map, either inserting a new entry or replacing
 * an old one.
 *
 * @param handle    Map handle
 * @param key       Key string
 * @param value     Value to store
 * @param replace   If false the operation will fail if the key is already set
 *
 * @return          1 on success, 0 otherwise
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native TrieSetCell(Trie:handle, const key[], any:value, bool:replace = true);

/**
 * Sets a string value in a hash map, either inserting a new entry or replacing
 * an old one.
 *
 * @param handle    Map handle
 * @param key       Key string
 * @param value     String to store
 * @param replace   If false the operation will fail if the key is already set
 *
 * @return          1 on success, 0 otherwise
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native TrieSetString(Trie:handle, const key[], const value[], bool:replace = true);

/**
 * Sets an array value in a hash map, either inserting a new entry or replacing
 * an old one.
 *
 * @param handle    Map handle
 * @param key       Key string
 * @param buffer    Array to store
 * @param size      Array size
 * @param replace   If false the operation will fail if the key is already set
 *
 * @return          1 on success, 0 otherwise
 * @error           If an invalid handle is provided an error will be
 *                  thrown. or invalid array size
 */
native TrieSetArray(Trie:handle, const key[], const any:buffer[], size, bool:replace = true);

/**
 * Retrieves a cell value from a hash map.
 *
 * @param handle    Map handle
 * @param key       Key string
 * @param value     Variable to store value to
 *
 * @return          True on success, false if either the key is not set or the
 *                  value type does not match (value is string or array)
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native bool:TrieGetCell(Trie:handle, const key[], &any:value);

/**
 * Retrieves a string from a hash map.
 *
 * @param handle        Map handle
 * @param key           Key string
 * @param output        Buffer to copy the value to
 * @param outputsize    Maximum size of buffer
 * @param size          Optional variable to store the number of cells written
 *                      to the buffer in
 *
 * @return              True on success, false if either the key is not set or
 *                      the value type does not match (value is cell or array)
 * @error               If an invalid handle is provided an error will be
 *                      thrown.
 */
native bool:TrieGetString(Trie:handle, const key[], output[], outputsize, &size = 0);

/**
 * Retrieves a string from a hash map.
 *
 * @param handle        Map handle
 * @param key           Key string
 * @param output        Array to copy the value to
 * @param outputsize    Maximum size of array
 * @param size          Optional variable to store the number of cells written
 *                      to the array in
 *
 * @return              True on success, false if either the key is not set or
 *                      the value type does not match (value is cell or string)
 * @error               If an invalid handle or array size is provided an error
 *                      will be thrown.
 */
native bool:TrieGetArray(Trie:handle, const key[], any:output[], outputsize, &size = 0);

/**
 * Removes an entry from a hash map.
 *
 * @param handle    Map handle
 * @param key       Key string
 *
 * @return          True on success, false if the key was never set
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native bool:TrieDeleteKey(Trie:handle, const key[]);

/**
 * Checks a hash map for the existence of an entry.
 *
 * @param handle    Map handle
 * @param key       Key string
 *
 * @return          True if the key is set, false otherwise
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native bool:TrieKeyExists(Trie:handle, const key[]);

/**
 * Destroys a hash map and frees its memory.
 *
 * @note The function automatically sets the variable passed to it to 0 to aid
 *       in preventing accidental usage after destroy.
 *
 * @param handle    Map handle
 *
 * @return          1 on success, 0 if an invalid handle was passed in
 */
native TrieDestroy(&Trie:handle);

/**
 * Returns the number of entries in a hash map.
 *
 * @param handle    Map handle
 *
 * @return          Number of elements in the hash map
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native TrieGetSize(Trie:handle);

/**
 * Creates a snapshot of all keys in a hash map. If the map is changed
 * afterwards,  the changes are not reflected in the snapshot.
 * Keys are not sorted.
 *
 * @param handle    Map handle
 *
 * @return          New map snapshot handle, which must be freed via
 *                  TrieSnapshotDestroy()
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native Snapshot:TrieSnapshotCreate(Trie:handle);

/**
 * Returns the number of keys in a map snapshot. Note that this may be
 * different from the size of the map, since the map can change after the
 * snapshot of its keys was taken.
 *
 * @param handle    Map snapshot handle
 *
 * @return          Number of keys
 * @error           If an invalid handle is provided an error will be
 *                  thrown.
 */
native TrieSnapshotLength(Snapshot:handle);

/**
 * Returns the buffer size required to store a given key. That is, it returns
 * the length of the key plus one.
 *
 * @param handle    Map snapshot handle
 * @param index     Key index (starting from 0)
 *
 * @return          Buffer size required to store the key string
 * @error           If an invalid handle is provided an error will be
 *                  thrown. or index out of range
 */
native TrieSnapshotKeyBufferSize(Snapshot:handle, index);

/**
 * Retrieves the key string of a given key in a map snapshot.
 *
 * @param handle    Map snapshot handle
 * @param index     Key index (starting from 0)
 * @param buffer    String buffer
 * @param maxlength Maximum buffer length
 *
 * @return          Number of bytes written to the buffer
 * @error           If an invalid handle is provided an error will be
 *                  thrown. or index out of range
 */
native TrieSnapshotGetKey(Snapshot:handle, index, buffer[], maxlength);

/**
 * Destroys a map snapshot and frees its memory.
 *
 * @note The function automatically sets the variable passed to it to 0 to aid
 *       in preventing accidental usage after destroy.
 *
 * @param handle    Map snapshot handle
 *
 * @return          1 on success, 0 if an invalid handle was passed in
 */
native TrieSnapshotDestroy(&Snapshot:handle);


/**
 * Creates an iterator for a map. It provides iterative read-only access to the
 * maps contents.
 *
 * @note Removing or adding keys to the underlying map will invalidate all its
 *       iterators. Updating values of existing keys is allowed and the changes 
 *       will be immediately reflected in the iterator.
 * @note Iterators are designed to be short-lived and not stored, and creating
 *       them is very cheap. Reading data from an iterator is just as fast as 
 *       reading directly from the map.
 * @note Just like in snapshots the keys are not sorted.
 *
 * @return              New iterator handle, which must be freed via TrieIterDestroy().
 * @error               Invalid Handle
 */
native TrieIter:TrieIterCreate(Trie:handle);

/**
 * Returns if the iterator has reached its end and no more data can be read.
 *
 * @param handle        Iterator handle
 *
 * @return              True if iterator has reached the end, false otherwise
 * @error               Invalid Handle
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native bool:TrieIterEnded(TrieIter:handle);

/**
 * Advances the iterator to the next key/value pair if one is available.
 *
 * @param handle        Iterator handle
 *
 * @error               Invalid Handle
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native TrieIterNext(TrieIter:handle);

/**
 * Retrieves the key the iterator currently points to.
 *
 * @param handle        Iterator handle.
 * @param key           Buffer to store the current key in.
 * @param outputsize    Maximum size of string buffer.
 *
 * @return              Nnumber of bytes written to the buffer
 * @error               Invalid handle
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native TrieIterGetKey(TrieIter:handle, key[], outputsize);

/**
 * Retrieves the number of elements in the underlying map.
 *
 * @note When used on a valid iterator this is exactly the same as calling TrieGetSize on the map directly.
 *
 * @param handle        Iterator handle
 *
 * @return              Number of elements in the map
 * @error               Invalid handle
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native TrieIterGetSize(TrieIter:handle);

/**
 * Retrieves a value at the current position of the iterator.
 *
 * @param handle        Iterator handle
 * @param value         Variable to store value in
 *
 * @return              True on success, false if the iterator is empty or the current
 *                      value is an array or a string.
 * @error               Invalid handle
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native bool:TrieIterGetCell(TrieIter:handle, &any:value);

/**
 * Retrieves a string at the current position of the iterator.
 *
 * @param handle        Iterator handle
 * @param buffer        Buffer to store the string in
 * @param outputsize    Maximum size of string buffer
 * @param size          Optional parameter to store the number of bytes written to the buffer.
 *
 * @return              True on success, false if the iterator is empty or the current value
 *                      is not a string.
 * @error               Invalid handle
 *                      Invalid buffer size
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native bool:TrieIterGetString(TrieIter:handle, buffer[], outputsize, &size = 0);

/**
 * Retrieves an array at the current position of the iterator.
 *
 * @param handle        Iterator handle
 * @param buffer        Buffer to store the array
 * @param outputsize    Maximum size of buffer
 * @param size          Optional parameter to store the number of bytes written to the buffer
 *
 * @return              True on success, false if the iterator is empty or the current
 *                      value is not an array.
 * @error               Invalid handle
 *                      Invalid buffer size
 *                      Iterator has been closed (underlying map destroyed)
 *                      Iterator is outdated
 */
native bool:TrieIterGetArray(TrieIter:handle, any:array[], outputsize, &size = 0);

/**
 * Destroys an iterator handle.
 *
 * @param handle    Iterator handle.
 *
 * @return          True on success, false if the value was never set.
 */
native TrieIterDestroy(&TrieIter:handle);
