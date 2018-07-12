// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _cellarray_included
	#endinput
#endif

#define _cellarray_included

/**
 * Cellarray tag declaration
 *
 * @note These dynamic arrays are intended to be used for a form of global
 *       storage without requiring a #define that needs to be increased each
 *       time the plugin author requires more storage. These are not designed
 *       to be a full replacement for normal arrays, as those are faster and
 *       should be used whenever possible.
 * @note Plugins are responsible for freeing all Array handles they acquire,
 *       including those from ArrayClone. Failing to free handles will result
 *       in the plugin and AMXX leaking memory.
 */
enum Array
{
	Invalid_Array = 0
};

/**
 * Returns the number of cells required to fit a string of the specified size
 * (including the null terminator).
 *
 * @param size          Number of bytes.
 *
 * @return              Minimum number of cells required to fit the byte count.
 */
stock ByteCountToCells(size)
{
	if (!size)
	{
		return 1;
	}

	return (size + 3) / 4;
}

/**
 * Creates a handle to a dynamically sized array.
 *
 * @note It is very important that the provided cellsize matches up with the
 *       buffer sizes that are passed with subsequent Array[Get|Set|Push] calls.
 * @note Initially the "reserved" parameter was intended to create blank entries
 *       that would immediately be usable with Array[Get|Set] functions. This
 *       functionality was never working as intended, and can now be achieved
 *       using ArrayResize().
 *
 * @param cellsize      Size of each array entry in cells
 * @param reserved      Pre-allocates space in the array for the specified
 *                      number of items. The items are not valid to read or set
 *                      until they have actually been pushed into the array.
 *
 * @return              New array handle, which must be freed via ArrayDestroy()
 * @error               If an invalid cellsize is provided an error will be
 * 	                    thrown.
 */
native Array:ArrayCreate(cellsize = 1, reserved = 32);

/**
 * Clones an array, returning a new handle with the same size and data.
 *
 * @param which         Array handle
 *
 * @return              Handle to the cloned array on success, 0 otherwise
 * @error               If an invalid handle is provided an error will be
 *                      thrown.
 */
native Array:ArrayClone(Array:which);

/**
 * Clears all entries from the array.
 *
 * @param which         Array handle
 *
 * @noreturn
 * @error               Invalid handle
 */
native ArrayClear(Array:which);

/**
 * Returns the number of elements in the array.
 *
 * @param which         Array handle
 *
 * @return              Number of elements in the array
 * @error               If an invalid handle is provided an error will be
 *                      thrown.
 */
native ArraySize(Array:which);

/**
 * Resizes an array.
 *
 * @note If the size is smaller than the current array size the array is
 *       truncated and data lost.
 *
 * @param which         Array handle
 * @param newsize       New size
 *
 * @noreturn
 * @error               If an invalid handle is provided or the resizing
 *                      operation runs out of memory, an error will be thrown.
 */
native bool:ArrayResize(Array:which, newsize);

/**
 * Retrieves an array of data from a cellarray.
 *
 * @note If the size parameter is specified as -1 the output buffer has to match
 *       the size the array was created with.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param output        Buffer to copy value to
 * @param size          If not set, assumes the buffer size is equal to the
 *                      cellsize. Otherwise, the specified size is used.
 *
 * @return              Number of cells copied
 * @error               If an invalid handle or index is provided an error will
 *                      be thrown.
 */
native ArrayGetArray(Array:which, item, any:output[], size = -1);

/**
 * Returns a single cell of data from an array
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param block         If the array has a cellsize >1 this optionally specifies
 *                      which block to read from
 * @param asChar        If true reads the value as a byte instead of a cell
 *
 * @return              Integer value
 * @error               If an invalid handle, index or block is provided an
 *                      error will be thrown.
 */
native any:ArrayGetCell(Array:which, item, block = 0, bool:asChar = false);

/**
 * Returieves string data from an array.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param output        Buffer to copy value to
 * @param size          Maximum size of the buffer
 *
 * @return              Number of characters copied
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayGetString(Array:which, item, output[], size);

/**
 * Fills an item's data with the contents of an array.
 *
 * @note If the size parameter is specified as -1 the input buffer has to match
 *       the size the array was created with.
 * @note The item index must already be valid. Use ArrayPushArray to create
 *       a new array item in the cellarray.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         Array to copy to the cellarray
 * @param size          If not set, assumes the buffer size is equal to the
 *                      cellsize. Otherwise, the specified size is used.
 *
 * @return              Number of cells copied
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArraySetArray(Array:which, item, const any:input[], size =-1);

/**
 * Sets an item's data to a single cell value.
 *
 * @note The item index must already be valid. Use ArrayPushArray to create
 *       a new array item in the cellarray.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         Value to set
 * @param block         If the array has a cellsize >1 this optionally specifies
 *                      which block to write to
 * @param asChar        If true writes the value as a byte instead of a cell
 *
 * @noreturn
 * @error               If an invalid handle, index or block is provided an
 *                      error will be thrown.
 */
native ArraySetCell(Array:which, item, any:input, block = 0, bool:asChar = false);

/**
 * Sets an item's data to a string value.
 *
 * @note The input will be truncated if it is longer than the cellsize the array
 *       was created with.
 * @note The item index must already be valid. Use ArrayPushString to create
 *       a new array item in the cellarray.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         String to copy to the array
 *
 * @return              Number of characters copied
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArraySetString(Array:which, item, const input[]);

/**
 * Creates a new item at the end of the cellarray and copies the provided array
 * into it.
 *
 * @note The input will be truncated if it is bigger than the cellsize the array
 *       was created with.
 *
 * @param which         Array handle
 * @param input         Array to copy to the cellarray
 * @param size          If not set, assumes the buffer size is equal to the
 *                      cellsize. Otherwise, the specified size is used.
 *
 * @return              Index of the new entry
 * @error               If an invalid handle is provided or the resizing
 *                      operation runs out of memory, an error will be thrown.
 */
native ArrayPushArray(Array:which, const any:input[], size = -1);

/**
 * Creates a new item at the end of the array and sets the item's single cell
 * value.
 *
 * @param which         Array handle
 * @param input         Value to set
 *
 * @return              Index of the new entry
 * @error               If an invalid handle is provided or the resizing
 *                      operation runs out of memory, an error will be thrown.
 */
native ArrayPushCell(Array:which, any:input);

/**
 * Creates a new item at the end of the array and copies the provided string
 * into it.
 *
 * @note The input will be truncated if it is longer than the cellsize the array
 *       was created with.
 *
 * @param which         Array handle
 * @param input         String to copy to the array
 *
 * @return              Index of the new entry
 * @error               If an invalid handle is provided or the resizing
 *                      operation runs out of memory, an error will be thrown.
 */
native ArrayPushString(Array:which, const input[]);

/**
 * Creates a new item behind the specified item and copies the provided array
 * into it. All items beyond it get shifted up by one.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         Array to copy to the cellarray
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayInsertArrayAfter(Array:which, item, const any:input[]);

/**
 * Creates a new item behind the specified item and sets the item's single cell
 * value. All items beyond it get shifted up by one.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         Value to set
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayInsertCellAfter(Array:which, item, any:input);

/**
 * Creates a new item behind the specified item and copies the provided string
 * into it. All items beyond it get shifted up by one.
 *
 * @note The input will be truncated if it is longer than the cellsize the array
 *       was created with.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         String to copy to the array
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayInsertStringAfter(Array:which, item, const input[]);

/**
 * Creates a new item in front of the specified item and copies the provided
 * array into it. All items beyond it get shifted up by one.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         Array to copy to the cellarray
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayInsertArrayBefore(Array:which, item, const any:input[]);

/**
 * Creates a new item in front of the specified item and sets the item's single
 * cell value. All items beyond it get shifted up by one.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         Value to set
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayInsertCellBefore(Array:which, item, const any:input);

/**
 * Creates a new item in front of the specified item and copies the provided
 * string into it. All items beyond it get shifted up by one.
 *
 * @note The input will be truncated if it is longer than the cellsize the array
 *       was created with.
 *
 * @param which         Array handle
 * @param item          Item index in the array
 * @param input         String to copy to the array
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayInsertStringBefore(Array:which, item, const input[]);

/**
 * Swaps the position of two items.
 *
 * @param which         Array handle
 * @param item1,item2   Item pair to swap
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArraySwap(Array:which, item1, item2);

/**
 * Deletes an item from the array. All items beyond it get shifted down by one.
 *
 * @param which         Array handle
 * @param item          Item to delete
 *
 * @noreturn
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native ArrayDeleteItem(Array:which, item);

/**
 * Searches through the array and returns the index of the first occurence of
 * the specified string.
 *
 * @param which         Array handle
 * @param item          String to search for
 *
 * @return              Array index on success, -1 if the string can't be found
 * @error               Invalid handle.
 */
native ArrayFindString(Array:which, const item[]);

/**
 * Searches through the array and returns the index of the first occurence of
 * the specified value.
 *
 * @param which         Array handle
 * @param item          Value to search for
 *
 * @return              Array index on success, -1 if the value can't be found
 * @error               If an invalid handle is provided an error will be
 *                      thrown.
 */
native ArrayFindValue(Array:which, any:item);

/**
 * Creates a special handle that can be passed to a string format routine for
 * printing as a string (with the %a format option).
 *
 * @note It is recommended to pass the function as a parameter to the format
 *       routine directly. The array item must contain a null-terminated string!
 * @note Do not save or otherwise use the handles returned by this function.
 * @note Example usage:
 *       console_print(id, "%a", ArrayGetStringHandle(MessageArray, i));
 *
 * @param which         Array handle
 * @param item          Item to retrieve handle of
 *
 * @return              Handle to the item
 * @error               If an invalid handle or an invalid index is provided an
 *                      error will be thrown.
 */
native DoNotUse:ArrayGetStringHandle(Array:which, item);

/**
 * Destroys the array and frees its memory.
 *
 * @note The function automatically sets the variable passed to it to 0 to aid
 *       in preventing accidental usage after destroy.
 *
 * @param which         Array to destroy
 *
 * @return              1 if the Array was destroyed, 0 if nothing had to be
 *                      destroyed (invalid handle)
 */
native ArrayDestroy(&Array:which);

/**
 * Similar to sorting.inc's CustomSort, the sorting algorithm then uses the
 * custom comparison function to sort the data.
 *
 * @note The function is called in the following manner:
 *
 * public MySortFunc(Array:array, item1, item2, const data[], data_size)
 *
 *   array           - Array handle in its current un-sorted state
 *   item1, item2    - Current item pair being compared
 *   data[]          - Extra data array passed to the sort func
 *   data_size       - Size of extra data
 *
 * @note The comparison function should return:
 *         -1 if item1 should go before item2
 *          0 if item1 and item2 are equal
 *          1 if item1 should go after item2
 *
 * @note All parameters after item2 are optional and do not need to be specified
 *       and used.
 * @note Unlike the sorting.inc version, the array passed to the callback is not
 *       in mid-sorted state.
 *
 * @param array         Array handle
 * @param comparefunc   Callback function used for comparison
 * @param data          Extra data that is passed through to the callback
 * @param data_size     Size of extra data
 *
 * @noreturn
 * @error               If an invalid handle or an invalid callback is provided
 *                      an error will be thrown.
 */
native ArraySort(Array:array, const comparefunc[], data[]="", data_size=0);

/**
 * A faster version of ArraySort, the sorting algorithm then uses the custom
 * comparison function to sort the data.
 *
 * @note The advantage of this function is that the data of the elements being
 *       compared is directly passed to the function, instead of the item
 *       indexes that are passed by ArraySort. This removes the need for calling
 *       ArrayGet[Cell|String|Array] every time before being able to compare the
 *       elements.
 *
 * @note For Arrays with a cellsize of 1 (used for storing integers and floats),
 *       the function is called in the following manner:
 *
 * public MySortFunc(Array:array, elem1, elem2, const data[], data_size)
 *
 *   array           - Array handle in its current un-sorted state
 *   elem1, elem2    - Current element pair being compared
 *   data[]          - Extra data array passed to the sort func
 *   data_size       - Size of extra data
 *
 * @note For Arrays with a cellsize larger than 1 (used for storing arrays and
 *       strings), the function is called in the following manner:
 *
 * public MySortFunc(Array:array, elem1[], elem2[], const data[], data_size)
 *
 *   array               - Array handle in its current un-sorted state
 *   elem1[], elem2[]    - Current element pair being compared
 *   data[]              - Extra data array passed to the sort func
 *   data_size           - Size of extra data
 *
 *
 * @note The comparison function should return:
 *         -1 if elem1 should go before elem2
 *          0 if elem1 and elem2 are equal
 *          1 if elem1 should go after elem2
 *
 * @note All parameters after item2 are optional and do not need to be specified
 *       and used.
 * @note Unlike the sorting.inc version, the array passed to the callback is not
 *       in mid-sorted state.
 *
 * @param array         Array handle
 * @param comparefunc   Callback function used for comparison
 * @param data          Extra data that is passed through to the callback
 * @param data_size     Size of extra data
 *
 * @noreturn
 * @error               If an invalid handle or an invalid callback is provided
 *                      an error will be thrown.
 */
native ArraySortEx(Array:array, const comparefunc[], data[]="", data_size=0);
