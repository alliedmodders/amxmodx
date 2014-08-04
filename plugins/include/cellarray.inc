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
 * These arrays are intended to be used for a form of global storage without 
 * requiring a #define that needs to be increased each time a person needs more 
 * storage.
 * These are not designed to be used as a replacement for normal arrays, as
 * normal arrays are faster and should be used whenever possible.
 */
 
enum Array
{
	Invalid_Array = 0
};

 /**
 * Given a maximum string size (including the null terminator), 
 * returns the number of cells required to fit that string.
 *
 * @param size          Number of bytes.
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
 * It is very important that the cellsize you provide matches up with the buffer sizes
 * that you pass with subsequent Array{Get,Set,Push} calls.
 *
 * @param cellsize      How many cells each entry in the array is.
 * @param reserved      How many blank entries are created immediately when the array is created.  
 *                      These entries are not valid to read from until called with ArraySet.
 *
 * @return              Handle to the array.
 */
native Array:ArrayCreate(cellsize = 1, reserved = 0);

/**
 * Clones an array, returning a new handle with the same size and data. 
 * You must close it.
 *
 * @param which         Array handle to be cloned.
 *
 * @return              New handle to the cloned array object on success, 0 on failure.
 * @error               Invalid handle.
 */
native Array:ArrayClone(Array:which);

/**
 * Clears all entries from the array.
 *
 * @param which         The array to clear.
 *
 * @noreturn
 * @error               Invalid handle.
 */
native ArrayClear(Array:which);

/**
 * Returns the number of elements in the array.
 *
 * @param which         The array to check.
 *
 * @return              How many elements are in the array.
 * @error               Invalid handle.
 */
native ArraySize(Array:which);

/**
 * Resizes an array.  If the size is smaller than the current size,
 * the array is truncated.
 *
 * @param which         Array handle.
 * @param newsize       New size.
 *
 * @noreturn
 * @error               Invalid handle or out of memory.
 */
native bool:ArrayResize(Array:which, newsize);

/**
 * Returns data within an array.  
 * Make sure the output buffer matches the size the array was created with!
 *
 * @param which         The array to retrieve the item from.
 * @param item          The item to retrieve (zero-based).
 * @param output        The output buffer to write.
 * @param size          If not set, assumes the buffer size is equal to the
 *                      cellsize.  Otherwise, the size passed is used.
 *
 * @return              Number of cells copied.
 * @error               Invalid handle or invalid index.
 */
native ArrayGetArray(Array:which, item, any:output[], size = -1);

/**
 * Returns a single cell of data from an array.  
 *
 * @param which         The array to retrieve the item from.
 * @param item          The item to retrieve (zero-based).
 * @param block         Optionally specify which block to read from
 *                      (useful if the blocksize > 0).
 * @param asChar        Optionally read as a byte instead of a cell.
 *
 * @return              Value read.
 * @error               Invalid handle, invalid index, or invalid block.
 */
native any:ArrayGetCell(Array:which, item, block = 0, bool:asChar = false);

/**
 * Returns a string value from an array.
 *
 * @param which         The array to retrieve the item from.
 * @param item          The item to retrieve (zero-based).
 * @param output        The variable to store the value in.
 * @param size          Character size of the output buffer.
 *
 * @return              Number of characters copied.
 * @error               Invalid handle or invalid index.
 */
native ArrayGetString(Array:which, item, output[], size);

/**
 * Sets an item's data with that of a local buffer.  
 * The buffer size must match what the cellsize that the array was created with!
 * The item must already exist, use ArrayPushArray to create a new item within the array.
 *
 * @param which         The array to set the item from within.
 * @param item          The item to set (zero-based).
 * @param input         The input buffer to store.
 * @param size          If not set, assumes the buffer size is equal to the
 *                      blocksize.  Otherwise, the size passed is used.
 *
 * @return              Number of cells copied.
 * @error               Invalid handle or invalid index.
 */
native ArraySetArray(Array:which, item, const any:input[], size =-1);

/**
 * Sets an array's single cell value.  Use this only on array that were created with a cellsize of 1!
 * The item must already exist, use ArrayPushCell to create a new item within the array.
 *
 * @param which         The array to set the item from within.
 * @param item          The item to set (zero-based).
 * @param input         The value to set.
 * @param block         Optionally specify which block to write to
 *                      (useful if the blocksize > 0).
 * @param asChar        Optionally set as a byte instead of a cell.
 *
 * @noreturn
 * @error               Invalid handle, invalid index, or invalid block.
 */
native ArraySetCell(Array:which, item, any:input, block = 0, bool:asChar = false);

/**
 * Sets a string value from an array.  
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 * The item must already exist, use ArrayPushString to create a new item within the array.
 *
 * @param which         The array to set the item from within.
 * @param item          The item to set (zero-based).
 * @param input         The string to set the item as.
 *
 * @return              Number of characters copied.
 * @error               Invalid handle or invalid index.
 */
native ArraySetString(Array:which, item, const input[]);

/**
 * Creates a new item at the end of the array and sets its data with that of a local buffer.  
 * The buffer size must match what the cellsize that the array was created with!
 *
 * @param which         The array to add the item to.
 * @param input         The input buffer to store.
 * @param size          If not set, the number of elements copied from the array
 *                      will be equal to the blocksize.  If set higher than the 
 *                      blocksize, the operation will be truncated.
 *
 * @return              Index of the new entry.
 * @error               Invalid handle or out of memory.
 */
native ArrayPushArray(Array:which, const any:input[], size = -1);

/**
 * Creates a new item and sets the array's single cell value.  
 * Use this only on array that were created with a cellsize of 1!
 *
 * @param which         The array to add the item to.
 * @param input         The value to set.
 *
 * @return              Index of the new entry.
 * @error               Invalid handle or out of memory.
 */
native ArrayPushCell(Array:which, any:input);

/**
 * Creates a new element in the array and sets its value to the input buffer.
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 *
 * @param which         The array to add the item to.
 * @param input         The string to set the item as.
 *
 * @return              Index of the new entry.
 * @error               Invalid handle or out of memory.
 */
native ArrayPushString(Array:which, const input[]);

/**
 * Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
 * The buffer size must match what the cellsize that the array was created with!
 *
 * @param which         The array to add the item to.
 * @param item          The item to insert after.
 * @param input         The input buffer to store.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayInsertArrayAfter(Array:which, item, const any:input[]);

/**
 * Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
 * Use this only on an array that was created with a cellsize of 1!
 *
 * @param which         The array to add the item to.
 * @param item          The item to insert after.
 * @param input         The value to set.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayInsertCellAfter(Array:which, item, any:input);

/**
 * Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 *
 * @param which         The array to add the item to.
 * @param item          The item to insert after.
 * @param input         The value to set.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayInsertStringAfter(Array:which, item, const input[]);

/**
 * Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
 * The buffer size must match what the cellsize that the array was created with!
 *
 * @param which         The array to add the item to.
 * @param item          The item to insert before.
 * @param input         The input buffer to store.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayInsertArrayBefore(Array:which, item, const any:input[]);

/**
 * Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
 * Use this only on an array that was created with a cellsize of 1!
 *
 * @param which         The array to add the item to.
 * @param item          The item to insert after.
 * @param input         The value to set.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayInsertCellBefore(Array:which, item, const any:input);

/**
 * Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 *
 * @param which         The array to add the item to.
 * @param item          The item to insert before.
 * @param input         The value to set.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayInsertStringBefore(Array:which, item, const input[]);

/**
 * Swaps the position of two items.
 *
 * @param which         The array that contains the items.
 * @param item1         The first item to swap.
 * @param item2         The second item to swap.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArraySwap(Array:which, item1, item2);

/**
 * Deletes an item from the array.  All items beyond it get shifted down 1 space.
 *
 * @param which         The array that contains the item to delete.
 * @param item          The item to delete.
 *
 * @noreturn
 * @error               Invalid handle or invalid index.
 */
native ArrayDeleteItem(Array:which, item);

/**
 * Returns the index for the first occurance of the provided string. If the string
 * cannot be located, -1 will be returned.
 *
 * @param which         Array handle.
 * @param item          String to search for.
 *
 * @return              Array index, or -1 on failure.
 * @error               Invalid handle.
 */
native ArrayFindString(Array:which, const item[]);
 
/**
 * Returns the index for the first occurance of the provided value. If the value
 * cannot be located, -1 will be returned.
 *
 * @param which         Array handle.
 * @param item          Value to search for.
 * 
 * @return              Array index, or -1 on failure.
 * @error               Invalid handle.
 */
native ArrayFindValue(Array:which, any:item); 

/**
 * Creates a handle that is passable to a format compliant routine for printing as a string (with the %a format option).  
 * It is suggested to pass the function directly as a parameter to the format routine.
 * The array contents must be a null-terminated string!
 *
 * An example usage:  client_print(id, print_chat, "%a", ArrayGetStringHandle(MessageArray, i));
 *
 * @param which         The array the string is stored in.
 * @param item          Which item to print the string value of.
 *
 * @return              Handle to the item directly.  Do not use or save stale handles.
 * @error               Invalid handle or invalid index.
 */
native DoNotUse:ArrayGetStringHandle(Array:which, item);

/**
 * Destroys the array, and resets the handle to 0 to prevent accidental usage after it is destroyed.
 *
 * @param which         The array to destroy.
 *
 * @noreturn
 * @error               Invalid handle.
 */
native ArrayDestroy(&Array:which);


/**
 * Similar to sorting.inc's CustomSort.
 * The sorting algorithm then uses your comparison function to sort the data.
 * The function is called in the following manner:
 * 
 * public MySortFunc(Array:array, item1, item2, const data[], data_size)
 *
 *  array           - Array handle in its current un-sorted state.
 *  item1, item2    - Current item pair being compared
 *  data[]          - Extra data array you passed to the sort func.
 *  data_size       - Size of extra data you passed to the sort func.
 *
 * Your function should return:
 *  -1 if item1 should go before item2
 *   0 if item1 and item2 are equal
 *   1 if item1 should go after item2
 *
 * Note that the parameters after item2 are all optional and you do not need to specify them.
 * Note that unlike the sorting.inc versions, the array passed to the callback is not in mid-sorted state.
 *
 * @param array         Array handle.
 * @param comparefunc   A callback function used for comparison.
 * @param data          Extra data array you passed to the sort func.
 * @param data_size     Size of extra data you passed to the sort func.
 *
 * @noreturn
 * @error               Invalid handle or invalid callback.
 */
native ArraySort(Array:array, const comparefunc[], data[]="", data_size=0);

/**
 * A faster version of ArraySort. 
 * The sorting algorithm then uses your comparison function to sort the data.
 *
 * The advantage of this native is that the Array elements being compared are 
 * directly passed into your function, instead of the item indexes that are passed by ArraySort. 
 * This removes the need for calling ArrayGet[Cell|String|Array] every time before being 
 * able to compare the elements.
 * 
 * For Arrays with a cellsize of 1 (used for storing integers and floats), 
 * the function is called in the following manner:
 * 
 * public MySortFunc(Array:array, elem1, elem2, const data[], data_size)
 *
 *  array           - Array handle in its current un-sorted state.
 *  elem1, elem2    - Current element pair being compared
 *  data[]          - Extra data array you passed to the sort func.
 *  data_size       - Size of extra data you passed to the sort func.
 *
 * For Arrays with a cellsize larger than 1 (used for storing arrays and strings),
 * the function is called in the following manner:
 *
 * public MySortFunc(Array:array, elem1[], elem2[], const data[], data_size)
 *
 *  array               - Array handle in its current un-sorted state.
 *  elem1[], elem2[]    - Current element pair being compared
 *  data[]              - Extra data array you passed to the sort func.
 *  data_size           - Size of extra data you passed to the sort func.
 *
 *
 * In both cases your function should return:
 *  -1 if elem1 should go before elem2
 *   0 if elem1 and elem2 are equal
 *   1 if elem1 should go after elem2
 *
 * Note that the parameters after elem2 are all optional and you do not need to specify them.
 * Note that unlike the sorting.inc versions, the array passed to the callback is not in mid-sorted state.
 *
 * @param array         Array handle.
 * @param comparefunc   A callback function used for comparison.
 * @param data          Extra data array you passed to the sort func.
 * @param data_size     Size of extra data you passed to the sort func.
 *
 * @noreturn
 * @error               Invalid handle, invalid callback or out of memory.
 */
native ArraySortEx(Array:array, const comparefunc[], data[]="", data_size=0);
