#if defined _cellarray_included
	#endinput
#endif

#define _cellarray_included

enum Array
{
	Invalid_Array = 0
};
/**
 * These arrays are intended to be used for a form of global storage without 
 * requiring a #define that needs to be increased each time a person needs more 
 * storage.
 * These are not designed to be used as a replacement for normal arrays, as
 * normal arrays are faster and should be used whenever possible.
 */

/**
 * Creates a handle to a dynamically sized array.
 * It is very important that the cellsize you provide matches up with the buffer sizes
 * that you pass with subsequent Array{Get,Set,Push} calls.
 *
 * @param cellsize		How many cells each entry in the array is.
 * @param reserved		How many blank entries are created immediately when the array is created.  These entries are not valid to read from until called with ArraySet.
 * @return 			Handle to the array.
 */
native Array:ArrayCreate(cellsize=1, reserved=32);

/**
 * Clears all entries from the array.
 *
 * @param which		The array to clear.
 * @return				1 on success, 0 on failure.
 */
native ArrayClear(Array:which);

/**
 * Returns the number of elements in the array.
 *
 * @param which		The array to check.
 * @return				How many elements are in the array.
 */
native ArraySize(Array:which);

/**
 * Returns data within an array.  
 * Make sure the output buffer matches the size the array was created with!
 *
 * @param which		The array to retrieve the item from.
 * @param item			The item to retrieve (zero-based).
 * @param output		The output buffer to write.
 */
native ArrayGetArray(Array:which, item, any:output[]);

/**
 * Returns a single cell of data from an array.  
 * Use this only with arrays that were created with a cellsize of 1!
 *
 * @param which		The array to retrieve the item from.
 * @param item			The item to retrieve (zero-based).
 * @return				The value of the cell.
 */
native any:ArrayGetCell(Array:which, item);

/**
 * Returns a string value from an array.
 *
 * @param which			The array to retrieve the item from.
 * @param item			The item to retrieve (zero-based).
 * @param output		The variable to store the value in.
 * @param size			Character size of the output buffer.
 */
native ArrayGetString(Array:which, item, output[], size);

/**
 * Sets an item's data with that of a local buffer.  
 * The buffer size must match what the cellsize that the array was created with!
 * The item must already exist, use ArrayPushArray to create a new item within the array.
 *
 * @param which			The array to set the item from within.
 * @param item			The item to set (zero-based).
 * @param input			The input buffer to store.
 */
native ArraySetArray(Array:which, item, const any:input[]);

/**
 * Sets an array's single cell value.  Use this only on array that were created with a cellsize of 1!
 * The item must already exist, use ArrayPushCell to create a new item within the array.
 *
 * @param which			The array to set the item from within.
 * @param item			The item to set (zero-based).
 * @param input			The value to set.
 */
native ArraySetCell(Array:which, item, any:input);

/**
 * Sets a string value from an array.  
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 * The item must already exist, use ArrayPushString to create a new item within the array.
 *
 * @param which			The array to set the item from within.
 * @param item			The item to set (zero-based).
 * @param input			The string to set the item as.
 */
native ArraySetString(Array:which, item, const input[]);

/**
 * Creates a new item at the end of the array and sets its data with that of a local buffer.  
 * The buffer size must match what the cellsize that the array was created with!
 *
 * @param which			The array to add the item to.
 * @param input			The input buffer to store.
 */
native ArrayPushArray(Array:which, const any:input[]);

/**
 * Creates a new item and sets the array's single cell value.  
 * Use this only on array that were created with a cellsize of 1!
 *
 * @param which			The array to add the item to.
 * @param input			The value to set.
 */
native ArrayPushCell(Array:which, any:input);

/**
 * Creates a new element in the array and sets its value to the input buffer.
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 *
 * @param which			The array to add the item to.
 * @param input			The string to set the item as.
 */
native ArrayPushString(Array:which, const input[]);

/**
 * Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
 * The buffer size must match what the cellsize that the array was created with!
 *
 * @param which			The array to add the item to.
 * @param item			The item to insert after.
 * @param input			The input buffer to store.
 */
native ArrayInsertArrayAfter(Array:which, item, const any:input[]);

/**
 * Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
 * Use this only on an array that was created with a cellsize of 1!
 *
 * @param which			The array to add the item to.
 * @param item			The item to insert after.
 * @param input			The value to set.
 */
native ArrayInsertCellAfter(Array:which, item, any:input);

/**
 * Inserts an item after the selected item.  All items beyond it get shifted up 1 space.
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 *
 * @param which			The array to add the item to.
 * @param item			The item to insert after.
 * @param input			The value to set.
 */
native ArrayInsertStringAfter(Array:which, item, const input[]);

/**
 * Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
 * The buffer size must match what the cellsize that the array was created with!
 *
 * @param which			The array to add the item to.
 * @param item			The item to insert before.
 * @param input			The input buffer to store.
 */
native ArrayInsertArrayBefore(Array:which, item, const any:input[]);

/**
 * Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
 * Use this only on an array that was created with a cellsize of 1!
 *
 * @param which			The array to add the item to.
 * @param item			The item to insert after.
 * @param input			The value to set.
 */
native ArrayInsertCellBefore(Array:which, item, const any:input);

/**
 * Inserts an item before the selected item.  All items beyond it, and the selected item get shifted up 1 space.
 * The stored string will be truncated if it is longer than the cellsize the array was created with!
 *
 * @param which			The array to add the item to.
 * @param item			The item to insert before.
 * @param input			The value to set.
 */
native ArrayInsertStringBefore(Array:which, item, const input[]);

/**
 * Swaps the position of two items.
 *
 * @param which			The array that contains the items.
 * @param item1			The first item to swap.
 * @param item2			The second item to swap.
 */
native ArraySwap(Array:which, item1, item2);

/**
 * Deletes an item from the array.  All items beyond it get shifted down 1 space.
 *
 * @param which			The array that contains the item to delete.
 * @param item			The item to delete.
 */
native ArrayDeleteItem(Array:which, item);

/**
 * Creates a handle that is passable to a format compliant routine for printing as a string (with the %a format option).  
 * It is suggested to pass the function directly as a parameter to the format routine.
 * The array contents must be a null-terminated string!
 *
 * An example usage:  client_print(id, print_chat, "%a", ArrayGetStringHandle(MessageArray, i));
 *
 * @param which			The array the string is stored in.
 * @param item			Which item to print the string value of.
 * @return				Handle to the item directly.  Do not use or save stale handles.
 */
native DoNotUse:ArrayGetStringHandle(Array:which, item);

/**
 * Destroys the array, and resets the handle to 0 to prevent accidental usage after it is destroyed.
 *
 * @param which			The array to destroy.
 */
native ArrayDestroy(&Array:which);



/**
 * Similar to sorting.inc's CustomSort.
 * The sorting algorithm then uses your comparison function to sort the data.
 * The function is called in the following manner:
 * 
 * public MySortFunc(Array:array, item1, item2, const data[], data_size)
 *
 *  array			- Array handle in its current un-sorted state.
 *  item1, item2	- Current item pair being compared
 *  data[]			- Extra data array you passed to the sort func.
 *  data_size		- Size of extra data you passed to the sort func.
 *
 * Your function should return:
 *  -1 if item1 should go before item2
 *   0 if item1 and item2 are equal
 *   1 if item1 should go after item2
 * Note that the parameters after item2 are all optional and you do not need to specify them.
 *
 * Note that unlike the sorting.inc versions, the array passed to the callback is not in mid-sorted state.
 */
native ArraySort(Array:array, const comparefunc[], data[]="", data_size=0);
