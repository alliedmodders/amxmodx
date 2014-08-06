// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _cellstack_included
	#endinput
#endif
#define _cellstack_included

enum Stack
{
	Invalid_Stack = 0
};

/**
 * Creates a stack structure.  A stack is a LIFO (last in, first out)
 * vector (array) of items.  It has O(1) insertion and O(1) removal.
 *
 * Stacks have two operations: Push (adding an item) and Pop (removes
 * items in reverse-push order).
 *
 * The contents of the stack are uniform; i.e. storing a string and then
 * retrieving it as an integer is NOT the same as StringToInt()!
 *
 * The "blocksize" determines how many cells each slot has; it cannot
 * be changed after creation.
 *
 * @param blocksize     The number of cells each entry in the stack can
 *                      hold.  For example, 32 cells is equivalent to:
 *                      new Array[X][32]
 *
 * @return              New stack Handle.
 * @error               Invalid block size.
 */
native Stack:CreateStack(blocksize = 1);

/**
 * Pushes a value onto the end of the stack, adding a new index.
 *
 * This may safely be used even if the stack has a blocksize
 * greater than 1.
 *
 * @param handle        Stack handle.
 * @param value         Value to push.
 *
 * @noreturn
 * @error               Invalid handle or out of memory.
 */
native PushStackCell(Stack:handle, any:value);

/**
 * Pushes a string onto the end of a stack, truncating it if it is
 * too big.
 *
 * @param handle        Stack handle.
 * @param value         String to push.
 *
 * @noreturn
 * @error               Invalid handle or out of memory.
 */
native PushStackString(Stack:handle, const value[]);

/**
 * Pushes an array of cells onto the end of a stack.  The cells
 * are pushed as a block (i.e. the entire array takes up one stack slot),
 * rather than pushing each cell individually.
 *
 * @param handle        Stack handle.
 * @param values        Block of values to copy.
 * @param size          If not set, the number of elements copied from the array
 *                      will be equal to the blocksize.  If set higher than the
 *                      blocksize, the operation will be truncated.
 * @noreturn
 * @error               Invalid handle or out of memory.
 */
native PushStackArray(Stack:handle, const any:values[], size= -1);

/**
 * Pops a cell value from a stack.
 *
 * @param handle        Stack handle.
 * @param value         Variable to store the value.
 * @param block         Optionally specify which block to read from
 *                      (useful if the blocksize > 0).
 * @param asChar        Optionally read as a byte instead of a cell.
 *
 * @return              True on success, false if the stack is empty.
 * @error               Invalid handle, Invalid block or Invalid byte.
 */
native bool:PopStackCell(Stack:handle, &any:value, block = 0, bool:asChar = false);

/**
 * Pops a string value from a stack.
 *
 * @param handle        Stack handle.
 * @param buffer        Buffer to store string.
 * @param maxlength     Maximum size of the buffer.
 * @param written       Number of characters copied.
 *
 * @return              True on success, false if the stack is empty.
 * @error               Invalid handle.
 */
native bool:PopStackString(Stack:handle, buffer[], maxlength, &written = 0);

/**
 * Pops an array of cells from a stack.
 *
 * @param handle        Stack handle.
 * @param buffer        Buffer to store the array in.
 * @param size          If not set, assumes the buffer size is equal to the
 *                      blocksize.  Otherwise, the size passed is used.
 *
 * @return              True on success, false if the stack is empty.
 * @error               Invalid handle.
 */
native bool:PopStackArray(Stack:handle, any:buffer[], size = -1);

/**
 * Checks if a stack is empty.
 *
 * @param handle        Stack handle.
 *
 * @return              True if empty, false if not empty.
 * @error               Invalid handle.
 */
native bool:IsStackEmpty(Stack:handle);

/**
 * Pops a value off a stack, ignoring it completely.
 *
 * @param handle        Stack handle.
 *
 * @return              True if something was popped, false otherwise.
 * @error               Invalid handle.
 */
stock PopStack(Stack:handle)
{
	new value;
	return PopStackCell(handle, value);
}

/**
 * Destroys the stack, and resets the handle to 0 to prevent accidental usage after it is destroyed.
 *
 * @param which         The stack to destroy.
 *
 * @noreturn
 */
native DestroyStack(&Stack:handle);
