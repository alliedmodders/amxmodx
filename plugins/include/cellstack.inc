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

/**
 * Stack tag declaration
 *
 * @note Plugins are responsible for freeing all Stack handles they acquire.
 *       Failing to free handles will result in the plugin and AMXX leaking
 *       memory.
 */
enum Stack
{
	Invalid_Stack = 0
};

/**
 * Creates a stack structure. A stack is a LIFO (last in, first out) vector of
 * of items. It has O(1) insertion and O(1) removal.
 *
 * @note Stacks provide only two operations: Push (adding an item to the top)
 * 		 and Pop (remove an item from the top, in reverse-push order).
 * @note The contents of the stack are uniform; i.e. storing a string and then
 *       retrieving it as an integer is NOT the same as str_to_num()!
 * @note The "blocksize" determines how many cells each stack slot has, it can
 *       not be changed after creation.
 *
 * @param blocksize     The number of cells each entry in the stack can hold
 *
 * @return              New stack Handle, which must be freed via DestroyStack()
 * @error               If an invalid blocksize is provided an error will be
 *                      thrown.
 */
native Stack:CreateStack(blocksize = 1);

/**
 * Pushes a value onto the end of the stack, adding a new index.
 *
 * @note This may safely be used even if the stack has a blocksize greater than
 *       1.
 *
 * @param handle    Stack handle
 * @param value     Value to push
 *
 * @noreturn
 * @error           If an invalid handle is provided or the resizing
 *                  operation runs out of memory, an error will be thrown.
 */
native PushStackCell(Stack:handle, any:value);

/**
 * Pushes a string onto the end of a stack, truncating it if it is too long.
 *
 * @param handle    Stack handle
 * @param value     String to push
 *
 * @noreturn
 * @error           If an invalid handle is provided or the resizing
 *                  operation runs out of memory, an error will be thrown.
 */
native PushStackString(Stack:handle, const value[]);

/**
 * Pushes an array of cells onto the end of a stack. The cells are pushed as a
 * block (i.e. the entire array takes up one stack slot), rather than pushing
 * each cell individually.
 *
 * @param handle    Stack handle
 * @param values    Block of values to copy
 * @param size      If not set, the number of elements copied from the array
 *                  will be equal to the blocksize, if set higher than the
 *                  blocksize, the operation will be truncated,
 *
 * @noreturn
 * @error           If an invalid handle is provided or the resizing
 *                  operation runs out of memory, an error will be thrown.
 */
native PushStackArray(Stack:handle, const any:values[], size= -1);

/**
 * Pops a cell value from a stack.
 *
 * @param handle    Stack handle
 * @param value     Variable to store the value in
 * @param block     Optionally specify which block to read from (useful if the
 *                  blocksize is > 0)
 * @param asChar    Optionally read as a byte instead of a cell
 *
 * @return          True on success, false if the stack is empty.
 * @error           If an invalid handle, invalid block or invalid byte is
 *                  provided, an error will be thrown.
 */
native bool:PopStackCell(Stack:handle, &any:value, block = 0, bool:asChar = false);

/**
 * Pops a string value from a stack.
 *
 * @param handle        Stack handle
 * @param buffer        Buffer to copy string to
 * @param maxlength     Maximum size of the buffer
 * @param written       Variable to store number of characters copied to
 *
 * @return              True on success, false if the stack is empty
 * @error               If an invalid handle is provided an error will be thrown.
 */
native bool:PopStackString(Stack:handle, buffer[], maxlength, &written = 0);

/**
 * Pops an array of cells from a stack.
 *
 * @param handle    Stack handle
 * @param buffer    Array to copy value to
 * @param size      Size of buffer, if not set (-1) assumes the size is equal to
 *                  the stack blocksize
 *
 * @return          True on success, false if the stack is empty
 * @error           If an invalid handle is provided an error will be thrown.
 */
native bool:PopStackArray(Stack:handle, any:buffer[], size = -1);

/**
 * Returns if a stack is empty.
 *
 * @param handle    Stack handle
 *
 * @return          True if empty, false if not empty
 * @error           If an invalid handle is provided an error will be thrown.
 */
native bool:IsStackEmpty(Stack:handle);

/**
 * Pops a value off a stack, ignoring it completely.
 *
 * @param handle    Stack handle
 *
 * @return          True if a value was popped, false if stack is empty
 * @error           If an invalid handle is provided an error will be thrown.
 */
stock PopStack(Stack:handle)
{
	new value;
	return PopStackCell(handle, value);
}

/**
 * Destroys a stack and frees its memory.
 *
 * @note The function automatically sets the variable passed to it to 0 to aid
 *       in preventing accidental usage after destroy.
 *
 * @param handle    Stack to destroy
 *
 * @return          1 if the Stack was destroyed, 0 if nothing had to be
 *                  destroyed (invalid handle)
 */
native DestroyStack(&Stack:handle);
