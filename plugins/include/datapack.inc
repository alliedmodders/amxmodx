// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _datapack_included
	#endinput
#endif
#define _datapack_included

/**
 * Datapack tag declaration
 *
 * @note Datapacks provide a way to store and move around arbitrary amounts (and
 *       types) of data in AMX Mox X. Data is packed into a single cell value -
 *       the DataPack handle. This handle can be passed around more easily, can
 *       be returned by functions and can simulate advanced concepts like string
 *       consummation.
 * @note Plugins are responsible for freeing all datapack handles they acquire.
 *       Failing to free handles will result in the plugin and AMXX leaking
 *       memory.
 */
enum DataPack
{
	Invalid_DataPack = 0
};

/**
 * Creates a new datapack.
 *
 * @return  New datapack handle, which must be freed via DestroyDataPack().
 */
native DataPack:CreateDataPack();

/**
 * Packs a cell value into a datapack.
 *
 * @param pack      Datapack handle
 * @param cell      Cell value to pack
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native WritePackCell(DataPack:pack, any:cell);

/**
 * Packs a float value into a datapack.
 *
 * @param pack      Datapack handle
 * @param val       Float value to pack
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native WritePackFloat(DataPack:pack, Float:val);

/**
 * Packs a string into a datapack.
 *
 * @param pack      Datapack handle
 * @param str       String to pack
 *
 * @return          Length of copied string
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native WritePackString(DataPack:pack, const str[]);

/**
 * Reads a cell from a Datapack.
 *
 * @param pack      Datapack handle
 *
 * @return          Cell value
 * @error           If an invalid handle is provided, or not enough data is left
 *                  in the datapack, an error will be thrown.
 */
native any:ReadPackCell(DataPack:pack);

/**
 * Reads a float from a datapack.
 *
 * @param pack      Datapack handle
 *
 * @return          Float value
 * @error           If an invalid handle is provided, or not enough data is left
 *                  in the datapack, an error will be thrown.
 */
native Float:ReadPackFloat(DataPack:pack);

/**
 * Reads a string from a Datapack.
 *
 * @param pack      Datapack handle
 * @param buffer    Buffer to copy string to
 * @param maxlen    Maximum size of buffer
 *
 * @return          Number of cells written to buffer
 * @error           If an invalid handle is provided, or not enough data is left
 *                  in the datapack, an error will be thrown.
 */
native ReadPackString(DataPack:pack, buffer[], maxlen);

/**
 * Resets the datapack read/write position to the start.
 *
 * @param pack      Datapack handle
 * @param clear     If true, clears the contained data
 *
 * @noreturn
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native ResetPack(DataPack:pack, bool:clear = false);

/**
 * Returns the datapack read/write position.
 *
 * @param pack      Datapack handle
 *
 * @return          Position in the datapack, only usable with calls to SetPackPosition
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native DataPackPos:GetPackPosition(DataPack:pack);

/**
 * Sets the datapack read/write position.
 *
 * @note This should only ever be used with (known to be valid) positions
 *       returned by GetPackPosition(). It is not possible for plugins to safely
 *       compute datapack positions.
 *
 * @param pack      Datapack handle
 * @param position  New position to set
 *
 * @noreturn
 * @error           If an invalid handle is provided, or the new position is
 *                  out of datapack bounds, an error will be thrown.
 */
native SetPackPosition(DataPack:pack, DataPackPos:position);

/**
 * Returns if the datapack has reached its end and no more data can be read.
 *
 * @param pack      Datapack handle
 *
 * @return          True if datapack has reached the end, false otherwise
 * @error           If an invalid handle is provided, an error will be thrown.
 */
native bool:IsPackEnded(DataPack:pack);

/**
 * Destroys the datapack and frees its memory.
 *
 * @param pack      Datapack handle
 *
 * @return          True if disposed, false otherwise
 */
native DestroyDataPack(&DataPack:pack);
