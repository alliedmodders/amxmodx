
#if defined _datapack_included
	#endinput
#endif
#define _datapack_included	

/**
 * DataPacks are a way to store and move around various types of data in AMX Mod X Scripting. 
 * Since some things are not possible in AMX Mod X, such as a function consuming a String, 
 * DataPacks help us get these Strings and other items where they need to go.
 */

enum DataPack
{
	Invalid_DataPack = 0
};

/**
 * Creates a new data pack.
 *
 * @return			A Handle to the data pack.
 */
native DataPack:CreateDataPack();

/**
 * Packs a normal cell into a data pack.
 *
 * @param pack		Handle to the data pack.
 * @param cell		Cell to add.
 * @noreturn
 * @error			Invalid handle.
 */
native WritePackCell(DataPack:pack, any:cell);

/**
 * Packs a float into a data pack.
 *
 * @param pack		Handle to the data pack.
 * @param val		Float to add.
 * @noreturn
 * @error			Invalid handle.
 */
native WritePackFloat(DataPack:pack, Float:val);

/**
 * Packs a string into a data pack.
 *
 * @param pack		Handle to the data pack.
 * @param str		String to add.
 * @return			Length of copied string.
 * @error			Invalid handle.
 */
native WritePackString(DataPack:pack, const str[]);

/**
 * Reads a cell from a data pack.
 *
 * @param pack		Handle to the data pack.
 * @return			Cell value.
 * @error			Invalid handle, or bounds error.
 */
native any:ReadPackCell(DataPack:pack);

/**
 * Reads a float from a data pack.
 *
 * @param pack		Handle to the data pack.
 * @return			Float value.
 * @error			Invalid handle, or bounds error.
 */
native Float:ReadPackFloat(DataPack:pack);

/**
 * Reads a string from a data pack.
 *
 * @param pack		Handle to the data pack.
 * @param buffer	Destination string buffer.
 * @param maxlen	Maximum length of output string buffer.
 * @return			Length of output string.
 * @error			Invalid handle, or bounds error.
 */
native ReadPackString(DataPack:pack, buffer[], maxlen);

/**
 * Resets the position in a data pack.
 *
 * @param pack		Handle to the data pack.
 * @param clear		If true, clears the contained data.
 * @noreturn
 * @error			Invalid handle.
 */
native ResetPack(DataPack:pack, bool:clear=false);

/**
 * Returns the read or write position in a data pack.
 *
 * @param pack		Handle to the data pack.
 * @return			Numerical position in the data pack.
 * @error			Invalid handle.
 */
native GetPackPosition(DataPack:pack);

/**
 * Sets the read/write position in a data pack.
 *
 * @param pack		Handle to the data pack.
 * @param position	New position to set.
 * @noreturn
 * @error			Invalid handle, or position is beyond the pack bounds.
 */
native SetPackPosition(DataPack:pack, position);

/**
 * Returns whether or not a specified number of bytes from the data pack
 *  position to the end can be read.
 *
 * @param pack		Handle to the data pack.
 * @param bytes		Number of bytes to simulate reading.
 * @return			True if can be read, false otherwise.
 * @error			Invalid handle.
 */
native bool:IsPackReadable(DataPack:pack, bytes);

/**
 * Disposes of a data pack.
 *
 * @param pack		Handle to the data pack.
 * @return			True if disposed, false otherwise.
 */
native DestroyDataPack(&DataPack:pack);
