// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Game Config Functions
//

#if defined _gameconfigs_included
	#endinput
#endif
#define _gameconfigs_included

enum GameConfig
{
	Invalid_GameConfig = 0
};

/**
 * Loads a game config file.
 *
 * @note The file path must be relative to the 'gamedata' folder under the data folder
 *       and the extension should be omitted.
 *
 * @param file          File to load
 *
 * @return              A handle to the game config file
 */
native GameConfig:LoadGameConfigFile(const file[]);

/**
 * Returns an offset value.
 *
 * @param handle        Game config handle
 * @param key           Key to retrieve from the offset section
 *
 * @return              An offset, or -1 on failure
 * @error               Invalid game config handle
 */
native GameConfGetOffset(GameConfig:handle, const key[]);

/**
 * Returns an offset value given a classname.
 *
 * @param handle        Game config handle
 * @param classname     Class name to match from the offset section
 * @param key           Key to retrieve from the offset section
 *
 * @return              An offset, or -1 on failure
 * @error               Invalid game config handle
 */
native GameConfGetClassOffset(GameConfig:handle, const classname[], const key[]);

/**
 * Gets the value of a key from the "Keys" section.
 *
 * @param handle        Game config handle
 * @param key           Key to retrieve from the Keys section
 * @param buffer        Destination string buffer
 * @param maxlen        Maximum length of output string buffer
 *
 * @return              True if key existed, false otherwise
 * @error               Invalid game config handle
 */
native bool:GameConfGetKeyValue(GameConfig:handle, const key[], buffer[], maxlen);

/**
 * Finds an address calculation in a GameConfig file.
 *
 * @param handle        Game config handle
 * @param name          Name of the property to find
 *
 * @return              An address calculated on success, otherwise 0 on failure.
 * @error               Invalid game config handle
 */
native GameConfGetAddress(GameConfig:handle, const name[]);

/**
 * Destroys a game config and frees its memory.
 *
 * @note The function automatically sets the variable passed to it to 0 to aid
 *       in preventing accidental usage after destroy.
 *
 * @param handle        Game config handle
 *
 * @return              1 on success, 0 if an invalid handle was passed in
 */
native CloseGameConfigFile(&GameConfig:handle);