// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Language Functions
//

#if defined _lang_included
  #endinput
#endif
#define _lang_included

/**
 * Returns the number of languages loaded.
 *
 * @return 			Number of languages loaded.		
 */
native get_langsnum();

/**
 * Returns the two-letter name of a language returned by get_langsnum()
 *
 * @param id		Language index, starting at 0
 * @param name 		Buffer to store the name in
 *
 * @noreturn
 */
native get_lang(id, name[3]);

/**
 * Registers a dictionary file, making sure the words are in the dictionary.
 *
 * @note The file should be in "addons/amxmodx/data/lang", but only the name
 *		 needs to be given. For example, register_dictionary("file.txt") will
 *		 be "addons/amxmodx/data/lang/file.txt".
 *
 * @param filename	Dictionary file name
 *
 * @return 			On success, the function will return 1, otherwise it will
 *					return 0 if the file couldn't be found or opened, and -1 if
 *					the dictionary was already registered by a plugin
 */
native register_dictionary(const filename[]);

/**
 * Checks if the language is loaded.
 *
 * @return 			1 if it is, 0 otherwise
 */
native lang_exists(const name[]);

enum TransKey
{
	TransKey_Bad = -1,
};

/**
 * Creates a new or finds an existing translation key.
 *
 * @param key		Key to create or find
 *
 * @return			Key index
 */
native TransKey:CreateLangKey(const key[]);
 
/**
 * Finds a translation key index without adding on failure.
 *
 * @param key		Key to search for
 *
 * @return			Key index, or -1 if not found
 */
native TransKey:GetLangTransKey(const key[]);
 
/**
 * Adds a new translation.
 *
 * @param lang		Two-letter language name
 * @param key		Language key
 * @param phrase	Translated text
 *
 * @noreturn
 */
native AddTranslation(const lang[3], TransKey:key, const phrase[]);

/**
 * Looks up the translation of the key for the given type.
 *
 * @note This does NOT format the output text! For example, if the key
 * 		 contains %s, the outputted text will also contain %s.
 * @note LANG_PLAYER is invalid in this, use a player index or LANG_SERVER.
 *
 * @param Output		Buffer to store the output in
 * @param OutputSize	Maximum buffer size
 * @param Key		 	Language key
 * @param id		 	Client index or LANG_SERVER
 *
 * @return 				1 on success, 0 otherwise
 */
native LookupLangKey(Output[], OutputSize, const Key[], const &id);

/**
 * Sets the global language target.
 *
 * @note This is useful for creating functions
 *       that will be compatible with the %l format specifier. Note that invalid
 *       indexes can be specified but the error will occur during translation,
 *       not during this function call.
 *
 * @param client    Client index or LANG_SERVER
 * @noreturn
 */
native SetGlobalTransTarget(client);
