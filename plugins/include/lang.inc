/* Language functions
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _lang_included
  #endinput
#endif
#define _lang_included

//return the number of languages loaded
native get_langsnum();

//sets name to the two-letter name of a language returned by get_langsnum
//index starts at 0
native get_lang(id, name[3]);

//registers a dictionary file, making sure the words are in the dictionary
// the file should be in "addons/amxx/data/lang/", but only the name needs to be
// given.  (e.g. register_dictionary("file.txt") will be addons/amxx/data/file.txt).
native register_dictionary(const filename[]);

//returns 1 if the language is loaded, 0 otherwise.
native lang_exists(const name[]);