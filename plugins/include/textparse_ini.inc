// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// INI Parser Functions
//

#if defined _textparse_ini_included
	#endinput
#endif
#define _textparse_ini_included

/**
 * This parser API is entirely event based.  
 * You must hook events to receive data.
 */
 
/**
 * The INI file format is defined as:
 *    WHITESPACE: 0x20, \n, \t, \r
 *    IDENTIFIER: A-Z a-z 0-9 _ - , + . $ ? / 
 *    STRING    : Any set of symbols
 * 
 * Basic syntax is comprised of SECTIONs.
 *    A SECTION is defined as:
 *    [SECTIONNAME]
 *    OPTION
 *    OPTION
 *    OPTION...
 *
 * SECTIONNAME is an IDENTIFIER.
 *    OPTION can be repeated any number of times, once per line.
 *    OPTION is defined as one of:
 *      KEY = "VALUE"
 *      KEY = VALUE
 *      KEY
 *    Where KEY is an IDENTIFIER and VALUE is a STRING.
 * 
 * WHITESPACE should always be omitted.
 *    COMMENTS should be stripped, and are defined as text occurring in:
 *    ;<TEXT>
 * 
 * Example file below.  Note that the second line is technically invalid.  
 * The event handler must decide whether this should be allowed.
 *    --FILE BELOW--
 *    [gaben]
 *    hi = clams
 *    bye = "NO CLAMS"
 *
 *    [valve]
 *    cannot
 *    maintain
 *    products
 */
  
/**
 * Parser invalid code.
 */
enum INIParser
{
	Invalid_INIParser = 0
};

/**
 * Creates a new INI parser.  
 * This is used to set parse hooks.
 *
 * @return              A new handle to an INI Parse structure.
 */
native INIParser:INI_CreateParser();

/**
 * Disposes of an INI parser.
 *
 * @param handle        Handle to an INI Parse structure.
 *
 * @return              True if disposed, false otherwise.
 */
native INI_DestroyParser(&INIParser:handle);

/**
 * Parses an INI config file.
 *
 * @param handle        A handle to an INI Parse structure.
 * @param file          A string containing the file path.
 * @param line          An optional by reference cell to store the last line number read.
 * @param col           An optional by reference cell to store the last column number read.
 * @param data          An optional handle or value to pass through to callback functions
 *
 * @return              An SMCParseError result.
 * @error               Invalid or corrupt handle. 
 */
native bool:INI_ParseFile(INIParser:handle, const file[], &line = 0, &col = 0, any:data = 0);

/**
 * Sets the INI_ParseStart function of a parse handle.
 *
 * @note  Below is the prototype of callback:
 *        -
 *          Called when parsing is started.
 *
 *          @param handle        A handle to an INI Parse structure.
 *          @param data          Handle or value passed in INI_ParseFile
 *
 *          @noreturn
 *
 *          public OnParseStart(INIParser:handle, any:data)
 *        -
 * @param handle        Handle to an INI Parse structure.
 * @param func          A ParseStart callback.
 *
 * @noreturn
 * @error               Invalid or corrupt handle. 
 */
native INI_SetParseStart(INIParser:handle, const func[]);

/**
 * Sets the INI_ParseEnd function of a parse handle.
 *
 * @note  Below is the prototype of callback:
 *        -
 *          Called when parsing is halted.
 *
 *          @param handle        A handle to an INI Parse structure.
 *          @param halted        True if abnormally halted, false otherwise.
 *          @param data          Handle or value passed in INI_ParseFile
 *
 *          @noreturn
 *
 *          public OnParseEnd(INIParser:handle, bool:halted, any:data)
 *        -
 * @param handle        Handle to an INI Parse structure.
 * @param func          A ParseEnd callback.
 *
 * @noreturn
 * @error               Invalid or corrupt handle. 
 */
native INI_SetParseEnd(INIParser:handle, const func[]);

/**
 * Sets the two main reader functions.
 *
 * @note  Below is the prototype of callback:
 *        -
 *          NewSection: 
 *              Called when the parser finds a new section.
 *
 *              @param handle           Handle to an INI Parse structure.
 *              @param section          Name of section in between the [ and ] characters.
 *              @param invalid_tokens   True if invalid tokens were detected in the name.
 *              @param close_bracket    True if a closing bracket was detected, false otherwise.
 *              @param extra_tokens     True if extra tokens were detected on the line.
 *              @param curtok           Contains current token in the line where the section name starts.
 *                                      You can add to this offset when failing to point to a token.
 *              @param data             Handle or value passed in INI_ParseFile
 *
 *              @return                 True to keep parsing, false otherwise.
 *
 *              public bool:OnNewSection(INIParser:handle, const section[], bool:invalid_tokens, bool:close_bracket, bool:extra_tokens, curtok, any:data)
 *
 *          KeyValue: 
 *              Called when the parser finds a new key/value pair.
 *
 *              @param handle           Handle to an INI Parse structure.
 *              @param key              Name of key.
 *              @param value            String containing value (with quotes stripped, if any).
 *              @param invalid_tokens   Whether or not the key contained invalid tokens.
 *              @param equal_token      There was an '=' sign present (in case the value is missing).
 *              @param quotes           Whether value was enclosed in quotes.
 *              @param curtok           Contains the token index of the start of the value string.  
 *                                      This can be changed when returning false.
 *              @param data             Handle or value passed in INI_ParseFile
 *
 *              @return                 True to keep parsing, false otherwise.
 * 
 *              public bool:OnKeyValue(INIParser:handle, const key[], const value[], bool:invalid_tokens, bool:equal_token, bool:quotes, curtok, any:data)
 *        -  
 * @param handle        Handle to an INI Parse structure.
 * @param kv            A KeyValue callback. 
 * @param ns            An optional NewSection callback. 
 *
 * @noreturn
 */
native INI_SetReaders(INIParser:smc, const kvFunc[], const nsFunc[] = "" );

/**
 * Sets a raw line reader on an INI parser handle.
 *
 * @note  Below is the prototype of callback:
 *        -
 *          Called whenever a raw line is read.
 *
 *          @param handle       The INI Parse handle.
 *          @param line         Contents of line.
 *          @param lineno       The line number it occurs on.
 *          @param curtok       Pointer to optionally store failed position in string.
 *          @param data         Handle or value passed in INI_ParseFile
 *
 *          @return             True to keep parsing, false otherwise.
 *
 *          public bool:OnRawLine(INIParser:smc, const line[], lineno, curtok, any:data)
 *
 * @param handle        Handle to an INI Parse structure.
 * @param func          A RawLine callback.
 *
 * @noreturn
 */
native INI_SetRawLine(INIParser:handle, const func[]);
