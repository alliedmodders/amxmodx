// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// String Manipulation
//

#if defined _string_included
  #endinput
#endif
#define _string_included

#include <string_const>

/**
 * @global 	Unless otherwise noted, all string functions which take in a 
 *			writable buffer and maximum length should NOT have the null terminator INCLUDED
 * 			in the length.  This means that this is valid: 
 * 			copy(string, charsmax(string), ...)
 */

/**
 * Calculates the length of a string.
 *
 * @param string		String to check.
 * @return				Number of valid character bytes in the string.
 */
native strlen(const string[]);

/**
 * Tests whether a string is found inside another string.
 *
 * @param source		String to search in.
 * @param string		Substring to find inside the original string.
 *
 * @return				-1 on failure (no match found). Any other value
 *						indicates a position in the string where the match starts.
 */
native contain(const source[], const string[]);

/**
 * Tests whether a string is found inside another string with case ignoring.
 *
 * @note This supports multi-byte characters (UTF-8) on comparison.
 *
 * @param source		String to search in.
 * @param string		Substring to find inside the original string.
 *
 * @return				-1 on failure (no match found). Any other value
 *						indicates a position in the string where the match starts.
 */
native containi(const source[], const string[]);

/**
 * Given a string, replaces the first occurrence of a search string with a 
 * replacement string.
 *
 * @param text			String to perform search and replacements on.
 * @param len			Maximum length of the string buffer.
 * @param what			String to search for.
 * @param with			String to replace the search string with.
 *
 * @return				The new string length after replacement, or 0 if no replacements were made.
 */
native replace(text[], len, const what[], const with[]);

/**
 * Given a string, replaces all occurrences of a search string with a 
 * replacement string.
 *
 * @note Similar to replace_all() stock, but implemented as native and 
 *       with different algorithm. This native doesn't error on bad 
 *       buffer size and will smartly cut off the string in a way 
 *       that pushes old data out.
 *	
 * @note Only available in 1.8.3 and above.
 * @note This supports multi-byte characters (UTF-8) on case insensitive comparison.
 *
 * @param text			String to perform search and replacements on.
 * @param maxlength		Maximum length of the string buffer.
 * @param search		String to search for.
 * @param replace		String to replace the search string with.
 * @param caseSensitive	If true (default), search is case sensitive.
 *
 * @return				Number of replacements that were performed.
 */
native replace_string(text[], maxlength, const search[], const replace[], bool:caseSensitive = true);

/**
 * Given a string, replaces the first occurrence of a search string with a 
 * replacement string.
 *
 * @note Similar to replace() native, but implemented with more options and 
 *       with different algorithm. This native doesn't error on bad 
 *       buffer size and will smartly cut off the string in a way 
 *       that pushes old data out.
 *	
 * @note Only available in 1.8.3 and above.
 * @note This supports multi-byte characters (UTF-8) on case insensitive comparison.
 *
 * @param text			String to perform search and replacements on.
 * @param maxlength		Maximum length of the string buffer.
 * @param search		String to search for.
 * @param replace		String to replace the search string with.
 * @param searchLen		If higher than -1, its value will be used instead of
 *						a strlen() call on the search parameter.
 * @param replaceLen	If higher than -1, its value will be used instead of
 *						a strlen() call on the replace parameter.
 * @param caseSensitive	If true (default), search is case sensitive.
 *
 * @return				Index into the buffer (relative to the start) from where
 *						the last replacement ended, or -1 if no replacements were
 *						made.
 */
native replace_stringex(text[], maxlength, const search[], const replace[], searchLen = -1, replaceLen = -1, bool:caseSensitive = true);

/**
 * Concatenates one string onto another.
 *
 * @param dest			String to append to.
 * @param len			Maximum length of entire buffer.
 * @param src			Source string to concatenate.
 * @param max			Number of characters to add.
 *
 * @return				Number of of all merged characters.
 */
native add(dest[],len,const src[],max=0);

/**
 * Formats a string according to the AMX Mod X format rules (see documentation).
 *
 * @note Example: format(dest, "Hello %s. You are %d years old", "Tom", 17).
 *       If any of your input buffers overlap with the destination buffer,
 *       format() falls back to a "copy-back" version as of 1.65.  This is 
 *       slower, so you should using a source string that is the same as
 *       the destination.
 *
 * @param output		Destination string buffer.
 * @param len			Maximum length of output string buffer.
 * @param format		Formatting rules.
 * @param ...			Variable number of format parameters.
 *
 * @return				Number of cells written.
 */
native format(output[], len, const format[], any:...);

/**
 * Formats a string according to the AMX Mod X format rules (see documentation).
 *
 * @note Same as format(), except does not perform a "copy back" check.
 *       This means formatex() is faster, but DOES NOT ALLOW this type
 *       of call:
 *         formatex(buffer, len, "%s", buffer)
 *         formatex(buffer, len, buffer, buffer)
 *         formatex(buffer, len, "%s", buffer[5])
 *       This is because the output is directly stored into "buffer", 
 *       rather than copied back at the end.
 *
 * @param output		Destination string buffer.
 * @param len			Maximum length of output string buffer.
 * @param format		Formatting rules.
 * @param ...			Variable number of format parameters.
 *
 * @return				Number of cells written.
 */
native formatex(output[], len, const format[], any:...);

/**
 * Formats and returns a string according to the AMX Mod X format rules
 * (see documentation).
 *
 * @note Example: menu_additem(menu, fmt("My first %s", "item")).
 * @note This should only be used for simple inline formatting like in the above example.
 *       Avoid using this function to store strings into variables as an additional
 *       copying step is required.
 * @note The buffer size is defined by MAX_FMT_LENGTH.
 *
 * @param format        Formatting rules.
 * @param ...           Variable number of format parameters.
 *
 * @return              Formatted string
 */
native [MAX_FMT_LENGTH]fmt(const format[], any:...);

/**
 * Formats a string according to the AMX Mod X format rules (see documentation).
 *
 * @note This is the same as format(), except it grabs parameters from a 
 *       parent parameter stack, rather than a local.  This is useful for 
 *       implementing your own variable argument functions.
 *
 * @note Replacement for format_args.  Much faster and %L compatible.
 *       This works exactly like vsnprintf() from C.
 *       You must pass in the output buffer and its size,
 *        the string to format, and the number of the FIRST variable
 *        argument parameter.  For example, for:
 *        function (a, b, c, ...)
 *        You would pass 4 (a is 1, b is 2, c is 3, et cetera).
 *       There is no vformatex().
 *
 * @param buffer		Destination string buffer.
 * @param len			Maximum length of output string buffer.
 * @param fmt			Formatting rules.
 * @param vararg		Argument number which contains the '...' symbol.
 *						Note: Arguments start at 1.
 * @return 				Number of bytes written.
 */
native vformat(buffer[], len, const fmt[], vararg);

/**
 * Formats a string according to the AMX Mod X format rules (see documentation).
 *
 * @note Same as vformat(), except works in normal style dynamic natives.
 *       Instead of passing the format arg string, you can only pass the 
 *       actual format argument number itself.
 *       If you pass 0, it will read the format string from an optional 
 *       fifth parameter.
 *
 * @param buffer		Destination string buffer.
 * @param len			Maximum length of output string buffer.
 * @param fmt_arg		Argument number which contains the format.
 * @param vararg		Argument number which contains the '...' symbol.
 *						Note: Arguments start at 1.
 * @return 				Number of bytes written.
 */
native vdformat(buffer[], len, fmt_arg, vararg, ...);

/**
 * Gets parameters from function as formated string. 
 *
 * @param output		Destination string buffer.
 * @param len			Maximum length of output string buffer.
 * @param pos			Argument number which contains the '...' symbol.
 *
 * @return				Number of bytes written.
 */
native format_args(output[], len, pos = 0);

/**
 * Converts an integer to a string.
 *
 * @param num			Integer to convert.
 * @param string		Buffer to store string in.
 * @param len			Maximum length of string buffer.
 *
 * @return				Number of cells written to buffer.
 */
native num_to_str(num,string[],len);

/**
 * Converts a string to an integer.
 *
 * @param string		String to convert.
 * @return				Integer conversion of string, or 0 on failure.
 */
native str_to_num(const string[]);

/**
 * Parses the 'string' interpreting its content as an integral number of the specified 'base', 
 * which is returned as integer value. The function also sets the value of 'endPos' to point 
 * to the position of the first character after the number.
 * 
 * This is the same as C++ strtol function with a difference on second param.
 * 
 * The function first discards as many whitespace characters as necessary until the first 
 * non-whitespace character is found. Then, starting from this character, takes as many 
 * characters as possible that are valid following a syntax that depends on the 'base' parameter,
 * and interprets them as a numerical value. Finally, a position of the first character following
 * the integer representation in 'string' is stored in 'endPos'.
 * 
 * If the value of 'base' is zero, the syntax expected is similar to that of integer constants, 
 * which is formed by a succession of :
 *    An optional sign character (+ or -)
 *    An optional prefix indicating octal or hexadecimal base ("0" or "0x"/"0X" respectively)
 *    A sequence of decimal digits (if no base prefix was specified) or either octal or hexadecimal digits if a specific prefix is present
 *
 * If the 'base' value is between 2 and 36, the format expected for the integral number is a succession 
 * of any of the valid digits and/or letters needed to represent integers of the specified radix 
 * (starting from '0' and up to 'z'/'Z' for radix 36). The sequence may optionally be preceded by 
 * a sign (either + or -) and, if base is 16, an optional "0x" or "0X" prefix.
 *
 * If the first sequence of non-whitespace characters in 'string' is not a valid integral number
 * as defined above, or if no such sequence exists because either 'string' is empty or it contains
 * only whitespace characters, no conversion is performed.
 *
 * @param string    The string to parse.
 * @param endPos    The position of the first character following the number.
 *                  On success and when containing only numbers, position is at the end of string, meaning equal to 'string' length.
 *                  On failure, position is sets always to 0.
 * @param base      The numerical base (radix) that determines the valid characters and their interpretation.
 *                  If this is 0, the base used is determined by the format in the sequence.
 * @return          On success, the function returns the converted integral number as integer value.
 *                  If no valid conversion could be performed, a zero value is returned.
 *                  If the value read is out of the range of representable values by a cell, 
 *                  the function returns 'cellmin' or 'cellmax'.
 */
native strtol(const string[], &endPos = 0, base = 0);

/**
 * Parses the 'string' interpreting its content as an floating point number and returns its value as a float.
 * The function also sets the value of 'endPos' to point to the position of the first character after the number.
 * 
 * This is the same as C++ strtod function with a difference on second param.
 * 
 * The function first discards as many whitespace characters as necessary until the first 
 * non-whitespace character is found. Then, starting from this character, takes as many 
 * characters as possible that are valid and interprets them as a numerical value. 
 * Finally, a position of the first character following the float representation in 'string' 
 * is stored in 'endPos'.
 * 
 * If the first sequence of non-whitespace characters in 'string' is not a valid float number
 * as defined above, or if no such sequence exists because either 'string' is empty or it contains
 * only whitespace characters, no conversion is performed.
 *
 * @param string    The string to parse.
 * @param endPos    The position of the first character following the number.
 *                  On success and when containing only numbers, position is at the end of string, meaning equal to 'string' length.
 *                  On failure, position is sets always to 0.
 * @return          On success, the function returns the converted floating point number as float value.
 *                  If no valid conversion could be performed, a zero value is returned.
 */
native Float:strtof(const string[], &endPos = 0);

/**
 * Converts a floating point number to a string.
 *
 * @param fl			Floating point number to convert.
 * @param string		Buffer to store string in.
 * @param len			Maximum length of string buffer.
 *
 * @return				Number of cells written to buffer.
 */
native float_to_str(Float:fl, string[], len);

/** 
 * Converts a string to a floating point number.
 *
 * @param string		String to convert to a foat.
 * @return				Floating point result, or 0.0 on error.
 */
native Float:str_to_float(const string[]);

/**
 * Returns whether two strings are equal.
 *
 * @param a				First string (left).
 * @param b				Second string (right).
 * @param c				Number of characters to compare.
 *
 * @return				True if equal, false otherwise.
 */
native equal(const a[],const b[],c=0);

/**
 * Returns whether two strings are equal with case ignoring.
 *
 * @note This supports multi-byte characters (UTF-8) on comparison.
 *
 * @param a				First string (left).
 * @param b				Second string (right).
 * @param c				Number of characters to compare.
 *
 * @return				True if equal, false otherwise.
 */
native equali(const a[], const b[], c = 0);

/**
 * Copies one string to another string.
 *
 * @note If the destination buffer is too small to hold the source string, the 
 *       destination will be truncated.
 *
 * @param dest			Destination string buffer to copy to.
 * @param len			Destination buffer length.
 * @param src			Source string buffer to copy from.
 *
 * @return				Number of cells written.
 */
native copy(dest[],len,const src[]);

/**
 * Copies one string to another string until ch is found.
 *
 * @param dest			Destination string buffer to copy to.
 * @param len			Destination buffer length.
 * @param src			Source string buffer to copy from.
 * @param ch			Character to search for.
 *
 * @return				Number of cells written.
 */
native copyc(dest[],len,const src[],ch);

/**
 * Sets string with given character.
 *
 * @param src			Destination string buffer to copy to.
 * @param len			Destination buffer length.
 * @param ch			Character to set string.
 *
 * @noreturn
 */
native setc(src[],len,ch);

/**
 * Gets parameters from text.
 *
 * @note Example: to split text: "^"This is^" the best year",
 *       call function like this: parse(text,arg1,len1,arg2,len2,arg3,len3,arg4,len4)
 *       and you will get: "This is", "the", "best", "year"
 *       Function returns number of parsed parameters.
 *
 * @param text			String to parse.
 * @param ...			Variable number of format parameters.
 *
 * @return				Number of parsed parameters.
 */
native parse(const text[], ... );

/**
 * Breaks a string in two by token.
 *
 * @note Trimming spaces is buggy. Consider strtok2 instead.
 *
 * @note See argbreak() for doing this with parameters.
 *       Example:
 *        str1[] = This *is*some text
 *        strtok(str1, left, 24, right, 24, '*')
 *        left will be "This "
 *        Right will be "is*some text"
 *        If you use trimSpaces, all spaces are trimmed from Left.
 *
 * @param text			String to tokenize
 * @param Left			Buffer to store left half
 * @param leftLen		Size of left buffer
 * @param Right			Buffer to store right half
 * @param rightLen		Size of right buffer
 * @param token			Token to split by
 * @param trimSpaces	Whether spaces are trimmed.
 *
 * @noreturn
 */
native strtok(const text[], Left[], leftLen, Right[], rightLen, token=' ', trimSpaces=0);

/**
 * Breaks a string in two by token.
 *
 * @note Only available in 1.8.3 and above.
 *
 * @param text			String to tokenize
 * @param left			Buffer to store left half
 * @param llen			Size of left buffer
 * @param right			Buffer to store right half
 * @param rlen			Size of right buffer
 * @param token			Token to split by
 * @param trim			Flags for trimming behavior, see above
 *
 * @return				Returns position of token in string if found, 
 *						-1 if token was not found
 */
native strtok2(const text[], left[], const llen, right[], const rlen, const token = ' ', const trim = 0);

/**
 * Removes whitespace characters from the beginning and end of a string.
 *
 * @param text			The string to trim.
 * @return				Number of bytes written.
 */
native trim(text[]);

/**
 * Converts all chars in string to lower case.
 *
 * @param string		The string to convert.
 * @return				Number of bytes written.
 */
native strtolower(string[]);

/**
 * Performs a multi-byte safe (UTF-8) conversion of all chars in string to lower case.
 *
 * @note Although most code points can be converted in-place, there are notable
 *       exceptions and the final length can vary.
 * @note Case mapping is not reversible. That is, toUpper(toLower(x)) != toLower(toUpper(x)).
 *
 * @param string		The string to convert.
 * @param maxlength		Optional size of the buffer. If 0, the length of the original string
 *                      will be used instead.
 *
 * @return				Number of bytes written.
 */
native mb_strtolower(string[], maxlength = 0);

/**
 * Converts all chars in string to upper case.
 *
 * @param string		The string to convert.
 * @return				Number of bytes written.
 */
native strtoupper(string[]);

/**
 * Performs a multi-byte safe (UTF-8) conversion of all chars in string to upper case.
 *
 * @note Although most code points can be converted in-place, there are notable
 *       exceptions and the final length can vary.
 * @note Case mapping is not reversible. That is, toUpper(toLower(x)) != toLower(toUpper(x)).
 *
 * @param string		The string to convert.
 * @param maxlength		Optional size of the buffer. If 0, the length of the original string
 *                      will be used instead.
 *
 * @return				Number of bytes written.
 */
native mb_strtoupper(string[], maxlength = 0);

/**
 * Make a string's first character uppercase.
 *
 * @param string		The string to convert.
 * @return				1 on success, otherwise 0.
 */
native ucfirst(string[]);

/**
 * Performs a multi-byte safe (UTF-8) conversion of a string's first character to upper case.
 *
 * @note Although most code points can be converted in-place, there are notable
 *       exceptions and the final length can vary.
 *
 * @param string		The string to convert.
 * @param maxlength		Optional size of the buffer. If 0, the length of the original string
 *                      will be used instead.
 *
 * @return				Number of bytes written.
 */
native mb_ucfirst(string[], maxlength = 0);

/**
 * Performs a multi-byte safe (UTF-8) conversion of all chars in string to title case.
 *
 * @note Although most code points can be converted in-place, there are notable
 *       exceptions and the final length can vary.
 * @note Any type of punctuation can break up a word, even if this is
 *       not grammatically valid. This happens because the titlecasing algorithm
 *       does not and cannot take grammar rules into account.
 * @note Examples:
 *         The running man                      | The Running Man
 *	       NATO Alliance                        | Nato Alliance
 *	       You're amazing at building libraries | You'Re Amazing At Building Libraries
 *
 * @param string		The string to convert.
 * @param maxlength		Optional size of the buffer. If 0, the length of the original string
 *                      will be used instead.
 *
 * @return				Number of bytes written.
 */
native mb_strtotitle(string[], maxlength = 0);

/**
 * Checks if the input string conforms to the category specified by the flags.
 *
 * @note This function can be used to check if the code points in a string are part
 *       of a category. Valid flags are part of the UTF8C_* list of defines.
 *       The category for a code point is defined as part of the entry in
 *       UnicodeData.txt, the data file for the Unicode code point database.
 * @note Flags parameter must be a combination of UTF8C_* flags or a single UTF8C_IS* flag.
 *       In order to main backwards compatibility with POSIX functions like `isdigit`
 *       and `isspace`, compatibility flags have been provided. Note, however, that
 *       the result is only guaranteed to be correct for code points in the Basic
 *       Latin range, between U+0000 and 0+007F. Combining a compatibility flag with
 *       a regular category flag will result in undefined behavior.
 * @note The function is greedy. This means it will try to match as many code
 *       points with the matching category flags as possible and return the offset in
 *       the input in bytes.
 *
 * @param input         The string to check
 * @param input_size    Size of the string, use 1 to check one character regardless its size
 * @param flags         Requested category, see UTF8C_* flags
 * @param output_size   Number of bytes in the input that conform to the specified
 *                      category flags
 * @return              True if the whole input of `input_size` conforms to the specified
 *                      category flags, false otherwise
 */
native bool:is_string_category(const input[], input_size, flags, &output_size = 0);

/**
 * Returns whether a character is numeric.
 *
 * @note Multi-byte characters will always return false.
 *
 * @param ch			Character to test.
 * @return				True if character is numeric, otherwise false.
 */
native isdigit(ch);

/**
 * Returns whether a character is an ASCII alphabet character.
 *
 * @note Multi-byte characters will always return false.
 *
 * @param ch			Character to test.
 * @return				True if character is alphabetical, otherwise false.
 */
native isalpha(ch);

/**
 * Returns whether a character is whitespace.
 *
 * @note Multi-byte characters will always return false.
 *
 * @param ch			Character to test.
 * @return				True if character is whitespace, otherwise false.
 */
native isspace(ch);

/**
 * Returns whether a character is numeric or an ASCII alphabet character.
 *
 * @note Multi-byte characters will always return false.
 *
 * @param ch			Character to test.
 * @return				True if character is numeric, otherwise false.
 */
native isalnum(ch);

/**
 * Returns if a character is multi-byte or not.
 *
 * @note Only available in 1.8.3 and above.
 *
 * @param ch			Character to test.
 * @return				0 for a normal 7-bit ASCII character,
 *						otherwise number of bytes in multi-byte character.
 */
native is_char_mb(ch);

/**
 * Returns whether an alphabetic character is uppercase.
 *
 * @note Only available in 1.8.3 and above.
 * @note Multi-byte characters will always return false.
 *
 * @param ch			Character to test.
 * @return				True if character is uppercase, otherwise false.
 */
native bool:is_char_upper(ch);

/**
 * Returns whether an alphabetic character is lowercase.
 *
 * @note Only available in 1.8.3 and above.
 * @note Multi-byte characters will always return false.
 *
 * @param ch			Character to test.
 * @return				True if character is lowercase, otherwise false.
 */
native bool:is_char_lower(ch);

/** 
 * Returns the number of bytes a character is using.  This is
 * for multi-byte characters (UTF-8).  For normal ASCII characters,
 * this will return 1.
 *
 * @note Only available in 1.8.3 and above.
 *
 * @param source		Source input string.
 * @return				Number of bytes the current character uses.
 */
native get_char_bytes(const source[]);

/**
 * Concatenates one string onto another.
 *
 * @param dest			String to append to.
 * @param source		Source string to concatenate.
 * @param maxlength		Maximum length of entire buffer.
 * @return				Number of bytes written.
 */
native strcat(dest[], const source[], maxlength);

/**
 * Tests whether a string is found inside another string.
 *
 * @note This supports multi-byte characters (UTF-8) on case insensitive comparison.
 *
 * @param string		String to search in.
 * @param sub			Substring to find inside the original string.
 * @param ignorecase	If true, search is case insensitive.
 *						If false (default), search is case sensitive.
 * @param pos			Start position to search from.
 * @return				-1 on failure (no match found). Any other value
 *						indicates a position in the string where the match starts.
 */
native strfind(const string[], const sub[], bool:ignorecase = false, pos = 0);

/**
 * Compares two strings lexographically.
 *
 * @note This supports multi-byte characters (UTF-8) on case insensitive comparison.
 *
 * @param string1		First string (left).
 * @param string2		Second string (right).
 * @param ignorecase	If true, comparison is case insensitive.
 *						If false (default), comparison is case sensitive.
 * @return				-1 if string1 < string2
 *						0 if string1 == string2
 *						1 if string1 > string2
 */
native strcmp(const string1[], const string2[], bool:ignorecase = false);

/**
 * Compares two strings parts lexographically.
 *
 * @note Only available in 1.8.3 and above.
 * @note This supports multi-byte characters (UTF-8) on case insensitive comparison.
 *
 * @param string1		First string (left).
 * @param string2		Second string (right).
 * @param num			Number of characters to compare.
 * @param ignorecase	If true, comparison is case insensitive.
 *						If false (default), comparison is case sensitive.
 * @return				-1 if string1 < string2
 *						0 if string1 == string2
 *						1 if string1 > string2
 */
native strncmp(const string1[], const string2[], num, bool:ignorecase = false);

/**
 * Parses an argument string to find the first argument. You can use this to
 * replace strbreak().
 *
 * @note Only available in 1.8.3 and above.
 *
 * @note You can use argparse() to break a string into all of its arguments:
 *       new arg[N], pos;
 *       while (true) {
 *         pos = argparse(string, pos, arg, sizeof(arg) - 1);
 *         if (pos == -1)
 *           break;
 *       }
 *
 * @note All initial whitespace is removed. Remaining characters are read until an
 *       argument separator is encountered. A separator is any whitespace not inside
 *       a double-quotation pair (i.e. "x b" is one argument). If only one quotation
 *       mark appears, argparse() acts as if one existed at the end of the string.
 *       Quotation marks are never written back, and do not act as separators. For
 *       example, "a""b""c" will return "abc". An empty quote pair ("") will count
 *       as an argument containing no characters.
 *
 * @note argparse() will write an empty string to argbuffer if no argument is found.
 *
 * @param text          String to tokenize.
 * @param pos           Position to start parsing from.
 * @param argbuffer     Buffer to store first argument.
 * @param maxlen        Size of the buffer.
 * @return              If no argument was found, -1 is returned. Otherwise,
 *                      the index to the next position to parse from is
 *                      returned. This might be the very end of the string.
 */
native argparse(const text[], pos, argbuffer[], maxlen);

/**
 * Returns text in a string up until a certain character sequence is reached.
 *
 * @note Only available in 1.8.3 and above.
 *
 * @param source		Source input string.
 * @param split			A string which specifies a search point to break at.
 * @param part			Buffer to store string part.
 * @param partLen		Maximum length of the string part buffer.
 *
 * @return				-1 if no match was found; otherwise, an index into source
 *						marking the first index after the searched text.  The
 *						index is always relative to the start of the input string.
 */
native split_string(const source[], const split[], part[], partLen);


// Always keep this at the bottom of this file.
#include <string_stocks>
