// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// String Manipulation Stocks
//

#if defined _string_stocks_included
  #endinput
#endif
#define _string_stocks_included

#if !defined _string_included
	#include <string>
#endif

/**
 * @global  Unless otherwise noted, all string functions which take in a
 *          writable buffer and maximum length should NOT have the null terminator INCLUDED
 *          in the length.  This means that this is valid:
 *          copy(string, charsmax(string), ...)
 */

/**
 * Returns whether a given string contains only digits.
 * This returns false for zero-length strings.
 *
 * @param sString       Character to test.
 * @return              True if string contains only digit, otherwise false.
 */
stock bool:is_str_num(const sString[])
{
	new i = 0;

	while (sString[i] && isdigit(sString[i]))
	{
		++i;
	}

	return sString[i] == 0 && i != 0;
}

/**
 * Returns an uppercase character to a lowercase character.
 *
 * @note Only available in 1.8.3 and above.
 *
 * @param chr           Characer to convert.
 * @return              Lowercase character on success,
 *                      no change on failure.
 */
stock char_to_upper(chr)
{
	if (is_char_lower(chr))
	{
		return (chr & ~(1<<5));
	}

	return chr;
}

/**
 * Returns a lowercase character to an uppercase character.
 *
 * @note Only available in 1.8.3 and above.
 *
 * @param chr           Characer to convert.
 * @return              Uppercase character on success,
 *                      no change on failure.
 */
stock char_to_lower(chr)
{
	if (is_char_upper(chr))
	{
		return (chr | (1<<5));
	}

	return chr;
}

/**
 * Backwards compatibility stock - use argbreak or argparse.
 * @deprecated          this function does not work properly.
 */
#pragma deprecated Use argbreak() instead
stock strbreak(const text[], Left[], leftLen, Right[], rightLen)
{
	return argbreak(text, Left, leftLen, Right, rightLen);
}

/**
 * Emulates strbreak() using argparse().
 *
 * @param text          Source input string.
 * @param left          Buffer to store string left part.
 * @param leftlen       Maximum length of the string part buffer.
 * @param right         Buffer to store string right part.
 * @param rightlen      Maximum length of the string part buffer.
 *
 * @return              -1 if no match was found; otherwise, an index into source
 *                      marking the first index after the searched text.  The
 *                      index is always relative to the start of the input string.
 */
stock argbreak(const text[], left[], leftlen, right[], rightlen)
{
	new pos = argparse(text, 0, left, leftlen);

	if (pos == -1)
	{
		return -1;
	}

	new textlen = strlen(text);

	while (pos < textlen && isspace(text[pos]))
	{
		pos++;
	}

	copy(right, rightlen, text[pos]);

	return pos;
}

/**
 * It is basically strbreak but you have a delimiter that is more than one character in length. By Suicid3.
 *
 * @param szInput       Source input string.
 * @param szLeft        Buffer to store left string part.
 * @param pL_Max        Maximum length of the string part buffer.
 * @param szRight       Buffer to store right string part.
 * @param pR_Max        Maximum length of the string part buffer.
 * @param szDelim       A string which specifies a search point to break at.
 *
 * @noreturn
 */
stock split(const szInput[], szLeft[], pL_Max, szRight[], pR_Max, const szDelim[])
{
	new iEnd = contain(szInput, szDelim);
	new iStart = iEnd + strlen(szDelim);

	// If delimiter isnt in Input just split the string at max lengths
	if (iEnd == -1)
	{
		iStart = copy(szLeft, pL_Max, szInput);
		copy(szRight, pR_Max, szInput[iStart]);
		return;
	}

	// If delimter is in Input then split at input for max lengths
	if (pL_Max >= iEnd)
		copy(szLeft, iEnd, szInput);
	else
		copy(szLeft, pL_Max, szInput);

	copy(szRight, pR_Max, szInput[iStart]);
}

/**
 * Removes a path from szFilePath leaving the name of the file in szFile for a pMax length.
 *
 * @param szFilePath    String to perform search and replacements on.
 * @param szFile        Buffer to store file name.
 * @param pMax          Maximum length of the string buffer.
 *
 * @noreturn
 */
stock remove_filepath(const szFilePath[], szFile[], pMax)
{
	new len = strlen(szFilePath);

	while ((--len >= 0) && (szFilePath[len] != '/') && (szFilePath[len] != '\')) { }

	copy(szFile, pMax, szFilePath[len + 1]);

	return;
}

/**
 * Replaces a contained string iteratively.
 *
 * @note Consider using replace_string() instead.
 *
 * @note This ensures that no infinite replacements will take place by
 *       intelligently moving to the next string position each iteration.
 *
 * @param string    String to perform search and replacements on.
 * @param len       Maximum length of the string buffer.
 * @param what      String to search for.
 * @param with      String to replace the search string with.
 *
 * @return          Number of replacements on success, otherwise 0.
 */
stock replace_all(string[], len, const what[], const with[])
{
	new pos = 0;

	if ((pos = contain(string, what)) == -1)
	{
		return 0;
	}

	new total = 0;
	new with_len = strlen(with);
	new diff = strlen(what) - with_len;
	new total_len = strlen(string);
	new temp_pos = 0;

	while (replace(string[pos], len - pos, what, with) != 0)
	{
		total++;

		/* jump to position after replacement */
		pos += with_len;

		/* update cached length of string */
		total_len -= diff;

		/* will the next call be operating on the last character? */
		if (pos >= total_len)
		{
			break;
		}

		/* find the next position from our offset */
		temp_pos = contain(string[pos], what);

		/* if it's invalid, we're done */
		if (temp_pos == -1)
		{
			break;
		}

		/* otherwise, reposition and update counters */
		pos += temp_pos;
	}

	return total;
}

/**
 * Breaks a string into pieces and stores each piece into an array of buffers.
 *
 * @param text              The string to split.
 * @param split             The string to use as a split delimiter.
 * @param buffers           An array of string buffers (2D array).
 * @param maxStrings        Number of string buffers (first dimension size).
 * @param maxStringLength   Maximum length of each string buffer.
 * @param copyRemainder     False (default) discard excess pieces, true to ignore
 *                          delimiters after last piece.
 * @return                  Number of strings retrieved.
 */
stock explode_string(const text[], const split[], buffers[][], maxStrings, maxStringLength, bool:copyRemainder = false)
{
	new reloc_idx, idx, total;

	if (maxStrings < 1 || !split[0])
	{
		return 0;
	}

	while ((idx = split_string(text[reloc_idx], split, buffers[total], maxStringLength)) != -1)
	{
		reloc_idx += idx;
		if (++total == maxStrings)
		{
			if (copyRemainder)
			{
				copy(buffers[total-1], maxStringLength, text[reloc_idx-idx]);
			}
			return total;
		}
	}

	copy(buffers[total++], maxStringLength, text[reloc_idx]);

	return total;
}

/**
 * Joins an array of strings into one string, with a "join" string inserted in
 * between each given string.  This function complements ExplodeString.
 *
 * @param strings       An array of strings.
 * @param numStrings    Number of strings in the array.
 * @param join          The join string to insert between each string.
 * @param buffer        Output buffer to write the joined string to.
 * @param maxLength     Maximum length of the output buffer.
 * @return              Number of bytes written to the output buffer.
 */
stock implode_strings(const strings[][], numStrings, const join[], buffer[], maxLength)
{
	new total, length, part_length;
	new join_length = strlen(join);

	for (new i=0; i<numStrings; i++)
	{
		length = copy(buffer[total], maxLength-total, strings[i]);
		total += length;

		if (length < part_length)
		{
			break;
		}

		if (i != numStrings - 1)
		{
			length = copy(buffer[total], maxLength-total, join);
			total += length;

			if (length < join_length)
			{
				break;
			}
		}
	}

	return total;
}
