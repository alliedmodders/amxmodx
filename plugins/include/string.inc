/* Strings manipulation
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

#if defined _string_included
  #endinput
#endif
#define _string_included

#define charsmax(%1) (sizeof(%1)-1)

/* Checks if source contains string. On success function
* returns position in source, on failure returns -1. */
native contain(const source[],const string[]);

/* Checks if source contains string with case ignoring. On success function
* returns position in source, on failure returns -1. */
native containi(const source[],const string[]);

/* Replaces given string to another in given text. */
native replace(text[], len, const what[], const with[]);

/* Adds one string to another. Last parameter different from 0, specifies
* how many chars we want to add. Function returns number of all merged chars. */
native add(dest[],len,const src[],max=0);

/* Fills string with given format and parameters.
 * Function returns number of copied chars.
 * Example: format(dest,"Hello %s. You are %d years old","Tom",17).
 * If any of your input buffers overlap with the destination buffer,
 *  format() falls back to a "copy-back" version as of 1.65.  This is 
 *  slower, so you should using a source string that is the same as
 *  the destination.
 */
native format(output[] ,len ,const format[] , any:...);

/* Same as format(), except does not perform a "copy back" check.
 * This means formatex() is faster, but DOES NOT ALLOW this type
 * of call:
 *  formatex(buffer, len, "%s", buffer)
 *  formatex(buffer, len, buffer, buffer)
 *  formatex(buffer, len, "%s", buffer[5])
 * This is because the output is directly stored into "buffer", 
 *  rather than copied back at the end.
 */
native formatex(output[] ,len ,const format[] , any:...);

/* Replacement for format_args.  Much faster and %L compatible.
 * This works exactly like vsnprintf() from C.
 * You must pass in the output buffer and its size,
 *  the string to format, and the number of the FIRST variable
 *  argument parameter.  For example, for:
 *  function (a, b, c, ...)
 *  You would pass 4 (a is 1, b is 2, c is 3, et cetera).
 * There is no vformatex().
 */
native vformat(buffer[], len, const fmt[], vararg);

/*
 * Same as vformat(), except works in normal style dynamic natives.
 * Instead of passing the format arg string, you can only pass the 
 *  actual format argument number itself.
 * If you pass 0, it will read the format string from an optional 
 *  fifth parameter.
 */
native vdformat(buffer[], len, fmt_arg, vararg, ...);

/* Gets parameters from function as formated string. */
native format_args(output[] ,len ,pos = 0);

/* Converts number to string. */
native num_to_str(num,string[],len);

/* Returns converted string to number. */
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
 *                  On failure, position is sets always to -1.
 * @param base      The numerical base (radix) that determines the valid characters and their interpretation.
 *                  If this is 0, the base used is determined by the format in the sequence.
 * @return          On success, the function returns the converted integral number as integer value.
 *                  If no valid conversion could be performed, a zero value is returned.
 *                  If the value read is out of the range of representable values by a cell, 
 *                  the function returns 'cellmin' or 'cellmax'.
 */
native strtol(const string[], &endPos = -1, base = 0);

/* Converts float to string. */
native float_to_str(Float:fl, string[], len);

/* Parses a float. */
native Float:str_to_float(const string[]);

/* Checks if two strings equal. If len var is set
* then there are only c chars comapred. */
native equal(const a[],const b[],c=0);

/* Checks if two strings equal with case ignoring. 
* If len var is set then there are only c chars comapred. */
native equali(const a[],const b[],c=0);

/* Copies one string to another. By len var
*  you may specify max. number of chars to copy. */
native copy(dest[],len,const src[]);

/* Copies one string to another until char ch is found. 
*  By len var you may specify max. number of chars to copy. */
native copyc(dest[],len,const src[],ch);

/* Sets string with given character. */
native setc(src[],len,ch);

/* Gets parameters from text.
* Example: to split text: "^"This is^" the best year",
* call function like this: parse(text,arg1,len1,arg2,len2,arg3,len3,arg4,len4)
* and you will get: "This is", "the", "best", "year"
* Function returns number of parsed parameters. */
native parse(const text[], ... );

/* Breaks a string into two halves, by token.
   See strbreak() for doing this with parameters.
   Example:
   str1[] = This *is*some text
   strtok(str1, left, 24, right, 24, '*')
   left will be "This "
   Right will be "is*some text"
   If you use trimSpaces, all spaces are trimmed from Left.
*/
native strtok(const text[], Left[], leftLen, Right[], rightLen, token=' ', trimSpaces=0);
   
/**
 * Below are the trim flags for strtok2
 *
 * You can specify how the left and right buffers will
 * be trimmed by strtok2. LTRIM trims spaces from the
 * left side. RTRIM trims from the right side.
 *
 * The defines TRIM_INNER, TRIM_OUTER and TRIM_FULL are
 * shorthands for commonly used flag combinations.
 *
 * When the initial string is trimmed, using TRIM_INNER
 * for all subsequent strtok2 calls will ensure that left
 * and right are always trimmed from both sides.
 *
 * Examples:
 * str1[] = "  This is  *  some text  "
 * strtok2(str1, left, 24, right, 24, '*', TRIM_FULL)
 *  left will be "This is", right will be "some text"
 *
 * str2[] = "  Here is  |  an  | example  "
 * trim(str2)
 * strtok2(str2, left, 24, right, 24, '|', TRIM_INNER)
 *  left will be "Here is", right will be "an  | example"
 * strtok2(right, left, 24, right, 24, '|', TRIM_INNER)
 *  left will be "an", right will be "example"
 *
 * str3[] = "  One  -  more  "
 * strtok2(str3, left, 24, right, 24, '-', TRIM_OUTER)
 *  left will be "One  ", right will be "  more"
 *
 * str4[] = "  Final  .  example  "
 * strtok2(str4, left, 24, right, 24, '.', LTRIM_LEFT|LTRIM_RIGHT)
 *  left will be "Final  ", right will be "example  "
*/
#define LTRIM_LEFT (1<<0)
#define RTRIM_LEFT (1<<1)
#define LTRIM_RIGHT (1<<2)
#define RTRIM_RIGHT (1<<3)

#define TRIM_INNER RTRIM_LEFT|LTRIM_RIGHT
#define TRIM_OUTER LTRIM_LEFT|RTRIM_RIGHT
#define TRIM_FULL TRIM_OUTER|TRIM_INNER

/**
 * Breaks a string in two by token
 *
 * Only available in 1.8.3 and above
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

/* Gets parameters from text one at a time
   It breaks a string into the first parameter and the rest of the parameters
   (A left side and right side of the string)
   Example: to split text: "^"This is^" the best year",
   strbreak(text, arg1, len1, arg2, len2)
   arg1="This is", arg2=the best year
   This is more useful than parse() because you can keep breaking
   any number of arguments */
native strbreak(const text[], Left[], leftLen, Right[], rightLen);

/* Strips spaces from the beginning and end of a string. */
native trim(text[]);

/* Converts all chars in string to lower case. */
native strtolower(string[]);

/* Converts all chars in string to upper case. */
native strtoupper(string[]);

/* Make a string's first character uppercase */
native ucfirst(string[]);

/* Returns true when value is digit. */
native isdigit(ch);

/* Returns true when value is letter. */
native isalpha(ch);

/* Returns true when value is space. */
native isspace(ch);

/* Returns true when value is letter or digit. */
native isalnum(ch);

/* Concatenates a string.  Maxlength is the total buffer of the destination. */
native strcat(dest[], const source[], maxlength);

/* Finds a string in another string.  Returns -1 if not found. */
native strfind(const string[], const sub[], ignorecase=0, pos=0);

/* Compares two strings with the C function strcmp().  Returns 0 on equal. */
native strcmp(const string1[], const string2[], ignorecase=0);

/* Tests if given string contains only digits. Also, returns false for zero-length strings. */
stock bool:is_str_num(const sString[])
{
	new i = 0;
	
	while (sString[i] && isdigit(sString[i]))
		++i;
	
	return sString[i] == 0 && i != 0;
}

/* It is basically strbreak but you have a delimiter that is more than one character in length.
   You pass the Input string, the Left output, the max length of the left output,
   the right output , the max right length, and then the delimiter string.
   By Suicid3
*/
stock split(const szInput[], szLeft[], pL_Max, szRight[], pR_Max, const szDelim[])
{
	new iEnd = contain(szInput, szDelim);
	new iStart = iEnd + strlen(szDelim);

	//If delimiter isnt in Input just split the string at max lengths
	if (iEnd == -1)
	{
		iStart = copy(szLeft, pL_Max, szInput);
		copy(szRight, pR_Max, szInput[iStart]);
		return;
	}

	//If delimter is in Input then split at input for max lengths
	if (pL_Max >= iEnd)
		copy(szLeft, iEnd, szInput);
	else
		copy(szLeft, pL_Max, szInput);

	copy(szRight, pR_Max, szInput[iStart]);

	return;
}

/* Removes a path from szFilePath leaving the name of the file in szFile for a pMax length. */
stock remove_filepath(const szFilePath[], szFile[], pMax)
{
	new len = strlen(szFilePath);

	while ((--len >= 0) && (szFilePath[len] != '/') && (szFilePath[len] != '\')) { }

	copy(szFile, pMax, szFilePath[len + 1]);

	return;
}

/* Replaces a contained string iteratively.
 * This ensures that no infinite replacements will take place by
 *  intelligently moving to the next string position each iteration.
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
