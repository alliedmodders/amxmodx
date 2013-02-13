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
