// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Regular Expressions Module
//

#include "amxxmodule.h"
#include <string.h>
#include "utils.h"

#if defined(WIN32)
	#define strcasecmp	 stricmp
	#define strncasecmp	_strnicmp
#endif

int UTIL_CheckValidChar(char *c)
{
	int count;
	int bytecount = 0;

	for (count = 1; (*c & 0xC0) == 0x80; count++)
	{
		c--;
	}

	switch (*c & 0xF0)
	{
	case 0xC0:
	case 0xD0:
	{
				 bytecount = 2;
				 break;
	}
	case 0xE0:
	{
				 bytecount = 3;
				 break;
	}
	case 0xF0:
	{
				 bytecount = 4;
				 break;
	}
	}

	if (bytecount != count)
	{
		return count;
	}

	return 0;
}

unsigned int strncopy(char *dest, const char *src, size_t count)
{
	if (!count)
	{
		return 0;
	}

	char *start = dest;
	while ((*src) && (--count))
	{
		*dest++ = *src++;
	}
	*dest = '\0';

	return (dest - start);
}

/**
 * NOTE: Do not edit this for the love of god unless you have
 * read the test cases and understand the code behind each one.
 * While I don't guarantee there aren't mistakes, I do guarantee
 * that plugins will end up relying on tiny idiosyncrasies of this
 * function, just like they did with AMX Mod X.
 *
 * There are explicitly more cases than the AMX Mod X version because
 * we're not doing a blind copy.  Each case is specifically optimized
 * for what needs to be done.  Even better, we don't have to error on
 * bad buffer sizes.  Instead, this function will smartly cut off the
 * string in a way that pushes old data out.
 */
char *UTIL_ReplaceEx(char *subject, size_t maxLen, const char *search, size_t searchLen, const char *replace, size_t replaceLen, bool caseSensitive)
{
	char *ptr = subject;
	size_t browsed = 0;
	size_t textLen = strlen(subject);

	/* It's not possible to search or replace */
	if (searchLen > textLen)
	{
		return NULL;
	}

	/* Handle the case of one byte replacement.
	* It's only valid in one case.
	*/
	if (maxLen == 1)
	{
		/* If the search matches and the replace length is 0,
		* we can just terminate the string and be done.
		*/
		if ((caseSensitive ? strcmp(subject, search) : strcasecmp(subject, search)) == 0 && replaceLen == 0)
		{
			*subject = '\0';
			return subject;
		}
		else
		{
			return NULL;
		}
	}

	/* Subtract one off the maxlength so we can include the null terminator */
	maxLen--;

	while (*ptr != '\0' && (browsed <= textLen - searchLen))
	{
		/* See if we get a comparison */
		if ((caseSensitive ? strncmp(ptr, search, searchLen) : strncasecmp(ptr, search, searchLen)) == 0)
		{
			if (replaceLen > searchLen)
			{
				/* First, see if we have enough space to do this operation */
				if (maxLen - textLen < replaceLen - searchLen)
				{
					/* First, see if the replacement length goes out of bounds. */
					if (browsed + replaceLen >= maxLen)
					{
						/* EXAMPLE CASE:
						* Subject: AABBBCCC
						* Buffer : 12 bytes
						* Search : BBB
						* Replace: DDDDDDDDDD
						* OUTPUT : AADDDDDDDDD
						* POSITION:           ^
						*/
						/* If it does, we'll just bound the length and do a strcpy. */
						replaceLen = maxLen - browsed;

						/* Note, we add one to the final result for the null terminator */
						strncopy(ptr, replace, replaceLen + 1);

						/* Don't truncate a multi-byte character */
						if (*(ptr + replaceLen - 1) & 1 << 7)
						{
							replaceLen -= UTIL_CheckValidChar(ptr + replaceLen - 1);
							*(ptr + replaceLen) = '\0';
						}
					}
					else
					{
						/* EXAMPLE CASE:
						* Subject: AABBBCCC
						* Buffer : 12 bytes
						* Search : BBB
						* Replace: DDDDDDD
						* OUTPUT : AADDDDDDDCC
						* POSITION:         ^
						*/
						/* We're going to have some bytes left over... */
						size_t origBytesToCopy = (textLen - (browsed + searchLen)) + 1;
						size_t realBytesToCopy = (maxLen - (browsed + replaceLen)) + 1;
						char *moveFrom = ptr + searchLen + (origBytesToCopy - realBytesToCopy);
						char *moveTo = ptr + replaceLen;

						/* First, move our old data out of the way. */
						memmove(moveTo, moveFrom, realBytesToCopy);

						/* Now, do our replacement. */
						memcpy(ptr, replace, replaceLen);
					}
				}
				else
				{
					/* EXAMPLE CASE:
					* Subject: AABBBCCC
					* Buffer : 12 bytes
					* Search : BBB
					* Replace: DDDD
					* OUTPUT : AADDDDCCC
					* POSITION:      ^
					*/
					/* Yes, we have enough space.  Do a normal move operation. */
					char *moveFrom = ptr + searchLen;
					char *moveTo = ptr + replaceLen;

					/* First move our old data out of the way. */
					size_t bytesToCopy = (textLen - (browsed + searchLen)) + 1;
					memmove(moveTo, moveFrom, bytesToCopy);

					/* Now do our replacement. */
					memcpy(ptr, replace, replaceLen);
				}
			}
			else if (replaceLen < searchLen)
			{
				/* EXAMPLE CASE:
				* Subject: AABBBCCC
				* Buffer : 12 bytes
				* Search : BBB
				* Replace: D
				* OUTPUT : AADCCC
				* POSITION:   ^
				*/
				/* If the replacement does not grow the string length, we do not
				* need to do any fancy checking at all.  Yay!
				*/
				char *moveFrom = ptr + searchLen;		/* Start after the search pointer */
				char *moveTo = ptr + replaceLen;		/* Copy to where the replacement ends */

				/* Copy our replacement in, if any */
				if (replaceLen)
				{
					memcpy(ptr, replace, replaceLen);
				}

				/* Figure out how many bytes to move down, including null terminator */
				size_t bytesToCopy = (textLen - (browsed + searchLen)) + 1;

				/* Move the rest of the string down */
				memmove(moveTo, moveFrom, bytesToCopy);
			}
			else
			{
				/* EXAMPLE CASE:
				* Subject: AABBBCCC
				* Buffer : 12 bytes
				* Search : BBB
				* Replace: DDD
				* OUTPUT : AADDDCCC
				* POSITION:     ^
				*/
				/* We don't have to move anything around, just do a straight copy */
				memcpy(ptr, replace, replaceLen);
			}

			return ptr + replaceLen;
		}
		ptr++;
		browsed++;
	}

	return NULL;
}
