// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <time.h>
#include "amxmodx.h"
#include <utf8rewind.h>

int UTIL_ReadFlags(const char* c) 
{
	int flags = 0;
	
	while (*c)
		flags |= (1<<(*c++ - 'a'));
	
	return flags;
}

void UTIL_GetFlags(char* f, int a)
{
	for (int i = 'a'; i <= 'z'; ++i)
	{
		if (a & 1) *f++ = i;
		a >>= 1;
	}

	*f = 0;
}

/* warning - don't pass here const string */
void UTIL_ShowMenu(edict_t* pEdict, int slots, int time, char *menu, int mlen)
{
	char *n = menu;
	char c = 0;
	int a;

	if (!gmsgShowMenu)
		return;			// some games don't support ShowMenu (Firearms)

	do
	{
		a = mlen;
		if (a > 175) a = 175;
		mlen -= a;
		c = *(n+=a);
		*n = 0;
		
		MESSAGE_BEGIN(MSG_ONE, gmsgShowMenu, NULL, pEdict);
		WRITE_SHORT(slots);
		WRITE_CHAR(time);
		WRITE_BYTE(c ? TRUE : FALSE);
		WRITE_STRING(menu);
		MESSAGE_END();
		*n = c;
		menu = n;
	}
	while (*n);
}

/* warning - don't pass here const string */
void UTIL_ShowMOTD(edict_t *client, char *motd, int mlen, const char *name)
{
	if (!gmsgMOTD)
		return;				// :TODO: Maybe output a warning log?

	if (gmsgServerName)
	{
		MESSAGE_BEGIN(MSG_ONE, gmsgServerName, NULL, client);
		WRITE_STRING(name);
		MESSAGE_END();
	}

	char *n = motd;
	char c = 0;
	int a;

	while (*n)
	{
		a = mlen;
		if (a > 175) a = 175;
		mlen -= a;
		c = *(n += a);
		*n = 0;
		
		MESSAGE_BEGIN(MSG_ONE, gmsgMOTD, NULL, client);
		WRITE_BYTE(c ? FALSE : TRUE);
		WRITE_STRING(motd);
		MESSAGE_END();
		*n = c;
		motd = n;
	}

	if (gmsgServerName)
	{
		MESSAGE_BEGIN(MSG_ONE, gmsgServerName, NULL, client);
		WRITE_STRING(hostname->string);
		MESSAGE_END();
	}
}

void UTIL_IntToString(int value, char *output)
{
	static const char *words[] = 
		{"zero ","one ","two ","three ","four ",
		"five ", "six ","seven ","eight ","nine ","ten ",
		"eleven ","twelve ","thirteen ","fourteen ","fifteen ",
		"sixteen ","seventeen ","eighteen ","nineteen ",
		"twenty ","thirty ","fourty ", "fifty ","sixty ",
		"seventy ","eighty ","ninety ",
		"hundred ","thousand "};
	
	*output = 0;
	if (value < 0) value = -value;
	int tho = value / 1000;
	int aaa = 0;
	
	if (tho)
	{
		aaa += sprintf(&output[aaa], "%s", words[tho]);
		aaa += sprintf(&output[aaa], "%s", words[29]);
		value = value % 1000;
	}

	int hun = value / 100;
	
	if (hun)
	{
		aaa += sprintf(&output[aaa], "%s", words[hun]);
		aaa += sprintf(&output[aaa], "%s", words[28]);
		value = value % 100;
	}

	int ten = value / 10;
	int unit = value % 10;
	
	if (ten)
		aaa += sprintf(&output[aaa], "%s", words[(ten > 1) ? (ten + 18) : (unit + 10)]);
	
	if (ten != 1 && (unit || (!value && !hun && !tho))) 
		sprintf(&output[aaa], "%s", words[unit]);
}

char* UTIL_SplitHudMessage(const char *src)
{
	static char message[512];
	short b = 0, d = 0, e = 0, c = -1;

	while (src[d] && e < 480)
	{
		if (src[d] == ' ')
		{
			c = e;
		}
		else if (src[d] == '\n')
		{
			c = -1;
			b = 0;
		}
		
		message[e++] = src[d++];
		
		if (++b == 69)
		{
			if (c == -1)
			{
				message[e++] = '\n';
				b = 0;
			} else {
				message[c] = '\n';
				b = e - c - 1;
				c = -1;
			}
		}
	}

	message[e] = 0;
	return message;
}

unsigned short FixedUnsigned16(float value, float scale)
{
	int output = (int)(value * scale);

	if (output < 0)
		output = 0;
	else if (output > 0xFFFF)
		output = 0xFFFF;

	return (unsigned short)output;
}

short FixedSigned16(float value, float scale)
{
	int output = (int)(value * scale);

	if (output > 32767)
		output = 32767;
	else if (output < -32768)
		output = -32768;

	return (short)output;
}

void UTIL_HudMessage(edict_t *pEntity, const hudtextparms_t &textparms, const char *pMessage)
{
	if (pEntity)
		MESSAGE_BEGIN(MSG_ONE_UNRELIABLE, SVC_TEMPENTITY, NULL, pEntity);
	else
		MESSAGE_BEGIN(MSG_BROADCAST, SVC_TEMPENTITY);

	WRITE_BYTE(29);
	WRITE_BYTE(textparms.channel & 0xFF);
	WRITE_SHORT(FixedSigned16(textparms.x, (1<<13)));
	WRITE_SHORT(FixedSigned16(textparms.y, (1<<13)));
	WRITE_BYTE(textparms.effect);
	WRITE_BYTE(textparms.r1);
	WRITE_BYTE(textparms.g1);
	WRITE_BYTE(textparms.b1);
	WRITE_BYTE(textparms.a1);
	WRITE_BYTE(textparms.r2);
	WRITE_BYTE(textparms.g2);
	WRITE_BYTE(textparms.b2);
	WRITE_BYTE(textparms.a2);
	WRITE_SHORT(FixedUnsigned16(textparms.fadeinTime, (1<<8)));
	WRITE_SHORT(FixedUnsigned16(textparms.fadeoutTime, (1<<8)));
	WRITE_SHORT(FixedUnsigned16(textparms.holdTime, (1<<8)));
	
	if (textparms.effect == 2)
		WRITE_SHORT(FixedUnsigned16(textparms.fxTime, (1<<8)));
	
	WRITE_STRING(pMessage);
	MESSAGE_END();
}

void UTIL_DHudMessage(edict_t *pEntity, const hudtextparms_t &textparms, const char *pMessage, unsigned int length)
{
	#define DRC_CMD_MESSAGE 6

	if (pEntity)
		MESSAGE_BEGIN(MSG_ONE_UNRELIABLE, SVC_DIRECTOR, NULL, pEntity);
	else
		MESSAGE_BEGIN(MSG_BROADCAST, SVC_DIRECTOR);

	WRITE_BYTE(length + 31); // message length (counting WRITE size)
	WRITE_BYTE(DRC_CMD_MESSAGE);
	WRITE_BYTE(textparms.effect);
	WRITE_LONG(textparms.b1 + (textparms.g1 << 8) + (textparms.r1 << 16)); // pack color.
	WRITE_LONG(amx_ftoc(textparms.x));
	WRITE_LONG(amx_ftoc(textparms.y));
	WRITE_LONG(amx_ftoc(textparms.fadeinTime));
	WRITE_LONG(amx_ftoc(textparms.fadeoutTime));
	WRITE_LONG(amx_ftoc(textparms.holdTime));
	WRITE_LONG(amx_ftoc(textparms.fxTime));
	WRITE_STRING(pMessage); // max length: 128. Truncated on the client.
	MESSAGE_END();
}

/**
 * User message size limit: 192 bytes
 * Actual available size: 188 bytes (with EOS)
 */
void UTIL_ClientPrint(edict_t *pEntity, int msg_dest, char *msg)
{
	if (!gmsgTextMsg)
		return;				// :TODO: Maybe output a warning log?

	const auto canUseFormatString = g_official_mod && !g_bmod_dod; // Temporary exclusion for DoD until officially supported
	const auto index = canUseFormatString ? 187 : 190;
	char c = msg[index];
	msg[index] = 0;			// truncate without checking with strlen()
	
	if (pEntity)
		MESSAGE_BEGIN(MSG_ONE, gmsgTextMsg, NULL, pEntity);
	else
		MESSAGE_BEGIN(MSG_BROADCAST, gmsgTextMsg);
	
	WRITE_BYTE(msg_dest);	// 1 byte
	if (canUseFormatString) 
		WRITE_STRING("%s");	// 3 bytes (2 + EOS)
	WRITE_STRING(msg);		// max 188 bytes (187 + EOS)
	MESSAGE_END();			// max 192 bytes
	msg[index] = c;
}

/**
 * User message size limit: 192 bytes
 * Actual available size: 188 bytes (with EOS)
 */
void UTIL_ClientSayText(edict_t *pEntity, int sender, char *msg)
{
	if (!gmsgSayText)
		return;				// :TODO: Maybe output a warning log?

	const auto canUseFormatString = g_official_mod && !g_bmod_dod; // Temporary exclusion for DoD until officially supported
	const auto index = canUseFormatString ? 187 : 190;
	char c = msg[index];
	msg[index] = 0;			// truncate without checking with strlen()

	MESSAGE_BEGIN(MSG_ONE, gmsgSayText, NULL, pEntity);
	WRITE_BYTE(sender);		// 1 byte
	if (canUseFormatString) 
		WRITE_STRING("%s");	// 3 bytes (2 + EOS)
	WRITE_STRING(msg);		// max 188 bytes (187 + EOS)
	MESSAGE_END();			// max 192 bytes
	msg[index] = c;
}

void UTIL_TeamInfo(edict_t *pEntity, int playerIndex, const char *pszTeamName)
{
	if (!gmsgTeamInfo)
		return;

	MESSAGE_BEGIN(MSG_ONE, gmsgTeamInfo, NULL, pEntity);
	WRITE_BYTE(playerIndex);
	WRITE_STRING(pszTeamName);
	MESSAGE_END();
}

// UTIL_FakeClientCommand
// PURPOSE: Sends a fake client command to GameDLL
// HOW DOES IT WORK:
//  1) Stores command and arguments into a global and sets the global "fake" flag to true
//  2) Invokes ClientCommand in GameDLL
//  3) meta_api.cpp overrides Cmd_Args, Cmd_Argv, Cmd_Argc and gives them fake values if the "fake" flag is set
//  4) unsets the global "fake" flag
void UTIL_FakeClientCommand(edict_t *pEdict, const char *cmd, const char *arg1, const char *arg2, bool fwd)
{
	if (!cmd) 
		return;						// no command 

	// store command
	g_fakecmd.argv[0] = cmd;
	// if only arg2 is passed, swap the arguments
	if (!arg1 && arg2)
	{
		arg1 = arg2;
		arg2 = NULL;
	}

	// store arguments
	if (arg2)
	{								// both arguments passed
		g_fakecmd.argc = 3;			// 2 arguments + 1 command
		// store arguments
		g_fakecmd.argv[1] = arg1;
		g_fakecmd.argv[2] = arg2;
		// build argument line
		ke::SafeSprintf(g_fakecmd.args, sizeof(g_fakecmd.args), "%s %s", arg1, arg2);
	}
	else if (arg1)
	{								// only one argument passed
		g_fakecmd.argc = 2;			// 1 argument + 1 command
		// store argument
		g_fakecmd.argv[1] = arg1;
		// build argument line
		ke::SafeSprintf(g_fakecmd.args, sizeof(g_fakecmd.args), "%s", arg1);
	}
	else
		g_fakecmd.argc = 1;			// no argmuents -> only one command

	/* Notify plugins about this command */
	if (fwd)
	{
		/* Set flag so read_argc/v/s functions will give proper value */
		g_fakecmd.notify = true;

		if (executeForwards(FF_ClientCommand, static_cast<cell>(GET_PLAYER_POINTER(pEdict)->index)) > 0)
		{
			g_fakecmd.notify = false;
			return;
		}

		/* check for command and if needed also for first argument and call proper function */
		CmdMngr::iterator aa = g_commands.clcmdprefixbegin(cmd);

		if (!aa)
		{
			aa = g_commands.clcmdbegin();
		}

		while (aa)
		{
			if ((*aa).matchCommandLine(cmd, arg1) && (*aa).getPlugin()->isExecutable((*aa).getFunction()))
			{
				if (executeForwards((*aa).getFunction(), static_cast<cell>(GET_PLAYER_POINTER(pEdict)->index),
					static_cast<cell>((*aa).getFlags()), static_cast<cell>((*aa).getId())) > 0)
				{
					g_fakecmd.notify = false;
					return;
				}	
			}
			++aa;
		}

		/* Unset flag */
		g_fakecmd.notify = false;
	}
	
	// set the global "fake" flag so the Cmd_Arg* functions will be superceded
	g_fakecmd.fake = true;
	// tell the GameDLL that the client sent a command
	MDLL_ClientCommand(pEdict);
	// unset the global "fake" flag
	g_fakecmd.fake = false;
}

unsigned int UTIL_GetUTF8CharBytes(const char *stream)
{
	unsigned char c = *(unsigned char *)stream;
	if (c & (1 << 7))
	{
		if (c & (1 << 5))
		{
			if (c & (1 << 4))
			{
				return 4;
			}
			return 3;
		}
		return 2;
	}
	return 1;
}

template int UTIL_CheckValidChar<char>(char *);
template int UTIL_CheckValidChar<cell>(cell *);

template <typename D>
int UTIL_CheckValidChar(D *c)
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

static char OutputBuffer1[MAX_BUFFER_LENGTH];
static char OutputBuffer2[MAX_BUFFER_LENGTH];

char* utf8stristr(const char *string1, const char *string2)
{
	auto string1Length = utf8casefold(string1, strlen(string1), OutputBuffer1, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
	auto string2Length = utf8casefold(string2, strlen(string2), OutputBuffer2, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

	OutputBuffer1[string1Length] = '\0';
	OutputBuffer2[string2Length] = '\0';

	return strstr(OutputBuffer1, OutputBuffer2);
}

int utf8strncasecmp(const char *string1, const char *string2, size_t n)
{
	auto string1Length = utf8casefold(string1, strlen(string1), OutputBuffer1, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);
	auto string2Length = utf8casefold(string2, strlen(string2), OutputBuffer2, MAX_BUFFER_LENGTH - 1, UTF8_LOCALE_DEFAULT, nullptr, TRUE);

	OutputBuffer1[string1Length] = '\0';
	OutputBuffer2[string2Length] = '\0';

	return n != 0 ? strncmp(OutputBuffer1, OutputBuffer2, n) : strcmp(OutputBuffer1, OutputBuffer2);
}

int utf8strcasecmp(const char *string1, const char *string2)
{
	return utf8strncasecmp(string1, string2, 0);
}

size_t UTIL_ReplaceAll(char *subject, size_t maxlength, const char *search, size_t searchLen, const char *replace, size_t replaceLen, bool caseSensitive)
{
	char *newptr, *ptr = subject;
	unsigned int total = 0;
	while ((newptr = UTIL_ReplaceEx(ptr, maxlength, search, searchLen, replace, replaceLen, caseSensitive)) != NULL)
	{
		total++;
		maxlength -= newptr - ptr;
		ptr = newptr;

		if (*ptr == '\0')
		{
			break;
		}
	}

	return total;
}

size_t UTIL_ReplaceAll(char *subject, size_t maxlength, const char *search, const char *replace, bool caseSensitive)
{
	return UTIL_ReplaceAll(subject, maxlength, search, strlen(search), replace, strlen(replace), caseSensitive);
}

template unsigned int strncopy<char, char>(char *, const char *, size_t);
template unsigned int strncopy<char, cell>(char *, const cell *, size_t);
template unsigned int strncopy<cell, char>(cell *, const char *, size_t);
template unsigned int strncopy<cell, cell>(cell *, const cell *, size_t);

template <typename D, typename S>
unsigned int strncopy(D *dest, const S *src, size_t count)
{
	if (!count)
	{
		return 0;
	}

	D *start = dest;

	while ((*src) && (--count))
	{
		*dest++ = *(unsigned char*)src++;
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
		if ((caseSensitive ? strcmp(subject, search) : utf8strcasecmp(subject, search)) == 0 && replaceLen == 0)
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
		if ((caseSensitive ? strncmp(ptr, search, searchLen) : utf8strncasecmp(ptr, search, searchLen)) == 0)
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

// From Metamod:Source
void UTIL_TrimLeft(char *buffer)
{
	/* Let's think of this as our iterator */
	char *i = buffer;

	/* Make sure the buffer isn't null */
	if (i && *i)
	{
		/* Add up number of whitespace characters */
		while (isspace(static_cast<unsigned char>(*i)))
		{
			i++;
		}

		/* If whitespace chars in buffer then adjust string so first non-whitespace char is at start of buffer */
		if (i != buffer)
		{
			memmove(buffer, i, (strlen(i) + 1) * sizeof(char));
		}
	}
}

void UTIL_TrimRight(char *buffer)
{
	/* Make sure buffer isn't null */
	if (buffer)
	{
		size_t len = strlen(buffer);

		/* Loop through buffer backwards while replacing whitespace chars with null chars */
		for (size_t i = len - 1; i < len; i--)
		{
			if (isspace(static_cast<unsigned char>(buffer[i])))
			{
				buffer[i] = '\0';
			}
			else 
			{
				break;
			}
		}
	}
}