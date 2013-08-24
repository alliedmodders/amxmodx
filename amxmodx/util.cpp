/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include <time.h>
#include "amxmodx.h"

#if ( defined(__linux__) || defined(__APPLE__) ) && !defined _vsnprintf
	#define _vsnprintf vsnprintf
#endif

char *UTIL_VarArgs(const char *fmt, ...)
{
	va_list ap;
	static char string[4096];

	va_start(ap, fmt);
	_vsnprintf(string, sizeof(string)-1, fmt, ap);
	va_end(ap);

	return string;
}

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

	while (*n)
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
	WRITE_BYTE(0);
	WRITE_BYTE(255);
	WRITE_BYTE(255);
	WRITE_BYTE(250);
	WRITE_BYTE(0);
	WRITE_SHORT(FixedUnsigned16(textparms.fadeinTime, (1<<8)));
	WRITE_SHORT(FixedUnsigned16(textparms.fadeoutTime, (1<<8)));
	WRITE_SHORT(FixedUnsigned16(textparms.holdTime, (1<<8)));
	
	if (textparms.effect == 2)
		WRITE_SHORT(FixedUnsigned16(textparms.fxTime, (1<<8)));
	
	WRITE_STRING(pMessage);
	MESSAGE_END();
}

/* warning - buffer of msg must be longer than 190 chars!
(here in AMX it is always longer) */
void UTIL_ClientPrint(edict_t *pEntity, int msg_dest, char *msg)
{
	if (!gmsgTextMsg)
		return;				// :TODO: Maybe output a warning log?

	char c = msg[190];
	msg[190] = 0;			// truncate without checking with strlen()
	
	if (pEntity)
		MESSAGE_BEGIN(MSG_ONE, gmsgTextMsg, NULL, pEntity);
	else
		MESSAGE_BEGIN(MSG_BROADCAST, gmsgTextMsg);
	
	WRITE_BYTE(msg_dest);
	WRITE_STRING(msg);
	MESSAGE_END();
	msg[190] = c;
}

void UTIL_ClientSayText(edict_t *pEntity, int sender, char *msg)
{
	if (!gmsgSayText)
		return;				// :TODO: Maybe output a warning log?

	char c = msg[190];
	msg[190] = 0;			// truncate without checking with strlen()

	MESSAGE_BEGIN(MSG_ONE, gmsgSayText, NULL, pEntity);
	WRITE_BYTE(sender);
	WRITE_STRING(msg);
	MESSAGE_END();
	msg[190] = c;
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
void UTIL_FakeClientCommand(edict_t *pEdict, const char *cmd, const char *arg1, const char *arg2)
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
		snprintf(g_fakecmd.args, 255, "%s %s", arg1, arg2);
		// if snprintf reached 255 chars limit, this will make sure there will be no access violation
		g_fakecmd.args[255] = 0;
	}
	else if (arg1)
	{								// only one argument passed
		g_fakecmd.argc = 2;			// 1 argument + 1 command
		// store argument
		g_fakecmd.argv[1] = arg1;
		// build argument line
		snprintf(g_fakecmd.args, 255, "%s", arg1);
		// if snprintf reached 255 chars limit, this will make sure there will be no access violation
		g_fakecmd.args[255] = 0;
	}
	else
		g_fakecmd.argc = 1;			// no argmuents -> only one command

	// set the global "fake" flag so the Cmd_Arg* functions will be superceded
	g_fakecmd.fake = true;
	// tell the GameDLL that the client sent a command
	MDLL_ClientCommand(pEdict);
	// unset the global "fake" flag
	g_fakecmd.fake = false;
}

