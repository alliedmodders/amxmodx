// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#include "amxxmodule.h"
#include "ns.h"
#include "utilfunctions.h"
#include "CPlayer.h"


/**
 * Scans through all hives and finds the one currently building
 */
int UTIL_FindBuildingHive(void)
{
	edict_t			*Entity=NULL;

	while ((Entity = UTIL_FindEntityByString(Entity,"classname","team_hive")))
	{
		//    is alive                   active                  not fully built
		if (Entity->v.health > 0 && Entity->v.solid > 0 && Entity->v.fuser1 < 1000)
		{
			return ENTINDEX_NEW(Entity);
		}
	}
	return 0;
}

edict_t *UTIL_FindEntityByString(edict_t *Start, const char *Keyword, const char *Value)
{
	edict_t		*Entity;

	Entity=FIND_ENTITY_BY_STRING(Start, Keyword, Value);

	if(!FNullEnt(Entity))
	{
		return Entity;
	}

	return NULL;
}

/**
 * Returns TRUE if the provided native is used in a loaded plugin
 * FALSE otherwise.
 */
BOOL UTIL_CheckForNative(const char *NativeName)
{
	AMX				*amx;
	char			 blah[64];
	int				 FunctionIndex;
	int				 i=0;

	strncpy(blah,NativeName,63);

	// Loop through all running scripts
	while((amx=MF_GetScriptAmx(i++))!=NULL)
	{ 
		// Scan for native
		if (MF_AmxFindNative(amx, blah, &FunctionIndex) == AMX_ERR_NONE)
		{
			// Native was found.
			return TRUE;

		}
	}

	return FALSE; // no native found in any loaded script
}
/**
 * Scans an amxx plugin for a public
 * returns whether or not that public exists
 */
BOOL UTIL_CheckForPublic(const char *publicname)
{
	AMX				*amx;
	char			 blah[64];
	int				 FunctionIndex;
	int				 i=0;

	strncpy(blah,publicname,63);

	// Loop through all running scripts
	while((amx=MF_GetScriptAmx(i++))!=NULL)
	{ 
		// Scan for public
		if (MF_AmxFindPublic(amx, blah, &FunctionIndex) == AMX_ERR_NONE)
		{
			// Public was found.
			return TRUE;

		}
	}

	return FALSE; // no public found in any loaded script
}

CPlayer *UTIL_PlayerByCID(int CID)
{
	int i=0;
	while (i++<gpGlobals->maxClients)
	{
		if (GETPLAYERUSERID(g_player[i].GetEdict())==CID)
		{
			return &g_player[i];
		}
	}

	return NULL;
}

/**
 * Converts a log string (eg: "sawce<1><STEAM_0:1:4560311><marine1team>")
 * into a player index
 */
int UTIL_LogToIndex(const char *LogLine)
{
	char				 NameBuffer[33]	;	// Temporary buffer to store the CID String
	char				*StrLocation;		// Location in the LogLine pointer
	unsigned int		 Count=0;			// Count for how many <'s we've passed
	size_t				 Length;			// Length of LogLine

	Length=strlen(LogLine);
	StrLocation=const_cast<char *>(LogLine) + Length; // Should now point to the last >

	while (Length--)
	{
		if (*StrLocation--=='<')
		{
			if (++Count==3) // 3rd match is end of CID
			{
				break;
			}
		}
	}

	if (Count!=3) // Invalid name somehow??
	{
		return 0;
	}

	if (Length > 32) // The name is too long somehow? stop here...
	{
		return 0;
	}
	strncpy(NameBuffer,LogLine,Length);

	Count=0;

	while ((int)Count++<gpGlobals->maxClients)
	{
		if (strcmp(NameBuffer,STRING(INDEXENT_NEW(Count)->v.netname))==0)
		{
			return Count;
		}
	}

	return 0;
}

char *UTIL_ToLowerCase(const char *str)
{
	size_t len = strlen(str);
	char *buffer = new char[len + 1];
	for (size_t i = 0; i < len; i++)
	{
		if (str[i] >= 'A' && str[i] <= 'Z')
			buffer[i] = tolower(str[i]);
		else
			buffer[i] = str[i];
	}
	buffer[len] = '\0';
	return buffer;
}
