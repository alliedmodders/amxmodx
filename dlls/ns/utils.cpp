/* AMX Mod X 
 *   Natural Selection Module 
 * 
 * by the AMX Mod X Development Team 
 *
 * This file is part of AMX Mod X. 
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

#include "sdk/amxxmodule.h"


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

