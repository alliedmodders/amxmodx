/* AMX Mod X
*
* by the AMX Mod X Development Team
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
*
*  Description: AMX Mod X Module Interface hooks
*/

#include <stdlib.h>
#include <stddef.h>

#include "sdk/amxxmodule.h"

#include "hamsandwich.h"

#include "CVector.h"
#include "CString.h"

#include "FileParser.h"

#include "NEW_Util.h"

#include "VTableManager.h"
#include "VTableEntries.h"

CVector<String *> SuffixNames;									/**< List of names being searched for in the config file. */
CVector<void (*)(const char *, const char *)> SuffixFunctions;	/**< Callback functions for when we find a hunted keyvalue. */
CVector<void (*)(void)> ConfigDoneCallbacks;						/**< Callbacks when our config file is done being parsed. */

edict_t *NEW_FirstEdict=NULL;		/**< Used for the NEW_Utils INDEXENT/ENTINDEX replacements. */
bool NEW_Initialized=false;			/**< Whether NEW_Utils has been initialized yet. */

void FP_SetupOffsets(const char *name, FP_ptrFeedback feedback);


unsigned int HAM_pev=0;				/**< Offset of pev from the this pointer. */
unsigned int HAM_pevset=0;			/**< Whether or not the pev offset has been set. */
unsigned int HAM_classbase=0;		/**< Offset of the vtable from the this pointer. */
unsigned int HAM_classbaseset=0;	/**< Whether or not the classbase offset has been set. */

char ModKey[256];					/**< Temporary buffer for holding the <modname>_<os> prefix. */

/**
 * Adds a callback to the config parser for this config suffix.
 *
 * @param suffix			The suffix to add, eg "takedamage" would be called for "cs_linux_takedamage"
 * @param callback			The function to call when this key is found.
 * @noreturn
 */
void RegisterKeySuffix(const char *suffix, void (*callback)(const char *,const char *))
{
	String *Suffix=new String(suffix);
	

	SuffixNames.push_back(Suffix);

	SuffixFunctions.push_back(callback);
};

/**
 * Adds this entry to the configdone callback.
 *
 * @param callback			Function to call when the config file is done being parsed.
 * @noreturn
 */
void RegisterConfigCallback(void (*callback)(void))
{
	ConfigDoneCallbacks.push_back(callback);
};
/**
 * Starts each vtable entry.  This needs to be edited for every additional hook.
 *
 * @noreturn
 */
void HAM_CallInitialization(void)
{
#define VTINIT(TableName) VTable##TableName::Initialize(&HAM_pev,&HAM_pevset,&HAM_classbase,&HAM_classbaseset)

	VTINIT(TakeDamage);
	VTINIT(Use);
	VTINIT(Killed);
	VTINIT(Blocked);
	VTINIT(Respawn);
	VTINIT(Restart);
	VTINIT(AddPoints);
	VTINIT(AddPointsToTeam);
	VTINIT(AddPlayerItem);
	VTINIT(RemovePlayerItem);
	VTINIT(BloodColor);
	VTINIT(Classify);
	VTINIT(GetToggleState);
	VTINIT(IsAlive);
	VTINIT(IsBSPModel);
	VTINIT(IsInWorld);
	VTINIT(IsMoving);
	VTINIT(IsNetClient);
	VTINIT(IsPlayer);
	VTINIT(IsSneaking);
	VTINIT(ObjectCaps);
	VTINIT(Think);
	VTINIT(Touch);


#undef VTINIT
}

/**
 * Tells all the table entries that the config file is done being parsed.  Register their natives now.
 *
 * @noreturn
 */
void HAM_CallConfigDone(void)
{
	int i=0;								/**< Iterator. */
	int end=ConfigDoneCallbacks.size();		/**< How many to parse. */

	while (i<end)
	{
		ConfigDoneCallbacks[i]();
		++i;
	}

	// done with this, free up the vector
	ConfigDoneCallbacks.clear();
};

/**
 * Simple wrapper to uniform string to number conversion. I don't just use -1 on strtol base because I don't want octal conversion.
 *
 * @param input			The input string.
 * @return				The input string converted to an unsigned integer.
 */
int HAM_StrToNum(const char *input)
{
	char *end; /**< Temporary pointer, needed for strtoul(). */

	// if begins with 0x or 0X it's to be interpretted as hex
	if (*input=='0' && 
		(*(input+1)=='x' || *(input+1)=='X'))
	{
		return strtoul(input,&end,16);
	}

	// otherwise it's to be interpretted as base 10
	return strtoul(input,&end,10);
}
/**
 * This is called every time a key with the <mod>_<os>_ prefix is found.
 *
 * @param key			The full key (eg: cs_windows_takedamage)
 * @param data			The corresponding data.
 * @return				true when key is used, false otherwise
 */
bool HAM_GetKey(const char *key, const char *data)
{
	char TempKey[512];				/**< Temporary buffer. */
	int i=0;						/**< Iterator. */
	int end=SuffixNames.size();		/**< How many suffixes to check. */
	bool found=false;				/**< Whether this key has been used or not. */

	while (i<end)
	{
		snprintf(TempKey,sizeof(TempKey)-1,"%s_%s",ModKey,SuffixNames[i]->c_str());
		if (strcmp(TempKey,key)==0)
		{
			SuffixFunctions[i](SuffixNames[i]->c_str(),data);
			found=true;
		}
		++i;
	}
	return found;
}
/**
 * Simple function to set the "pev" field that is used by all forwards.
 *
 * @param key			The key suffix being forwarded.
 * @param data			The data corresponding to the key.
 * @noreturn
 */
void HAM_SetPev(const char *key, const char *data)
{
	HAM_pev=HAM_StrToNum(data);
	HAM_pevset=1;
}
/**
 * Simple function to set the "classbase" field that is used by all natives when built with GCC.
 *
 * @param key			The key suffix being forwarded.
 * @param data			The data corresponding to the key.
 * @noreturn
 */
#if defined __linux__
void HAM_SetClassBase(const char *key, const char *data)
{
	HAM_classbase=HAM_StrToNum(data);
	HAM_classbaseset=1;
}
#endif

void OnAmxxAttach()
{
	HAM_CallInitialization();
#ifdef __linux__
	snprintf(ModKey,sizeof(ModKey)-1,"%s_linux",MF_GetModname());
#else
	snprintf(ModKey,sizeof(ModKey)-1,"%s_windows",MF_GetModname());
#endif

	RegisterKeySuffix("pev",HAM_SetPev);

	// this is only needed for Linux
#if defined __linux__
	RegisterKeySuffix("classbase",HAM_SetClassBase);
#else // Emulate it being set on Windows, since it's not needed
	HAM_classbase=0;
	HAM_classbaseset=1;
#endif

	RegisterRegisterNatives();

	FP_SetupOffsets(ModKey,HAM_GetKey);

	HAM_CallConfigDone();

	/* TODO: Cbase natives
	if (HAM_Set & HAM_GOT_PEV)
	{
		HAM_RegisterCbaseFast();
	}
	HAM_RegisterCbaseSafe();

	VTH_Natives();
	*/
}

void OnPluginsLoaded()
{
	NEW_Initialize(INDEXENT(0));

};
void OnPluginsUnloaded()
{
	VTMan.Cleanup();
};
