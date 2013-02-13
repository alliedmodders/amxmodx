/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

#include "amxxmodule.h"
#include <extdll.h>

#include "NEW_Util.h"
#include "CVector.h"
#include "forward.h"
#include "hook.h"
#include "ham_const.h"
#include "hooklist.h"
#include "offsets.h"
#include <assert.h>

edict_t *NEW_FirstEdict;
bool NEW_Initialized;

extern CVector<Hook*> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];

extern AMX_NATIVE_INFO RegisterNatives[];
extern AMX_NATIVE_INFO ReturnNatives[];
extern AMX_NATIVE_INFO pdata_natives[];
extern AMX_NATIVE_INFO pdata_natives_safe[];


extern hook_t hooklist[];

int ReadConfig(void);

void OnAmxxAttach(void)
{
	// Assert that the enum is aligned properly with the table

	assert(strcmp(hooklist[Ham_FVecVisible].name, "fvecvisible")==0);
	assert(strcmp(hooklist[Ham_Player_UpdateClientData].name, "player_updateclientdata")==0);
	assert(strcmp(hooklist[Ham_Item_AddToPlayer].name, "item_addtoplayer")==0);
	assert(strcmp(hooklist[Ham_Weapon_ExtractAmmo].name, "weapon_extractammo")==0);
	assert(strcmp(hooklist[Ham_TS_BreakableRespawn].name, "ts_breakablerespawn")==0);
	assert(strcmp(hooklist[Ham_NS_UpdateOnRemove].name, "ns_updateonremove")==0);
	assert(strcmp(hooklist[Ham_TS_ShouldCollide].name, "ts_shouldcollide")==0);

	MF_AddNatives(pdata_natives_safe);
	if (ReadConfig() > 0)
	{
		if (Offsets.IsValid())
		{
			MF_AddNatives(RegisterNatives);
			MF_AddNatives(ReturnNatives);
			MF_AddNatives(pdata_natives);
		}
		else
		{
#ifdef _WIN32
			MF_Log("Error: pev and base not set for section \"%s windows\", cannot register natives.", MF_GetModname());
#elif defined(__linux__)
			MF_Log("Error: pev and base not set for section \"%s linux\", cannot register natives.", MF_GetModname());
#elif defined(__APPLE__)
			MF_Log("Error: pev and base not set for section \"%s mac\", cannot register natives.", MF_GetModname());
#endif
		}
	}
	else
	{
		MF_Log("Error: Cannot read config file, natives not registered!");
	}
}

void HamCommand(void);

void OnPluginsUnloaded(void)
{

	CVector <Hook *>::iterator end;
	for (int i = 0; i < HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
	{
		end=hooks[i].end();

		for (CVector<Hook*>::iterator j=hooks[i].begin();
			 j!=end;
			 ++j)
		{
			delete (*j);
		}
		hooks[i].clear();
	}
}

void OnPluginsLoaded(void)
{
	NEW_Initialize(INDEXENT(0));
}
void OnMetaAttach(void)
{
	REG_SVR_COMMAND("ham", HamCommand);
}
