// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#include "amxxmodule.h"
#include <extdll.h>

#include <amtl/am-vector.h>
#include "forward.h"
#include "hook.h"
#include "ham_const.h"
#include "hooklist.h"
#include "offsets.h"
#include <assert.h>
#include "DataHandler.h"
#include "hook_specialbot.h"
#include <HLTypeConversion.h>

HLTypeConversion TypeConversion;

extern ke::Vector<Hook*> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];
extern CHamSpecialBotHandler SpecialbotHandler;

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

	assert(strcmp(hooklist[Ham_GetDeathActivity].name, "getdeathactivity")==0);
	assert(strcmp(hooklist[Ham_StopFollowing].name, "stopfollowing")==0);
	assert(strcmp(hooklist[Ham_CS_Player_OnTouchingWeapon].name, "cstrike_player_ontouchingweapon")==0);
	assert(strcmp(hooklist[Ham_DOD_Weapon_Special].name, "dod_weapon_special")==0);
	assert(strcmp(hooklist[Ham_TFC_RadiusDamage2].name, "tfc_radiusdamage2")==0);
	assert(strcmp(hooklist[Ham_ESF_Weapon_HolsterWhenMeleed].name, "esf_weapon_holsterwhenmeleed") == 0);
	assert(strcmp(hooklist[Ham_NS_Weapon_GetDeployTime].name, "ns_weapon_getdeploytime")==0);
	assert(strcmp(hooklist[Ham_SC_MedicCallSound].name, "sc_mediccallsound")==0);
	assert(strcmp(hooklist[Ham_SC_Player_CanTouchPlayer].name, "sc_player_cantouchplayer")==0);
	assert(strcmp(hooklist[Ham_SC_Weapon_ChangeWeaponSkin].name, "sc_weapon_changeweaponskin")==0);
	assert(strcmp(hooklist[Ham_Item_GetItemInfo].name, "item_getiteminfo") == 0);

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

extern CStack<ItemInfo *> g_FreeIIs;

void OnAmxxDetach()
{
	while (!g_FreeIIs.empty())
	{
		delete g_FreeIIs.front();
		g_FreeIIs.pop();
	}
}

void HamCommand(void);

void OnPluginsUnloaded(void)
{
	for (size_t i = 0; i < HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
	{
		for (size_t j = 0; j < hooks[i].length(); ++j)
		{
			delete hooks[i].at(j);
		}
		hooks[i].clear();
	}
}

void OnPluginsLoaded(void)
{
	TypeConversion.init();
}

void OnMetaAttach(void)
{
	REG_SVR_COMMAND("ham", HamCommand);
}

void SetClientKeyValue(int clientIndex, char *infobuffer, const char *key, const char *value)
{
	SpecialbotHandler.CheckClientKeyValue(clientIndex, infobuffer, key, value);
	RETURN_META(MRES_IGNORED);
}
