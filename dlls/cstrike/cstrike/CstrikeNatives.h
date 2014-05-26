/* AMX Mod X 
   *   Counter-Strike Module 
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

// cstrike MODULE TO DO HERE: http://www.amxmodx.org/forums/viewtopic.php?t=45
// This implementation uses Vexd's way (lightly modified) of setting models on players.

#include "amxxmodule.h"
#include <extdll.h>
#include <meta_api.h>
#include "CstrikePlayer.h"

#define GETINFOKEYBUFFER				(*g_engfuncs.pfnGetInfoKeyBuffer)
#define	SETCLIENTKEYVALUE				(*g_engfuncs.pfnSetClientKeyValue)
#define GETCLIENTKEYVALUE				(*g_engfuncs.pfnInfoKeyValue)
#define CREATENAMEDENTITY				(*g_engfuncs.pfnCreateNamedEntity)

#if defined(__linux__) || defined (__APPLE__)
	#define EXTRAOFFSET					5 // offsets 5 higher in Linux builds
	#define EXTRAOFFSET_WEAPONS			4 // weapon offsets are obviously only 4 steps higher on Linux!
	#define ACTUAL_EXTRA_OFFSET			20 // actual, byte-addressable offset
#else
	#define EXTRAOFFSET					0 // no change in Windows builds
	#define EXTRAOFFSET_WEAPONS			0
	#define ACTUAL_EXTRA_OFFSET			0
#endif // defined(__linux__) || defined(__APPLE__)
/*
	Offset history:
	041029:
	Confirmed (pretty much) ALL offsets for 32 bit Windows, 32 bit Linux and amd64 (yes that's all of them).
	Found out that weapon offsets are only +4 higher on Linux.
	Also backpack ammo offsets were all obviously 5 steps too high since unknown time...

*/

#define HAS_SHIELD      (1<<24) //16777216 

// "player" entities
#if !defined __amd64__
	// 32 bit offsets here
	#define OFFSET_ARMORTYPE			112 + EXTRAOFFSET
	#define OFFSET_TEAM					114 + EXTRAOFFSET
	#define OFFSET_CSMONEY				115 + EXTRAOFFSET
	#define OFFSET_PRIMARYWEAPON		116 + EXTRAOFFSET
	#define OFFSET_LASTACTIVITY			124 + EXTRAOFFSET
	#define OFFSET_INTERNALMODEL		126 + EXTRAOFFSET
	#define OFFSET_NVGOGGLES			129 + EXTRAOFFSET
	#define OFFSET_DEFUSE_PLANT			193 + EXTRAOFFSET
	#define OFFSET_VIP					209 + EXTRAOFFSET
	#define OFFSET_TK					216 + EXTRAOFFSET // 040926
	#define OFFSET_HOSTAGEKILLS			217 + EXTRAOFFSET
	#define OFFSET_MAPZONE				235 + EXTRAOFFSET
	#define OFFSET_ISDRIVING			350 + EXTRAOFFSET // 040926
	#define OFFSET_STATIONARY			362 + EXTRAOFFSET // 040927 (363 works also!)
	#define OFFSET_ZOOMTYPE				363 + EXTRAOFFSET

	#define OFFSET_AWM_AMMO				377 + EXTRAOFFSET // 041029: All of these *_AMMO:s were changed -5
	#define OFFSET_SCOUT_AMMO			378 + EXTRAOFFSET
	#define OFFSET_PARA_AMMO			379 + EXTRAOFFSET
	#define OFFSET_FAMAS_AMMO			380 + EXTRAOFFSET
	#define OFFSET_M3_AMMO				381 + EXTRAOFFSET
	#define OFFSET_USP_AMMO				382 + EXTRAOFFSET
	#define OFFSET_FIVESEVEN_AMMO		383 + EXTRAOFFSET
	#define OFFSET_DEAGLE_AMMO			384 + EXTRAOFFSET
	#define OFFSET_P228_AMMO			385 + EXTRAOFFSET
	#define OFFSET_GLOCK_AMMO			386 + EXTRAOFFSET
	#define OFFSET_FLASH_AMMO			387 + EXTRAOFFSET
	#define OFFSET_HE_AMMO				388 + EXTRAOFFSET
	#define OFFSET_SMOKE_AMMO			389 + EXTRAOFFSET
	#define OFFSET_C4_AMMO				390	+ EXTRAOFFSET

	#define OFFSET_CSDEATHS				444 + EXTRAOFFSET // 040926
	#define OFFSET_SHIELD				510 + EXTRAOFFSET
	// "weapon_*" entities
	#define OFFSET_WEAPONTYPE			43 + EXTRAOFFSET_WEAPONS
	#define OFFSET_CLIPAMMO				51 + EXTRAOFFSET_WEAPONS
	#define OFFSET_SILENCER_FIREMODE	74 + EXTRAOFFSET_WEAPONS
	// "hostage_entity" entities
	#define OFFSET_HOSTAGEFOLLOW		86 + EXTRAOFFSET
	#define OFFSET_HOSTAGE_NEXTUSE		100 + EXTRAOFFSET
	#define OFFSET_HOSTAGE_LASTUSE		483 + EXTRAOFFSET
	#define OFFSET_HOSTAGEID			487 + EXTRAOFFSET
	// "armoury_entity"
	#define OFFSET_ARMOURY_TYPE			34 + EXTRAOFFSET_WEAPONS
	// C4 offsets
	#define OFFSET_C4_EXPLODE_TIME		100 + EXTRAOFFSET
	#define OFFSET_C4_DEFUSING			0x181 + ACTUAL_EXTRA_OFFSET
#else
	// Amd64 offsets here
	#define OFFSET_ARMORTYPE			137 + EXTRAOFFSET
	#define OFFSET_TEAM					139 + EXTRAOFFSET // +25
	#define OFFSET_CSMONEY				140 + EXTRAOFFSET // +25
	#define OFFSET_PRIMARYWEAPON		141 + EXTRAOFFSET // +25
	#define OFFSET_INTERNALMODEL		152 + EXTRAOFFSET // +26
	#define OFFSET_NVGOGGLES			155 + EXTRAOFFSET // +26
	#define OFFSET_DEFUSE_PLANT			219 + EXTRAOFFSET // +26
	#define OFFSET_VIP					242 + EXTRAOFFSET // +27
	#define OFFSET_TK					249 + EXTRAOFFSET // +33 040927
	#define OFFSET_MAPZONE				268 + EXTRAOFFSET // +27
	#define OFFSET_ISDRIVING			386 + EXTRAOFFSET // 040927
	#define OFFSET_STATIONARY			400 + EXTRAOFFSET // 040927 (401 works also)
	#define OFFSET_ZOOMTYPE				402 + EXTRAOFFSET

	#define OFFSET_AWM_AMMO				426 + EXTRAOFFSET // +44
	#define OFFSET_SCOUT_AMMO			427 + EXTRAOFFSET // +44
	#define OFFSET_PARA_AMMO			428 + EXTRAOFFSET // +44
	#define OFFSET_FAMAS_AMMO			429 + EXTRAOFFSET // +44
	#define OFFSET_M3_AMMO				430 + EXTRAOFFSET // +44
	#define OFFSET_USP_AMMO				431 + EXTRAOFFSET // +44
	#define OFFSET_FIVESEVEN_AMMO		432 + EXTRAOFFSET // +44
	#define OFFSET_DEAGLE_AMMO			433 + EXTRAOFFSET // +44
	#define OFFSET_P228_AMMO			434 + EXTRAOFFSET // +44
	#define OFFSET_GLOCK_AMMO			435 + EXTRAOFFSET // +44
	#define OFFSET_FLASH_AMMO			436 + EXTRAOFFSET // +44
	#define OFFSET_HE_AMMO				437 + EXTRAOFFSET // +44
	#define OFFSET_SMOKE_AMMO			438 + EXTRAOFFSET // +44
	#define OFFSET_C4_AMMO				439	+ EXTRAOFFSET // +44

	#define OFFSET_CSDEATHS				493 + EXTRAOFFSET // +49
	#define OFFSET_SHIELD				559 + EXTRAOFFSET
	// "weapon_*" entities
	#define OFFSET_WEAPONTYPE			57 + EXTRAOFFSET // +14
	#define OFFSET_CLIPAMMO				65 + EXTRAOFFSET // +14
	#define OFFSET_SILENCER_FIREMODE	88 + EXTRAOFFSET // +14
	// "hostage_entity" entities
	#define OFFSET_HOSTAGEFOLLOW		51 + EXTRAOFFSET // +21, long=51, int=107! (must use the long* offset because pointers on amd64 are stored the size of longs, 8 bytes, instead of the usual int 4 bytes.)
	#define OFFSET_HOSTAGEID			516 + EXTRAOFFSET // +29
#endif

#if defined __linux__
#define CS_DETOURCOPYBYTES_CLIENTCOMMAND		6
#elif defined __APPLE__
#define CS_DETOURCOPYBYTES_CLIENTCOMMAND		5
#else
#define CS_DETOURCOPYBYTES_CLIENTCOMMAND		6
#define CS_CLICMD_OFFS_USEBOTARGS				2
#define CS_CLICMD_OFFS_BOTARGS					22
#endif


// Ids of weapons in CS
#define CSW_P228						1
//#define CSW_SHIELD					2
#define CSW_SCOUT						3
#define CSW_HEGRENADE					4
#define CSW_XM1014						5
#define CSW_C4							6
#define CSW_MAC10						7
#define CSW_AUG							8
#define CSW_SMOKEGRENADE				9
#define CSW_ELITE						10
#define CSW_FIVESEVEN					11
#define CSW_UMP45						12
#define CSW_SG550						13
#define CSW_GALI						14
#define CSW_FAMAS						15
#define CSW_USP							16
#define CSW_GLOCK18						17
#define CSW_AWP							18
#define CSW_MP5NAVY						19
#define CSW_M249						20
#define CSW_M3							21
#define CSW_M4A1						22
#define CSW_TMP							23
#define CSW_G3SG1						24
#define CSW_FLASHBANG					25
#define CSW_DEAGLE						26
#define CSW_SG552						27
#define CSW_AK47						28
//#define CSW_KNIFE						29
#define CSW_P90							30
#define CSW_VEST						31 // Brand new invention!
#define CSW_VESTHELM					32 // Brand new invention!

// These are used with armoury_entity:s.
#define CSA_MP5NAVY						0
#define CSA_TMP							1
#define CSA_P90							2
#define CSA_MAC10						3
#define CSA_AK47						4
#define CSA_SG552						5
#define CSA_M4A1						6
#define CSA_AUG							7
#define CSA_SCOUT						8
#define CSA_G3SG1						9
#define CSA_AWP							10
#define CSA_M3							11
#define CSA_XM1014						12
#define CSA_M249						13
#define CSA_FLASHBANG					14
#define CSA_HEGRENADE					15
#define CSA_VEST						16
#define CSA_VESTHELM					17
#define CSA_SMOKEGRENADE				18

#define M4A1_SILENCED					(1<<2)
#define M4A1_ATTACHSILENCEANIM			6
#define M4A1_DETACHSILENCEANIM			13
#define USP_SILENCED					(1<<0)
#define USP_ATTACHSILENCEANIM			7
#define USP_DETACHSILENCEANIM			15

#define GLOCK_SEMIAUTOMATIC				0
#define GLOCK_BURSTMODE					2
#define FAMAS_AUTOMATIC					0
#define FAMAS_BURSTMODE					16

#define PLAYER_IS_VIP					(1<<8)

#define PLAYER_IN_BUYZONE				(1<<0)

#define TEAM_UNASSIGNED					0
#define TEAM_T							1
#define TEAM_CT							2
#define TEAM_SPECTATOR					3

#define CAN_PLANT_BOMB					(1<<8) // 256
#define HAS_DEFUSE_KIT					(1<<16) // 65536

#define DEFUSER_COLOUR_R				0
#define DEFUSER_COLOUR_G				160
#define DEFUSER_COLOUR_B				0

#define HAS_NVGOGGLES					(1<<0)

#define SCOREATTRIB_NOTHING				0
#define SCOREATTRIB_DEAD				1
#define SCOREATTRIB_BOMB				2 // t only
#define SCOREATTRIB_VIP					4 // ct only

#define AMD64_STATIONARY_NO				2
#define AMD64_STATIONARY_YES			3

#define CS_ARMOR_NONE					0
#define CS_ARMOR_KEVLAR					1
#define CS_ARMOR_ASSAULTSUIT			2

#define CS_FIRST_ZOOM					0x28
#define CS_SECOND_AWP_ZOOM				0xA
#define CS_SECOND_NONAWP_ZOOM			0xF
#define CS_AUGSG552_ZOOM				0x37
#define CS_NO_ZOOM						0x5A

enum CS_Internal_Models {
	CS_DONTCHANGE = 0,
	CS_CT_URBAN = 1,
	CS_T_TERROR = 2,
	CS_T_LEET = 3,
	CS_T_ARCTIC = 4,
	CS_CT_GSG9 = 5,
	CS_CT_GIGN = 6,
	CS_CT_SAS = 7,
	CS_T_GUERILLA = 8,
	CS_CT_VIP = 9,
	CZ_T_MILITIA = 10,
	CZ_CT_SPETSNAZ = 11
};

enum
{
	CS_RESET_ZOOM = 0,
	CS_SET_NO_ZOOM,
	CS_SET_FIRST_ZOOM,
	CS_SET_SECOND_ZOOM,
	CS_SET_AUGSG552_ZOOM,
};
// cstrike-specific defines above

extern CCstrikePlayer g_players[33];
extern int g_zooming[33];
extern bool g_precachedknife;
extern bool g_noknives;
// Globals above

void InitializeHacks();
void ShutdownHacks();

#define CHECK_ENTITY(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if (x != 0 && FNullEnt(INDEXENT(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}

#define CHECK_PLAYER(x) \
	if (x < 1 || x > gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player out of range (%d)", x); \
		return 0; \
	} else { \
		if (!MF_IsPlayerIngame(x) || FNullEnt(MF_GetPlayerEdict(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", x); \
			return 0; \
		} \
	}

#define CHECK_NONPLAYER(x) \
	if (x < 1 || x <= gpGlobals->maxClients || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Non-player entity %d out of range", x); \
		return 0; \
	} else { \
		if (FNullEnt(INDEXENT(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid non-player entity %d", x); \
			return 0; \
		} \
	}

#define GETEDICT(n) \
	((n >= 1 && n <= gpGlobals->maxClients) ? MF_GetPlayerEdict(n) : INDEXENT(n))

