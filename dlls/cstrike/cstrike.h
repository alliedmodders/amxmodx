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

#include <extdll.h>
#include <meta_api.h>
#include <modules.h>

meta_globals_t *gpMetaGlobals;		// Variables provided to plugins.
gamedll_funcs_t *gpGamedllFuncs;	// Pair of function tables provided by game DLL.
mutil_funcs_t *gpMetaUtilFuncs;		// Meta Utility Function table type.
enginefuncs_t g_engfuncs;			// Engine hands this to DLLs for functionality callbacks
globalvars_t  *gpGlobals;			// JGHG says: contains info on server, like maxcliens, (time?) etc, stringbase is here :-) seems to be used with entity classnames...

// Must provide at least one of these...
static META_FUNCTIONS gMetaFunctionTable; /* = {
	NULL,						// pfnGetEntityAPI				HL SDK; called before game DLL
	NULL,						// pfnGetEntityAPI_Post			META; called after game DLL
	NULL,						// pfnGetEntityAPI2				HL SDK2; called before game DLL
	NULL,						// pfnGetEntityAPI2_Post		META; called after game DLL
	NULL,						// pfnGetNewDLLFunctions		HL SDK2; called before game DLL
	NULL,						// pfnGetNewDLLFunctions_Post	META; called after game DLL
	NULL,						// pfnGetEngineFunctions		META; called before HL engine
	NULL						// pfnGetEngineFunctions_Post	META; called after HL engine
}; */

pfnamx_engine_g* g_engAmxFunc;
pfnmodule_engine_g* g_engModuleFunc;

#define NAME "Counter-Strike"
#define AUTHOR "AMX Mod X Dev Team"
#define VERSION "0.1"
#define URL "http://www.amxmodx.org"
#define LOGTAG "AMXCS"
#define DATE __DATE__

// cstrike-specific defines below
#if defined __linux__
	#define OFFSET_WEAPONTYPE			43 + 5
	#define OFFSET_SILENCER_FIREMODE	74 + 5
	#define OFFSET_TEAM					114 + 5
	#define OFFSET_CSMONEY				115 + 5
	#define OFFSET_DEFUSE_PLANT			193 + 5
	#define OFFSET_VIP					215 + 5
	#define OFFSET_BUYZONE				241 + 5

	#define OFFSET_AWM_AMMO				382 + 5
	#define OFFSET_SCOUT_AMMO			383 + 5
	#define OFFSET_PARA_AMMO			384 + 5
	#define OFFSET_FAMAS_AMMO			385 + 5
	#define OFFSET_M3_AMMO				386 + 5
	#define OFFSET_USP_AMMO				387 + 5
	#define OFFSET_FIVESEVEN_AMMO		388 + 5
	#define OFFSET_DEAGLE_AMMO			389 + 5
	#define OFFSET_P228_AMMO			390 + 5
	#define OFFSET_GLOCK_AMMO			391 + 5
	#define OFFSET_FLASH_AMMO			392 + 5
	#define OFFSET_HE_AMMO				393 + 5
	#define OFFSET_SMOKE_AMMO			394 + 5
	#define OFFSET_C4_AMMO				395 + 5

	#define OFFSET_CSDEATHS				449 + 5
	#define OFFSET_HOSTAGEID			487 + 5
#else
	#define OFFSET_WEAPONTYPE			43
	#define OFFSET_SILENCER_FIREMODE	74
	#define OFFSET_TEAM					114
	#define OFFSET_CSMONEY				115
	#define OFFSET_DEFUSE_PLANT			193
	#define OFFSET_VIP					215
	#define OFFSET_BUYZONE				241

	#define OFFSET_AWM_AMMO				382
	#define OFFSET_SCOUT_AMMO			383
	#define OFFSET_PARA_AMMO			384
	#define OFFSET_FAMAS_AMMO			385
	#define OFFSET_M3_AMMO				386
	#define OFFSET_USP_AMMO				387
	#define OFFSET_FIVESEVEN_AMMO		388
	#define OFFSET_DEAGLE_AMMO			389
	#define OFFSET_P228_AMMO			390
	#define OFFSET_GLOCK_AMMO			391
	#define OFFSET_FLASH_AMMO			392
	#define OFFSET_HE_AMMO				393
	#define OFFSET_SMOKE_AMMO			394
	#define OFFSET_C4_AMMO				395	

	#define OFFSET_CSDEATHS				449
	#define OFFSET_HOSTAGEID			487
#endif // defined __linux__


// Offsets of ammo amount in player entities
/*
382	int	awm
383	int	scout, ak, g3
384	int	para
385	int	famas, m4a1, aug, sg550, galil, sg552
386	int	m3, xm
387	int	usp, ump, mac
388	int	fiveseven, p90
389	int	deagle
390	int	p228
391	int	glock, mp5, tmp, elites
392	int	flash
393	int	he
394	int	smoke
395	int	c4
*/

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

#define M4A1_UNSILENCED					0
#define M4A1_SILENCED					4
#define USP_UNSILENCED					0
#define USP_SILENCED					1

#define GLOCK_SEMIAUTOMATIC				0
#define GLOCK_BURSTMODE					2
#define FAMAS_AUTOMATIC					0
#define FAMAS_BURSTMODE					16

#define PLAYER_IS_NOT_VIP				0
#define PLAYER_IS_VIP					256

#define TEAM_T							1
#define TEAM_CT							2
#define TEAM_SPECTATOR					3

#define NO_DEFUSE_OR_PLANTSKILL			0
#define CAN_PLANT_BOMB					256
#define HAS_DEFUSE_KIT					65536

#define DEFUSER_COLOUR_R				0
#define DEFUSER_COLOUR_G				160
#define DEFUSER_COLOUR_B				0

// cstrike-specific defines above

// Globals below
plugin_info_t Plugin_info = {
  META_INTERFACE_VERSION,
  NAME,
  VERSION,
  DATE,
  AUTHOR,
  URL,
  LOGTAG,
  PT_ANYPAUSE,
  PT_ANYPAUSE,
};
module_info_s module_info = {
  NAME,
  AUTHOR,
  VERSION,
  AMX_INTERFACE_VERSION,
  RELOAD_MODULE,
};

//int g_msgMoney;
//int g_msgTextMsg;
//int g_msgStatusIcon;
// Globals above
