// cstrike MODULE TO DO HERE: http://www.amxmod.info/forums/viewtopic.php?p=187#187

#include <extdll.h>
#include <meta_api.h>
#include "amx/modules.h"

// Check later if all of these really are needed!
meta_globals_t *gpMetaGlobals;		// Variables provided to plugins.
gamedll_funcs_t *gpGamedllFuncs;	// Pair of function tables provided by game DLL.
mutil_funcs_t *gpMetaUtilFuncs;		// Meta Utility Function table type.
enginefuncs_t g_engfuncs;			// Engine hands this to DLLs for functionality callbacks
globalvars_t  *gpGlobals;			// JGHG says: contains info on server, like maxclients, (time?) etc, stringbase is here :-) seems to be used with entity classnames...

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

#define NAME "Counter-strike module"
#define AUTHOR "JGHG"
#define VERSION "0.1"
#define URL "http://?"
#define LOGTAG "cstrike"
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
	#define OFFSET_CSDEATHS				449
	#define OFFSET_HOSTAGEID			487
#endif // defined __linux__

#define CSW_FAMAS						15
#define CSW_USP							16
#define CSW_GLOCK18						17
#define CSW_M4A1						22

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
// Globals above
