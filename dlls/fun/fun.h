// FUN MODULE TO DO HERE: http://www.amxmod.info/forums/viewtopic.php?t=37
//#define FUN_LINUX // UNCOMMENT WHEN COMPILING FOR LINUX, OR PDATA OFFSETS WILL CRASH SERVER

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
static META_FUNCTIONS gMetaFunctionTable = {
	NULL,						// pfnGetEntityAPI				HL SDK; called before game DLL
	NULL,						// pfnGetEntityAPI_Post			META; called after game DLL
	NULL,						// pfnGetEntityAPI2				HL SDK2; called before game DLL
	NULL,						// pfnGetEntityAPI2_Post		META; called after game DLL
	NULL,						// pfnGetNewDLLFunctions		HL SDK2; called before game DLL
	NULL,						// pfnGetNewDLLFunctions_Post	META; called after game DLL
	GetEngineFunctions,			// pfnGetEngineFunctions		META; called before HL engine
	NULL						// pfnGetEngineFunctions_Post	META; called after HL engine
};

pfnamx_engine_g* g_engAmxFunc;				// These seem to be meta/amxmod related
pfnmodule_engine_g* g_engModuleFunc;		// These seem to be meta/amxmod related

#define NAME "fun"
#define AUTHOR "JGHG"
#define VERSION "0.3"
#define URL "http://?"
#define LOGTAG "fun"
#define DATE __DATE__

// Fun-specific defines below
#define GETCLIENTLISTENING	(*g_engfuncs.pfnVoice_GetClientListening)
#define SETCLIENTLISTENING	(*g_engfuncs.pfnVoice_SetClientListening)
#define	SF_NORESPAWN		( 1 << 30 )// !!!set this bit on guns and stuff that should never respawn.

#if defined FUN_LINUX
	#define CSMONEYOFFSET	115 + 5
	#define CSDEATHSOFFSET	449 + 5
#else
	#define CSMONEYOFFSET	115	// Note that linux offsets need to be 5 higher (120 in this case)
	#define CSDEATHSOFFSET	449
#endif // defined LINUX

#define HITGROUP_GENERIC	0 // none
#define HITGROUP_HEAD		1
#define HITGROUP_CHEST		2
#define HITGROUP_STOMACH	3
#define HITGROUP_LEFTARM	4
#define HITGROUP_RIGHTARM	5
#define HITGROUP_LEFTLEG	6
#define HITGROUP_RIGHTLEG	7
// Fun-specific defines above

// Globals below
plugin_info_t Plugin_info = {
  META_INTERFACE_VERSION,
  NAME,
  VERSION,
  DATE,
  AUTHOR,
  URL,
  LOGTAG,
  PT_ANYTIME,
  PT_ANYTIME,
};
module_info_s module_info = {
  NAME,
  AUTHOR,
  VERSION,
  AMX_INTERFACE_VERSION,
  RELOAD_MODULE,
};
int g_body = 0;				// bits of parts of body to hit
// Globals above
