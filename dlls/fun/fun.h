/* AMX Mod X
*   Fun Module
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

#include <extdll.h>
#include <meta_api.h>
#include <modules.h>

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
	GetEntityAPI2,				// pfnGetEntityAPI2				HL SDK2; called before game DLL
	NULL,						// pfnGetEntityAPI2_Post		META; called after game DLL
	NULL,						// pfnGetNewDLLFunctions		HL SDK2; called before game DLL
	NULL,						// pfnGetNewDLLFunctions_Post	META; called after game DLL
	GetEngineFunctions,			// pfnGetEngineFunctions		META; called before HL engine
	NULL						// pfnGetEngineFunctions_Post	META; called after HL engine
};

pfnamx_engine_g* g_engAmxFunc;				// These seem to be meta/amxmod related
pfnmodule_engine_g* g_engModuleFunc;		// These seem to be meta/amxmod related

#define NAME "Fun"
#define AUTHOR "AMX Mod X Dev Team"
#define VERSION "0.1"
#define URL "http://www.amxmodx.org"
#define LOGTAG "FUN"
#define DATE __DATE__

// Fun-specific defines below
#define CVAR_FUN_VERSION		"fun_version"
#define GETCLIENTLISTENING		(*g_engfuncs.pfnVoice_GetClientListening)
#define SETCLIENTLISTENING		(*g_engfuncs.pfnVoice_SetClientListening)
#define	SF_NORESPAWN			(1 << 30)// !!!set this bit on guns and stuff that should never respawn.
#define STANDARDTIMESTEPSOUND	400

#if defined __linux__
	#define OFFSET_CSMONEY		115 + 5
	#define OFFSET_CSDEATHS		449 + 5
#else
	#define OFFSET_CSMONEY		115	// Note that linux offsets need to be 5 higher (120 in this case)
	#define OFFSET_CSDEATHS		449
#endif // defined __linux__

#define HITGROUP_GENERIC		0 // none
#define HITGROUP_HEAD			1
#define HITGROUP_CHEST			2
#define HITGROUP_STOMACH		3
#define HITGROUP_LEFTARM		4
#define HITGROUP_RIGHTARM		5
#define HITGROUP_LEFTLEG		6
#define HITGROUP_RIGHTLEG		7
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
cvar_t fun_version = {"fun_version", "0.1", FCVAR_EXTDLL};
int g_body = 0;				// bits of parts of body to hit
bool silent[33];			// used for set_user_footsteps()
// Globals above
