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

#include "amxxmodule.h"

// Fun-specific defines below
#define GETCLIENTLISTENING		(*g_engfuncs.pfnVoice_GetClientListening)
#define SETCLIENTLISTENING		(*g_engfuncs.pfnVoice_SetClientListening)
#define SETCLIENTMAXSPEED		(*g_engfuncs.pfnSetClientMaxspeed)
#define GETPLAYERAUTHID			(*g_engfuncs.pfnGetPlayerAuthId)
#define	SF_NORESPAWN			(1 << 30)// !!!set this bit on guns and stuff that should never respawn.
#define STANDARDTIMESTEPSOUND	400

#define HITGROUP_GENERIC		0 // none
#define HITGROUP_HEAD			1 // 1 << 1 = 2
#define HITGROUP_CHEST			2 // 1 << 2 = 4
#define HITGROUP_STOMACH		3 // 8
#define HITGROUP_LEFTARM		4 // 16
#define HITGROUP_RIGHTARM		5 // 32
#define HITGROUP_LEFTLEG		6 // 64
#define HITGROUP_RIGHTLEG		7 // 128
// Fun-specific defines above

// The stuff below might end up in a class soon
//int g_zones_toHit[33];		// where can people hit other people?
//int g_zones_getHit[33];		// where can people get hit by other people?
char g_bodyhits[33][33];
bool g_silent[33];			// used for set_user_footsteps()
//int g_ResetHUD;
bool g_ResetHUDbool;
edict_t* g_edict;
//bool g_bot[33];				// is user bot? <--- removed, only needed with akimbot
// Globals above
