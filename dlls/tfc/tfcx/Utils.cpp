/*
 * TFCX 
 * Copyright (c) 2004 Lukasz Wlasinski
 *
 *
 *    This program is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

#include "amxxmodule.h"
#include "tfcx.h"

traceVault traceData[] = {
	{ "tf_gl_grenade", TFC_WPN_GL, ACT_NADE_PUT, 2.0 },
	{ "tf_gl_pipebomb", TFC_WPN_PL, ACT_NADE_PUT, 2.0 }, 
	{ "tf_weapon_normalgrenade", TFC_WPN_NORMALGRENADE, ACT_NADE_SHOT|ACT_NADE_PUT|ACT_NADE_THROW, 2.0 },
	{ "tf_weapon_nailgrenade", TFC_WPN_NAILGRENADE, ACT_NADE_SHOT|ACT_NADE_PUT|ACT_NADE_THROW, 2.0 },
	{ "tf_weapon_mirvgrenade", TFC_WPN_MIRVGRENADE, ACT_NADE_SHOT|ACT_NADE_PUT|ACT_NADE_THROW, 2.0 },
	{ "tf_weapon_napalmgrenade", TFC_WPN_NAPALMGRENADE, ACT_NADE_SHOT|ACT_NADE_THROW, 0.0 },
	{ "tf_weapon_gasgrenade", TFC_WPN_GASGRENADE, ACT_NADE_SHOT|ACT_NADE_THROW, 0.0 },
	{ "tf_weapon_empgrenade", TFC_WPN_EMPGRENADE, ACT_NADE_SHOT|ACT_NADE_THROW, 0.0 },
	{ "tf_weapon_mirvbomblet", TFC_WPN_MIRVGRENADE,ACT_NADE_PUT, 2.0 },
	{ "tf_rpg_rocket", TFC_WPN_RPG,ACT_NADE_PUT, 2.0 },
	{ "caltrop", TFC_WPN_CALTROP, ACT_NADE_SHOT|ACT_NADE_THROW,0.0 },
	{ "tf_weapon_concussiongrenade", TFC_WPN_CONCUSSIONGRENADE, ACT_NADE_THROW, 0.0 },

	//	{ "timer", TFC_WPN_TIMER,ACT_NADE_NONE, 2.0 },

};

bool ignoreBots (edict_t *pEnt, edict_t *pOther){
	if ( !rankBots && ( pEnt->v.flags & FL_FAKECLIENT || ( pOther && pOther->v.flags & FL_FAKECLIENT ) ) )
		return true;
	return false;
}

bool isModuleActive(){
	if ( !(int)CVAR_GET_FLOAT("tfcstats_pause") )
		return true;
	return false;
}

/*
medikit,iSlot:-1,iId:3
spanner,iSlot:2,iId:4
axe,iSlot:-1,iId:5
sniperrifle,iSlot:1,iId:6
autorifle,iSlot:1,iId:7
shotgun,iSlot:1,iId:8
supershotgun,iSlot:1,iId:9
ng,iSlot:3,iId:10
superng,iSlot:3,iId:11
gl,iSlot:4,iId:12
flamethrower,iSlot:2,iId:13
rpg,iSlot:4,iId:14
ic,iSlot:4,iId:15
ac,iSlot:1,iId:17
tranq,iSlot:1,iId:20
railgun,iSlot:3,iId:21
pl,iSlot:4,iId:22
knife,iSlot:-1,iId:23
*/
weaponsVault weaponData[] = {
	{ "", "", -1 }, // 0
	{ "timer", "", -1 }, // 1
	{ "sentrygun", "", -1 }, // 2
	{ "medikit", "", -1 , true},
	{ "spanner", "", 2 , true },
	{ "axe", "", -1 , true },
	{ "sniperrifle", "", 1 },
	{ "autorifle", "", 1 },
	{ "shotgun", "", 1 },
	{ "supershotgun", "", 1 },
	{ "ng", "", 3 },
	{ "superng", "", 3 },
	{ "gl", "", 4 },
	{ "flamethrower", "", 2 },
	{ "rpg", "", 4 },
	{ "ic", "", 4 },
	{ "flames", "", -1 }, // 16
	{ "ac", "", 1 },
	{ "", "", -1 }, // 18
	{ "", "", -1 }, // 19
	{ "tranq", "", 1 },
	{ "railgun", "", 3 },
	{ "pl", "", 4 },
	{ "knife", "", -1 , true },
	{ "caltrop", "", -1 }, // 24
	{ "concussion", "", -1 },
	{ "grenade", "", -1 },
	{ "nailgrenade", "", -1 },
	{ "mirvgrenade", "", -1 },
	{ "napalm", "", -1 },
	{ "gas", "", -1 },
	{ "emp", "", -1 },
};
