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
	{ "timer","ti", TFC_WPN_TIMER,ACT_NADE_NONE, 2.0 , 0 , 2 },
	{ "caltrop","ca", TFC_WPN_CALTROP, ACT_NADE_SHOT,0.0 , 0 , 2 },
	{ "tf_rpg_rocket","roc", TFC_WPN_RPG,ACT_NADE_PUT, 2.0 , 7 , 3 },
	{ "tf_gl_grenade","gl_g", TFC_WPN_GL, ACT_NADE_PUT, 2.0 , 3 , 4 },
	{ "tf_gl_pipebomb","gl_p", TFC_WPN_PL, ACT_NADE_PUT, 2.0 , 3 , 4 }, 
	{ "tf_weapon_gasgrenade","ga", TFC_WPN_GASGRENADE, ACT_NADE_SHOT, 0.0 , 10 , 2 },
	{ "tf_weapon_empgrenade","em", TFC_WPN_EMPGRENADE, ACT_NADE_SHOT, 0.0 , 10 , 2 },
	{ "tf_weapon_mirvbomblet","mirvb", TFC_WPN_MIRVGRENADE,ACT_NADE_PUT, 2.0 , 10 , 5 },
	{ "tf_weapon_nailgrenade","nai", TFC_WPN_NAILGRENADE, ACT_NADE_SHOT|ACT_NADE_PUT, 2.0 , 10 , 3 },
	{ "tf_weapon_mirvgrenade","mirvg", TFC_WPN_MIRVGRENADE, ACT_NADE_SHOT|ACT_NADE_PUT, 2.0 , 10 , 5 },
	{ "tf_weapon_napalmgrenade","nap", TFC_WPN_NAPALMGRENADE, ACT_NADE_SHOT, 0.0 , 10 , 3 },
	{ "tf_weapon_normalgrenade","no", TFC_WPN_NORMALGRENADE, ACT_NADE_SHOT|ACT_NADE_PUT, 2.0 , 10 , 2 },
	{ "tf_weapon_concussiongrenade","co", TFC_WPN_CONCUSSIONGRENADE, ACT_NADE_NONE, 0.0 , 10 , 2 },
};

bool ignoreBots (edict_t *pEnt, edict_t *pOther)
{
	rankBots = (int)tfcstats_rankbots->value ? true : false;
	if ( !rankBots && ( pEnt->v.flags & FL_FAKECLIENT || ( pOther && pOther->v.flags & FL_FAKECLIENT ) ) )
		return true;
	return false;
}

bool isModuleActive(){
	if ( !(int)CVAR_GET_FLOAT("tfcstats_pause") )
		return true;
	return false;
}

bool util_strncmp( const char *sz1, const char *sz2, int size){
	int i = 0;
	while( sz1[i] && i<=size){
		if ( sz1[i] != sz2[i] )
			return false;
		i++;
	}
	return true;
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
	{ "timer", "infection", -1 }, // 1
	{ "sentrygun", "sentrygun", -1 }, // 2
	{ "medikit", "medikit", -1 , true},
	{ "spanner", "spanner", 2 , true },
	{ "axe", "axe", -1 , true },
	{ "sniperrifle", "sniperrifle", 1 },
	{ "autorifle", "autorifle", 1 },
	{ "shotgun", "shotgun", 1 },
	{ "supershotgun", "supershotgun", 1 },
	{ "ng", "nails", 3 },
	{ "superng", "supernails", 3 },
	{ "gl", "gl_grenade", 4 },
	{ "flamethrower", "flamethrower", 2 },
	{ "rpg", "rocket", 4 },
	{ "ic", "ic", 4 },
	{ "flames", "flames", -1 }, // 16
	{ "ac", "ac", 1 },
	{ "", "", -1 }, // 18
	{ "", "", -1 }, // 19
	{ "tranq", "tranq", 1 },
	{ "railgun", "railgun", 3 },
	{ "pl", "pipebomb", 4 },
	{ "knife", "knife", -1 , true },
	{ "caltrop", "caltrop", -1 }, // 24
	{ "concussion", "concussiongrenade", -1 },
	{ "grenade", "normalgrenade", -1 },
	{ "nailgrenade", "nailgrenade", -1 },
	{ "mirvgrenade", "mirvgrenade", -1 },
	{ "napalm", "napalmgrenade", -1 },
	{ "gas", "gasgrenade", -1 },
	{ "emp", "empgrenade", -1 },
};
