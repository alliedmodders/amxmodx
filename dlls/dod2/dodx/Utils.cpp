/*
 * DoDX 
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
#include "dodx.h"

/* Weapon names aren't send in WeaponList message in DoD */
weapon_t weaponData[] = {
	{ false,false,"mortar","mortar" },
	{ true,true,"amerknife","knife" }, // aknife->bknife
	{ false,true,"gerknife","knife" },
	{ false,false,"colt","Colt" }, 
	{ false,false,"luger","Luger" },
	{ true,false,"garand","Garand" }, // Garand->Garand butt
	{ false,false,"scopedkar","scoped K98" }, 
	{ false,false,"thompson","Thompson" },
	{ false,false,"mp44","STG44" },
	{ false,false,"spring","Springfield" },  
	{ true,false,"kar","K98" }, // KAR->KAR bayonet
	{ false,false,"bar","BAR" },
	{ false,false,"mp40","MP40" }, 
	{ false,false,"grenade","handgrenade" },
	{ false,false,"grenade2","stickgrenade" },
	{ false,false,"stickgrenade_ex","stickgrenade_ex" },
	{ false,false,"handgrenade_ex","handgrenade_ex" },
	{ false,false,"mg42","MG42" },
	{ false,false,"30cal",".30 cal" },
	{ false,true,"spade","spade" },
	{ true,false,"m1carbine","M1 Carbine" },  // M1 Carbine->Folding Carbine
	{ false,false,"mg34","MG34" },
	{ false,false,"greasegun","Greasegun" },
	{ true,false,"fg42","FG42" }, // FG42 -> scoped FG42
	{ true,false,"k43","K43" },
	{ true,false,"enfield","Enfield" }, // Enfield->Scoped Enfield->Enfield bayonet
	{ false,false,"sten","Sten" },
	{ false,false,"bren","Bren" }, 
	{ false,false,"webley","Webley" },
	{ false,false,"bazooka","Bazooka" },
	{ false,false,"pschreck","Panzerschrek" },
	{ false,false,"piat","Piat" },
	{ false,false,"scoped_fg42","scoped FG42" },
	{ false,false,"fcarbine","Folding Carbine" },
	{ false,true,"bayonet","K98 bayonet" }, // KAR bayonet
	{ false,false,"scoped_enfield","scoped Enfield"},
	{ false,false,"mills_bomb","mills bomb" },
	{ false,true,"brit_knife","knife" }, 
	{ false,true,"garandbutt","Garand butt" }, // Garand butt
	{ false,true,"enf_bayonet","Enfield bayonet" },
	{ false,false,"mortar","mortar" }, // mortar new id
	{ false,true,"k43butt","K43 butt" },
};

/* Function will select correct id */
int get_weaponid(CPlayer* pPlayer){
	int weapon = pPlayer->current;
	switch(weapon) {
	case  1: if ( g_map.detect_allies_country ) weapon = 37; break;
	case  5: if ( pPlayer->pEdict->v.button&IN_ATTACK2 ) weapon = 38; break;
	case 10: if ( pPlayer->pEdict->v.button&IN_ATTACK2 ) weapon = 34; break;
	case 20: 
		if ( g_map.detect_allies_paras ) weapon = 33;
		break;
	case 23: 
		if ( pPlayer->pEdict->v.weaponmodel )
			pPlayer->wpnModel = pPlayer->pEdict->v.weaponmodel; 
		if ( !( pPlayer->wpnModel&(1<<3) ) ) 
			weapon = 32; 	
		break;
	case 24: if ( pPlayer->pEdict->v.button&IN_ATTACK2 ) weapon = 41; break;
	case 25:
		if ( pPlayer->pEdict->v.weaponmodel )
			pPlayer->wpnModel = pPlayer->pEdict->v.weaponmodel;
		if ( pPlayer->wpnModel&(1<<3) ) 
			 weapon = 35; 
		else if ( pPlayer->pEdict->v.button&IN_ATTACK2 )
			 weapon = 39;
		 break;
	}
	return weapon;
}

traceVault traceData[] = {
	{ "grenade", 13, ACT_NADE_PUT|ACT_NADE_SHOT, 2.0 }, // or 36
	{ "grenade2", 14, ACT_NADE_PUT|ACT_NADE_SHOT, 2.0 },  
	{ "shell_bazooka", 29, ACT_NADE_PUT, 2.0 },
	{ "shell_pschreck", 30, ACT_NADE_PUT, 2.0 },
	{ "shell_piat", 31, ACT_NADE_PUT, 2.0 },
	{ "monster_mortar", 40, ACT_NADE_PUT|ACT_NADE_SHOT, 2.0 },
};

bool ignoreBots (edict_t *pEnt, edict_t *pOther){
	if ( !rankBots && ( pEnt->v.flags & FL_FAKECLIENT || ( pOther && pOther->v.flags & FL_FAKECLIENT ) ) )
		return true;
	return false;
}

bool isModuleActive(){
	if ( !(int)CVAR_GET_FLOAT("dodstats_pause") )
		return true;
	return false;
}

edict_t *FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue)
{
	edict_t *pentEntity;
	pentEntity=FIND_ENTITY_BY_STRING(pentStart, szKeyword, szValue);
	if(!FNullEnt(pentEntity))
		return pentEntity;
	return NULL;
}

edict_t *FindEntityByClassname(edict_t *pentStart, const char *szName)
{
	return FindEntityByString(pentStart, "classname", szName);
}
