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
#include "tsx.h"

weapon_t weaponData[] = { 
	{ 1,"Kung Fu","kung_fu",3 }, // id 0 like in WeaponInfo , DeathMsg 
	{ 0,"Glock-18","glock-18",1 },
	{ 0,"Unk1","Unk1",0 }, // bomb ?
	{ 0,"Mini-Uzi","mini-uzi",1 }, 
	{ 0,"BENELLI-M3","benelli_m3",1 }, 
	{ 0,"M4A1","m4a1",1 },
	{ 0,"MP5SD","mp5sd",1 },
	{ 0,"MP5K","mp5k",1 },
	{ 0,"Akimbo Berettas","akimbo_berettas",1 },
	{ 0,"SOCOM-MK23","socom-mk23",1 },
	{ 0,"Akimbo MK23","akimbo_mk23",1 },
	{ 0,"USAS-12","usas-12",1 },
	{ 0,"Desert Eagle","desert_eagle",1 },
	{ 0,"AK47","ak47",1 },
	{ 0,"Five-seveN","five-seven",1 },
	{ 0,"STEYR-AUG","steyr-aug",1 },
	{ 0,"Akimbo Mini-Uzi","akimbo_mini-uzi",1 },
	{ 0,"STEYR-TMP","steyr-tmp",1 },
	{ 0,"Barrett M82A1","barrett_m82a1",1 },
	{ 0,"MP7-PDW","mp7-pdw",1 }, 
	{ 0,"SPAS-12","spas-12",1 },
	{ 0,"Golden Colts","golden_colts",1 },
	{ 0,"Glock-20C","glock-20c",1 },
	{ 0,"UMP", "ump",1 }, 
	{ 0,"M61 Grenade","m61_grenade",1 },
	{ 1,"Combat Knife","combat_knife",1 },
	{ 0,"MOSSBERG 500","mossberg_500",1 },
	{ 0,"M16A4","m16a4",1 },
	{ 0,"Ruger-MK1","ruger-mk1",1 },
	{ 0,"C4","c4",0 },
	{ 0,"Akimbo Five-seveN","akimbo_five-seven",1 },
	{ 0,"Raging Bull","raging_bull",1 },
	{ 0,"M60E3","m60e3",1 }, 
	{ 0,"Sawed-off","sawed-off",1 }, // 33 
	{ 1,"Katana", "katana",2 }, 
	{ 1,"Seal Knife","seal_knife",1 }, // 35 
	{ 1,"Kung Fu","kung_fu",3 }, // Again new id 36 
	{ 1,"Throwing Knife","throwing_knife",2 }, // new id 37 
	{ 0,"breakable", "breakable", 1 },
};

bool ignoreBots (edict_t *pEnt, edict_t *pOther){
	if ( !rankBots && ( pEnt->v.flags & FL_FAKECLIENT || ( pOther && pOther->v.flags & FL_FAKECLIENT ) ) )
		return true;
	return false;
}

bool isModuleActive(){
	if ( !(int)CVAR_GET_FLOAT("tsstats_pause") )
		return true;
	return false;
}
