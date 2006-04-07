/*
 * dodfun 
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
#include "dodfun.h"

static cell AMX_NATIVE_CALL set_player_stamina(AMX *amx, cell *params){ // id,(re)set,min,max
	int index = params[1];
	CHECK_PLAYER(index)
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( params[2] ){ // 0 set , 1 reset
		pPlayer->staminaMin = 0;
		pPlayer->staminaMax = 100;
		pPlayer->staminaSet = false;
		return 1;
	}

	int min = params[3];
	if ( min<0 || min>100 ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid minimum stamina %d", min);
		return 0;
	}
	int max = params[4];
	if ( max<min || max>100 ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid maximum stamina %d", max);
		return 0;
	}
	if ( pPlayer->ingame ){
		pPlayer->staminaMin = min;
		pPlayer->staminaMax = max;
		pPlayer->staminaSet = true;
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL nade_set_fuse(AMX *amx, cell *params){ // id,(re)set,time,type
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( params[2] ){ // 0 set , 1 reset
		pPlayer->fuseSet = false;
		pPlayer->nadeFuse = 0;
		return 1;
	}

	float fFuse = *(float *)((void *)&params[3]);
	if ( fFuse<0.1 || fFuse>20.0 ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid fuse %f", fFuse);
		return 0;
	}

	int iFType = params[4];

	if ( pPlayer->ingame ){
		pPlayer->nadeFuse = fFuse;
		pPlayer->fuseSet = true;
		pPlayer->fuseType = iFType;
		return 1;
	}
	return 0;
}

AMX_NATIVE_INFO base_Natives[] = {
  { "dod_set_stamina", set_player_stamina },
  { "dod_set_fuse", nade_set_fuse },

  ///*******************
  { NULL, NULL } 
};
