// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Lukasz Wlasinski.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DoD Fun Module
//

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
