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

static cell AMX_NATIVE_CALL set_user_class(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	int iClass = params[2];

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if ( !pPlayer->ingame )
		return 0;


	if (iClass){
		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_CLASS) = iClass;
		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_RCLASS) = 0; // disable random class
	}
	else {
		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_RCLASS) = 1; // set random class

	}

	return 1;
}

static cell AMX_NATIVE_CALL set_user_team(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	int iTeam = params[2];
	if ( iTeam<1 || iTeam>3 ){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( pPlayer->ingame ){
		
		pPlayer->killPlayer();
		pPlayer->pEdict->v.team = iTeam;

		switch( iTeam ){
		case 1:	pPlayer->setTeamName("Allies");
			break;
		case 2:	pPlayer->setTeamName("Axis");
			break;
		case 3: pPlayer->setTeamName("Spectators");
			break;
		}

		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_RCLASS) = 1; // set random class

		if ( params[3] ){
			MESSAGE_BEGIN(MSG_ALL,gmsgPTeam);
			WRITE_BYTE(pPlayer->index);
			WRITE_BYTE( iTeam );
			MESSAGE_END();
		}
		
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_nextclass(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if ( pPlayer->ingame ){
		return *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_CLASS);
	}

	return 0;
}

static cell AMX_NATIVE_CALL is_randomclass(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if ( pPlayer->ingame ){
		return *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_RCLASS);
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_deaths(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		return *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_DEATHS );
	}
	return 0;
}

static cell AMX_NATIVE_CALL set_user_deaths(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_DEATHS ) = params[2];
		if ( params[3]){
			//ScoreShort message
			MESSAGE_BEGIN(MSG_ALL,gmsgScoreShort);
			WRITE_BYTE(pPlayer->index);
			WRITE_SHORT( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_SCORE ) );
			WRITE_SHORT((int)pPlayer->pEdict->v.frags);
			WRITE_SHORT(params[2]);
			WRITE_BYTE(1);
			MESSAGE_END();
		}
	}
	return 1;
}

static cell AMX_NATIVE_CALL set_user_score(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame){
		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_SCORE ) = params[2];

		if ( params[3]){
			//ScoreShort message
			MESSAGE_BEGIN(MSG_ALL,gmsgScoreShort);
			WRITE_BYTE(pPlayer->index);
			WRITE_SHORT(params[2]);
			WRITE_SHORT((int)pPlayer->pEdict->v.frags);
			WRITE_SHORT( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_DEATHS ) );
			WRITE_BYTE(1);
			MESSAGE_END();
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL set_user_teamname(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( pPlayer->ingame ){
		
		int iLen;
		char *szTeamName = MF_GetAmxString(amx, params[1], 0, &iLen);

		pPlayer->setTeamName(szTeamName);
	
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_teamname(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( pPlayer->ingame ){
		
		char szTeamName[16];
		pPlayer->getTeamName(szTeamName);

		return MF_SetAmxString(amx, params[2],szTeamName,params[3]);
	
	}

	return 1;
}

static cell AMX_NATIVE_CALL is_weapon_deployed(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		if ( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_WDEPLOY) == 1 )
			return 1;
	}
	return 0;
}


static cell AMX_NATIVE_CALL set_user_ammo(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( !pPlayer->ingame )
		return 0;

	switch(params[2]){

		//53,284,316
		case DODW_COLT:
		case DODW_LUGER:
		case DODW_WEBLEY:
			*( (int*)pPlayer->pEdict->pvPrivateData + 53+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 284+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 316+LINUXOFFSET ) = params[3];
			break;

		//54,283,315
		case DODW_GARAND:
		case DODW_KAR:
		case DODW_SCOPED_KAR:
		case DODW_ENFIELD:
			*( (int*)pPlayer->pEdict->pvPrivateData + 54+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 283+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 315+LINUXOFFSET ) = params[3];
			break;

		//57,286,318
		case DODW_STG44:
		case DODW_BAR:
		case DODW_FG42:
		case DODW_BREN:
			*( (int*)pPlayer->pEdict->pvPrivateData + 57+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 286+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 318+LINUXOFFSET ) = params[3];
			break;

		//56,281,313
		case DODW_THOMPSON:
		case DODW_GREASEGUN:
		case DODW_MP40:
		case DODW_STEN:
			*( (int*)pPlayer->pEdict->pvPrivateData + 56+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 281+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 313+LINUXOFFSET ) = params[3];
			break;

		//58,282,314
		case DODW_K43:
		case DODW_M1_CARBINE:
		case DODW_MG34:
			*( (int*)pPlayer->pEdict->pvPrivateData + 58+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 282+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 314+LINUXOFFSET ) = params[3];
			break;

		//55,285,317
		case DODW_SPRINGFIELD:
			*( (int*)pPlayer->pEdict->pvPrivateData + 55+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 285+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 317+LINUXOFFSET ) = params[3];
			break;

		//59,289,321
		case DODW_HANDGRENADE:
			*( (int*)pPlayer->pEdict->pvPrivateData + 59+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 289+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 321+LINUXOFFSET ) = params[3];
			break;

		//61,291,323
		case DODW_STICKGRENADE:
			*( (int*)pPlayer->pEdict->pvPrivateData + 61+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 291+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 323+LINUXOFFSET ) = params[3];
			break;

		//287,319
		case DODW_MG42:
			*( (int*)pPlayer->pEdict->pvPrivateData + 287+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 319+LINUXOFFSET ) = params[3];
			break;

		//288,320
		case DODW_30_CAL:
			*( (int*)pPlayer->pEdict->pvPrivateData + 288+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 320+LINUXOFFSET ) = params[3];
			break;

		//49,293,325
		case DODW_BAZOOKA:
		case DODW_PANZERSCHRECK:
		case DODW_PIAT:
			*( (int*)pPlayer->pEdict->pvPrivateData + 49+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 293+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 325+LINUXOFFSET ) = params[3];
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_ammo(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( !pPlayer->ingame )
		return 0;

	switch(params[2]){
		
		//53,284,316
		case DODW_COLT:
		case DODW_LUGER:
		case DODW_WEBLEY:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 53+LINUXOFFSET );
			break;		

		//49,293,325
		case DODW_BAZOOKA:
		case DODW_PANZERSCHRECK:
		case DODW_PIAT:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 49+LINUXOFFSET );
			break;

		//54,283,315
		case DODW_GARAND:
		case DODW_KAR:
		case DODW_SCOPED_KAR:
		case DODW_ENFIELD:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 54+LINUXOFFSET );
			break;

		//55,285,317
		case DODW_SPRINGFIELD:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 55+LINUXOFFSET );
			break;

		//56,281,313
		case DODW_THOMPSON:
		case DODW_GREASEGUN:
		case DODW_MP40:
		case DODW_STEN:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 56+LINUXOFFSET );
			break;

		//57,286,318
		case DODW_STG44:
		case DODW_BAR:
		case DODW_FG42:
		case DODW_BREN:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 57+LINUXOFFSET );
			break;

		//58,282,314
		case DODW_K43:
		case DODW_M1_CARBINE:
		case DODW_MG34:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 58+LINUXOFFSET );
			break;

		//59,289,321
		case DODW_HANDGRENADE:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 59+LINUXOFFSET );
			break;

		//61,291,323
		case DODW_STICKGRENADE:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 61+LINUXOFFSET );
			break;

		//287,319
		case DODW_MG42:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 287+LINUXOFFSET );
			break;

		//288,320
		case DODW_30_CAL:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 288+LINUXOFFSET );
			break;

	}

	return 1;
}


static cell AMX_NATIVE_CALL test_pd(AMX *amx, cell *params){
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients){
		MF_RaiseAmxError(amx,AMX_ERR_NATIVE);
		return 0;
	}
	int type = params[2];
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		int i;
		FILE *bp = fopen("pdtest.txt","at");
		for ( i=1;i<1000;i++ ){
			switch(type){
			case 0:	fprintf(bp,"%d %d\n",i,*( (int*)pPlayer->pEdict->pvPrivateData + i));
				break;
			case 1: fprintf(bp,"%d %f\n",i,*( (float*)pPlayer->pEdict->pvPrivateData + i));
				break;
			case 2: 
				fprintf(bp,"%d %c\n",i+1000,*( (char*)pPlayer->pEdict->pvPrivateData + i + 1000));
				break;
			//MF_PrintSrvConsole("",*( (int*)pPlayer->pEdict->pvPrivateData + i) );
			}
		}
		fclose(bp);
	}
	return 0;
}

AMX_NATIVE_INFO pd_Natives[] = {

  { "dod_set_user_class", set_user_class },
  { "dod_set_user_team", set_user_team },
  { "dod_get_next_class", get_user_nextclass },
  { "dod_is_randomclass", is_randomclass },
  { "dod_get_pl_deaths", get_user_deaths },
  { "dod_set_pl_deaths", set_user_deaths },
  { "dod_set_user_score", set_user_score },
  { "dod_set_pl_teamname", set_user_teamname },
  { "dod_get_pl_teamname", get_user_teamname },  
  { "dod_is_deployed", is_weapon_deployed },

  { "dod_get_user_ammo", get_user_ammo },  
  { "dod_set_user_ammo", set_user_ammo },  

  { "dod_test_pd", test_pd },
  ///*******************
  { NULL, NULL } 
};