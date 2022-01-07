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

static cell AMX_NATIVE_CALL set_user_class(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index)
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
	CHECK_PLAYER(index);
	int iTeam = params[2];
	if ( iTeam<1 || iTeam>3 ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid team id %d", iTeam);
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
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if ( pPlayer->ingame ){
		return *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_CLASS);
	}

	return 0;
}

static cell AMX_NATIVE_CALL is_randomclass(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if ( pPlayer->ingame ){
		return *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_RCLASS);
	}
	return 0;
}

static cell AMX_NATIVE_CALL get_user_deaths(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		return *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_DEATHS );
	}
	return -1;
}

static cell AMX_NATIVE_CALL set_user_deaths(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);
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
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame){
		*( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_SCORE ) = params[2];

		if ( params[3]){
		/*
			//ScoreShort message
			MESSAGE_BEGIN(MSG_ALL,gmsgScoreShort);
			WRITE_BYTE(pPlayer->index);
			WRITE_SHORT(params[2]);
			WRITE_SHORT((int)pPlayer->pEdict->v.frags);
			WRITE_SHORT( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_DEATHS ) );
			WRITE_BYTE(1);
			MESSAGE_END();
		*/
			MESSAGE_BEGIN(MSG_ALL, gmsgObjScore);
			WRITE_BYTE(pPlayer->index);
			WRITE_SHORT(params[2]);
			MESSAGE_END();
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL set_user_frags(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame){
		pPlayer->pEdict->v.frags = (float)params[2];

		if ( params[3]){
		/*
			//ScoreShort message
			
			MESSAGE_BEGIN(MSG_ALL,gmsgScoreShort);
			WRITE_BYTE(pPlayer->index);
			WRITE_SHORT( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_SCORE ) );
			WRITE_SHORT((int)pPlayer->pEdict->v.frags);
			WRITE_SHORT( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_DEATHS ) );
			WRITE_BYTE(1);
			MESSAGE_END();
		*/
			MESSAGE_BEGIN(MSG_ALL, gmsgFrags);
			WRITE_BYTE(pPlayer->index);
			WRITE_SHORT((int)pPlayer->pEdict->v.frags);
			MESSAGE_END();
		}
	}
	return 1;
}

static cell AMX_NATIVE_CALL get_user_frags(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
		return (int)pPlayer->pEdict->v.frags;

	return -1;
}

static cell AMX_NATIVE_CALL set_user_teamname(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if ( pPlayer->ingame ){
		
		int iLen;
		char *szTeamName = MF_GetAmxString(amx, params[2], 0, &iLen);

		pPlayer->setTeamName(szTeamName);
	
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_teamname(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);

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
	CHECK_PLAYER(index);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	if (pPlayer->ingame){
		if ( *( (int*)pPlayer->pEdict->pvPrivateData + STEAM_PDOFFSET_WDEPLOY) == 1 )
			return 1;
	}
	return 0;
}


static cell AMX_NATIVE_CALL set_user_ammo(AMX *amx, cell *params){
	int index = params[1];
	CHECK_PLAYER(index);
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
		case DODW_SCOPED_ENFIELD:
			*( (int*)pPlayer->pEdict->pvPrivateData + 54+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 283+LINUXOFFSET ) = params[3];
			*( (int*)pPlayer->pEdict->pvPrivateData + 315+LINUXOFFSET ) = params[3];
			break;

		//57,286,318
		case DODW_STG44:
		case DODW_BAR:
		case DODW_FG42:
		case DODW_BREN:
		case DODW_SCOPED_FG42:
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
		case DODW_FOLDING_CARBINE:
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
		case DODW_MILLS_BOMB:
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
	CHECK_PLAYER(index);
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
		case DODW_SCOPED_ENFIELD:
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
		case DODW_SCOPED_FG42:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 57+LINUXOFFSET );
			break;

		//58,282,314
		case DODW_K43:
		case DODW_M1_CARBINE:
		case DODW_MG34:
		case DODW_FOLDING_CARBINE:
			return *( (int*)pPlayer->pEdict->pvPrivateData + 58+LINUXOFFSET );
			break;

		//59,289,321
		case DODW_HANDGRENADE:
		case DODW_MILLS_BOMB:
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

static cell AMX_NATIVE_CALL objective_set_data(AMX *amx, cell *params){ // index, key, ivalue , szvalue
	int index = params[1];
	if ( index < 0  || index > mObjects.count ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Index out of range (%d)", index);
		return 0;
	}

	edict_t *pent = mObjects.obj[index].pEdict;

	int iLen;
	int ivalue = params[3];
	char *szValue = MF_GetAmxString(amx, params[4], 0, &iLen);
	
	CP_VALUE key = (CP_VALUE)params[2];
	switch ( key ){
		case CP_owner :
			mObjects.UpdateOwner( index, ivalue );
			return 1;
		case CP_default_owner :
			mObjects.obj[index].default_owner = ivalue;
			GET_CP_PD(pent).default_owner = ivalue;
			return 1;
		case CP_visible :
			mObjects.obj[index].visible = ivalue;
			mObjects.obj[index].pEdict->v.spawnflags = 1 - ivalue;
			return 1;
		case CP_icon_neutral :
			mObjects.obj[index].icon_neutral = ivalue;
			GET_CP_PD(pent).icon_neutral = ivalue;
			return 1;
		case CP_icon_allies :
			mObjects.obj[index].icon_allies = ivalue;
			GET_CP_PD(pent).icon_allies = ivalue;
			return 1;
		case CP_icon_axis :
			mObjects.obj[index].icon_axis = ivalue;
			GET_CP_PD(pent).icon_axis = ivalue;
			return 1;
		case CP_origin_x :
			mObjects.obj[index].origin_x = (float)ivalue;
			// reinit
			return 1;
		case CP_origin_y :
			mObjects.obj[index].origin_y = (float)ivalue;
			// reinit
			return 1;
		case CP_can_touch :
			GET_CP_PD(pent).can_touch = ivalue;
			return 1;
		case CP_pointvalue :
			GET_CP_PD(pent).pointvalue = ivalue;
			return 1;
		
		case CP_points_for_cap :
			GET_CP_PD(pent).points_for_player = ivalue;
			return 1;
		case CP_team_points :
			GET_CP_PD(pent).points_for_team = ivalue;
			return 1;

		case CP_model_body_neutral :
			GET_CP_PD(pent).model_body_neutral = ivalue;
			return 1;
		case CP_model_body_allies :
			GET_CP_PD(pent).model_body_allies = ivalue;
			return 1;
		case CP_model_body_axis :
			GET_CP_PD(pent).model_body_axis = ivalue;
			return 1;

		// Strings

		case CP_name :
			mObjects.obj[index].pEdict->v.netname = MAKE_STRING(szValue);
			return 1;
		case CP_cap_message :
			strcpy(GET_CP_PD(mObjects.obj[index].pEdict).cap_message,szValue);
			return 1;
		case CP_reset_capsound :
			mObjects.obj[index].pEdict->v.noise = MAKE_STRING(szValue);
			return 1;
		case CP_allies_capsound :
			mObjects.obj[index].pEdict->v.noise1 = MAKE_STRING(szValue);
			return 1;
		case CP_axis_capsound :
			mObjects.obj[index].pEdict->v.noise2 = MAKE_STRING(szValue);
			return 1;
		case CP_targetname :
			mObjects.obj[index].pEdict->v.targetname = MAKE_STRING(szValue);
			return 1;

		case CP_model_neutral :
			strcpy(GET_CP_PD(pent).model_neutral,szValue);
			return 1;
		case CP_model_allies :
			strcpy(GET_CP_PD(pent).model_allies,szValue);
			return 1;
		case CP_model_axis :
			strcpy(GET_CP_PD(pent).model_axis,szValue);
			return 1;

		default:
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL objective_get_data(AMX *amx, cell *params){ // flagid, key, ivalue szvalue[],len=0
	int index = params[1];
	if ( index < 0  || index > mObjects.count ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Index out of range (%d)", index);
		return 0;
	}
	int len = params[4];
	CP_VALUE key = (CP_VALUE)params[2];
	
	switch ( key ){
		case CP_edict :
			return ENTINDEX(mObjects.obj[index].pEdict);
		case CP_area :
			GET_CAPTURE_AREA(index)
			return mObjects.obj[index].areaflags == 2 ? ENTINDEX(mObjects.obj[index].pAreaEdict) : 0;
		case CP_owner :
			return mObjects.obj[index].owner;
		case CP_default_owner :
			return mObjects.obj[index].default_owner;
		case CP_visible :
			return mObjects.obj[index].visible;
		case CP_icon_neutral :
			return mObjects.obj[index].icon_neutral;
		case CP_icon_allies :
			return mObjects.obj[index].icon_allies;
		case CP_icon_axis :
			return mObjects.obj[index].icon_axis;
		case CP_origin_x :
			return (int)mObjects.obj[index].origin_x;
		case CP_origin_y :
			return (int)mObjects.obj[index].origin_y;
		case CP_can_touch :
			return GET_CP_PD( mObjects.obj[index].pEdict ).can_touch;
		case CP_pointvalue :
			return GET_CP_PD( mObjects.obj[index].pEdict ).pointvalue;

		case CP_points_for_cap :
			return GET_CP_PD( mObjects.obj[index].pEdict ).points_for_player;
		case CP_team_points :
			return GET_CP_PD( mObjects.obj[index].pEdict ).points_for_team;

		case CP_model_body_neutral :
			return GET_CP_PD(mObjects.obj[index].pEdict).model_body_neutral;
		case CP_model_body_allies :
			return GET_CP_PD(mObjects.obj[index].pEdict).model_body_allies;
		case CP_model_body_axis :
			return GET_CP_PD(mObjects.obj[index].pEdict).model_body_axis;

		// strings

		case CP_name :
			if ( len ){
				MF_SetAmxString(amx,params[3],STRING(mObjects.obj[index].pEdict->v.netname),len);
			}
			return 1;
		case CP_cap_message :
			if ( len ){
				MF_SetAmxString(amx,params[3],GET_CP_PD(mObjects.obj[index].pEdict).cap_message,len);
			}
			return 1;
		case CP_reset_capsound :
			if ( len ){
				MF_SetAmxString(amx,params[3],STRING(mObjects.obj[index].pEdict->v.noise),len);
			}
			return 1;
		case CP_allies_capsound :
			if ( len ){
				MF_SetAmxString(amx,params[3],STRING(mObjects.obj[index].pEdict->v.noise1),len);
			}
			return 1;
		case CP_axis_capsound :
			if ( len ){
				MF_SetAmxString(amx,params[3],STRING(mObjects.obj[index].pEdict->v.noise2),len);
			}
			return 1;
		case CP_targetname :
			if ( len ){
				MF_SetAmxString(amx,params[3],STRING(mObjects.obj[index].pEdict->v.targetname),len);
			}
			return 1;
		case CP_model_neutral :
			if ( len ){
				MF_SetAmxString(amx,params[3],GET_CP_PD(mObjects.obj[index].pEdict).model_neutral,len);
			}
			return 1;
		case CP_model_allies :
			if ( len ){
				MF_SetAmxString(amx,params[3],GET_CP_PD(mObjects.obj[index].pEdict).model_allies,len);
			}
			return 1;
		case CP_model_axis :
			if ( len ){
				MF_SetAmxString(amx,params[3],GET_CP_PD(mObjects.obj[index].pEdict).model_axis,len);
			}
			return 1;

		default:
			break;
	}
	return 1;
}

static cell AMX_NATIVE_CALL objectives_get_num(AMX *amx, cell *params){
	return mObjects.count;
}

static cell AMX_NATIVE_CALL objectives_reinit(AMX *amx, cell *params){ // index
	int player = params[1];
	if ( player < 0  || player > gpGlobals->maxClients ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Index out of range (%d)", player);
		return 0;
	}
	mObjects.InitObj( player == 0 ? MSG_ALL:MSG_ONE, player == 0 ? NULL: GETEDICT(player) );

	return 1;
}

static cell AMX_NATIVE_CALL area_get_data(AMX *amx, cell *params){ // flagid, key, ivalue szvalue[],len=0
	int index = params[1];
	if ( index < 0  || index > mObjects.count ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Index out of range (%d)", index);
		return 0;
	}
	int len = params[4];
	CA_VALUE key = (CA_VALUE)params[2];

	GET_CAPTURE_AREA(index)

	switch ( key ){
		case CA_edict :
			return ENTINDEX(mObjects.obj[index].pAreaEdict);
		case CA_allies_numcap :
			return GET_CA_PD( mObjects.obj[index].pAreaEdict ).allies_numcap;
		case CA_axis_numcap :
			return GET_CA_PD( mObjects.obj[index].pAreaEdict ).axis_numcap;
		case CA_timetocap :
			return GET_CA_PD( mObjects.obj[index].pAreaEdict ).time_to_cap;
		case CA_can_cap :
			return GET_CA_PD( mObjects.obj[index].pAreaEdict ).can_cap;

		// strings
		case CA_target:
			if ( len ){
				MF_SetAmxString(amx,params[3],STRING(mObjects.obj[index].pAreaEdict->v.target),len);
			}
			return 1;
		case CA_sprite:
			if ( len ){
				MF_SetAmxString(amx,params[3],GET_CA_PD(mObjects.obj[index].pAreaEdict).hud_sprite,len);
			}
			return 1;
	}
	return 1;
}

static cell AMX_NATIVE_CALL area_set_data(AMX *amx, cell *params){ // index, key, ivalue , szvalue
	int index = params[1];
	if ( index < 0  || index > mObjects.count ){
		MF_LogError(amx, AMX_ERR_NATIVE, "Index out of range (%d)", index);
		return 0;
	}
	int iLen;
	int ivalue = params[3];
	char *szValue = MF_GetAmxString(amx, params[4], 0, &iLen);
	
	CA_VALUE key = (CA_VALUE)params[2];

	GET_CAPTURE_AREA(index)

	switch ( key ){
		case CA_allies_numcap :
			GET_CA_PD( mObjects.obj[index].pAreaEdict ).allies_numcap = ivalue;
			return 1;
		case CA_axis_numcap :
			GET_CA_PD( mObjects.obj[index].pAreaEdict ).axis_numcap = ivalue;
			return 1;
		case CA_timetocap :
			GET_CA_PD( mObjects.obj[index].pAreaEdict ).time_to_cap = ivalue;
			return 1;
		case CA_can_cap :
			GET_CA_PD( mObjects.obj[index].pAreaEdict ).can_cap = ivalue;
			return 1;
		// strings
		case CA_target:
			mObjects.obj[index].pAreaEdict->v.target = MAKE_STRING(szValue);
			return 1;
		case CA_sprite:
			strcpy(GET_CA_PD( mObjects.obj[index].pAreaEdict ).hud_sprite,szValue);
			return 1;

		default:
			break;
	}
	return 1;
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

  { "dod_get_user_kills", get_user_frags },
  { "dod_set_user_kills", set_user_frags },

  { "objective_set_data", objective_set_data },
  { "objective_get_data", objective_get_data },
  { "objectives_get_num", objectives_get_num },
  { "objectives_reinit", objectives_reinit },
  { "area_set_data", area_set_data },
  { "area_get_data", area_get_data },
  ///*******************
  { NULL, NULL } 
};
