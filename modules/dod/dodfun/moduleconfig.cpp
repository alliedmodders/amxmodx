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

#include <string.h>
#include "amxxmodule.h"
#include "dodfun.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);
CPlayer* mPlayer;
CPlayer* gPlayerRocket;
CPlayer players[33];

CObjective mObjects;

int mState;
int mDest;
int mPlayerIndex;

int iFGrenade;
int iFRocket;
int iFInitCP;

int gmsgCurWeapon;
int gmsgScoreShort;
int gmsgPTeam;
int gmsgInitObj;
int gmsgSetObj;
int gmsgFrags;
int gmsgObjScore;

struct sUserMsg {
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} g_user_msg[] = {
	{ "InitObj",&gmsgInitObj,Client_InitObj,false},
	{ "CurWeapon",&gmsgCurWeapon,Client_CurWeapon,false },
	{ "ScoreShort",&gmsgScoreShort,NULL,false },
	{ "PTeam",&gmsgPTeam,NULL,false },
	{ "SetObj",&gmsgSetObj,Client_SetObj,false },
	{ "Frags",&gmsgFrags,NULL,false },
	{ "ObjScore", &gmsgObjScore, NULL, false },

	{ 0,0,0,false }
};

int RegUserMsg_Post(const char *pszName, int iSize){
	for (int i = 0; g_user_msg[ i ].name; ++i ){
		if ( !*g_user_msg[i].id && strcmp( g_user_msg[ i ].name , pszName  ) == 0 ){
			int id = META_RESULT_ORIG_RET( int );

			*g_user_msg[ i ].id = id;
		
			if ( g_user_msg[ i ].endmsg )
				modMsgsEnd[ id  ] = g_user_msg[ i ].func;
			else
				modMsgs[ id  ] = g_user_msg[ i ].func;

			break;
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ){

	for( int i = 1; i <= gpGlobals->maxClients; ++i )
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );

	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate() {
	for(int i = 1;i<=gpGlobals->maxClients; ++i){
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
		if (pPlayer->ingame)
			pPlayer->Disconnect();
	}
	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]  ){
	GET_PLAYER_POINTER(pEntity)->Connect();
	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void ClientDisconnect( edict_t *pEntity ) {
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (pPlayer->ingame) 
		pPlayer->Disconnect();
	RETURN_META(MRES_IGNORED);
}

void ClientPutInServer_Post( edict_t *pEntity ) {
	GET_PLAYER_POINTER(pEntity)->PutInServer();
	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged_Post( edict_t *pEntity, char *infobuffer ) {
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	if ( !pPlayer->ingame && pPlayer->IsBot() ) {
		pPlayer->PutInServer();
	}

	RETURN_META(MRES_IGNORED);
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	if (ed){
		mPlayerIndex = ENTINDEX(ed);
		mPlayer = GET_PLAYER_POINTER_I(mPlayerIndex);
	} else {
		mPlayerIndex = 0;
		mPlayer = NULL;
	}
	mState = 0;
	mDest = msg_dest;
	if ( msg_type < 0 || msg_type >= MAX_REG_MSGS )
		msg_type = 0;
	function=modMsgs[msg_type];
	endfunction=modMsgsEnd[msg_type];
	RETURN_META(MRES_IGNORED);
}

void MessageEnd_Post(void) {
	if (endfunction) (*endfunction)(NULL);
	RETURN_META(MRES_IGNORED);
}

void WriteByte_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteChar_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteShort_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteLong_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteAngle_Post(float flValue) {
	if (function) (*function)((void *)&flValue);
	RETURN_META(MRES_IGNORED);
}

void WriteCoord_Post(float flValue) {
	if (function) (*function)((void *)&flValue);
	RETURN_META(MRES_IGNORED);
}

void WriteString_Post(const char *sz) {
	if (function) (*function)((void *)sz);
	RETURN_META(MRES_IGNORED);
}

void WriteEntity_Post(int iValue) {
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post(edict_t *pEntity) 
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	// Stamina
	if(pPlayer->staminaSet) 
	{
		if ( (int)pEntity->v.fuser4 > pPlayer->staminaMax)
			pEntity->v.fuser4 = (float)pPlayer->staminaMax;

		else if ( (int)pEntity->v.fuser4 < pPlayer->staminaMin)
			pEntity->v.fuser4 = (float)pPlayer->staminaMin;
	}

	if(pPlayer->current == DODW_BAZOOKA || pPlayer->current == DODW_PANZERSCHRECK || pPlayer->current == DODW_PIAT)
	{
		if(!(pPlayer->pEdict->v.oldbuttons&IN_ATTACK) && (pPlayer->pEdict->v.button&IN_ATTACK))
			gPlayerRocket = GET_PLAYER_POINTER(pEntity);	// Store player ID for rocket_shoot() event
	}

	RETURN_META(MRES_IGNORED);
}

void SetModel_Post(edict_t *e, const char *m) {
	
	/* For a grenade, this function gets called twice. The first time, all models are w_grenade.mdl and dmgtime is 0.
	*    The second time, the proper model and dmgtime are set.
	*    gPlayerRocket is set in PlayerPreThink_Post for all rockets.
	*    So, if neither is set, the model isn't being set on a grenade or rocket and we just return.
	*    TNT bundles/satchel charges also initially use w_grenade.mdl, so we can't efficiently filter based on model.
	*/
	if (!gPlayerRocket && !e->v.dmgtime) {
		RETURN_META(MRES_IGNORED);
	}

	int w_id = 0;

	if(e->v.owner && e->v.dmgtime) {	// owner was always set in my testing, but better safe than sorry

		CPlayer* pPlayer = GET_PLAYER_POINTER(e->v.owner);
		// current weapon is never set to DODW_MILLS_BOMB; only DODW_HANDGRENADE/DODW_STICKGRENADE
		bool newNade = (pPlayer->current == DODW_HANDGRENADE || pPlayer->current == DODW_STICKGRENADE) ? true : false;

		if(m[9]=='g' && m[10]=='r' && m[12]=='n') {			// w_grenade.mdl (Allies)
			w_id = newNade ? DODW_HANDGRENADE : DODW_HANDGRENADE_EX;
		} else if(m[9]=='s' && m[10]=='t' && m[11]=='i') {		// w_stick.mdl (Axis)
			w_id = newNade ? DODW_STICKGRENADE : DODW_STICKGRENADE_EX;
		} else if(m[9]=='m' && m[10]=='i') {				// w_mills.mdl (British)
			w_id = newNade ? DODW_MILLS_BOMB : DODW_HANDGRENADE_EX;
			// A mills_ex weapon should be added in modules/dod/dodfun/CMisc.h, plugins/include/dodconst.inc,
			//   and probably modules/dod/dodx/NBase.cpp. The DODMAX_WEAPONS define should be updated too.
		}

		if(!w_id) {	// Fail-safe, just in case..
			RETURN_META(MRES_IGNORED);
		}

		MF_ExecuteForward(iFGrenade, pPlayer->index, ENTINDEX(e), w_id);	// Call grenade_throw() event

		/* fuse start */
		if(pPlayer->fuseSet) {

			if(newNade) {

				if(pPlayer->fuseType & 1<<0) {
					e->v.dmgtime += pPlayer->nadeFuse - 5.0;
				}
			} else { 

				float fExp = e->v.dmgtime - gpGlobals->time;
				e->v.dmgtime += pPlayer->nadeFuse - fExp;
			}
		}
		/* fuse end */

	} else if(gPlayerRocket && strstr(m, "rocket")) {

		// Since "rocket" exists in the model name, there are only 3 possibilities.
		if(m[9]=='b') {			// w_bazooka_rocket.mdl (Allies)
			w_id = DODW_BAZOOKA;
		} else if(m[10]=='s') {	// w_pschreck_rocket.mdl (Axis)
			w_id = DODW_PANZERSCHRECK;
		} else if(m[10]=='i') {	// w_piat_rocket.mdl (British)
			w_id = DODW_PIAT;
		}

		MF_ExecuteForward(iFRocket, gPlayerRocket->index, ENTINDEX(e), w_id);	// Call rocket_shoot() event

		gPlayerRocket = NULL;
	}

	RETURN_META(MRES_IGNORED);
}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "dod") == 0)
		return AMXX_GAME_OK;

	return AMXX_GAME_BAD;
}
void OnAmxxAttach() 
{
	MF_AddNatives( base_Natives );
	MF_AddNatives( pd_Natives );
}

void OnPluginsLoaded()
{
	iFGrenade = MF_RegisterForward("grenade_throw",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*Grenade Ent*/,FP_CELL/*Weapon ID*/,FP_DONE);
	iFRocket = MF_RegisterForward("rocket_shoot",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*Rocket Ent*/,FP_CELL/*Weapon ID*/,FP_DONE);
	iFInitCP = MF_RegisterForward("controlpoints_init",ET_IGNORE,FP_DONE);
}
