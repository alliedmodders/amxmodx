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
// TSX Module
//

#include "tsx.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);
CPlayer* mPlayer;
int mPlayerIndex;
int mState;
CPlayer players[33];

bool is_theonemode;
bool rankBots;

int g_death_info = -1;
int g_damage_info = -1;

int gKnifeOffset;

int gmsgResetHUD;
int gmsgWeaponInfo;
int gmsgClipInfo;
int gmsgScoreInfo;
int gmsgTSHealth;
int gmsgTSState;

int gmsgWStatus;
int gmsgTSCash;
int gmsgTSSpace;
int gmsgPwUp;

RankSystem g_rank;

cvar_t init_tsstats_maxsize ={"tsstats_maxsize","3500", 0 , 3500.0 };
cvar_t init_tsstats_reset ={"tsstats_reset","0"};
cvar_t init_tsstats_rank ={"tsstats_rank","0"};
cvar_t *tsstats_rankbots;
cvar_t *tsstats_pause;

cvar_t init_tsstats_rankbots ={"tsstats_rankbots","1"};
cvar_t init_tsstats_pause = {"tsstats_pause","0"};
cvar_t *tsstats_maxsize;
cvar_t *tsstats_reset;
cvar_t *tsstats_rank;

struct sUserMsg
{
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} g_user_msg[] = {
	{ "ResetHUD",&gmsgResetHUD,Client_ResetHUD_End,true },
	{ "WeaponInfo",&gmsgWeaponInfo,Client_WeaponInfo,false },
	{ "ClipInfo",&gmsgClipInfo,Client_ClipInfo,false },
	{ "ScoreInfo",&gmsgScoreInfo,Client_ScoreInfo,false },
	{ "TSHealth",&gmsgTSHealth,Client_TSHealth_End,true },
	{ "TSState",&gmsgTSState,Client_TSState,false },
	{ "WStatus",&gmsgWStatus,Client_WStatus,false },
	{ "TSCash",&gmsgTSCash,Client_TSCash,false },
	{ "TSSpace",&gmsgTSSpace,Client_TSSpace,false },
	{ "PwUp",&gmsgPwUp,Client_PwUp,false},

	{ 0,0,0,false }
};

const char* get_localinfo( const char* name , const char* def = 0 )
{
	const char* b = LOCALINFO( (char*)name );
	if (((b==0)||(*b==0)) && def )
		SET_LOCALINFO((char*)name,(char*)(b = def) );
	return b;
}

int RegUserMsg_Post(const char *pszName, int iSize)
{
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

void check_stunts(edict_s *player)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(player);

	if(pPlayer->checkstate == 0) return;

	/*int stunttype;
	int newstate = pPlayer->state;
	int oldstate = pPlayer->oldstate;
	
	if(newstate == 0) stunttype = STUNT_NONE;
	else if(newstate == 2) stunttype = STUNT_DIVE;
	else if(oldstate == 2) stunttype = STUNT_GETUP;
	else if( pPlayer->GetOffset(TSX_SROLL_OFFSET) == 1) stunttype = STUNT_ROLL;
	else if( pPlayer->GetOffset(TSX_SDUCK_OFFSET) == 1 ) stunttype = STUNT_DUCK;
	else stunttype = STUNT_FLIP;*/

	pPlayer->checkstate = 0;

	//MF_ExecuteForward(Stunt,pPlayer->index,stunttype);
}

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax )
{

	is_theonemode =	(int)CVAR_GET_FLOAT("mp_theonemode") ? true:false;

	rankBots = (int)tsstats_rankbots->value ? true:false;

	for( int i = 1; i <= gpGlobals->maxClients; ++i )
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );
	
	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post( edict_t *pEntity )
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	check_stunts(pEntity);

	if ( !isModuleActive() ) // stats only
		return;

	if (pPlayer->clearStats && pPlayer->clearStats < gpGlobals->time && pPlayer->ingame)
	{
		pPlayer->clearStats = 0.0f;
		pPlayer->rank->updatePosition( &pPlayer->life );
		pPlayer->restartStats(false);
	}

	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate()
{
	int i;
	for(i = 1;i<=gpGlobals->maxClients; ++i){
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
		if (pPlayer->rank) pPlayer->Disconnect();
	}

	if ( (g_rank.getRankNum() >= (int)tsstats_maxsize->value) || ((int)tsstats_reset->value == 1) ) {
		CVAR_SET_FLOAT("tsstats_reset",0.0);
		g_rank.clear();
	}
	
	g_rank.saveRank( MF_BuildPathname("%s",get_localinfo("tsstats") ) );

	// clear custom weapons info
	for ( i=TSMAX_WEAPONS-TSMAX_CUSTOMWPNS;i<TSMAX_WEAPONS;i++)
		weaponData[i].custom = false;

	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]) 
{
	GET_PLAYER_POINTER(pEntity)->Connect(pszAddress);
	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void ClientDisconnect( edict_t *pEntity )
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (pPlayer->ingame) pPlayer->Disconnect();
	RETURN_META(MRES_IGNORED);
}

void ClientPutInServer_Post( edict_t *pEntity )
{
	GET_PLAYER_POINTER(pEntity)->PutInServer();
	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged_Post( edict_t *pEntity, char *infobuffer )
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	const char* name = INFOKEY_VALUE(infobuffer,"name");
	const char* oldname = STRING(pEntity->v.netname);

	if ( pPlayer->ingame ){
		if ( strcmp(oldname,name) ) {
			if (!tsstats_rank->value)
				pPlayer->rank = g_rank.findEntryInRank( name, name );
			else
				pPlayer->rank->setName( name );
		}
	}
	else if ( pPlayer->IsBot() ) {
		pPlayer->PutInServer();
	}

	RETURN_META(MRES_IGNORED);
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (ed){
		mPlayerIndex = ENTINDEX(ed);
		mPlayer = GET_PLAYER_POINTER_I(mPlayerIndex);
	} else {
		mPlayerIndex = 0;
		mPlayer = NULL;
	}
	mState = 0;
	if ( msg_type < 0 || msg_type >= MAX_REG_MSGS )
		msg_type = 0;
	function=modMsgs[msg_type];
	endfunction=modMsgsEnd[msg_type];
	RETURN_META(MRES_IGNORED);
}

void MessageEnd_Post(void)
{
	if (endfunction) (*endfunction)(NULL);
	RETURN_META(MRES_IGNORED);
}

void WriteByte_Post(int iValue)
{
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteChar_Post(int iValue)
{
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteShort_Post(int iValue)
{
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteLong_Post(int iValue)
{
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void WriteAngle_Post(float flValue)
{
	if (function) (*function)((void *)&flValue);
	RETURN_META(MRES_IGNORED);
}

void WriteCoord_Post(float flValue)
{
	if (function) (*function)((void *)&flValue);
	RETURN_META(MRES_IGNORED);
}

void WriteString_Post(const char *sz)
{
	if (function) (*function)((void *)sz);
	RETURN_META(MRES_IGNORED);
}

void WriteEntity_Post(int iValue)
{
	if (function) (*function)((void *)&iValue);
	RETURN_META(MRES_IGNORED);
}

void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr)
{
	if (ptr->pHit&&(ptr->pHit->v.flags& (FL_CLIENT | FL_FAKECLIENT) )&&
		e&&(e->v.flags& (FL_CLIENT | FL_FAKECLIENT) )){
		GET_PLAYER_POINTER(e)->aiming = ptr->iHitgroup;
	}
	RETURN_META(MRES_IGNORED);
}

void OnMetaAttach()
{
	
	CVAR_REGISTER (&init_tsstats_maxsize);
	CVAR_REGISTER (&init_tsstats_reset);
	CVAR_REGISTER (&init_tsstats_rank);
	tsstats_maxsize=CVAR_GET_POINTER(init_tsstats_maxsize.name);
	tsstats_reset=CVAR_GET_POINTER(init_tsstats_reset.name);
	tsstats_rank=CVAR_GET_POINTER(init_tsstats_rank.name);

	CVAR_REGISTER (&init_tsstats_rankbots);
	CVAR_REGISTER (&init_tsstats_pause);
	tsstats_rankbots = CVAR_GET_POINTER(init_tsstats_rankbots.name);
	tsstats_pause = CVAR_GET_POINTER(init_tsstats_pause.name);

}

void OnPluginsLoaded()
{
	g_damage_info = MF_RegisterForward("client_damage", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	g_death_info = MF_RegisterForward("client_death", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "ts") == 0)
		return AMXX_GAME_OK;

	return AMXX_GAME_BAD;
}
void OnAmxxAttach()
{

	gKnifeOffset = TSKNIFE_OFFSET;

	MF_AddNatives( stats_Natives );
	MF_AddNatives( base_Natives );

	const char* path =  get_localinfo("tsstats_score","addons/amxmodx/data/tsstats.amxx");
	if ( path && *path ) 
	{
		char error[128];
		g_rank.loadCalc( MF_BuildPathname("%s",path) , error  );
	}
	if ( !g_rank.begin() )
	{		
		g_rank.loadRank( MF_BuildPathname("%s",get_localinfo("tsstats","addons/amxmodx/data/tsstats.dat") ) );
	}
}

void OnAmxxDetach()
{
	g_rank.clear();
	g_rank.unloadCalc();
}
