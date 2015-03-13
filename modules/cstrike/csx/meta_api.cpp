// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// CSX Module
//

#include "amxxmodule.h"
#include "rank.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];

void (*function)(void*);
void (*endfunction)(void*);

CPlayer players[33];

CPlayer* mPlayer;

int mPlayerIndex;
int mState;

RankSystem g_rank;

Grenades g_grenades;

int iFGrenade;
int iFDeath;
int iFDamage;

int iFBPlanted;
int iFBDefused;
int iFBPlanting;
int iFBDefusing;
int iFBExplode;

int g_bombAnnounce;
int g_Planter;
int g_Defuser;

bool rankBots;

int gmsgCurWeapon;
int gmsgDeathMsg;
int gmsgDamage;
int gmsgDamageEnd;
int gmsgWeaponList;
int gmsgResetHUD;
int gmsgAmmoX;
int gmsgScoreInfo;
int gmsgAmmoPickup;
int gmsgSendAudio;
int gmsgTextMsg;
int gmsgBarTime;

int g_CurrentMsg;

cvar_t init_csstats_maxsize ={"csstats_maxsize","3500", 0 , 3500.0 };
cvar_t init_csstats_reset ={"csstats_reset","0"};
cvar_t init_csstats_rank ={"csstats_rank","1"};
cvar_t *csstats_maxsize;
cvar_t *csstats_reset;
cvar_t *csstats_rank;

cvar_t* csstats_rankbots;
cvar_t* csstats_pause;
cvar_t init_csstats_rankbots ={"csstats_rankbots","0"};
cvar_t init_csstats_pause = {"csstats_pause","0"};

struct sUserMsg
{
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} g_user_msg[] = {
	{"CurWeapon",	&gmsgCurWeapon,		Client_CurWeapon,	false},
	{"Damage",		&gmsgDamage,		Client_Damage,		false},	
	{"Damage",		&gmsgDamageEnd,		Client_Damage_End,	true},
	{"WeaponList",	&gmsgWeaponList,	Client_WeaponList,	false},
	{"ResetHUD",	&gmsgResetHUD,		Client_ResetHUD,	true},
	{"AmmoX",		&gmsgAmmoX,			Client_AmmoX,		false},
	{"ScoreInfo",	&gmsgScoreInfo,		Client_ScoreInfo,	false},
	{"AmmoPickup",	&gmsgAmmoPickup,	Client_AmmoPickup,	false},
	{"SendAudio",	&gmsgSendAudio,		Client_SendAudio,	false},
	{"TextMsg",		&gmsgTextMsg,		Client_TextMsg,		false},
	{"BarTime",		&gmsgBarTime,		Client_BarTime,		false},
	{"DeathMsg",	&gmsgDeathMsg,		Client_DeathMsg,	false},

	{0, 0, 0, false}
};

int RegUserMsg_Post(const char *pszName, int iSize)
{
	for (int i = 0; g_user_msg[ i ].name; ++i )
	{
		if ( !*g_user_msg[i].id && strcmp( g_user_msg[ i ].name , pszName  ) == 0 )
		{
			int id = META_RESULT_ORIG_RET( int );

			*g_user_msg[ i ].id = id;
		
			if ( g_user_msg[ i ].endmsg )
				modMsgsEnd[ id  ] = g_user_msg[ i ].func;
			else
				modMsgs[ id  ] = g_user_msg[ i ].func;
			//break;
		}
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

const char* get_localinfo( const char* name , const char* def = 0 )
{
	const char* b = LOCALINFO( (char*)name );
	if (((b==0)||(*b==0)) && def )
		SET_LOCALINFO((char*)name,(char*)(b = def) );
	return b;
}

void ClientKill_Post(edict_t *pEntity)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if ( pPlayer->IsAlive())
		RETURN_META(MRES_IGNORED);

	MF_ExecuteForward( iFDamage,static_cast<cell>(pPlayer->index), static_cast<cell>(pPlayer->index) , 
		static_cast<cell>(0), static_cast<cell>(0), static_cast<cell>(0), static_cast<cell>(0) );		// he would
	pPlayer->saveKill(pPlayer,0,0,0);
	MF_ExecuteForward( iFDeath,static_cast<cell>(pPlayer->index), static_cast<cell>(pPlayer->index),
		static_cast<cell>(0), static_cast<cell>(0), static_cast<cell>(0) );

	RETURN_META(MRES_IGNORED);
}

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ){

	rankBots = (int)csstats_rankbots->value ? true:false;

	for( int i = 1; i <= gpGlobals->maxClients; ++i)
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );
	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post( edict_t *pEntity ) {
    if ( !isModuleActive() )
	{
		RETURN_META(MRES_IGNORED);
	}

	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (pPlayer->clearStats && pPlayer->clearStats < gpGlobals->time ){

		if ( !ignoreBots(pEntity) ){
			pPlayer->clearStats = 0.0f;
			if (pPlayer->rank)
				pPlayer->rank->updatePosition( &pPlayer->life );
			pPlayer->restartStats(false);
		}
	}
	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate() {
	int i;
	for( i = 1;i<=gpGlobals->maxClients; ++i){
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
		if (pPlayer->rank) pPlayer->Disconnect();
	}
	if ( (g_rank.getRankNum() >= (int)csstats_maxsize->value) || ((int)csstats_reset->value == 1 ) ) {
		CVAR_SET_FLOAT("csstats_reset",0.0);
		g_rank.clear(); // clear before save to file
	}
	g_rank.saveRank( MF_BuildPathname("%s",get_localinfo("csstats")) );	

	// clear custom weapons info
	for ( i=MAX_WEAPONS;i<MAX_WEAPONS+MAX_CWEAPONS;i++)
		weaponData[i].used = false;

	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]  ){
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	
	if (pPlayer->pEdict == NULL)
	{
		pPlayer->Init(ENTINDEX(pEntity), pEntity);
	}
	
	pPlayer->Connect(pszAddress);

	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void ClientDisconnect( edict_t *pEntity ) {
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (pPlayer->rank) pPlayer->Disconnect();
	RETURN_META(MRES_IGNORED);
}

void ClientPutInServer_Post( edict_t *pEntity ) {
	GET_PLAYER_POINTER(pEntity)->PutInServer();
	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged_Post( edict_t *pEntity, char *infobuffer ) {
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	if (pPlayer->pEdict == NULL)
	{
		pPlayer->Init(ENTINDEX(pEntity), pEntity);
	}

	const char* name = INFOKEY_VALUE(infobuffer,"name");
	const char* oldname = STRING(pEntity->v.netname);

	if ( pPlayer->rank ){
		if ( strcmp(oldname,name) != 0 ) {
			if ((int)csstats_rank->value == 0)
				pPlayer->rank = g_rank.findEntryInRank( name, name );
			else
				pPlayer->rank->setName( name );
		}
	}
	else if ( pPlayer->IsBot() ) {
		pPlayer->Connect( "127.0.0.1" );
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
		mPlayer = 0;
	}
	mState = 0;
	g_CurrentMsg = msg_type;
	if ( g_CurrentMsg < 0 || g_CurrentMsg >= MAX_REG_MSGS )
		g_CurrentMsg = 0;
	function=modMsgs[g_CurrentMsg];
	endfunction=modMsgsEnd[g_CurrentMsg];
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

void StartFrame_Post(){
	if (g_bombAnnounce){
		switch (g_bombAnnounce){
		case BOMB_PLANTING:
			MF_ExecuteForward( iFBPlanting, static_cast<cell>(g_Planter) );
			break;
		case BOMB_PLANTED:
			MF_ExecuteForward( iFBPlanted, static_cast<cell>(g_Planter) );
			break;
		case BOMB_EXPLODE:
			MF_ExecuteForward( iFBExplode, static_cast<cell>(g_Planter), static_cast<cell>(g_Defuser) );
			break;
		case BOMB_DEFUSING:
			MF_ExecuteForward( iFBDefusing, static_cast<cell>(g_Defuser) );
			break;
		case BOMB_DEFUSED:
			MF_ExecuteForward( iFBDefused, static_cast<cell>(g_Defuser) );
			break;
		}
		g_bombAnnounce = 0;
	}
	RETURN_META(MRES_IGNORED);
}

void SetModel_Post(edict_t *e, const char *m){

	if ( !isModuleActive() )
	{
		RETURN_META(MRES_IGNORED);
	}

	if ( e->v.owner && m[7]=='w' && m[8]=='_' ){
		int w_id = 0;
		CPlayer *pPlayer = GET_PLAYER_POINTER(e->v.owner);
		switch(m[9]){
		case 'h':
			w_id = CSW_HEGRENADE;
			g_grenades.put(e, 2.0, 4, pPlayer);
			pPlayer->saveShot(CSW_HEGRENADE);
			break;
		case 'f':
			if (m[10]=='l') w_id = CSW_FLASHBANG;
			break;
		case 's':
			if (m[10]=='m') w_id = CSW_SMOKEGRENADE;
			break;
		}
		if ( w_id )	
			MF_ExecuteForward( iFGrenade, static_cast<cell>(pPlayer->index), 
			static_cast<cell>(ENTINDEX(e)), static_cast<cell>(w_id));
	}

	RETURN_META(MRES_IGNORED);
}

void EmitSound_Post(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch) {
	if (sample[0]=='w'&&sample[1]=='e'&&sample[8]=='k'&&sample[9]=='n'&&sample[14]!='d'){
		CPlayer*pPlayer = GET_PLAYER_POINTER(entity);
		pPlayer->saveShot(pPlayer->current);
	}
	RETURN_META(MRES_IGNORED);
}

void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr)
{
	if (ptr->pHit && (ptr->pHit->v.flags & (FL_CLIENT|FL_FAKECLIENT))
		&& e 
		&& (e->v.flags & (FL_CLIENT|FL_FAKECLIENT)) 
		&& ptr->iHitgroup)
	{
		CPlayer *pPlayer = GET_PLAYER_POINTER(e);
		if (pPlayer->current != CSW_KNIFE)
		{
			pPlayer->aiming = ptr->iHitgroup;
		}
	}

	RETURN_META(MRES_IGNORED);
}

void OnMetaAttach() {
	CVAR_REGISTER (&init_csstats_maxsize);
	CVAR_REGISTER (&init_csstats_reset);
	CVAR_REGISTER (&init_csstats_rank);
	csstats_maxsize=CVAR_GET_POINTER(init_csstats_maxsize.name);
	csstats_reset=CVAR_GET_POINTER(init_csstats_reset.name);
	csstats_rank=CVAR_GET_POINTER(init_csstats_rank.name);

	CVAR_REGISTER (&init_csstats_rankbots);
	CVAR_REGISTER (&init_csstats_pause);
	csstats_rankbots = CVAR_GET_POINTER(init_csstats_rankbots.name);
	csstats_pause = CVAR_GET_POINTER(init_csstats_pause.name);
}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "cstrike") == 0 ||
		strcasecmp(game, "czero") == 0)
	{
		return AMXX_GAME_OK;
	}
	return AMXX_GAME_BAD;
}
void OnAmxxAttach(){
	MF_AddNatives(stats_Natives);
	const char* path =  get_localinfo("csstats_score");
	if ( path && *path ) 
	{
		char error[128];
		g_rank.loadCalc( MF_BuildPathname("%s",path) , error  );
	}
	
	if ( !g_rank.begin() )
	{		
		g_rank.loadRank( MF_BuildPathname("%s",
			get_localinfo("csstats") ) );
	}
}

void OnAmxxDetach() {
	g_grenades.clear();
	g_rank.clear();
	g_rank.unloadCalc();
}

void OnPluginsLoaded(){
	iFDeath = MF_RegisterForward("client_death",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFDamage = MF_RegisterForward("client_damage",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFBPlanted = MF_RegisterForward("bomb_planted",ET_IGNORE,FP_CELL,FP_DONE);
	iFBDefused = MF_RegisterForward("bomb_defused",ET_IGNORE,FP_CELL,FP_DONE);
	iFBPlanting = MF_RegisterForward("bomb_planting",ET_IGNORE,FP_CELL,FP_DONE);
	iFBDefusing = MF_RegisterForward("bomb_defusing",ET_IGNORE,FP_CELL,FP_DONE);
	iFBExplode = MF_RegisterForward("bomb_explode",ET_IGNORE,FP_CELL,FP_CELL,FP_DONE);
	iFGrenade = MF_RegisterForward("grenade_throw",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
}
