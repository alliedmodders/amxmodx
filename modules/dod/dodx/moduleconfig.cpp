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
// DODX Module
//

#include "amxxmodule.h"
#include "dodx.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);
CPlayer* mPlayer;
CPlayer players[33];
CMapInfo g_map;

bool rankBots;
int mState;
int mDest;
int mCurWpnEnd;
int mPlayerIndex;

int AlliesScore;
int AxisScore;

int iFDamage = -1;
int iFDeath = -1;
int iFScore = -1;
int iFSpawnForward = -1;
int iFTeamForward = -1;
int iFClassForward = -1;
int iFScopeForward = -1;
int iFProneForward = -1;
int iFWpnPickupForward = -1;
int iFCurWpnForward = -1;
int iFGrenadeExplode = -1;
int iFRocketExplode = -1;
int iFObjectTouched = -1;
int iFStaminaForward = -1;

int gmsgCurWeapon;
int gmsgCurWeaponEnd;
int gmsgHealth;
int gmsgResetHUD;
int gmsgObjScore;
int gmsgRoundState;
int gmsgTeamScore;
int gmsgScoreShort;
int gmsgPTeam;
int gmsgAmmoX;
int gmsgAmmoShort;
int gmsgSetFOV;
int gmsgSetFOV_End;
int gmsgObject;
int gmsgObject_End;
int gmsgPStatus;

RankSystem g_rank;
Grenades g_grenades;

cvar_t init_dodstats_maxsize ={"dodstats_maxsize","3500", 0 , 3500.0 };
cvar_t init_dodstats_reset ={"dodstats_reset","0"};
cvar_t init_dodstats_rank ={"dodstats_rank","0"};
cvar_t init_dodstats_rankbots ={"dodstats_rankbots","1"};
cvar_t init_dodstats_pause = {"dodstats_pause","0"};
cvar_t *dodstats_maxsize;
cvar_t *dodstats_reset;
cvar_t *dodstats_rank;
cvar_t *dodstats_rankbots;
cvar_t *dodstats_pause;

// User Messages
struct sUserMsg 
{
	const char *name;
	int* id;
	funEventCall func;
	bool endmsg;
}
g_user_msg[] = 
{
	{ "CurWeapon",	&gmsgCurWeapon,			Client_CurWeapon,		false },
	{ "CurWeapon",	&gmsgCurWeaponEnd,		Client_CurWeapon_End,	true  },
	{ "ObjScore",	&gmsgObjScore,			Client_ObjScore,		false },
	{ "RoundState",	&gmsgRoundState,		Client_RoundState,		false },
	{ "Health",		&gmsgHealth,			Client_Health_End,		true  },
	{ "ResetHUD",	&gmsgResetHUD,			Client_ResetHUD_End,	true  },
	{ "TeamScore",	&gmsgTeamScore,			Client_TeamScore,		false },
	{ "AmmoX",		&gmsgAmmoX,				Client_AmmoX,			false },
	{ "AmmoShort",	&gmsgAmmoShort,			Client_AmmoShort,		false },
	{ "SetFOV",		&gmsgSetFOV,			Client_SetFOV,			false },
	{ "SetFOV",		&gmsgSetFOV_End,		Client_SetFOV_End,		true  },
	{ "Object",		&gmsgObject,			Client_Object,			false },
	{ "Object",		&gmsgObject_End,		Client_Object_End,		true  },
	{ "PStatus",	&gmsgPStatus,			Client_PStatus,			false },
	{ "ScoreShort",	&gmsgScoreShort,		NULL,					false },
	{ "PTeam",		&gmsgPTeam,				NULL,					false },
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
	for (int i = 0; g_user_msg[i].name; ++i )
	{
		if(!*g_user_msg[i].id && strcmp(g_user_msg[i].name, pszName) == 0)
		{
			int id = META_RESULT_ORIG_RET(int);

			*g_user_msg[i].id = id;

			if(g_user_msg[i].endmsg)
				modMsgsEnd[id] = g_user_msg[i].func;
			else
				modMsgs[id] = g_user_msg[i].func;
			break;
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ){

	rankBots = (int)dodstats_rankbots->value ? true:false;

	for( int i = 1; i <= gpGlobals->maxClients; ++i )
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );


	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post(edict_t *pEntity) 
{
	if ( !isModuleActive() )
		RETURN_META(MRES_IGNORED);

	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
	if (!pPlayer->ingame)
		RETURN_META(MRES_IGNORED);

	// Zors
	pPlayer->PreThink();

	if(pPlayer->clearStats && pPlayer->clearStats < gpGlobals->time)
	{
		if(!ignoreBots(pEntity))
		{
			pPlayer->clearStats = 0.0f;
			pPlayer->rank->updatePosition( &pPlayer->life );
			pPlayer->restartStats(false);
		}
	}

	if(pPlayer->clearRound && pPlayer->clearRound < gpGlobals->time)
	{
		pPlayer->clearRound = 0.0f;
		memset(static_cast<void *>(&pPlayer->round),0,sizeof(pPlayer->round));
		memset(&pPlayer->weaponsRnd,0,sizeof(pPlayer->weaponsRnd));
	}

	if (pPlayer->sendScore && pPlayer->sendScore < gpGlobals->time)
	{
		pPlayer->sendScore = 0;
		MF_ExecuteForward(iFScore, pPlayer->index, pPlayer->lastScore, pPlayer->savedScore);
	}

	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate() 
{
	int i;
	for( i = 1;i<=gpGlobals->maxClients; ++i)
	{
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
		if (pPlayer->ingame) pPlayer->Disconnect();
	}

	if ( (g_rank.getRankNum() >= (int)dodstats_maxsize->value) || ((int)dodstats_reset->value == 1) ) 
	{
		CVAR_SET_FLOAT("dodstats_reset",0.0);
		g_rank.clear();
	}

	g_rank.saveRank( MF_BuildPathname("%s",get_localinfo("dodstats") ) );

	// clear custom weapons info
	for ( i=DODMAX_WEAPONS-DODMAX_CUSTOMWPNS;i<DODMAX_WEAPONS;i++)
		weaponData[i].needcheck = false;

	g_map.Init();

	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]  )
{
	GET_PLAYER_POINTER(pEntity)->Connect(pszName,pszAddress);

	RETURN_META_VALUE(MRES_IGNORED, TRUE);
}

void ClientDisconnect( edict_t *pEntity ) 
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);

	if (pPlayer->ingame)
		pPlayer->Disconnect();

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

	if ( pPlayer->ingame)
	{
		if ( strcmp(oldname,name) ) 
		{
			if (!dodstats_rank->value)
				pPlayer->rank = g_rank.findEntryInRank( name, name );
			else
				pPlayer->rank->setName( name );
		}
	}

	else if ( pPlayer->IsBot() ) 
	{
		pPlayer->Connect( name , "127.0.0.1" );
		pPlayer->PutInServer();
	}

	RETURN_META(MRES_IGNORED);
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) 
{
	if(ed)
	{
		mPlayerIndex = ENTINDEX(ed);
		mPlayer = GET_PLAYER_POINTER_I(mPlayerIndex);
	} 
	
	else 
	{
		mPlayerIndex = 0;
		mPlayer = NULL;
	}

	mDest = msg_dest;
	mState = 0;

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

void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr) 
{
	if(ptr->pHit && (ptr->pHit->v.flags&(FL_CLIENT | FL_FAKECLIENT)) &&	e && (e->v.flags&(FL_CLIENT | FL_FAKECLIENT)))
	{
		GET_PLAYER_POINTER(e)->aiming = ptr->iHitgroup;
		RETURN_META(MRES_IGNORED);
	}

	if(e && e->v.owner && e->v.owner->v.flags&(FL_CLIENT | FL_FAKECLIENT))
	{
		CPlayer *pPlayer = GET_PLAYER_POINTER(e->v.owner);

		for(int i = 0;i < MAX_TRACE; i++)
		{
			if(strcmp(traceData[i].szName, STRING(e->v.classname)) == 0)
			{
				int grenId = (traceData[i].iId == 13 && g_map.detect_allies_country) ? 36 : traceData[i].iId;
				int rocketId = traceData[i].iId;

				if(traceData[i].iAction&ACT_NADE_SHOT)
				{
					if(traceData[i].iId == 13 && g_map.detect_allies_country)
						pPlayer->saveShot(grenId);
					else
						pPlayer->saveShot(traceData[i].iId);
				}
				
				else if(traceData[i].iAction&ACT_ROCKET_SHOT)
						pPlayer->saveShot(traceData[i].iId);

				cell position[3];
				position[0] = amx_ftoc(v2[0]);
				position[1] = amx_ftoc(v2[1]);
				position[2] = amx_ftoc(v2[2]);
				cell pos = MF_PrepareCellArray(position, 3);

				if(traceData[i].iAction&ACT_NADE_PUT)
				{
					g_grenades.put(e, traceData[i].fDel, grenId, GET_PLAYER_POINTER(e->v.owner));
					MF_ExecuteForward(iFGrenadeExplode, GET_PLAYER_POINTER(e->v.owner)->index, pos, grenId);
				}

				if(traceData[i].iAction&ACT_ROCKET_PUT)
					MF_ExecuteForward(iFRocketExplode, pPlayer->index, pos, rocketId);

				break;
			}
		}
	}
	RETURN_META(MRES_IGNORED);
}

void DispatchKeyValue_Post( edict_t *pentKeyvalue, KeyValueData *pkvd )
{
	if ( !pkvd->szClassName ){ 
		// info_doddetect
		if ( pkvd->szValue[0]=='i' && pkvd->szValue[5]=='d' ){
			g_map.pEdict = pentKeyvalue;
			g_map.initialized = true;
		}
		RETURN_META(MRES_IGNORED);
	}
	// info_doddetect
	if ( g_map.initialized && pentKeyvalue == g_map.pEdict ){
		if ( pkvd->szKeyName[0]=='d' && pkvd->szKeyName[7]=='a' ){
			if ( pkvd->szKeyName[8]=='l' ){
				switch ( pkvd->szKeyName[14] ){
				case 'c':
					g_map.detect_allies_country=atoi(pkvd->szValue);
					break;
				case 'p':
					g_map.detect_allies_paras=atoi(pkvd->szValue);
					break;
				}
			}
			else if ( pkvd->szKeyName[12]=='p' ) g_map.detect_axis_paras=atoi(pkvd->szValue);
		}
	}
	RETURN_META(MRES_IGNORED);
}

void SetClientKeyValue(int id, char *protocol, const char *type, const char *var)
{
	// ID: Number
	// protocol: \name\Sgt.MEOW\topcolor\1\bottomcolor\1\cl_lw\1\team\axis\model\axis-inf 
	// type: model
	// var: axis-inf

	// Check to see if its a player and we are setting a model
	if(strcmp(type, "model") == 0 && 
		(strcmp(var, "axis-inf") == 0 ||
		 strcmp(var, "axis-para") == 0 || 
		 strcmp(var, "us-inf") == 0 ||
		 strcmp(var, "us-para") == 0 || 
		 strcmp(var, "brit-inf") == 0))
	{
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);
		if(!pPlayer->ingame)
			RETURN_META(MRES_IGNORED);

		if(pPlayer->setModel())
			RETURN_META(MRES_SUPERCEDE);
	}

	RETURN_META(MRES_IGNORED);
}


void OnMetaAttach()
{
	CVAR_REGISTER (&init_dodstats_maxsize);
	CVAR_REGISTER (&init_dodstats_reset);
	CVAR_REGISTER (&init_dodstats_rank);
	CVAR_REGISTER (&init_dodstats_rankbots);
	CVAR_REGISTER (&init_dodstats_pause);
	dodstats_maxsize=CVAR_GET_POINTER(init_dodstats_maxsize.name);
	dodstats_reset=CVAR_GET_POINTER(init_dodstats_reset.name);
	dodstats_rank=CVAR_GET_POINTER(init_dodstats_rank.name);
	dodstats_rankbots = CVAR_GET_POINTER(init_dodstats_rankbots.name);
	dodstats_pause = CVAR_GET_POINTER(init_dodstats_pause.name);
}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "dod") == 0)
		return AMXX_GAME_OK;

	return AMXX_GAME_BAD;
}
void OnAmxxAttach()
{
	MF_AddNatives( stats_Natives );
	MF_AddNatives( base_Natives );

	const char* path =  get_localinfo("dodstats_score","addons/amxmodx/data/dodstats.amxx");

	if ( path && *path )
	{
		char error[128];
		g_rank.loadCalc( MF_BuildPathname("%s",path) , error, sizeof(error));
	}

	if ( !g_rank.begin() )
	{
		g_rank.loadRank( MF_BuildPathname("%s",
		get_localinfo("dodstats","addons/amxmodx/data/dodstats.dat") ) );
	}

	g_map.Init();
}

void OnAmxxDetach()
{
	g_rank.clear();
	g_grenades.clear();
	g_rank.unloadCalc();
}

void OnPluginsLoaded()
{
	iFDeath = MF_RegisterForward("client_death",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFDamage = MF_RegisterForward("client_damage",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFScore = MF_RegisterForward("client_score",ET_IGNORE,FP_CELL,FP_CELL,FP_CELL,FP_DONE);
	iFTeamForward = MF_RegisterForward("dod_client_changeteam",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*team*/,FP_CELL/*oldteam*/,FP_DONE);
	iFSpawnForward = MF_RegisterForward("dod_client_spawn",ET_IGNORE,FP_CELL/*id*/,FP_DONE);
	iFClassForward = MF_RegisterForward("dod_client_changeclass",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*class*/,FP_CELL/*oldclass*/,FP_DONE);
	iFScopeForward = MF_RegisterForward("dod_client_scope",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*value*/,FP_DONE);
	iFWpnPickupForward = MF_RegisterForward("dod_client_weaponpickup",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*weapon*/,FP_CELL/*value*/,FP_DONE);
	iFProneForward = MF_RegisterForward("dod_client_prone",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*value*/,FP_DONE);
	iFCurWpnForward = MF_RegisterForward("dod_client_weaponswitch",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*wpnold*/,FP_CELL/*wpnew*/,FP_DONE);
	iFGrenadeExplode = MF_RegisterForward("dod_grenade_explosion",ET_IGNORE,FP_CELL/*id*/,FP_ARRAY/*pos[3]*/,FP_CELL/*wpnid*/,FP_DONE);
	iFRocketExplode = MF_RegisterForward("dod_rocket_explosion",ET_IGNORE,FP_CELL/*id*/,FP_ARRAY/*pos[3]*/,FP_CELL/*wpnid*/,FP_DONE);
	iFObjectTouched = MF_RegisterForward("dod_client_objectpickup",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*object*/,FP_ARRAY/*pos[3]*/,FP_CELL/*value*/,FP_DONE);
	iFStaminaForward = MF_RegisterForward("dod_client_stamina",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*stamina*/,FP_DONE);
}
