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

#include "tfcx.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);
CPlayer* mPlayer;
CPlayer players[33];

bool rankBots;
int pdTimerOwner;
int pdSentryGunOwner;
int pdAmmo[6];

int gmsgCurWeapon;
int gmsgDamage;
int gmsgDamage2;
int gmsgWeaponList;
int gmsgResetHUD;
int gmsgAmmoX;
int gmsgScoreInfo;
int gmsgAmmoPickup;

int mState;
int mPlayerIndex;

int g_death_info = -1;
int g_damage_info = -1;

int g_AlliesFlags[4];

RankSystem g_rank;
Grenades g_grenades;

cvar_t init_tfcstats_maxsize ={"tfcstats_maxsize","3500", 0 , 3500.0 };
cvar_t init_tfcstats_reset ={"tfcstats_reset","0"};
cvar_t init_tfcstats_rank ={"tfcstats_rank","0"};
cvar_t *tfcstats_rankbots;
cvar_t *tfcstats_pause;

cvar_t init_tfcstats_rankbots ={"tfcstats_rankbots","1"};
cvar_t init_tfcstats_pause = {"tfcstats_pause","0"};
cvar_t *tfcstats_maxsize;
cvar_t *tfcstats_reset;
cvar_t *tfcstats_rank;

struct sUserMsg {
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} g_user_msg[] = {
	{ "CurWeapon" , &gmsgCurWeapon , Client_CurWeapon, false },
	{ "Damage" , &gmsgDamage,Client_Damage, false  },
	{ "Damage" , &gmsgDamage2,Client_Damage_End, true  },
	//{ "WeaponList" , &gmsgWeaponList, Client_WeaponList, false},
	{ "ResetHUD" , &gmsgResetHUD,Client_ResetHUD, true },
	{ "AmmoX" , &gmsgAmmoX, Client_AmmoX , false },
	{ "ScoreInfo" , &gmsgScoreInfo, Client_ScoreInfo, false},
	{ "AmmoPickup" , &gmsgAmmoPickup, Client_AmmoPickup , false },
	{ 0 , 0,0,false }
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

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax ){
	
	rankBots = (int)tfcstats_rankbots->value ? true:false;

	for( int i = 1; i <= gpGlobals->maxClients; ++i )
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );

	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post( edict_t *pEntity ) {
	CPlayer *pPlayer = GET_PLAYER_POINTER(pEntity);
        if ( !isModuleActive() ) // stats only
		return;

	if (pPlayer->clearStats && pPlayer->clearStats < gpGlobals->time && pPlayer->ingame){

		if ( !ignoreBots(pEntity,NULL) ){
			pPlayer->clearStats = 0.0f;
			pPlayer->rank->updatePosition( &pPlayer->life );
			pPlayer->restartStats(false);
		}

	}
	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate() {
	int i;
	for(i = 1;i<=gpGlobals->maxClients; ++i){
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
		if (pPlayer->rank) pPlayer->Disconnect();
	}

	if ( (g_rank.getRankNum() >= (int)tfcstats_maxsize->value) || ((int)tfcstats_reset->value == 1) ) {
		CVAR_SET_FLOAT("tfcstats_reset",0.0);
		g_rank.clear();
	}
	
	g_rank.saveRank( MF_BuildPathname("%s",get_localinfo("tfcstats") ) );

	// clear custom weapons info
	for ( i=TFCMAX_WEAPONS-TFCMAX_CUSTOMWPNS;i<TFCMAX_WEAPONS;i++)
		weaponData[i].ammoSlot = false;
	
	g_grenades.clear();

	g_AlliesFlags[0]=0;
	g_AlliesFlags[1]=0;
	g_AlliesFlags[2]=0;
	g_AlliesFlags[3]=0;


	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect_Post( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]  ){
	GET_PLAYER_POINTER(pEntity)->Connect(pszAddress);
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
	const char* name = INFOKEY_VALUE(infobuffer,"name");
	const char* oldname = STRING(pEntity->v.netname);

	if ( pPlayer->rank ){
		if ( strcmp(oldname,name) != 0 ) {
			if ((int)tfcstats_rank->value == 0)
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
		mPlayer = NULL;
	}
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

void SetModel_Post(edict_t *e, const char *m){
	if(e->v.owner && e->v.owner->v.flags&(FL_CLIENT | FL_FAKECLIENT)){
		CPlayer *pPlayer =  GET_PLAYER_POINTER(e->v.owner);
		if ( !STRING(e->v.classname)[0] ){ // current weapon shot
			if ( pPlayer->classId != TFC_PC_SOLDIER )
				pPlayer->saveShot(pPlayer->current);
		}
	}
	RETURN_META(MRES_IGNORED);
}

void TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *e, TraceResult *ptr) {
	if ( !e )
		RETURN_META(MRES_IGNORED);

	if (ptr->pHit&&(ptr->pHit->v.flags& (FL_CLIENT | FL_FAKECLIENT) )&&
		(e->v.flags& (FL_CLIENT | FL_FAKECLIENT) )){
		GET_PLAYER_POINTER(e)->aiming = ptr->iHitgroup;
	}
	else{
		if ( e->v.owner && e->v.owner->v.flags& (FL_CLIENT | FL_FAKECLIENT) ){
			CPlayer *pPlayer = GET_PLAYER_POINTER(e->v.owner);
			int i;
			for ( i=0;i<MAX_TRACE;i++){
				if ( util_strncmp( traceData[i].szTag,traceData[i].start ? STRING(e->v.classname)+traceData[i].start :  STRING(e->v.classname) ,traceData[i].stop) ){
					if ( traceData[i].iAction & ACT_NADE_SHOT  ){
						pPlayer->saveShot(traceData[i].iId);
					}
					if ( traceData[i].iAction & ACT_NADE_PUT ){
						g_grenades.put(e,traceData[i].fDel,traceData[i].iId,pPlayer);
					}
					break;
				}
			}
		}
	}
	RETURN_META(MRES_IGNORED);
}

void OnMetaAttach() {
	
	CVAR_REGISTER (&init_tfcstats_maxsize);
	CVAR_REGISTER (&init_tfcstats_reset);
	CVAR_REGISTER (&init_tfcstats_rank);
	tfcstats_maxsize=CVAR_GET_POINTER(init_tfcstats_maxsize.name);
	tfcstats_reset=CVAR_GET_POINTER(init_tfcstats_reset.name);
	tfcstats_rank=CVAR_GET_POINTER(init_tfcstats_rank.name);

	CVAR_REGISTER (&init_tfcstats_rankbots);
	CVAR_REGISTER (&init_tfcstats_pause);
	tfcstats_rankbots = CVAR_GET_POINTER(init_tfcstats_rankbots.name);
	tfcstats_pause = CVAR_GET_POINTER(init_tfcstats_pause.name);

}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "tfc") == 0)
		return AMXX_GAME_OK;

	return AMXX_GAME_BAD;
}
void OnAmxxAttach() {

	MF_AddNatives( stats_Natives );
	MF_AddNatives( base_Natives );

	const char* path =  get_localinfo("tfcstats_score","addons/amxmodx/data/tfcstats.amxx");
	if ( path && *path ) 
	{
		char error[128];
		g_rank.loadCalc( MF_BuildPathname("%s",path) , error  );
	}
	if ( !g_rank.begin() )
	{		
		g_rank.loadRank( MF_BuildPathname("%s",get_localinfo("tfcstats","addons/amxmodx/data/tfcstats.dat") ) );
	}

	// set default PrivateData offsets
	pdTimerOwner = PD_TIMER_OWNER;
	pdSentryGunOwner = PD_SENTRY_OWNER;
	pdAmmo[TFC_AMMO_SHELLS] = PD_AMMO_SHELLS;
	pdAmmo[TFC_AMMO_BULLETS] = PD_AMMO_BULLETS;
	pdAmmo[TFC_AMMO_CELLS] = PD_AMMO_CELLS;
	pdAmmo[TFC_AMMO_ROCKETS] = PD_AMMO_ROCKETS; 
	pdAmmo[TFC_AMMO_NADE1] = PD_AMMO_NADE1;
	pdAmmo[TFC_AMMO_NADE2] = PD_AMMO_NADE2;
	
	g_AlliesFlags[0]=0;
	g_AlliesFlags[1]=0;
	g_AlliesFlags[2]=0;
	g_AlliesFlags[3]=0;
}

void OnPluginsLoaded()
{
	g_damage_info = MF_RegisterForward("client_damage", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	g_death_info = MF_RegisterForward("client_death", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}

void DispatchKeyValue(edict_t *pentKeyvalue, KeyValueData *pkvd)
{
	if (pkvd->szClassName && strcmp(pkvd->szClassName,"info_tfdetect")==0)
	{
		if (pkvd->szKeyName && strncmp(pkvd->szKeyName,"team",4)==0)
		{
			if (strcmp(pkvd->szKeyName,"team1_allies")==0 && pkvd->szValue!=NULL)
			{
				g_AlliesFlags[0]=atoi(pkvd->szValue);

				RETURN_META(MRES_IGNORED);
			}
			else if (strcmp(pkvd->szKeyName,"team2_allies")==0 && pkvd->szValue!=NULL)
			{
				g_AlliesFlags[1]=atoi(pkvd->szValue);

				RETURN_META(MRES_IGNORED);
			}
			else if (strcmp(pkvd->szKeyName,"team3_allies")==0 && pkvd->szValue!=NULL)
			{
				g_AlliesFlags[2]=atoi(pkvd->szValue);

				RETURN_META(MRES_IGNORED);
			}
			else if (strcmp(pkvd->szKeyName,"team4_allies")==0 && pkvd->szValue!=NULL)
			{
				g_AlliesFlags[3]=atoi(pkvd->szValue);

				RETURN_META(MRES_IGNORED);
			}
		}
	}
	RETURN_META(MRES_IGNORED);
}
