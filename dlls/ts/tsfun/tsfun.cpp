/*
 * Copyright (c) 2003-2004 Lukasz Wlasinski
 *
 *    This file is part of TS XMod.
 *
 *    TS XMod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    TS XMod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with TS XMod; if not, write to the Free Software Foundation,
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

#include "tsfun.h"
#include "CNatives.h"

funEventCall modMsgsEnd[MAX_REG_MSGS];
funEventCall modMsgs[MAX_REG_MSGS];
void (*function)(void*);
void (*endfunction)(void*);
CPlayer* mPlayer;
int mPlayerIndex;
int mState;
CPlayer players[33];

int gKnifeOffset;

int gmsgResetHUD;
int gmsgWeaponInfo;
int gmsgClipInfo;
int gmsgScoreInfo;
int gmsgTSHealth;

int gmsgWStatus;
int gmsgTSCash;
int gmsgTSState;
int gmsgPwUp;

struct sUserMsg {
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

int Powerup = -1;
int KungFoo = -1;	//-1 is an invalid forward so initiate this to that
int Death = -1;
int Damage = -1;
int Stunt = -1;

void OnPluginsLoaded()
{
   //mymodule_Touch - the forward name
   //ET_IGNORE - means PLUGIN_HANDLED will be ignored (other option is ET_STOP)
   //FP_CELL - first parameter is a normal cell
   //FP_CELL - second parameter is a normal cell
   //FP_DONE - that's the end of the parameter descriptions
	KungFoo = MF_RegisterForward("Melee_Attack", ET_STOP, FP_CELL, FP_FLOAT,FP_FLOAT, FP_DONE);
	Death = MF_RegisterForward("client_death", ET_IGNORE, FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL, FP_DONE);
	Damage = MF_RegisterForward("client_damage", ET_IGNORE, FP_CELL,FP_CELL,FP_CELL,FP_CELL,FP_CELL, FP_DONE);
	Powerup = MF_RegisterForward("client_powerup", ET_IGNORE, FP_CELL, FP_CELL, FP_DONE);
	Stunt = MF_RegisterForward("client_stunt", ET_IGNORE, FP_CELL, FP_CELL, FP_DONE);
}

void check_powerup(edict_s *player)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(player);
	int offset1 = 423;

#ifdef __linux__
	offset1 += 5;
#endif

	int id = ENTINDEX(player);
	int val3 = *((int *)player->pvPrivateData + offset1);

	if(pPlayer->set_423 == -1)
	{
		pPlayer->set_423 = val3;
		return;
	}
	else if(pPlayer->set_423 != val3) MF_ExecuteForward(Powerup, id,val3);
}

void check_kungfoo(edict_s *player)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(player);
	int offset1 = 454;
	int offset2 = 455;

#ifdef __linux__
	offset1 += 5;
	offset2 += 5;
#endif

	int id = ENTINDEX(player);
	float val4 = *((float *)player->pvPrivateData + offset1);
	float val5 = *((float *)player->pvPrivateData + offset2);

	if(pPlayer->set_454 == -1.0)
	{
		pPlayer->set_454 = val4;
		pPlayer->set_455 = val5;
		return;
	}
	else
	{
		if(pPlayer->set_454 != val4 || pPlayer->set_455 != val5) 
		{
			int check = MF_ExecuteForward(KungFoo, id,val4,val5);
			if(check) 
			{
				*((float *)player->pvPrivateData + offset1) = 0.0;
				*((float *)player->pvPrivateData + offset2) = 0.0;

				pPlayer->set_454 = -1.0;
				pPlayer->set_455 = -1.0;
			}
		}
	}
}

void check_stunts(edict_s *player)
{
	CPlayer *pPlayer = GET_PLAYER_POINTER(player);

	if(pPlayer->checkstate == 0) return;

	int stunttype;
	int newstate = pPlayer->state;
	int oldstate = pPlayer->oldstate;
	
	if(newstate == 0) stunttype = STUNT_NONE;
	else if(newstate == 2) stunttype = STUNT_DIVE;
	else if(oldstate == 2) stunttype = STUNT_GETUP;
	else if( pPlayer->GetOffsetInt(158) == 1) stunttype = STUNT_ROLL;
	else if( pPlayer->GetOffsetInt(27) == 1 ) stunttype = STUNT_DUCK;
	else stunttype = STUNT_FLIP;

	pPlayer->checkstate = 0;

	MF_ExecuteForward(Stunt,pPlayer->index,stunttype);
}

void PlayerPreThink_Post(edict_s *player)
{
	check_powerup(player);
	check_kungfoo(player);
	check_stunts(player);
	RETURN_META(MRES_IGNORED);
}


void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	if (ed)
	{
		mPlayerIndex = ENTINDEX(ed);
		mPlayer = GET_PLAYER_POINTER_I(mPlayerIndex);
	} 
	else 
	{

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

			break;
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ServerActivate_Post( edict_t *pEdictList, int edictCount, int clientMax )
{
	for( int i = 1; i <= gpGlobals->maxClients; ++i )
		GET_PLAYER_POINTER_I(i)->Init( i , pEdictList + i );
	
	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate() 
{
	int i;
	for(i = 1;i<=gpGlobals->maxClients; ++i){
		CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);
		pPlayer->Disconnect();
	}

	// clear custom weapons info
	for ( i=TSMAX_WEAPONS-TSMAX_CUSTOMWPNS;i<TSMAX_WEAPONS;i++)
		weaponData[i].custom = false;

	RETURN_META(MRES_IGNORED);
}

void OnAmxxAttach() 
{

	gKnifeOffset = TSKNIFE_OFFSET;
	MF_AddNatives( base_Natives );
}

