#include "ns.h"


#include <sdk_util.h> //useful almost everywhere
#include <usercmd.h>
#include <entity_state.h>

CSpawn ns_spawnpoints;
CPlayer g_player[33];
edict_t *player_edicts[33];

int gmsgHudText2=0;
int ChangeclassForward = 0;
int BuiltForward = 0;

// Index of last entity hooked in CreateNamedEntity
int iCreateEntityIndex;
BOOL iscombat;
int g_MsgInfo[10];
int g_MsgLength;
int gmsgScoreInfo=0;
int hooked_msg=0;
int hooked_dest=0;


int gmsgShowMenu=0;
int gmsgResetHUD=0;

// Module is attaching to AMXX
void OnAmxxAttach()
{
	MF_AddNatives(ns_misc_natives);
	MF_AddNatives(ns_menu_natives);
	MF_AddNatives(ns_pdata_natives);
}

// All plugins have loaded (probably around Spawning worldspawn..
void OnPluginsLoaded()
{

	gmsgHudText2 = GET_USER_MSG_ID(&Plugin_info,"HudText2",NULL);
	// Check the map name and see if it's combat or not.
	iscombat=FALSE;
	char mapname[255];
	strcpy(mapname,STRING(gpGlobals->mapname));
	if ((mapname[0]=='c' || mapname[0]=='C') && (mapname[1]=='o' || mapname[0]=='O') && mapname[2]=='_')
		iscombat=TRUE;

	ChangeclassForward = MF_RegisterForward("client_changeclass", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL);
	// No sense in this if it's combat..
	if (!iscombat)
		BuiltForward = MF_RegisterForward("client_built", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL);
}


void ClientCommand(edict_t *pEntity)
{
	CPlayer *player = GET_PLAYER_E(pEntity);
	if (player->ClientCommand())
		RETURN_META(MRES_SUPERCEDE);
	RETURN_META(MRES_IGNORED);
}

int Spawn(edict_t *pEntity) 
{
	// Everything starting up:
	//   - Reset CPlayer classes
	//   - Reset CSpawn classes
	if (FStrEq(STRING(pEntity->v.classname),"worldspawn"))
	{
		int i;
		for (i=0;i<=32;i++)
		{
			CPlayer *player = GET_PLAYER_I(i);
			player->Reset();
		}
		ns_spawnpoints.clear();
	}
	else if (FStrEq(STRING(pEntity->v.classname),"info_player_start"))
	{
		// Mark down the ready room spawn point.
		ns_spawnpoints.put(0,pEntity->v.origin);
	}
	else if (FStrEq(STRING(pEntity->v.classname),"info_team_start"))
	{
		// Mark down the team based spawn point.
		ns_spawnpoints.put(pEntity->v.team,pEntity->v.origin);
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}


void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	// Mark down proper edicts (fixes INDEXENT() bug).
	// Reset CPlayer classes (again?)
	for(int i = 1; i <= gpGlobals->maxClients;i++) 
	{
		player_edicts[i]=pEdictList + i;
		CPlayer *player = GET_PLAYER_I(i);
		player->edict=pEdictList + i;
		player->index=i;
		player->pev=&player->edict->v;
		player->oldimpulse=0;
		player->Reset();
		player->connected=false;
	}
	RETURN_META(MRES_IGNORED);
}
void PlayerPreThink(edict_t *pEntity)
{
	CPlayer *player = GET_PLAYER_E(pEntity);
	player->PreThink();
	RETURN_META(MRES_IGNORED);
}
void PlayerPreThink_Post(edict_t *pEntity)
{
	CPlayer *player = GET_PLAYER_E(pEntity);
	player->PreThink_Post();
	RETURN_META(MRES_IGNORED);
}
void PlayerPostThink_Post(edict_t *pEntity)
{
	CPlayer *player = GET_PLAYER_E(pEntity);
	player->PostThink_Post();
	RETURN_META(MRES_IGNORED);
}


void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (gmsgShowMenu==0)
		gmsgShowMenu=GET_USER_MSG_ID(&Plugin_info,"ShowMenu",NULL);
	if (gmsgResetHUD==0)
		gmsgResetHUD=GET_USER_MSG_ID(&Plugin_info,"ResetHUD",NULL);
	if (msg_dest == MSG_ALL || msg_dest == MSG_BROADCAST)
		hooked_dest = 0;
	else
		hooked_dest = ENTINDEX(ed);
	hooked_msg = msg_type;
	RETURN_META(MRES_IGNORED);
}
void MessageEnd_Post(void)
{
	if (hooked_msg == gmsgResetHUD && hooked_dest != 0)
	{
		CPlayer *player = GET_PLAYER_I(hooked_dest);
		if (!player->connected && player->bot)
			RETURN_META(MRES_IGNORED);
		if (player->menucmd.inmenu == true && player->menucmd.time > gpGlobals->time)
		{
			// Reset the hold time, or the menu display and menu keys will become terribly out of sync.
			player->menuhudtext.holdTime=player->menucmd.time - gpGlobals->time;
			HudMessage(/* 0 */ hooked_dest,player->menuhudtext,player->menucmd.text);
		}
	}
	RETURN_META(MRES_IGNORED);
}

 
// Parse log messages here for any desired information (structure_built, etc.)
// The following logs are needed:
// name<CID><AUTHID><TEAM> triggered "structure_built" (type "type") -- client_built
// name<CID><AUTHID><TEAM> changed role to "class" -- client_changeclass
void AlertMessage_Post(ALERT_TYPE atype, char *szFmt, ...)
{
	if (atype != at_logged || iscombat)
		RETURN_META(MRES_IGNORED);
	va_list LogArg;
	char *sz, *message;
	const char *b;
	char szParm[5][128];
	int argc,len;
	va_start(LogArg, szFmt);
	sz = va_arg(LogArg, char *);
	va_end(LogArg);
	message = sz;
	b=message;
	argc=0;
	// Parse the damn message
	while (*b && *b!='\0' && argc<5)
	{

		len=0;
		if (*b == '"')
		{
			b++; // Skip over the "
			while (*b && *b != '"' && len < 127)
			{
				szParm[argc][len]=*b;
				b++;
				len++;
			}
			//*szParm='\0';
			szParm[argc][len]='\0';
			if (*b && *b == '"' && *b+1 != '\0' && *b+2 != '\0')
			{
				b+=2;
				argc++;
			}
			else
			{
				argc++;
				break;
			}
		}
		else if (*b == '(')
		{
			b++; // Skip over the (
			while (*b && *b != ')' && len < 127)
			{
				szParm[argc][len]=*b;
				b++;
				len++;
			}
			szParm[argc][len]='\0';
			if (*b && *b == ')' && *b+1 != '\0' && *b+2 != '\0')
			{
				b+=2;
				argc++;
			}
			else
			{
				argc++;
				break;
			}
		}
		else
		{
			while (*b && *b != '"' && *b != '(' && len < 127)
			{
				szParm[argc][len]=*b;
				b++;
				len++;
			}
			szParm[argc][len]='\0';
			if (*b != '"' && *b != '(' && *b != '\0' && *b+1 != '\0' && *b+2 != '\0')
			{
				b+=2;
				argc++;
			}
			else
			{
				argc++;
				if (*b == '\0')
					break;
			}

		}
		
		
	}
/*
	if (argc == 3) // changed role to = 3 long
	{
		if (FStrEq((const char *)szParm[1],"changed role to "))
		{
			int index=LogToIndex(szParm[0]);
			if (!index)
				RETURN_META(MRES_IGNORED);
			CPlayer *player = GET_PLAYER_I(index);
			int iImpulse=0;
			int iClass=1;
			if (INDEXENT2(index)->v.team != 0)
			{
				if (FStrEq((const char *)szParm[2],"gestate"))
				{
					iImpulse = player->oldpev.impulse;
				}
				if (FStrEq((const char *)szParm[2],"none"))
					iClass=0;
				if (iClass > 0)
				{
					ns2amx_changeclass.execute(index,player->oldpev.iuser3,player->pev->iuser3,iImpulse);
				}
			}
		}
	}
	else */
	if (argc == 4) // structure_built / structure_destroyed are 4 long
	{
		//"NAME<CID><AUTH><TEAM>" triggered "structure_built" (type "TYPE")
		if (FStrEq((const char *)szParm[2],"structure_built"))
		{
			int index=LogToIndex(szParm[0]);
			if (!index)
				RETURN_META(MRES_IGNORED);
			CPlayer *player = GET_PLAYER_I(index);

			int iForward=0;
			int iType=player->pev->impulse;
			if (FStrEq((const char *)szParm[3],"type \"team_hive\""))
			{
				iForward=2;
				iCreateEntityIndex=Find_Building_Hive();
			}
			else if (FStrEq((const char *)szParm[3],"type \"offensechamber\""))
			{
				iForward=2;
			}
			else if (FStrEq((const char *)szParm[3],"type \"movementchamber\""))
			{
				iForward=2;
			}
			else if (FStrEq((const char *)szParm[3],"type \"sensorychamber\""))
			{
				iForward=2;
			}
			else if (FStrEq((const char *)szParm[3],"type \"defensechamber\""))
			{
				iForward=2;
			}
			else if (FStrEq((const char *)szParm[3],"type \"alienresourcetower\""))
			{
				iForward=2;
			}
			else
			{
				iForward = 1;
			}
//			ns2amx_built.execute(index,iCreateEntityIndex,iForward,iType);
			MF_ExecuteForward(BuiltForward, index, iCreateEntityIndex, iForward, iType);
			iCreateEntityIndex=0;
		}
	}
	RETURN_META(MRES_IGNORED);
}

// We hook newly created entities here.
// This is where we check for client_built created entities.
edict_t* CreateNamedEntity(int className)
{
	if (iscombat)
		RETURN_META_VALUE(MRES_IGNORED,0);
	edict_t *pEntity;
	// Incase another plugin supercedes/overrides, use their returned value here.
	// (Untested).
	if (gpMetaGlobals->status >= MRES_OVERRIDE)
	{
		pEntity=META_RESULT_OVERRIDE_RET(edict_t *);
		iCreateEntityIndex=ENTINDEX(pEntity);
		RETURN_META_VALUE(MRES_IGNORED, false);
	}
	else
	{
		pEntity=CREATE_NAMED_ENTITY(className);
		if (!FNullEnt(pEntity))
		{
			iCreateEntityIndex=ENTINDEX(pEntity);
			RETURN_META_VALUE(MRES_SUPERCEDE,pEntity);
		}
		RETURN_META_VALUE(MRES_SUPERCEDE,pEntity);
	}
	RETURN_META_VALUE(MRES_IGNORED,false);
}

// Map is changing/server is shutting down.
// We do all cleanup routines here, since, as noted in metamod's dllapi
// ServerDeactivate is the very last function called before the server loads up a new map.
void ServerDeactivate(void)
{
	for (int i=1;i<=gpGlobals->maxClients;i++)
	{
		CPlayer *player = GET_PLAYER_I(i);
		if (player->connected)
			player->Disconnect();
	}
	ns_spawnpoints.clear();
	RETURN_META(MRES_IGNORED);
}

// Reset player data here..
qboolean ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ])
{
	// Client's connecting.  Freshen up his save data, and mark him as being connected.
	CPlayer *player = GET_PLAYER_E(pEntity);
	player->Connect();
	RETURN_META_VALUE(MRES_HANDLED,0);
}
void ClientDisconnect(edict_t *pEntity)
{
	// Client is disconnecting, clear all his saved information.
	CPlayer *player = GET_PLAYER_E(pEntity);
	player->Disconnect();
	RETURN_META(MRES_HANDLED);
}

// NS resets pev->fov every single frame, but this is called right before the data is sent to the client.
// Reset FOV if we need to.
void UpdateClientData( const struct edict_s *ent, int sendweapons, struct clientdata_s *cd )
{
	edict_t *pEntity = (edict_t*)ent;
	CPlayer *player = GET_PLAYER_E(pEntity);
	if (player->foved)
		pEntity->v.fov = player->fov;

	RETURN_META(MRES_HANDLED);

}