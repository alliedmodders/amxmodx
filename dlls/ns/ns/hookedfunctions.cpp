#include "ns.h"


#include <sdk_util.h> //useful almost everywhere
#include <usercmd.h>
#include <entity_state.h>

CSpawn ns_spawnpoints;
CPlayer g_player[33];
edict_t *player_edicts[33];

int gmsgHudText2=0;
int ChangeclassForward = -1;
int BuiltForward = -1;
int SpawnForward = -1;
int TeamForward = -1;
// Index of last entity hooked in CreateNamedEntity
int iCreateEntityIndex;
BOOL iscombat;
int gmsgScoreInfo=0;

// Module is attaching to AMXX
void OnAmxxAttach()
{
	MF_AddNatives(ns_misc_natives);
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

	ChangeclassForward = MF_RegisterForward("client_changeclass", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	// No sense in this if it's combat..
	if (!iscombat)
		BuiltForward = MF_RegisterForward("client_built", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	SpawnForward = MF_RegisterForward("client_spawn",ET_IGNORE,FP_CELL/*id*/,FP_DONE);
	TeamForward = MF_RegisterForward("client_changeteam",ET_IGNORE,FP_CELL/*id*/,FP_CELL/*new team*/,FP_CELL/*old team*/,FP_DONE);
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
		//player->index=i;
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
			if (BuiltForward != -1)
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
int LogToIndex(char logline[128])
{
	char *cname;
	// Format of log line:
	// name<CID><auth><team>
	// We need to find their ID from their name...
	int x,y=0;
	char cindex[64];
	// first we find the location of the start of the index
	// Name can contain <>'s, so we go from the end up.
	for (x=strlen(logline);x>=0;x--)
	{
		if (logline[x]=='<')
		{
			y++;
			if (y==3)
			{
				y=x;
				break;
			}
		}
	}
	// We found the end of the name, now copy the rest down.
	y--;
	x=0;
	while (x<=y)
	{
		cindex[x]=logline[x];
		x++;
	}
	cindex[x]='\0';
	// Now we have their name, now cycle through all players to find which index it is
	x=1;
	for (x;x<=gpGlobals->maxClients;x++)
	{
		cname=strdup(cindex);
		if (!FNullEnt(INDEXENT2(x)))
		{
			if (FStrEq(cname,STRING(INDEXENT2(x)->v.netname)))
			{
				return x;
			}
		}
	}
	return 0;
}
int Find_Building_Hive(void)
{
	edict_t *pEntity=NULL;
	while (pEntity = UTIL_FindEntityByString(pEntity,"classname","team_hive"))
	{
		if (pEntity->v.health > 0 && pEntity->v.solid > 0 && pEntity->v.fuser1 < 1000)
		{
			return ENTINDEX(pEntity);
		}
	}
	return 0;
}




int AMX_MAKE_STRING(AMX *oPlugin, cell tParam, int &iLength)
{
	char *szNewValue = MF_GetAmxString(oPlugin, tParam, 0, &iLength);
	return ALLOC_STRING(szNewValue);
}

// Makes a char pointer out of an AMX cell.
char *AMX_GET_STRING(AMX *oPlugin, cell tParam, int &iLength) 
{
	char *szNewValue = MF_GetAmxString(oPlugin, tParam, 0, &iLength);
	return (char*)STRING(ALLOC_STRING(szNewValue));
}
edict_t	*UTIL_PlayerByIndexE( int playerIndex )
{

	if ( playerIndex > 0 && playerIndex <= gpGlobals->maxClients )
	{
		edict_t *pPlayerEdict = INDEXENT2( playerIndex );
		if ( pPlayerEdict && !pPlayerEdict->free )
		{
			return pPlayerEdict;
		}
	}
	
	return NULL;
}
edict_t *UTIL_FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue)
{
	edict_t *pentEntity;
	pentEntity=FIND_ENTITY_BY_STRING(pentStart, szKeyword, szValue);
	if(!FNullEnt(pentEntity))
		return pentEntity;
	return NULL;
}
