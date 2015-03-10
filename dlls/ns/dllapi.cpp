// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

/* Calls going from the engine to the game dll are handled here */

#ifndef HAVE_STDINT_H
#define HAVE_STDINT_H
#endif

#include "amxxmodule.h"
#include "ns.h"
#include "utilfunctions.h"
#include "SpawnManager.h"
#include "GameManager.h"
#include "CPlayer.h"
#include "MessageHandler.h"
#include "LocationManager.h"
#include "ParticleManager.h"

LocationManager LocationMan;
GameManager GameMan;
SpawnManager SpawnMan;

extern edict_t* avhgameplay;

CPlayer g_player[33];
extern void *GameRules;

bool NEW_Initialized=false;
edict_t *NEW_FirstEdict=NULL;

/**
 * This is only called during the CountDown
 * This call will unhook itself with Metamod
 * when it is finished.
 */
void StartFrame()
{
	GameMan.StartFrame();
	RETURN_META(MRES_IGNORED);
}

/**
 * Check spawning for:
 * - Worldspawn
 *   - Initialize NEW_Utilities
 *   - Clear CPlayer / CSpawn data
 * - info_team_start (team spawn point)
 *   - Save in list
 * - info_player_start (ready room spawn point)
 *   - Save in list
 */
int DispatchSpawn(edict_t *pEntity) 
{
	if (!NEW_Initialized)
	{
		NEW_Initialize(pEntity);
	}
	if (ENTINDEX_NEW(pEntity)==0) // worldspawn
	{
		int i=0;
		while (i<=32)
		{
			GET_PLAYER_I(i++)->Reset();
		}

		LocationMan.Clear();

		SpawnMan.Clear();

	}
	else if (FStrEq(STRING(pEntity->v.classname),"info_player_start"))
	{
		// Mark down the ready room spawn point.
		SpawnMan.InsertReadyRoom(pEntity);
	}
	else if (FStrEq(STRING(pEntity->v.classname),"info_team_start"))
	{
		// Mark down the team based spawn point.
		SpawnMan.Insert(pEntity);
	}

	else if (FStrEq(STRING(pEntity->v.classname),"env_particles_custom"))
	{
		ParticleMan.Add(STRING(pEntity->v.targetname),0);
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void DispatchKeyValue(edict_t *pEntity,KeyValueData *pkvd)
{
	if (strcmp(pkvd->szKeyName,"locationname")==0)
	{
		if (pkvd->szClassName && strcmp(pkvd->szClassName,"info_location")==0)
		{
			// this is a BSP model, so calling SetModel 
			// will update the mins/maxs 
			SET_MODEL(pEntity,STRING(pEntity->v.model));

			// Add it to our list
			LocationMan.Add(pkvd->szValue,pEntity);
			RETURN_META(MRES_IGNORED);
		}	
	}
	RETURN_META(MRES_IGNORED);
}

void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	// Reset CPlayer classes (again?)
	for(int i = 1; i <= gpGlobals->maxClients;i++) 
	{

		CPlayer *player=GET_PLAYER_I(i);

		player->FullReset();

		player->SetEdict(pEdictList + i);

	}

	GameRules=NULL;

	RETURN_META(MRES_IGNORED);
}

void ServerActivate_Post(edict_t *pEdictList, int edictCount, int clientMax)
{
	Initialize_MessageHandler();

	g_pFunctionTable_Post->pfnServerActivate=NULL;

	RETURN_META(MRES_IGNORED);
}

/**
 * PlayerPreThink, PlayerPreThink_Post and PlayerPostThink_Post
 * all disable their Metamod hook calls when they are no longer needed.
 * (Actually it is disabled in the native calls)
 */
void PlayerPreThink(edict_t *pEntity)
{
	GET_PLAYER_E(pEntity)->PreThink();
	RETURN_META(MRES_IGNORED);
}
void PlayerPreThink_Post(edict_t *pEntity)
{
	GET_PLAYER_E(pEntity)->PreThink_Post();

	RETURN_META(MRES_IGNORED);
}
void PlayerPostThink_Post(edict_t *pEntity)
{
	GET_PLAYER_E(pEntity)->PostThink_Post();

	RETURN_META(MRES_IGNORED);
}

 

// Map is changing/server is shutting down.
// We do all cleanup routines here, since, as noted in metamod's dllapi
// ServerDeactivate is the very last function called before the server loads up a new map.
void ServerDeactivate(void)
{
	for (int i=1;i<=gpGlobals->maxClients;i++)
	{
		GET_PLAYER_I(i)->Disconnect();
	}
	GameRules = NULL;
	avhgameplay = NULL;
	RETURN_META(MRES_IGNORED);
}

// Reset player data here..
qboolean ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ])
{
	// Client's connecting.  Freshen up his save data, and mark him as being connected.
	GET_PLAYER_E(pEntity)->Connect();

	RETURN_META_VALUE(MRES_HANDLED,0);
}
void ClientDisconnect(edict_t *pEntity)
{
	// Client is disconnecting, clear all his saved information.
	GET_PLAYER_E(pEntity)->Disconnect(1);

	RETURN_META(MRES_HANDLED);
}

/**
 * NS resets pev->fov every single frame, but this is called right before the data is sent to the client.
 * Reset FOV if we need to.
 * -
 * NOTE: This function is not called if no clients have FoV changed
 */
void UpdateClientData( const struct edict_s *ent, int sendweapons, struct clientdata_s *cd )
{
	GET_PLAYER_E(const_cast<edict_t *>(static_cast<const edict_t *>(ent)))->UpdateFOV();

	RETURN_META(MRES_HANDLED);

}
