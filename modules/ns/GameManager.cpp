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

/* This holds the functions which determine which hooks I need forwareded */

#include "amxxmodule.h"
#include "ns.h"
#include "utilfunctions.h"
#include "GameManager.h"
#include "CPlayer.h"

void GameManager::HookPreThink(void)
{
	// Only need to hook prethink if at least 1 plugins has any of these forwards:
	// client_spawn, client_changeteam, client_changeclass

	if (UTIL_CheckForPublic("client_spawn") ||
		UTIL_CheckForPublic("client_changeteam") ||
		UTIL_CheckForPublic("client_changeclass"))
	{
		g_pFunctionTable->pfnPlayerPreThink=PlayerPreThink;
		return;
	}

	g_pFunctionTable->pfnPlayerPreThink=NULL;
};

void GameManager::HookPostThink_Post(void)
{
	int i=0;
	while (i++<gpGlobals->maxClients)
	{
		if (GET_PLAYER_I(i)->NeedPostThink_Post())
		{
			g_pFunctionTable_Post->pfnPlayerPostThink=PlayerPostThink_Post;
			return;
		}
	}

	g_pFunctionTable_Post->pfnPlayerPostThink=NULL;
};
void GameManager::HookPreThink_Post(void)
{
	int i=1;
	while (i<gpGlobals->maxClients)
	{
		if (GET_PLAYER_I(i++)->NeedPreThink_Post())
		{
			g_pFunctionTable_Post->pfnPlayerPreThink=PlayerPreThink_Post;
			return;
		}
	}

	g_pFunctionTable_Post->pfnPlayerPreThink=NULL;
};
void GameManager::HookUpdateClientData(void)
{
	int i=1;
	while (i<gpGlobals->maxClients)
	{
		if (GET_PLAYER_I(i++)->NeedUpdateClientData())
		{
			g_pFunctionTable->pfnUpdateClientData=UpdateClientData;
			return;
		}
	}

	g_pFunctionTable->pfnUpdateClientData=NULL;
};
void GameManager::HookLogs()
{
	// These forwards only are needed in classic NS
	if (!IsCombat() && UTIL_CheckForPublic("client_built")) 
	{
		// Only hook if this public exists (wasteful otherwise)
		m_BuiltForward = MF_RegisterForward("client_built", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		g_pengfuncsTable_Post->pfnAlertMessage=AlertMessage_Post;
		g_pengfuncsTable_Post->pfnCreateNamedEntity=CreateNamedEntity_Post;
	} 
	else 
	{
		// no need for these hooks in co
		g_pengfuncsTable_Post->pfnAlertMessage=NULL;
		g_pengfuncsTable_Post->pfnCreateNamedEntity=NULL;
	}

};
