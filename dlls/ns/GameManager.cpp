/* AMX Mod X 
 *   Natural Selection Module 
 * 
 * by the AMX Mod X Development Team 
 *
 * This file is part of AMX Mod X. 
 * 
 * 
 *  This program is free software; you can redistribute it and/or modify it 
 *  under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at 
 *  your option) any later version. 
 * 
 *  This program is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of 
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 *  General Public License for more details. 
 * 
 *  You should have received a copy of the GNU General Public License 
 *  along with this program; if not, write to the Free Software Foundation, 
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
 * 
 *  In addition, as a special exception, the author gives permission to 
 *  link the code of this program with the Half-Life Game Engine ("HL 
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve, 
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all 
 *  respects for all of the code used other than the HL Engine and MODs 
 *  from Valve. If you modify this file, you may extend this exception 
 *  to your version of the file, but you are not obligated to do so. If 
 *  you do not wish to do so, delete this exception statement from your 
 *  version. 
 */ 

/* This holds the functions which determine which hooks I need forwareded */

#include "sdk/amxxmodule.h"

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
