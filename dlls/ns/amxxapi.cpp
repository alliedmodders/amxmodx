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

/* Calls sent by AMX Mod X are handled here */

#include "sdk/amxxmodule.h"

#include "ns.h"
#include "utilfunctions.h"

#include "CVector.h"
#include "CString.h"

#include "GameManager.h"
#include "TitleManager.h"
#include "MessageHandler.h"
#include "ParticleManager.h"

#include "AllocString.h"

extern int gmsgHudText2;
extern BOOL iscombat;

TitleManager TitleMan;
ParticleManager ParticleMan;

void MFuncs_Initialize(void);

// Native register calls here
void AddNatives_MemberFunc();
void AddNatives_Particles();
void AddNatives_Player();
void AddNatives_PlayerMemory();
void AddNatives_Weapons();
void AddNatives_Structure();
void AddNatives_General();

// All plugins have loaded (called during spawning worldspawn)
void OnPluginsLoaded()
{

	// This message is used for the ns_popup native
	GameMan.GetMessageIDs();

	// Check the map name and see if it's combat or not.
	GameMan.EvaluateCombat();

	GameMan.RegisterForwards();

	GameMan.CheckAllHooks();

	AllocStringList.Clear();

	TitleMan.LoadTitles();

	GameMan.CheckMap();

	ParticleMan.ReadFile();
}



int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "ns") == 0 ||
		strcasecmp(game, "nsp") == 0)
	{
		return AMXX_GAME_OK;
	}
	return AMXX_GAME_BAD;
}
// Module is attaching to AMXX
void OnAmxxAttach()
{
	AddNatives_MemberFunc();
	AddNatives_Particles();
	AddNatives_Player();
	AddNatives_PlayerMemory();
	AddNatives_Weapons();
	AddNatives_Structure();
	AddNatives_General();

	MFuncs_Initialize();
}

