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

/* Calls sent by AMX Mod X are handled here */

#include "sdk/amxxmodule.h"

#include "ns.h"
#include "utilfunctions.h"
#include "GameManager.h"
#include "TitleManager.h"
#include "MessageHandler.h"
#include "ParticleManager.h"

#include "AllocString.h"
#include <amtl/am-string.h>

extern int gmsgHudText2;
extern BOOL iscombat;

TitleManager TitleMan;
ParticleManager ParticleMan;

void MFuncs_Initialize(char *base);
void MPlayerFuncs_Initialize(char *base);

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

	char *FuncBase;
	char FileName[256];
	DLHANDLE DLLBase;
#ifdef __linux__
	ke::SafeSprintf(FileName, sizeof(FileName), "%s/dlls/ns_i386.so", MF_GetModname());
#else
	ke::SafeSprintf(FileName, sizeof(FileName), "%s\\dlls\\ns.dll", MF_GetModname());
#endif

	DLLBase = DLOPEN(FileName);
	FuncBase = (char *)DLSYM(DLLBase, MAKE_OFFSET(BASE));
	DLCLOSE(DLLBase);

	MFuncs_Initialize(FuncBase);
	MPlayerFuncs_Initialize(FuncBase);
}

