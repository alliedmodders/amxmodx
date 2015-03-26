// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef __MODULES_H__
#define __MODULES_H__

#include "amx.h"

#undef DLLEXPORT
#if defined(_WIN32)
	#define DLLEXPORT __declspec(dllexport)
#else
	#define DLLEXPORT __attribute__((visibility("default")))
	#define WINAPI
#endif

#undef C_DLLEXPORT
#define C_DLLEXPORT extern "C" DLLEXPORT

#define RELOAD_MODULE 0
#define STATIC_MODULE 1

typedef enum
{
	Player_Name,		//String
	Player_Ip,			//String
	Player_Team,		//String
	Player_Ingame,		//bool
	Player_Authorized,	//bool
	Player_Vgui,		//bool
	Player_Time,		//float
	Player_Playtime,	//float
	Player_MenuExpire,	//float
	Player_Weapons,		//struct{int,int}[32]
	Player_CurrentWeapon,	//int
	Player_TeamID,			//int
	Player_Deaths,			//int
	Player_Aiming,			//int
	Player_Menu,			//int
	Player_Keys,			//int
	Player_Flags,			//int[32]
	Player_Newmenu,			//int
	Player_NewmenuPage,		//int
}  PlayerProp;

int CheckModules(AMX *amx, char error[128]);
bool LoadModule(const char *shortname, PLUG_LOADTIME now, bool simplify=true, bool noFileBail=false);
const char *StrCaseStr(const char *as, const char *bs);

class Debugger;
Debugger *DisableDebugHandler(AMX *amx);
void EnableDebugHandler(AMX *amx, Debugger *pd);

const char* GetFileName(AMX *amx);

inline cell FloatToCell(float input)
{
	REAL output = input;
	return *(cell *)&output;
}

#endif // __MODULES_H__
