// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_NATIVES_H
#define _INCLUDE_NATIVES_H

//only 16 for now sorry
#if !defined CALLFUNC_MAXPARAMS
#define CALLFUNC_MAXPARAMS 16
#endif

#define CALLFUNC_FLAG_BYREF			1
#define CALLFUNC_FLAG_BYREF_REUSED	2

#define N_CELL		1
#define	N_ARRAY		2
#define N_BYREF		3
#define	N_VARARG	4

struct regnative
{
	AMX *amx;
	ke::AString name;
	char *pfn;
	int func;
	int style;
};

extern "C" void amxx_DynaInit(void *ptr);
extern "C" void amxx_DynaMake(char *buffer, int id);
extern "C" int amxx_DynaFunc(AMX *amx, cell *params);
extern "C" int amxx_DynaCodesize();

AMX_NATIVE_INFO *BuildNativeTable();
void ClearPluginLibraries();

//I couldn't resist :)
extern AMX_NATIVE_INFO g_NativeNatives[];

#endif //_INCLUDE_NATIVES_H
