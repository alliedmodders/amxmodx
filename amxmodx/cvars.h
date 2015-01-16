// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef CVARS_H
#define CVARS_H

#include "cvardef.h"
#include "CString.h"

class CDetour;

class CCVar
{
	cvar_t cvar;
	String name;
	String plugin;

public:
	CCVar(const char* pname, const char* pplugin, int pflags, float pvalue) : name(pname), plugin(pplugin)
	{
		cvar.name = (char*)name.c_str();
		cvar.flags = pflags;
		cvar.string = "";
		cvar.value = pvalue;
	}

	inline cvar_t* getCvar() {
		return &cvar;
	}
	inline const char* getPluginName() {
		return plugin.c_str();
	}
	inline const char* getName() {
		return name.c_str();
	}
	inline bool operator == (const char* string) {
		return (strcmp(name.c_str(), string) == 0);
	}
	int plugin_id;
};

void CreateCvarHook(void);

extern CDetour *Cvar_DirectSetDetour;

#endif // CVARS_H
