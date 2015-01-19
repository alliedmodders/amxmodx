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
#include <am-inlinelist.h>
#include <sm_namehashset.h>

class CDetour;

struct CvarInfo : public ke::InlineListNode<CvarInfo>
{
	cvar_t*      var;
	ke::AString  name;
	ke::AString  defaultval;
	ke::AString  plugin;
	int          pluginId;
	bool         amxmodx;

	static inline bool matches(const char *name, const CvarInfo* info)
	{
		return strcmp(name, info->var->name) == 0;
	}
};

typedef NameHashSet<CvarInfo*> CvarsCache;
typedef ke::InlineList<CvarInfo> CvarsList;

class CvarManager
{
	public:

		CvarManager();
		~CvarManager();

	public:

		void      CreateCvarHook();
		cvar_t*   CreateCvar(const char* name, const char* value, float fvalue, int flags, const char* plugin, int plugnId);
		cvar_t*   FindCvar(const char* name);
		CvarInfo* FindCvar(size_t index);
		size_t    GetRegCvarsCount();
		void      OnConsoleCommand();

	private:

		CvarsCache m_Cache;
		CvarsList  m_Cvars;
		size_t     m_AmxmodxCvars;
		CDetour*   m_HookDetour;
};

extern CvarManager g_CvarManager;

#endif // CVARS_H
