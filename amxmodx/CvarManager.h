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

#include "amxmodx.h"
#include <am-vector.h>
#include <am-inlinelist.h>
#include <sm_namehashset.h>

class CDetour;

enum CvarBounds
{
	CvarBound_Upper = 0,
	CvarBound_Lower
};

struct Forward
{
	enum fwdstate
	{
		FSTATE_INVALID = 0,
		FSTATE_OK,
		FSTATE_STOP,
	};

	Forward(int id_, const char* handler) : id(id_), state(FSTATE_OK), callback(handler) {};
	Forward()                             : id(-1) , state(FSTATE_INVALID) {};

	~Forward()
	{
		unregisterSPForward(id);
	}

	int         id;
	fwdstate    state;
	ke::AString callback;
};

struct CvarPlugin
{
	CvarPlugin(int id, Forward* fwd) : pluginId(id), forward(fwd) {};
	CvarPlugin(int id)               : pluginId(id), forward(new Forward()) {};

	int pluginId;
	ke::AutoPtr<Forward> forward;
};

typedef ke::Vector<CvarPlugin*> CvarsHook;

struct CvarInfo : public ke::InlineListNode<CvarInfo>
{
	CvarInfo(const char* name_, const char* helpText, bool hasMin_, float min_, bool hasMax_, float max_, 
			 const char* plugin_, int pluginId_)
		:
		name(name_), description(helpText),	hasMin(hasMin_), minVal(min_), hasMax(hasMax_), maxVal(max_),
		plugin(plugin_), pluginId(pluginId_) {};

	CvarInfo(const char* name_)
		:
		name(name_), defaultval(""), description(""), hasMin(false), minVal(0), hasMax(false), maxVal(0),
		plugin(""), pluginId(-1), amxmodx(false) {};

	cvar_t*      var;
	ke::AString  name;
	ke::AString  defaultval;
	ke::AString  description;
	bool         hasMin;
	bool         hasMax;
	float        minVal;
	float        maxVal;

	ke::AString  plugin;
	int          pluginId;

	CvarsHook    hooks;
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

		cvar_t*   CreateCvar(const char* name, const char* value, const char* plugin, int pluginId, 
							 int flags = 0, const char* helpText = "",
							 bool hasMin = false, float min = 0, 
							 bool hasMax = false, float max = 0);

		CvarInfo* FindCvar(const char* name);
		CvarInfo* FindCvar(size_t index);
		bool      CacheLookup(const char* name, CvarInfo** info);

		Forward*  HookCvarChange(cvar_t* var, AMX* amx, cell param, const char** callback);

		size_t    GetRegCvarsCount();

		void      OnConsoleCommand();
		void      OnPluginUnloaded();
		void      OnAmxxShutdown();

	private:

		CvarsCache m_Cache;
		CvarsList  m_Cvars;
		size_t     m_AmxmodxCvars;
		CDetour*   m_HookDetour;
};

extern CvarManager g_CvarManager;

#endif // CVARS_H
