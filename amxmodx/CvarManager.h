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
#include <amtl/am-vector.h>
#include <amtl/am-inlinelist.h>
#include <sm_namehashset.h>

class CDetour;

enum CvarBounds
{
	CvarBound_Upper = 0,
	CvarBound_Lower
};

struct AutoForward
{
	enum fwdstate
	{
		FSTATE_INVALID = 0,
		FSTATE_OK,
		FSTATE_STOP,
	};

	AutoForward(int id_, const char* handler) : id(id_), state(FSTATE_OK), callback(handler) {};
	AutoForward()                             : id(-1) , state(FSTATE_INVALID) {};

	~AutoForward()
	{
		unregisterSPForward(id);
	}

	int         id;
	fwdstate    state;
	ke::AString callback;
};

struct CvarHook
{
	CvarHook(int id, AutoForward* fwd) : pluginId(id), forward(fwd) {};
	CvarHook(int id)                   : pluginId(id), forward(new AutoForward()) {};

	int pluginId;
	ke::AutoPtr<AutoForward> forward;
};

struct CvarBind
{
	enum CvarType
	{
		CvarType_Int,
		CvarType_Float,
		CvarType_String,
	};

	CvarBind(int id_, CvarType type_, cell* varAddress_, size_t varLength_)
		: 
		pluginId(id_), 
		type(type_), 
		varAddress(varAddress_), 
		varLength(varLength_) {};

	int      pluginId;
	CvarType type;
	cell*    varAddress;
	size_t   varLength;
};

struct CvarBound
{
	CvarBound()
		:
		hasMin(false), minVal(0), 
		hasMax(false), maxVal(0),
		minPluginId(-1),
		maxPluginId(-1) {};

	bool    hasMin;
	float   minVal;
	bool    hasMax;
	float   maxVal;
	int     minPluginId;
	int     maxPluginId;
};

typedef ke::Vector<CvarHook*> CvarsHook;
typedef ke::Vector<CvarBind*> CvarsBind;

struct CvarInfo : public ke::InlineListNode<CvarInfo>
{
	CvarInfo(const char* name_, const char* helpText, const char* plugin_, int pluginId_)
		:
		name(name_), description(helpText),	
		plugin(plugin_), pluginId(pluginId_), bound() {};

	CvarInfo(const char* name_)
		:
		name(name_), defaultval(""), description(""), 
		plugin(""), pluginId(-1), bound(), amxmodx(false) {};

	cvar_t*      var;
	ke::AString  name;
	ke::AString  defaultval;
	ke::AString  description;

	ke::AString  plugin;
	int          pluginId;

	CvarBound    bound;
	CvarsBind    binds;
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

		CvarInfo* CreateCvar(const char* name, const char* value, const char* plugin, int pluginId, int flags = 0, const char* helpText = "");
		CvarInfo* FindCvar(const char* name);
		CvarInfo* FindCvar(size_t index);
		bool      CacheLookup(const char* name, CvarInfo** info);

		AutoForward*  HookCvarChange(cvar_t* var, AMX* amx, cell param, const char** callback);
		bool          BindCvar(CvarInfo* info, CvarBind::CvarType type, AMX* amx, cell varofs, size_t varlen = 0);
		void          SetCvarMin(CvarInfo* info, bool set, float value, int pluginId);
		void          SetCvarMax(CvarInfo* info, bool set, float value, int pluginId);

		size_t    GetRegCvarsCount();
		CvarsList* GetCvarsList();

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
