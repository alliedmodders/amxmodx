// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "CvarManager.h"
#include "amxmodx.h"
#include <CDetour/detours.h>
#include <auto-string.h>

CvarManager g_CvarManager;

/** 
 * Returns true to call original function, otherwise false to block it.
 */
bool Cvar_DirectSet_Custom(cvar_t* var, const char* value)
{
	CvarInfo* info = nullptr;

	if (!var || !value                                    // Sanity checks against bogus pointers.
		|| strcmp(var->string, value) == 0                // Make sure old and new values are different to not trigger callbacks.
		|| !g_CvarManager.CacheLookup(var->name, &info))  // No data in cache, nothing to do.
	{
		return true;
	}

	if (info->bound.hasMin || info->bound.hasMax) // cvar_s doesn't have min/max mechanism, so we check things here.
	{
		float fvalue = atof(value); 
		bool oob = false;

		if (info->bound.hasMin && fvalue < info->bound.minVal)
		{
			oob = true;
			fvalue = info->bound.minVal;
		}
		else if (info->bound.hasMax && fvalue > info->bound.maxVal)
		{
			oob = true;
			fvalue = info->bound.maxVal;
		}

		if (oob) // Found value out of bound, set new value and block original call.
		{
			CVAR_SET_FLOAT(var->name, fvalue);
			return false;
		}
	}

	if (!info->hooks.empty())
	{
		int lastResult = 0;

		for (size_t i = 0; i < info->hooks.length(); ++i)
		{
			CvarHook* hook = info->hooks[i];

			if (hook->forward->state == AutoForward::FSTATE_OK) // Our callback can be enable/disabled by natives.
			{
				int result = executeForwards(hook->forward->id, reinterpret_cast<cvar_t*>(var), var->string, value);

				if (result >= lastResult)
				{
					lastResult = result;
				}
			}
		}

		if (lastResult) // A plugin wants to block the forward.
		{
			return false;
		}
	}

	if (!info->binds.empty()) // Time to update our available binds.
	{
		for (size_t i = 0; i < info->binds.length(); ++i)
		{
			CvarBind* bind = info->binds[i];

			switch (bind->type)
			{
				case CvarBind::CvarType_Int:
				{
					*bind->varAddress = atoi(value);
					break;
				}
				case CvarBind::CvarType_Float:
				{
					float fvalue = atof(value);
					*bind->varAddress = amx_ftoc(fvalue);
					break;
				}
				case CvarBind::CvarType_String:
				{
					set_amxstring_simple(bind->varAddress, value, bind->varLength);
					break;
				}
			}
		}
	}

	// Nothing to block.
	return true;
}

DETOUR_DECL_STATIC2(Cvar_DirectSet, void, struct cvar_s*, var, const char*, value)
{
	if (Cvar_DirectSet_Custom(var, value))
	{
		DETOUR_STATIC_CALL(Cvar_DirectSet)(var, value);
	}
}

CvarManager::CvarManager() : m_AmxmodxCvars(0), m_HookDetour(nullptr)
{
}

CvarManager::~CvarManager()
{
	OnAmxxShutdown();
}

void CvarManager::CreateCvarHook(void)
{
	// void PF_Cvar_DirectSet(struct cvar_s *var, const char *value) // = pfnCvar_DirectSet
	// {
	//   	Cvar_DirectSet(var, value); // <- We want to hook this.
	// }

	byte *baseAddress = (byte *)g_engfuncs.pfnCvar_DirectSet;
	uintptr_t *functionAddress = nullptr;

#if defined(WIN32)
	// 55              push    ebp
	// 8B EC           mov     ebp, esp
	// 8B 45 0C        mov     eax, [ebp+arg_4]
	// 8B 4D 08        mov     ecx, [ebp+arg_0]
	// 50              push    eax
	// 51              push    ecx
	// E8 XX XX XX XX  call    Cvar_DirectSet
	const byte opcodeJump = 0xE8;
#else
	// E9 XX XX XX XX  jmp     Cvar_DirectSet
	const byte opcodeJump = 0xE9;
#endif

	const byte opcodeJumpSize     = 5;
	const byte opcodeJumpByteSize = 1;

	const int maxBytesLimit = 20;

	for (size_t i = 0; i < maxBytesLimit; ++i, ++baseAddress)
	{
		if (*baseAddress == opcodeJump)
		{
			functionAddress = (uintptr_t *)(&baseAddress[opcodeJumpSize] + *(uintptr_t *)&baseAddress[opcodeJumpByteSize]);
			break;
		}
	}

	if (functionAddress)
	{
		m_HookDetour = DETOUR_CREATE_STATIC_FIXED(Cvar_DirectSet, (void *)functionAddress);

		if (m_HookDetour)
		{
			m_HookDetour->EnableDetour();
		}
	}
}

cvar_t* CvarManager::CreateCvar(const char* name, const char* value, const char* plugin, int pluginId, int flags,
								const char* helpText, bool hasMin, float min, bool hasMax, float max)
{
	cvar_t*    var = nullptr;
	CvarInfo* info = nullptr;

	if (!CacheLookup(name, &info))
	{
		// Not cached - Is cvar already exist?
		var = CVAR_GET_POINTER(name);

		// Whether it exists, we need to prepare a new entry.
		info = new CvarInfo(name, helpText, hasMin, min, hasMax, max, plugin, pluginId);

		if (var)
		{
			// Cvar already exists. Just copy.
			// "string" will be set after. "value" and "next" are automatically set.
			info->var        = var;
			info->defaultval = var->string;
			info->amxmodx    = false;
		}
		else
		{
			// Registers a new cvar.
			static cvar_t cvar_reg_helper;

			// "string" will be set after. "value" and "next" are automatically set.
			cvar_reg_helper.name   = info->name.chars();
			cvar_reg_helper.string = "";
			cvar_reg_helper.flags  = flags;

			// Adds cvar to global list.
			CVAR_REGISTER(&cvar_reg_helper);

			// Registering can fail if name is already a registered command.
			var = CVAR_GET_POINTER(name);

			// If so, we can't go further.
			if (!var)
			{
				delete info;
				return nullptr;
			}

			// If ok, we got a valid pointer, we can copy.
			info->var        = var;
			info->defaultval = value;
			info->amxmodx    = true;

			// Keeps track count of cvars registered by AMXX.
			++m_AmxmodxCvars;
		}

		// Add a new entry in the caches.
		m_Cvars.append(info);
		m_Cache.insert(name, info);

		// Make sure that whether an existing or new cvar is set to the given value.
		CVAR_DIRECTSET(var, value);
	}

	// Detour is disabled on map change.
	m_HookDetour->EnableDetour();

	return info->var;
}

CvarInfo* CvarManager::FindCvar(const char* name)
{
	cvar_t* var = nullptr;
	CvarInfo* info = nullptr;

	// Detour is disabled on map change.
	m_HookDetour->EnableDetour();

	// Do we have already cvar in cache?
	if (CacheLookup(name, &info))
	{
		return info;
	}

	// Cvar doesn't exist.
	if (!(var = CVAR_GET_POINTER(name)))
	{
		return nullptr;
	}

	// Create a new entry.
	info = new CvarInfo(name);
	info->var = var;

	// Add entry in the caches.
	m_Cvars.append(info);
	m_Cache.insert(name, info);

	return info;
}

CvarInfo* CvarManager::FindCvar(size_t index)
{
	// Used by get_plugins_cvar native.
	// For compatibility, only cvars registered by AMXX are concerned.

	size_t iter_id = 0;

	for (CvarsList::iterator iter = m_Cvars.begin(); iter != m_Cvars.end(); iter++)
	{
		if (iter->amxmodx && iter_id++ == index)
		{
			return *(iter);
		}
	}

	return nullptr;
}

bool CvarManager::CacheLookup(const char* name, CvarInfo** info)
{
	return m_Cache.retrieve(name, info);
}

AutoForward* CvarManager::HookCvarChange(cvar_t* var, AMX* amx, cell param, const char** callback)
{
	CvarInfo* info = nullptr;

	// A cvar is guaranteed to be in cache if pointer is got from
	// get_cvar_pointer and register_cvar natives. Though it might be 
	// provided by another way. If by any chance we run in such 
	// situation, we create a new entry right now.

	if (!CacheLookup(var->name, &info))
	{
		// Create a new entry.
		info = new CvarInfo(var->name);
		info->var = var;

		// Add entry in the caches.
		m_Cvars.append(info);
		m_Cache.insert(info->name.chars(), info);
	}

	int length;
	*callback = get_amxstring(amx, param, 0, length);

	int forwardId = registerSPForwardByName(amx, *callback, FP_CELL, FP_STRING, FP_STRING, FP_DONE);

	// Invalid callback, it could be: not a public function, wrongly named, or simply missing.
	if (forwardId == -1)
	{
		return nullptr;
	}

	// Detour is disabled on map change.
	m_HookDetour->EnableDetour();
	
	AutoForward* forward = new AutoForward(forwardId, *callback);
	info->hooks.append(new CvarHook(g_plugins.findPlugin(amx)->getId(), forward));

	return forward;
}

size_t CvarManager::GetRegCvarsCount()
{
	return m_AmxmodxCvars;
}

AutoString convertFlagsToString(int flags)
{
	AutoString flagsName;

	if (flags > 0)
	{
		if (flags & FCVAR_ARCHIVE)          flagsName = flagsName + "FCVAR_ARCHIVE ";
		if (flags & FCVAR_USERINFO)         flagsName = flagsName + "FCVAR_USERINFO ";
		if (flags & FCVAR_SERVER)           flagsName = flagsName + "FCVAR_SERVER ";
		if (flags & FCVAR_EXTDLL)           flagsName = flagsName + "FCVAR_EXTDLL ";
		if (flags & FCVAR_CLIENTDLL)        flagsName = flagsName + "FCVAR_CLIENTDLL ";
		if (flags & FCVAR_PROTECTED)        flagsName = flagsName + "FCVAR_PROTECTED ";
		if (flags & FCVAR_SPONLY)           flagsName = flagsName + "FCVAR_SPONLY ";
		if (flags & FCVAR_PRINTABLEONLY)    flagsName = flagsName + "FCVAR_PRINTABLEONLY ";
		if (flags & FCVAR_UNLOGGED)         flagsName = flagsName + "FCVAR_UNLOGGED ";
		if (flags & FCVAR_NOEXTRAWHITEPACE) flagsName = flagsName + "FCVAR_NOEXTRAWHITEPACE ";
	}

	if (!flagsName.length())
	{
		flagsName = "-";
	}

	return flagsName;
}

void CvarManager::OnConsoleCommand()
{
	size_t index = 0;
	size_t indexToSearch = 0;
	ke::AString partialName;

	int argcount = CMD_ARGC();

	// amxx cvars <partial cvar name> <index from listing>
	// E.g.:
	//   amxx cvars test   <- list all cvars from plugin name starting by "test"
	//   amxx cvars 2      <- show informations about cvar in position 2 from "amxx cvars" list
	//   amxx cvars test 2 <- show informations about cvar in position 2 from "amxx cvars test" list

	if (argcount > 2)
	{
		const char* argument = CMD_ARGV(2);
		
		indexToSearch = atoi(argument); // amxx cvars 2

		if (!indexToSearch)
		{
			partialName = argument; // amxx cvars test

			if (argcount > 3)       // amxx cvars test 2
			{
				indexToSearch = atoi(CMD_ARGV(3));
			}
		}
	}

	if (!indexToSearch)
	{
		print_srvconsole("\nManaged cvars:\n");
		print_srvconsole("       %-24.23s %-24.23s %-18.17s %-8.7s %-8.7s %-8.7s\n", "NAME", "VALUE", "PLUGIN", "BINDED", "HOOKED", "BOUNDED");
		print_srvconsole(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - \n");

	}

	for (CvarsList::iterator iter = m_Cvars.begin(); iter != m_Cvars.end(); iter++)
	{
		CvarInfo* ci = (*iter);

		// List any cvars having a status either created, hooked, binded or bounded by a plugin.
		bool in_list = ci->amxmodx || !ci->binds.empty() || !ci->hooks.empty() || ci->bound.hasMin || ci->bound.hasMax;

		if (in_list && (!partialName.length() || strncmp(ci->plugin.chars(), partialName.chars(), partialName.length()) == 0))
		{
			if (!indexToSearch)
			{
				print_srvconsole(" [%3d] %-24.23s %-24.23s %-18.17s %-8.7s %-8.7s %-8.7s\n", ++index, ci->name.chars(), ci->var->string,
								 ci->plugin.length() ? ci->plugin.chars() : "-",
								 ci->binds.empty() ? "no" : "yes",
								 ci->hooks.empty() ? "no" : "yes",
								 ci->bound.hasMin || ci->bound.hasMax ? "yes" : "no");
			}
			else 
			{	
				if (++index != indexToSearch)
				{
					continue;
				}

				print_srvconsole("\nCvar details :\n\n");
				print_srvconsole(" Cvar name   : %s\n", ci->var->name);
				print_srvconsole(" Value       : %s\n", ci->var->string);
				print_srvconsole(" Def. value  : %s\n", ci->defaultval.chars());
				print_srvconsole(" Description : %s\n", ci->description.chars());
				print_srvconsole(" Flags       : %s\n\n", convertFlagsToString(ci->var->flags).ptr());

				print_srvconsole(" %-12s  %-26.25s %s\n", "STATUS", "PLUGIN", "INFOS");
				print_srvconsole(" - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n");
			
				if (ci->amxmodx)
				{
					print_srvconsole(" Registered    %-26.25s %s\n", ci->plugin.chars(), "-");
				}

				if (ci->bound.hasMin)
				{
					print_srvconsole(" Min Bounded   %-26.25s %f\n", g_plugins.findPlugin(ci->bound.minPluginId)->getName(), ci->bound.minVal);
				}

				if (ci->bound.hasMax)
				{
					print_srvconsole(" Max Bounded   %-26.25s %f\n", g_plugins.findPlugin(ci->bound.maxPluginId)->getName(), ci->bound.maxVal);
				}

				if (!ci->binds.empty())
				{
					for (size_t i = 0; i < ci->binds.length(); ++i)
					{
						print_srvconsole(" Binded        %-26.25s %s\n", g_plugins.findPlugin(ci->binds[i]->pluginId)->getName(), "-");
					}
				}

				if (!ci->hooks.empty())
				{
					for (size_t i = 0; i < ci->hooks.length(); ++i)
					{
						CvarHook* hook = ci->hooks[i];

						print_srvconsole(" Hooked        %-26.25s %s (%s)\n", g_plugins.findPlugin(hook->pluginId)->getName(),
										 hook->forward->callback.chars(), 
										 hook->forward->state == AutoForward::FSTATE_OK ? "active" : "inactive");
					}
				}
				break;
			}
		}
	}
}

void CvarManager::OnPluginUnloaded()
{
	// Clear only plugin hooks list.
	for (CvarsList::iterator cvar = m_Cvars.begin(); cvar != m_Cvars.end(); cvar++)
	{
		for (size_t i = 0; i < (*cvar)->binds.length(); ++i)
		{
			delete (*cvar)->binds[i];
		}

		for (size_t i = 0; i < (*cvar)->hooks.length(); ++i)
		{
			delete (*cvar)->hooks[i];
		}

		(*cvar)->binds.clear();
		(*cvar)->hooks.clear();
	}

	// There is no point to enable detour if at next map change
	// no plugins hook cvars.
	m_HookDetour->DisableDetour();
}

void CvarManager::OnAmxxShutdown()
{
	// Free everything.

	for (CvarsList::iterator cvar = m_Cvars.begin(); cvar != m_Cvars.end(); cvar = m_Cvars.erase(cvar))
	{
		for (size_t i = 0; i < (*cvar)->binds.length(); ++i)
		{
			delete (*cvar)->binds[i];
		}

		for (size_t i = 0; i < (*cvar)->hooks.length(); ++i)
		{
			delete (*cvar)->hooks[i];
		}

		delete (*cvar);
	}

	m_Cache.clear();
	m_HookDetour->Destroy();
}
