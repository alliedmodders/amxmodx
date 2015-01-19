// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "cvars.h"
#include "amxmodx.h"
#include <CDetour/detours.h>

CvarManager g_CvarManager;

DETOUR_DECL_STATIC2(Cvar_DirectSet, void, struct cvar_s*, var, const char*, value)
{
	static bool calledFromCallback = false;

	if (!calledFromCallback && var && value)
	{
		if (strcmp(var->string, value) != 0)
		{
			calledFromCallback = true;

			if (executeForwards(FF_CvarChanged, reinterpret_cast<cell>(var), var->string, value, var->name) > 0)
			{
				//return;
			}

			calledFromCallback = false;
		}
	}

	DETOUR_STATIC_CALL(Cvar_DirectSet)(var, value);
}

CvarManager::CvarManager() : m_AmxmodxCvars(0), m_HookDetour(nullptr)
{
}

CvarManager::~CvarManager()
{
	CvarsList::iterator iter = m_Cvars.begin();

	while (iter != m_Cvars.end())
	{
		CvarInfo* info = (*iter);

		iter = m_Cvars.erase(iter);

		delete info;
	}

	m_Cache.clear();
	m_HookDetour->Destroy();
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

cvar_t* CvarManager::CreateCvar(const char* name, const char* value, float fvalue, int flags, const char* plugin, int plugnId)
{
	cvar_t*    var = nullptr;
	CvarInfo* info = nullptr;

	// Is cvar already cached ?
	if (!m_Cache.retrieve(name, &info))
	{
		// Not cached - Is cvar already exist?
		var = CVAR_GET_POINTER(name);

		// Whether it exists, we need to prepare a new entry.
		info = new CvarInfo();

		// Shared datas.
		info->name     = name;
		info->plugin   = plugin;
		info->pluginId = plugnId;

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

	return info->var;
}

cvar_t* CvarManager::FindCvar(const char* name)
{
	cvar_t* var = nullptr;
	CvarInfo* info = nullptr;

	// Do we have already cvar in cache?
	if (m_Cache.retrieve(name, &info))
	{
		return info->var;
	}

	// Cvar doesn't exist.
	if (!(var = CVAR_GET_POINTER(name)))
	{
		return nullptr;
	}

	// Create a new entry.
	info = new CvarInfo();
	info->var      = var;
	info->name     = name;
	info->plugin   = "";
	info->pluginId = -1;
	info->amxmodx  = false;

	// Add entry in the caches.
	m_Cvars.append(info);
	m_Cache.insert(name, info);

	return var;
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

size_t CvarManager::GetRegCvarsCount()
{
	return m_AmxmodxCvars;
}

void CvarManager::OnConsoleCommand()
{
	print_srvconsole("Registered cvars:\n");
	print_srvconsole("       %-24.23s %-24.23s %-16.15s\n", "name", "value", "plugin");

	size_t index = 0;
	ke::AString pluginName;

	if (CMD_ARGC() > 2) // Searching for cvars registered to a plugin
	{
		pluginName = CMD_ARGV(2);
	}

	for (CvarsList::iterator iter = m_Cvars.begin(); iter != m_Cvars.end(); iter++)
	{
		CvarInfo* ci = (*iter);

		if (ci->amxmodx && (!pluginName.length() || strncmp(ci->name.chars(), pluginName.chars(), pluginName.length()) == 0))
		{
			print_srvconsole(" [%3d] %-24.23s %-24.23s %-16.15s\n", ++index, ci->name.chars(), ci->var->string, ci->plugin.chars());
		}
	}
}
