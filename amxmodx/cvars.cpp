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
#include "nongpl_matches.h"

char CVarTempBuffer[64];
const char *invis_cvar_list[5] = { "amxmodx_version", "amxmodx_modules", "amx_debug", "amx_mldebug", "amx_client_languages" };

// create_cvar(const name[], const default_value[], flags = 0, const description[] = "", bool:has_min = false, Float:min_val = 0.0, bool:has_max = false, Float:max_val = 0.0)
static cell AMX_NATIVE_CALL create_cvar(AMX *amx, cell *params)
{
	int length;
	const char* name     = get_amxstring(amx, params[1], 0, length);
	const char* value    = get_amxstring(amx, params[2], 1, length);
	const char* helpText = get_amxstring(amx, params[4], 2, length);

	int flags    = params[3];
	bool hasMin  = params[5] != 0;
	bool hasMax  = params[7] != 0;
	float minVal = 0;
	float maxVal = 0;

	if (hasMin)
	{
		minVal = amx_ctof(params[6]);

		if (hasMax && minVal > maxVal)
		{
			LogError(amx, AMX_ERR_NATIVE, "A lower bound can't be above an upper bound");
			return 0;
		}
	}
	
	if (hasMax)
	{
		maxVal = amx_ctof(params[8]);

		if (hasMin && maxVal < minVal)
		{
			LogError(amx, AMX_ERR_NATIVE, "An upper bound can't be below a lower bound");
			return 0;
		}
	}

	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);

	if (CheckBadConList(name, 0))
	{
		plugin->AddToFailCounter(1);
	}

	cvar_t* var = g_CvarManager.CreateCvar(name, value, plugin->getName(), plugin->getId(), flags, helpText, hasMin, minVal, hasMax, maxVal);

	return reinterpret_cast<cell>(var);
}

// register_cvar(const name[], const string[], flags=0, Float:fvalue=0.0)
static cell AMX_NATIVE_CALL register_cvar(AMX *amx, cell *params)
{
	int length;
	const char* name  = get_amxstring(amx, params[1], 0, length);
	const char* value = get_amxstring(amx, params[2], 1, length);

	int   flags = params[3];
	float fvalue = amx_ctof(params[4]);

	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);

	if (CheckBadConList(name, 0))
	{
		plugin->AddToFailCounter(1);
	}

	cvar_t* var = g_CvarManager.CreateCvar(name, value, plugin->getName(), plugin->getId(), flags);

	return reinterpret_cast<cell>(var);
}

// cvar_exists(const cvar[])
static cell AMX_NATIVE_CALL cvar_exists(AMX *amx, cell *params)
{
	int ilen;
	return (g_CvarManager.FindCvar(get_amxstring(amx, params[1], 0, ilen)) ? 1 : 0);
}

// get_cvar_pointer(const cvar[])
static cell AMX_NATIVE_CALL get_cvar_pointer(AMX *amx, cell *params)
{
	int len;
	const char *name = get_amxstring(amx, params[1], 0, len);

	CvarInfo* info = g_CvarManager.FindCvar(name);

	return reinterpret_cast<cell>(info ? info->var : 0);
}

// hook_cvar_change(cvarHandle, const callback[])
static cell AMX_NATIVE_CALL hook_cvar_change(AMX *amx, cell *params)
{
	cvar_t* var = reinterpret_cast<cvar_t*>(params[1]);

	if (!var)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cvar handle: %p", var);
		return 0;
	}

	const char* callback;
	AutoForward* forward = g_CvarManager.HookCvarChange(var, amx, params[2], &callback);

	if (!forward)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function \"%s\" is not present", callback);
		return 0;
	}

	return reinterpret_cast<cell>(forward);
}

// enable_cvar_hook(cvarhook:handle);
static cell AMX_NATIVE_CALL enable_cvar_hook(AMX *amx, cell *params)
{
	AutoForward* forward = reinterpret_cast<AutoForward*>(params[1]);

	if (!forward)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cvar hook handle: %p", forward);
		return 0;
	}

	forward->state = AutoForward::FSTATE_OK;

	return 1;
}

// disable_cvar_hook(cvarhook:handle);
static cell AMX_NATIVE_CALL disable_cvar_hook(AMX *amx, cell *params)
{
	AutoForward* forward = reinterpret_cast<AutoForward*>(params[1]);

	if (!forward)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cvar hook handle: %p", forward);
		return 0;
	}

	forward->state =  AutoForward::FSTATE_STOP;

	return 1;
}

// get_cvar_flags(const cvar[])
static cell AMX_NATIVE_CALL get_cvar_flags(AMX *amx, cell *params)
{
	int ilen;
	char* sCvar = get_amxstring(amx, params[1], 0, ilen);

	CvarInfo* info = g_CvarManager.FindCvar(sCvar);

	return info ? info->var->flags : 0;
}

// get_cvar_float(const cvarname[])
static cell AMX_NATIVE_CALL get_cvar_float(AMX *amx, cell *params)
{
	int length;
	const char* name = get_amxstring(amx, params[1], 0, length);

	CvarInfo* info = g_CvarManager.FindCvar(name);

	return info ? amx_ftoc(info->var->value) : 0;
}

// get_cvar_num(const cvarname[])
static cell AMX_NATIVE_CALL get_cvar_num(AMX *amx, cell *params)
{
	int length;
	const char* name = get_amxstring(amx, params[1], 0, length);

	CvarInfo* info = g_CvarManager.FindCvar(name);

	return info ? (int)info->var->value : 0;
}

// get_cvar_string(const cvarname[], output[], iLen)
static cell AMX_NATIVE_CALL get_cvar_string(AMX *amx, cell *params)
{
	int length;
	const char* name = get_amxstring(amx, params[1], 0, length);

	CvarInfo* info = g_CvarManager.FindCvar(name);

	const char *value = info ? info->var->string : "";
	length = info ? strlen(value) : 0;

	return set_amxstring_utf8(amx, params[2], value, length, params[3] + 1); // + EOS
}

// set_cvar_flags(const cvar[], flags)
static cell AMX_NATIVE_CALL set_cvar_flags(AMX *amx, cell *params)
{
	int ilen;
	const char* sCvar = get_amxstring(amx, params[1], 0, ilen);

	if (!strcmp(sCvar, "amx_version") || !strcmp(sCvar, "amxmodx_version") || !strcmp(sCvar, "fun_version") || !strcmp(sCvar, "sv_cheats"))
		return 0;

	CvarInfo* info = g_CvarManager.FindCvar(sCvar);

	if (info)
	{
		info->var->flags |= (int)(params[2]);
		return 1;
	}

	return 0;
}

// set_cvar_float(const cvar[], Float:value)
static cell AMX_NATIVE_CALL set_cvar_float(AMX *amx, cell *params)
{
	int length;
	const char* name = get_amxstring(amx, params[1], 0, length);

	CvarInfo* info = g_CvarManager.FindCvar(name);

	if (info)
	{
		UTIL_Format(CVarTempBuffer, sizeof(CVarTempBuffer) - 1, "%f", amx_ctof(params[2]));
		CVAR_DIRECTSET(info->var, &CVarTempBuffer[0]);
	}

	return 1;
}

// set_cvar_num(const cvarname[], value)
static cell AMX_NATIVE_CALL set_cvar_num(AMX *amx, cell *params)
{
	int length;
	const char* name = get_amxstring(amx, params[1], 0, length);
	int value = params[2];

	CvarInfo* info = g_CvarManager.FindCvar(name);

	if (info)
	{
		UTIL_Format(CVarTempBuffer, sizeof(CVarTempBuffer) - 1, "%d", value);
		CVAR_DIRECTSET(info->var, &CVarTempBuffer[0]);
	}

	return 1;
}

// set_cvar_string(const cvar[], const value[])
static cell AMX_NATIVE_CALL set_cvar_string(AMX *amx, cell *params)
{
	int length;
	const char* name  = get_amxstring(amx, params[1], 0, length);

	CvarInfo* info = g_CvarManager.FindCvar(name);

	if (info)
	{
		CVAR_DIRECTSET(info->var, get_amxstring(amx, params[2], 1, length));
	}

	return 1;
}

// get_pcvar_flags(pcvar)
static cell AMX_NATIVE_CALL get_pcvar_flags(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return ptr->flags;
}

// Float:get_pcvar_float(pcvar)
static cell AMX_NATIVE_CALL get_pcvar_float(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return amx_ftoc(ptr->value);
}

// get_pcvar_num(pcvar)
static cell AMX_NATIVE_CALL get_pcvar_num(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return (int)ptr->value;
}

// bool:get_pcvar_bool(pcvar)
static cell AMX_NATIVE_CALL get_pcvar_bool(AMX *amx, cell *params)
{
	return !!get_pcvar_num(amx, params);
}

// get_pcvar_string(pcvar, string[], maxlen)
static cell AMX_NATIVE_CALL get_pcvar_string(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return set_amxstring_utf8(amx, params[2], ptr->string ? ptr->string : "", ptr->string ? strlen(ptr->string) : 0, params[3] + 1); // EOS
}

// get_pcvar_bounds(pcvar, CvarBounds:type, &Float:value)
static cell AMX_NATIVE_CALL get_pcvar_bounds(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	CvarInfo* info = nullptr;

	if (!ptr || !(info = g_CvarManager.FindCvar(ptr->name)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	bool hasBound = false;
	float bound;

	switch (params[2])
	{
		case CvarBound_Lower:
			hasBound = info->bound.hasMin;
			bound = info->bound.minVal;
			break;
		case CvarBound_Upper:
			hasBound = info->bound.hasMax;
			bound = info->bound.maxVal;
			break;
		default:
			LogError(amx, AMX_ERR_NATIVE, "Invalid CvarBounds value: %d", params[2]);
			return 0;
	}

	*get_amxaddr(amx, params[3]) = amx_ftoc(bound);

	return hasBound;
}

bool bind_pcvar(CvarInfo* info, CvarBind::CvarType type, AMX* amx, cell varofs, size_t varlen = 0)
{
	if (varofs > amx->hlw) // If variable address is not inside global area, we can't bind it.
	{
		LogError(amx, AMX_ERR_NATIVE, "A global variable must be provided");
		return false;
	}

	int pluginId = g_plugins.findPluginFast(amx)->getId();
	cell* address = get_amxaddr(amx, varofs);

	// To avoid unexpected behavior, probably better to error such situations.
	for (size_t i = 0; i < info->binds.length(); ++i)
	{
		CvarBind* bind = info->binds[i];

		if (bind->pluginId == pluginId)
		{
			if (bind->varAddress == address)
			{
				LogError(amx, AMX_ERR_NATIVE, "A same variable can't be binded with several cvars");
				return false;
			}
		}
	}

	CvarBind* bind = new CvarBind(pluginId, type, get_amxaddr(amx, varofs), varlen);

	info->binds.append(bind);

	// Update right away variable with current cvar value.
	switch (type)
	{
		case CvarBind::CvarType_Int:
			*bind->varAddress = atoi(info->var->string);
			break;
		case CvarBind::CvarType_Float:
			*bind->varAddress = amx_ftoc(info->var->value);
			break;
		case CvarBind::CvarType_String:
			set_amxstring_simple(bind->varAddress, info->var->string, bind->varLength);
			break;
	}

	return true;
}

// bind_pcvar_float(pcvar, &Float:var)
static cell AMX_NATIVE_CALL bind_pcvar_float(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	CvarInfo* info = nullptr;

	if (!ptr || !(info = g_CvarManager.FindCvar(ptr->name)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return bind_pcvar(info, CvarBind::CvarType_Float, amx, params[2]);
}

// bind_pcvar_num(pcvar, &any:var)
static cell AMX_NATIVE_CALL bind_pcvar_num(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	CvarInfo* info = nullptr;

	if (!ptr || !(info = g_CvarManager.FindCvar(ptr->name)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return bind_pcvar(info, CvarBind::CvarType_Int, amx, params[2]);
}

// bind_pcvar_string(pcvar, any:var[], varlen)
static cell AMX_NATIVE_CALL bind_pcvar_string(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	CvarInfo* info = nullptr;

	if (!ptr || !(info = g_CvarManager.FindCvar(ptr->name)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	return bind_pcvar(info, CvarBind::CvarType_String, amx, params[2], params[3]);
}

// set_pcvar_flags(pcvar, flags)
static cell AMX_NATIVE_CALL set_pcvar_flags(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	ptr->flags = static_cast<int>(params[2]);

	return 1;
}

// set_pcvar_float(pcvar, Float:num)
static cell AMX_NATIVE_CALL set_pcvar_float(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	UTIL_Format(CVarTempBuffer, sizeof(CVarTempBuffer) - 1, "%f", amx_ctof(params[2]));
	CVAR_DIRECTSET(ptr, &CVarTempBuffer[0]);

	return 1;
}

// set_pcvar_num(pcvar, num)
static cell AMX_NATIVE_CALL set_pcvar_num(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	UTIL_Format(CVarTempBuffer, sizeof(CVarTempBuffer) - 1, "%d", params[2]);
	CVAR_DIRECTSET(ptr, &CVarTempBuffer[0]);

	return 1;
}

// set_pcvar_string(pcvar, const string[])
static cell AMX_NATIVE_CALL set_pcvar_string(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	if (!ptr)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	int len;

	CVAR_DIRECTSET(ptr, get_amxstring(amx, params[2], 0, len));

	return 1;
}

// set_pcvar_bounds(pcvar, CvarBounds:type, bool:set, Float:value = 0.0)
static cell AMX_NATIVE_CALL set_pcvar_bounds(AMX *amx, cell *params)
{
	cvar_t *ptr = reinterpret_cast<cvar_t *>(params[1]);
	CvarInfo* info = nullptr;

	if (!ptr || !(info = g_CvarManager.FindCvar(ptr->name)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid CVAR pointer");
		return 0;
	}

	bool set = params[3] != 0;
	int pluginId = g_plugins.findPluginFast(amx)->getId();
	float value = 0;
	bool should_update = false;

	switch (params[2])
	{
		case CvarBound_Lower:
		{
			info->bound.hasMin = set;

			if (set)
			{
				value = amx_ctof(params[4]);

				if (info->bound.hasMax && value > info->bound.maxVal)
				{
					LogError(amx, AMX_ERR_NATIVE, "A lower bound can't be above an upper bound");
					return 0;
				}

				should_update = true;

				info->bound.minVal = value;
				info->bound.minPluginId = pluginId;
			}

			break;
		}
		case CvarBound_Upper:
		{
			info->bound.hasMax = set;

			if (set)
			{
				value = amx_ctof(params[4]);

				if (info->bound.hasMin && value < info->bound.minVal)
				{
					LogError(amx, AMX_ERR_NATIVE, "An upper bound can't be below a lower bound");
					return 0;
				}

				should_update = true;

				info->bound.maxVal = value;
				info->bound.maxPluginId = pluginId;
			}

			break;
		}
		default:
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid CvarBounds value: %d", params[2]);
			return 0;
		}
	}

	if (should_update)
	{
		CVAR_SET_FLOAT(ptr->name, value);
	}

	return 1;
}

// remove_cvar_flags(const cvar[], flags=-1)
static cell AMX_NATIVE_CALL remove_cvar_flags(AMX *amx, cell *params)
{
	int ilen;
	char* sCvar = get_amxstring(amx, params[1], 0, ilen);

	if (!strcmp(sCvar, "amx_version") || !strcmp(sCvar, "amxmodx_version") || !strcmp(sCvar, "fun_version") || !strcmp(sCvar, "sv_cheats"))
		return 0;

	CvarInfo* info = g_CvarManager.FindCvar(sCvar);

	if (info)
	{
		info->var->flags &= ~((int)(params[2]));
		return 1;
	}

	return 0;
}

// get_plugins_cvar(id, name[], namelen, &flags=0, &plugin_id=0, &pcvar_handle=0, description[]="", desc_len=0)
static cell AMX_NATIVE_CALL get_plugins_cvar(AMX *amx, cell *params)
{
	CvarInfo* info = g_CvarManager.FindCvar(params[1]);

	if (info)
	{
		set_amxstring(amx, params[2], info->name.chars(), params[3]);
		*get_amxaddr(amx, params[4]) = info->var->flags;
		*get_amxaddr(amx, params[5]) = info->pluginId;
		*get_amxaddr(amx, params[6]) = reinterpret_cast<cell>(info->var);

		if (*params / sizeof(cell) >= 7)
		{
			set_amxstring(amx, params[7], info->description.chars(), params[8]);
		}

		return 1;
	}

	return 0;
}

// get_plugins_cvarsnum()
static cell AMX_NATIVE_CALL get_plugins_cvarsnum(AMX *amx, cell *params)
{
	return g_CvarManager.GetRegCvarsCount();
}

#if defined AMD64
static bool g_warned_ccqv = false;
#endif
// query_client_cvar(id, const cvar[], const resultfunc[])
static cell AMX_NATIVE_CALL query_client_cvar(AMX *amx, cell *params)
{
	int numParams = params[0] / sizeof(cell);

	if (numParams != 3 && numParams != 5)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed!");
		return 0;
	}

#if defined AMD64
	if (!g_warned_ccqv)
	{
		LogError(amx, AMX_ERR_NATIVE, "[AMXX] Client CVAR Querying is not available on AMD64 (one time warn)");
		g_warned_ccqv = true;
	}

	return 0;
#endif

	if (!g_NewDLL_Available)
	{
		LogError(amx, AMX_ERR_NATIVE, "Client CVAR querying is not enabled - check MM version!");
		return 0;
	}

	int id = params[1];

	if (id < 1 || id > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", id);
		return 0;
	}

	CPlayer *pPlayer = GET_PLAYER_POINTER_I(id);

	if (!pPlayer->initialized || pPlayer->IsBot())
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d is either not connected or a bot", id);
		return 0;
	}

	int dummy;
	const char *cvarname = get_amxstring(amx, params[2], 0, dummy);
	const char *resultfuncname = get_amxstring(amx, params[3], 1, dummy);

	// public clientcvarquery_result(id, const cvar[], const result[], [const param[]])
	int iFunc;

	if (numParams == 5 && params[4] != 0)
		iFunc = registerSPForwardByName(amx, resultfuncname, FP_CELL, FP_STRING, FP_STRING, FP_ARRAY, FP_DONE);
	else
		iFunc = registerSPForwardByName(amx, resultfuncname, FP_CELL, FP_STRING, FP_STRING, FP_DONE);

	if (iFunc == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function \"%s\" is not present", resultfuncname);
		return 0;
	}

	ClientCvarQuery_Info *queryObject = new ClientCvarQuery_Info;
	queryObject->resultFwd = iFunc;
	queryObject->requestId = MAKE_REQUESTID(PLID);

	if (numParams == 5 && params[4] != 0)
	{
		queryObject->paramLen = params[4] + 1;
		queryObject->params = new cell[queryObject->paramLen];

		if (!queryObject->params)
		{
			delete queryObject;
			unregisterSPForward(iFunc);
			LogError(amx, AMX_ERR_MEMORY, "Hmm. Out of memory?");
			return 0;
		}

		memcpy(reinterpret_cast<void*>(queryObject->params), reinterpret_cast<const void *>(get_amxaddr(amx, params[5])), queryObject->paramLen * sizeof(cell));

		queryObject->params[queryObject->paramLen - 1] = 0;
	}
	else {
		queryObject->params = NULL;
		queryObject->paramLen = 0;
	}

	pPlayer->queries.push_back(queryObject);

	QUERY_CLIENT_CVAR_VALUE2(pPlayer->pEdict, cvarname, queryObject->requestId);

	return 1;
}


AMX_NATIVE_INFO g_CvarNatives[] =
{
	{"create_cvar",				create_cvar},
	{"register_cvar",			register_cvar},
	{"cvar_exists",				cvar_exists},
	{"get_cvar_pointer",		get_cvar_pointer},

	{"hook_cvar_change",        hook_cvar_change},
	{"enable_cvar_hook",		enable_cvar_hook},
	{"disable_cvar_hook",		disable_cvar_hook},

	{"get_cvar_flags",			get_cvar_flags},
	{"get_cvar_float",			get_cvar_float},
	{"get_cvar_num",			get_cvar_num},
	{"get_cvar_string",			get_cvar_string},

	{"set_cvar_flags",			set_cvar_flags},
	{"set_cvar_float",			set_cvar_float},
	{"set_cvar_num",			set_cvar_num},
	{"set_cvar_string",			set_cvar_string},

	{"get_pcvar_flags",			get_pcvar_flags},
	{"get_pcvar_float",			get_pcvar_float},
	{"get_pcvar_num",			get_pcvar_num},
	{"get_pcvar_bool",			get_pcvar_bool},
	{"get_pcvar_string",		get_pcvar_string},
	{"get_pcvar_bounds",		get_pcvar_bounds},

	{"set_pcvar_flags",			set_pcvar_flags},
	{"set_pcvar_float",			set_pcvar_float},
	{"set_pcvar_num",			set_pcvar_num},
	{"set_pcvar_bool",			set_pcvar_num},
	{"set_pcvar_string",		set_pcvar_string},
	{"set_pcvar_bounds",		set_pcvar_bounds},

	{"remove_cvar_flags",		remove_cvar_flags},

	{"bind_pcvar_float",		bind_pcvar_float},
	{"bind_pcvar_num",			bind_pcvar_num},
	{"bind_pcvar_string",		bind_pcvar_string},

	{"get_plugins_cvar",		get_plugins_cvar},
	{"get_plugins_cvarsnum",	get_plugins_cvarsnum},
	
	{"query_client_cvar",		query_client_cvar},

	{NULL,						NULL}
};