// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "gameconfigs.h"
#include "amxmodx.h"
#include "CGameConfigs.h"

NativeHandle<GameConfigNative> GameConfigHandle;

// native GameConfig:LoadGameConfigFile(const file[]);
static cell AMX_NATIVE_CALL LoadGameConfigFile(AMX *amx, cell *params)
{
	int length;
	const char *filename = get_amxstring(amx, params[1], 0, length);

	IGameConfig *config = nullptr;
	char error[128];

	if (!ConfigManager.LoadGameConfigFile(filename, &config, error, sizeof(error)))
	{
		LogError(amx, AMX_ERR_NATIVE, "Unable to open %s: %s", filename, error);
		return 0;
	}

	int handle = GameConfigHandle.create();

	auto configHandle = GameConfigHandle.lookup(handle);
	
	if (!configHandle)
	{
		return 0;
	}

	configHandle->m_config = config;

	return handle;
}

// native GameConfGetOffset(GameConfig:handle, const key[]);
static cell AMX_NATIVE_CALL GameConfGetOffset(AMX *amx, cell *params)
{
	GameConfigNative *handle = GameConfigHandle.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid game config handle %d", params[1]);
		return 0;
	}

	int length;
	TypeDescription value;

	const char *key = get_amxstring(amx, params[2], 0, length);

	if (!handle->m_config->GetOffset(key, &value))
	{
		return -1;
	}

	return value.fieldOffset;
}

// native GameConfGetClassOffset(GameConfig:handle, const classname[], const key[]);
static cell AMX_NATIVE_CALL GameConfGetClassOffset(AMX *amx, cell *params)
{
	GameConfigNative *handle = GameConfigHandle.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid game config handle %d", params[1]);
		return 0;
	}

	int length;
	TypeDescription value;

	const char *classname = get_amxstring(amx, params[2], 0, length);
	const char *key = get_amxstring(amx, params[3], 1, length);

	if (!handle->m_config->GetOffsetByClass(classname, key, &value))
	{
		return -1;
	}

	return value.fieldOffset;
}

// native bool:GameConfGetKeyValue(GameConfig:handle, const key[], buffer[], maxlen);
static cell AMX_NATIVE_CALL GameConfGetKeyValue(AMX *amx, cell *params)
{
	GameConfigNative *handle = GameConfigHandle.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid game config handle %d", params[1]);
		return 0;
	}

	int length;
	const char *value;
	const char *key = get_amxstring(amx, params[2], 0, length);

	if (!(value = handle->m_config->GetKeyValue(key)))
	{
		return 0;
	}

	set_amxstring_utf8(amx, params[3], value, strlen(value), params[4]);

	return 1;
}

// native GameConfGetAddress(GameConfig:handle, const name[]);
static cell AMX_NATIVE_CALL GameConfGetAddress(AMX *amx, cell *params)
{
	GameConfigNative *handle = GameConfigHandle.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid game config handle %d", params[1]);
		return 0;
	}

	int length;
	void *value;

	const char *key = get_amxstring(amx, params[2], 0, length);

	if (!handle->m_config->GetAddress(key, &value))
	{
		return 0;
	}

	return reinterpret_cast<cell>(value);
}

// native CloseGameConfigFile(&GameConfig:handle);
static cell AMX_NATIVE_CALL CloseGameConfigFile(AMX *amx, cell *params)
{
	cell *address = get_amxaddr(amx, params[1]);

	GameConfigNative *handle = GameConfigHandle.lookup(*address);

	if (!handle)
	{
		return 0;
	}

	if (GameConfigHandle.destroy(*address))
	{
		*address = 0;
		return 1;
	}

	return 0;
}


AMX_NATIVE_INFO g_GameConfigNatives[] = 
{
	{ "LoadGameConfigFile"    , LoadGameConfigFile     },
	{ "GameConfGetOffset"     , GameConfGetOffset      },
	{ "GameConfGetClassOffset", GameConfGetClassOffset },
	{ "GameConfGetKeyValue"   , GameConfGetKeyValue    },
	{ "GameConfGetAddress"    , GameConfGetAddress     },
	{ "CloseGameConfigFile"   , CloseGameConfigFile    },
	{ nullptr                 , nullptr                }
};
