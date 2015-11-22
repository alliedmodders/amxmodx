// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#include "fakemeta_amxx.h"
#include "pdata_shared.h"

// native any:get_gamerules_int(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_int(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[3];
	CHECK_DATA(data, element, BaseFieldType::Integer);

	return PvData::GetInt(*GameRulesAddress, data, element);
}

// native set_gamerules_int(const class[], const member[], any:value, element = 0);
static cell AMX_NATIVE_CALL set_gamerules_int(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Integer);

	if (data.fieldType == FieldType::FIELD_STRUCTURE || data.fieldType == FieldType::FIELD_CLASS)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Setting directly to a class or structure address is not available");
		return 0;
	}

	PvData::SetInt(*GameRulesAddress, data, params[3], element);

	return 0;
}


// native Float:get_gamerules_float(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_float(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[3];
	CHECK_DATA(data, element, BaseFieldType::Float);

	return PvData::GetFloat(*GameRulesAddress, data, element);
}

// native set_gamerules_float(const class[], const member[], Float:value, element = 0);
static cell AMX_NATIVE_CALL set_gamerules_float(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Float);

	PvData::SetFloat(*GameRulesAddress, data, amx_ctof(params[3]), element);

	return 1;
}


// native get_gamerules_vector(const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_vector(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Vector);

	PvData::GetVector(*GameRulesAddress, data, MF_GetAmxAddr(amx, params[3]), element);

	return 1;
}

// native set_gamerules_vector(const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL set_gamerules_vector(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Vector);

	PvData::GetVector(*GameRulesAddress, data, MF_GetAmxAddr(amx, params[3]), element);

	return 1;
}


// native get_gamerules_entity(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_entity(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[3];
	CHECK_DATA(data, element, BaseFieldType::Entity);

	return PvData::GetEntity(*GameRulesAddress, data, element);
}

// native set_gamerules_entity(const class[], const member[], value, element = 0);
static cell AMX_NATIVE_CALL set_gamerules_entity(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	int value = params[3];

	if (value != -1)
	{
		CHECK_ENTITY(value);
	}

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Entity);

	PvData::SetEntity(*GameRulesAddress, data, params[3], element);

	return 0;
}


// native get_gamerules_string(const class[], const member[], value[], maxlen, element = 0);
static cell AMX_NATIVE_CALL get_gamerules_string(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::String);

	auto buffer = params[3];
	auto maxlen = params[4];

	auto string = PvData::GetString(*GameRulesAddress, data, element);

	if (data.fieldSize)
	{
		maxlen = ke::Min(maxlen, data.fieldSize);
	}

	return MF_SetAmxStringUTF8Char(amx, buffer, string ? string : "", string ? strlen(string) : 0, maxlen);
}

// native set_gamerules_string(const class[], const member[], const value[], element = 0);
static cell AMX_NATIVE_CALL set_gamerules_string(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::String);

	int length;
	const char *value = MF_GetAmxString(amx, params[3], 0, &length);

	return PvData::SetString(*GameRulesAddress, data, value, length, element);
}


// native get_gamerules_size(const class[], const member[]);
static cell AMX_NATIVE_CALL get_gamerules_size(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	return data.fieldSize;
}

// native find_gamerules_info(const class[], const member[], &FieldType:type = FIELD_NONE, &arraysize = 0, &bool:unsigned = false);
static cell AMX_NATIVE_CALL find_gamerules_info(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, GamerulesConfig);

	*MF_GetAmxAddr(amx, params[3]) = static_cast<cell>(data.fieldType);
	*MF_GetAmxAddr(amx, params[4]) = ke::Max<int>(0, data.fieldSize);
	*MF_GetAmxAddr(amx, params[5]) = data.fieldUnsigned != 0;

	return data.fieldOffset;
}


AMX_NATIVE_INFO pdata_gamerules_natives[] =
{
	{ "get_gamerules_int"   , get_gamerules_int    },
	{ "set_gamerules_int"   , set_gamerules_int    },
	{ "get_gamerules_float" , get_gamerules_float  },
	{ "set_gamerules_float" , set_gamerules_float  },
	{ "get_gamerules_vector", get_gamerules_vector },
	{ "set_gamerules_vector", set_gamerules_vector },
	{ "get_gamerules_entity", get_gamerules_entity },
	{ "set_gamerules_entity", set_gamerules_entity },
	{ "get_gamerules_string", get_gamerules_string },
	{ "set_gamerules_string", set_gamerules_string },
	{ "get_gamerules_size"  , get_gamerules_size   },
	{ "find_gamerules_info" , find_gamerules_info  },
	{ nullptr               , nullptr              }
};
