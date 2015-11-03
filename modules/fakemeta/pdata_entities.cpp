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

// native any:get_ent_data(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Integer);

	return PvData::GetInt(entity, data, element);
}

// native set_ent_data(entity, const class[], const member[], any:value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::Integer);

	if (data.fieldType == FieldType::FIELD_STRUCTURE || data.fieldType == FieldType::FIELD_CLASS)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Setting directly to a class or structure address is not available");
		return 0;
	}

	PvData::SetInt(entity, data, params[4], element);

	return 1;
}


// native Float:get_ent_data_float(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_float(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Float);

	return PvData::GetFloat(entity, data, element);
}

// native set_ent_data_float(entity, const classname[], const member[], Float:value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data_float(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::Float);

	PvData::SetFloat(entity, data, amx_ctof(params[4]), element);

	return 1;
}


// native get_ent_data_vector(entity, const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_vector(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::Vector);

	PvData::GetVector(entity, data, MF_GetAmxAddr(amx, params[4]), element);

	return 1;
}

// native set_ent_data_vector(entity, const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL set_ent_data_vector(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::Vector);

	PvData::SetVector(entity, data, MF_GetAmxAddr(amx, params[4]), element);

	return 1;
}


// native get_ent_data_entity(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_entity(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[4];
	CHECK_DATA(data, element, BaseFieldType::Entity);

	return PvData::GetEntity(entity, data, element);
}

// native set_ent_data_entity(entity, const class[], const member[], value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data_entity(AMX *amx, cell *params)
{
	int entity = params[1];
	int value = params[4];

	CHECK_ENTITY(entity);

	if (value != -1)
	{
		CHECK_ENTITY(value);
	}

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::Entity);

	PvData::SetEntity(entity, data, value, element);

	return 1;
}


// native get_ent_data_string(entity, const class[], const member[], value[], maxlen, element = 0);
static cell AMX_NATIVE_CALL get_ent_data_string(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[6];
	CHECK_DATA(data, element, BaseFieldType::String);

	auto buffer = params[4];
	auto maxlen = params[5];

	auto string = PvData::GetString(entity, data, element);

	if (data.fieldSize)
	{
		maxlen = ke::Min(maxlen, data.fieldSize);
	}

	return MF_SetAmxStringUTF8Char(amx, buffer, string ? string : "", string ? strlen(string) : 0, maxlen);
}

// native set_ent_data_string(entity, const class[], const member[], const value[], element = 0);
static cell AMX_NATIVE_CALL set_ent_data_string(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, CommonConfig);

	int element = params[5];
	CHECK_DATA(data, element, BaseFieldType::String);

	int length;
	const char *value = MF_GetAmxString(amx, params[4], 0, &length);

	return PvData::SetString(entity, data, value, length, element);
}


// native get_ent_data_size(const class[], const member[]);
static cell AMX_NATIVE_CALL get_ent_data_size(AMX *amx, cell *params)
{
	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, CommonConfig);

	return data.fieldSize;
}

// native find_ent_data_info(const class[], const member[], &FieldType:type = FIELD_NONE, &arraysize = 0, &bool:unsigned = false);
static cell AMX_NATIVE_CALL find_ent_data_info(AMX *amx, cell *params)
{
	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, CommonConfig);

	*MF_GetAmxAddr(amx, params[3]) = static_cast<cell>(data.fieldType);
	*MF_GetAmxAddr(amx, params[4]) = ke::Max<int>(0, data.fieldSize);
	*MF_GetAmxAddr(amx, params[5]) = data.fieldUnsigned != 0;

	return data.fieldOffset;
}


AMX_NATIVE_INFO pdata_entities_natives[] =
{
	{ "get_ent_data"        , get_ent_data         },
	{ "set_ent_data"        , set_ent_data         },
	{ "get_ent_data_float"  , get_ent_data_float   },
	{ "set_ent_data_float"  , set_ent_data_float   },
	{ "get_ent_data_vector" , get_ent_data_vector  },
	{ "set_ent_data_vector" , set_ent_data_vector  },
	{ "get_ent_data_entity" , get_ent_data_entity  },
	{ "set_ent_data_entity" , set_ent_data_entity  },
	{ "get_ent_data_string" , get_ent_data_string  },
	{ "set_ent_data_string" , set_ent_data_string  },
	{ "get_ent_data_size"   , get_ent_data_size    },
	{ "find_ent_data_info"  , find_ent_data_info   },
	{ nullptr               , nullptr              }
};