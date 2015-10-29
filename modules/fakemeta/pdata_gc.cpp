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

#define GET_TYPE_DESCRIPTION(position, data, baseType, list)  \
	GET_TYPE_DESCRIPTION_START(position, data, baseType)      \
	CHECK_##list##_OFFSET(className, memberName, data)        \
	CHECK_ERROR(className, memberName)                        \
	GET_TYPE_DESCRIPTION_END(memberName, data, baseType)

#define GET_TYPE_DESCRIPTION_START(position, data, baseType)                               \
	int classLength, memberLength;                                                         \
	const char *className  = MF_GetAmxString(amx, params[position], 0, &classLength);      \
	const char *memberName = MF_GetAmxString(amx, params[position + 1], 1, &memberLength); \
	if (!classLength || !memberLength)                                                     \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Either class (\"%s\") or member (\"%s\") is empty", className, memberName); \
		return 0;                                                                          \
	}

#define CHECK_ENTITY_OFFSET(className, memberName, data)                         \
	else if (!CommonConfig->GetOffsetByClass(className, memberName, &data))

#define CHECK_GAMERULES_OFFSET(className, memberName, data)                      \
	else if (!GamerulesConfig->GetOffsetByClass(className, memberName, &data))

#define CHECK_ALL_OFFSET(className, memberName, data)                            \
	else if (!CommonConfig->GetOffsetByClass(className, memberName, &data) &&    \
			 !GamerulesConfig->GetOffsetByClass(className, memberName, &data))

#define CHECK_ERROR(className, memberName)                                       \
	{                                                                            \
		MF_LogError(amx, AMX_ERR_NATIVE, "Could not find class \"%s\" and/or member \"%s\" in gamedata", className, memberName); \
		return 0;                                                                \
	}

#define GET_TYPE_DESCRIPTION_END(memberName, data, baseType) \
	else if (data.fieldOffset < 0)                                                         \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset %d retrieved from \"%s\" member", data.fieldOffset, memberName); \
		return 0;                                                                          \
	}                                                                                      \
	else if (baseType > BaseFieldType::None && baseType != GetBaseDataType(data))          \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Data field is not %s-based", BaseFieldTypeName[static_cast<int>(baseType)]); \
		return 0;                                                                          \
	}

#define CHECK_ELEMENT(element)                                                             \
	if (element < 0 || (element > 0 && element >= data.fieldSize))                         \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid element index %d, value must be between 0 and %d", element, data.fieldSize);  \
		return 0;                                                                          \
	}                                                                                      \
	else if (element > 0 && !data.fieldSize)                                               \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Member \"%s\" is not an array. Element %d is invalid.", memberName, element);\
		return 0;                                                                          \
	}

#define CHECK_GAMERULES()                                                                  \
	if (!GameRulesAddress)                                                                 \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "%s is disabled. Check your AMXX log.", __FUNCTION__);  \
		return 0;                                                                          \
	}


// native any:get_ent_data(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Integer, ENTITY);

	int element = params[4];
	CHECK_ELEMENT(element);

	return PvData::GetInt(entity, data, element);
}

// native set_ent_data(entity, const class[], const member[], any:value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Integer, ENTITY);

	int element = params[5];
	CHECK_ELEMENT(element);

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
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Float, ENTITY);

	int element = params[4];
	CHECK_ELEMENT(element);

	return PvData::GetFloat(entity, data, element);
}

// native set_ent_data_float(entity, const classname[], const member[], Float:value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data_float(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Float, ENTITY);

	int element = params[5];
	CHECK_ELEMENT(element);

	PvData::SetFloat(entity, data, amx_ctof(params[4]), element);

	return 1;
}


// native get_ent_data_vector(entity, const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_vector(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Vector, ENTITY);

	int element = params[5];
	CHECK_ELEMENT(element);

	PvData::GetVector(entity, data, MF_GetAmxAddr(amx, params[4]), element);

	return 1;
}

// native set_ent_data_vector(entity, const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL set_ent_data_vector(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Vector, ENTITY);

	int element = params[5];
	CHECK_ELEMENT(element);

	PvData::SetVector(entity, data, MF_GetAmxAddr(amx, params[4]), element);

	return 1;
}


// native get_ent_data_entity(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_entity(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Entity, ENTITY);

	int element = params[4];
	CHECK_ELEMENT(element);

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
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Entity, ENTITY);

	int element = params[5];
	CHECK_ELEMENT(element);

	PvData::SetEntity(entity, data, value, element);

	return 1;
}


// native get_ent_data_string(entity, const class[], const member[], value[], maxlen, element = 0);
static cell AMX_NATIVE_CALL get_ent_data_string(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::String, ENTITY);

	int element = params[6];
	CHECK_ELEMENT(element);

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
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::String, ENTITY);

	int element = params[5];
	CHECK_ELEMENT(element);

	int length;
	const char *value = MF_GetAmxString(amx, params[4], 0, &length);

	return PvData::SetString(entity, data, value, length, element);
}


// native get_ent_data_size(const class[], const member[]);
static cell AMX_NATIVE_CALL get_ent_data_size(AMX *amx, cell *params)
{
	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None, ENTITY);

	return data.fieldSize;
}

// native find_ent_data_info(const class[], const member[], &FieldType:type = FIELD_NONE, &arraysize = 0, &bool:unsigned = false);
static cell AMX_NATIVE_CALL find_ent_data_info(AMX *amx, cell *params)
{
	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None, ENTITY);

	*MF_GetAmxAddr(amx, params[3]) = static_cast<cell>(data.fieldType);
	*MF_GetAmxAddr(amx, params[4]) = ke::Max<int>(0, data.fieldSize);
	*MF_GetAmxAddr(amx, params[5]) = data.fieldUnsigned != 0;

	return data.fieldOffset;
}




// native any:get_gamerules_int(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_int(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Integer, GAMERULES);

	int element = params[3];
	CHECK_ELEMENT(element);

	return PvData::GetInt(GameRulesAddress, data, element);
}

// native set_gamerules_int(const class[], const member[], any:value, element = 0);
static cell AMX_NATIVE_CALL set_gamerules_int(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Integer, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	if (data.fieldType == FieldType::FIELD_STRUCTURE || data.fieldType == FieldType::FIELD_CLASS)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Setting directly to a class or structure address is not available");
		return 0;
	}

	PvData::SetInt(GameRulesAddress, data, params[3], element);

	return 0;
}


// native Float:get_gamerules_float(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_float(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Float, GAMERULES);

	int element = params[3];
	CHECK_ELEMENT(element);

	return PvData::GetFloat(GameRulesAddress, data, element);
}

// native set_gamerules_float(const class[], const member[], Float:value, element = 0);
static cell AMX_NATIVE_CALL set_gamerules_float(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Float, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	PvData::SetFloat(GameRulesAddress, data, amx_ctof(params[3]), element);

	return 1;
}


// native get_gamerules_vector(const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_vector(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Vector, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	PvData::GetVector(GameRulesAddress, data, MF_GetAmxAddr(amx, params[3]), element);

	return 1;
}

// native set_gamerules_vector(const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL set_gamerules_vector(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Vector, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	PvData::GetVector(GameRulesAddress, data, MF_GetAmxAddr(amx, params[3]), element);

	return 1;
}


// native get_gamerules_entity(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_entity(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Entity, GAMERULES);

	int element = params[3];
	CHECK_ELEMENT(element);

	return PvData::GetEntity(GameRulesAddress, data, element);
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
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Entity, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	PvData::SetEntity(GameRulesAddress, data, params[3], element);

	return 0;
}


// native get_gamerules_string(const class[], const member[], value[], maxlen, element = 0);
static cell AMX_NATIVE_CALL get_gamerules_string(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::String, GAMERULES);

	int element = params[5];
	CHECK_ELEMENT(element);

	auto buffer = params[3];
	auto maxlen = params[4];

	auto string = PvData::GetString(GameRulesAddress, data, element);

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
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::String, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	int length;
	const char *value = MF_GetAmxString(amx, params[3], 0, &length);

	return PvData::SetString(GameRulesAddress, data, value, length, element);
}


// native get_gamerules_size(const class[], const member[]);
static cell AMX_NATIVE_CALL get_gamerules_size(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None, GAMERULES);

	return data.fieldSize;
}

// native find_gamerules_info(const class[], const member[], &FieldType:type = FIELD_NONE, &arraysize = 0, &bool:unsigned = false);
static cell AMX_NATIVE_CALL find_gamerules_info(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None, GAMERULES);

	*MF_GetAmxAddr(amx, params[3]) = static_cast<cell>(data.fieldType);
	*MF_GetAmxAddr(amx, params[4]) = ke::Max<int>(0, data.fieldSize);
	*MF_GetAmxAddr(amx, params[5]) = data.fieldUnsigned != 0;

	return data.fieldOffset;
}


AMX_NATIVE_INFO pdata_gc_natives[] =
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
