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
#include <amtl/am-algorithm.h>

enum class BaseFieldType
{
	None,
	Integer,
	Float,
	Vector,
	Entity,
	String,
};

static const char *BaseFieldTypeName[] =
{
	"none",
	"integer",
	"float",
	"vector",
	"entity",
	"string",
};

static BaseFieldType GetBaseDataType(TypeDescription &data)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_INTEGER:
		case FieldType::FIELD_STRINGINT:
		case FieldType::FIELD_SHORT:
		case FieldType::FIELD_CHARACTER:
		case FieldType::FIELD_CLASS:
		case FieldType::FIELD_STRUCTURE:
		case FieldType::FIELD_POINTER:
		case FieldType::FIELD_FUNCTION:
		case FieldType::FIELD_BOOLEAN:
		{
			return BaseFieldType::Integer;
		}
		case FieldType::FIELD_FLOAT:
		{
			return BaseFieldType::Float;
		}
		case FieldType::FIELD_VECTOR:
		{
			return BaseFieldType::Vector;
		}
		case FieldType::FIELD_CLASSPTR:
		case FieldType::FIELD_ENTVARS:
		case FieldType::FIELD_EDICT:
		case FieldType::FIELD_EHANDLE:
		{
			return BaseFieldType::Entity;
		}
		case FieldType::FIELD_STRINGPTR:
		case FieldType::FIELD_STRING:
		{
			return BaseFieldType::String;
		}
	}

	return BaseFieldType::None;
}


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


cell GetData(void *pObject, TypeDescription &data, int element)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_INTEGER:
		case FieldType::FIELD_STRINGINT:
		{
			return get_pdata<int32>(pObject, data.fieldOffset, element);
		}
		case FieldType::FIELD_CLASS:
		case FieldType::FIELD_STRUCTURE:
		{
			return get_pdata_direct<cell>(pObject, data.fieldOffset);
		}
		case FieldType::FIELD_POINTER:
		case FieldType::FIELD_FUNCTION:
		{
			return reinterpret_cast<cell>(get_pdata<void*>(pObject, data.fieldOffset, element));
		}
		case FieldType::FIELD_SHORT:
		{
			if (data.fieldUnsigned)
			{
				return get_pdata<uint16>(pObject, data.fieldOffset, element);
			}
			else
			{
				return get_pdata<int16>(pObject, data.fieldOffset, element);
			}
		}
		case FieldType::FIELD_CHARACTER:
		{
			if (data.fieldUnsigned)
			{
				return get_pdata<uint8>(pObject, data.fieldOffset, element);
			}
			else
			{
				return get_pdata<int8>(pObject, data.fieldOffset, element);
			}
		}
		case FieldType::FIELD_BOOLEAN:
		{
			return get_pdata<bool>(pObject, data.fieldOffset, element) ? 1 : 0;
		}
	}

	return 0;
}

void SetData(void *pObject, TypeDescription &data, cell value, int element)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_INTEGER:
		case FieldType::FIELD_STRINGINT:
		{
			set_pdata<int32>(pObject, data.fieldOffset, static_cast<int32>(value), element);
			break;
		}
		case FieldType::FIELD_POINTER:
		case FieldType::FIELD_FUNCTION:
		{
			set_pdata<void*>(pObject, data.fieldOffset, reinterpret_cast<void*>(value), element);
			break;
		}
		case FieldType::FIELD_SHORT:
		{
			if (data.fieldUnsigned)
			{
				set_pdata<uint16>(pObject, data.fieldOffset, static_cast<uint16>(value), element);
			}
			else
			{
				set_pdata<int16>(pObject, data.fieldOffset, static_cast<uint16>(value), element);
			}
			break;
		}
		case FieldType::FIELD_CHARACTER:
		{
			if (data.fieldUnsigned)
			{
				set_pdata<uint8>(pObject, data.fieldOffset, static_cast<uint8>(value), element);
			}
			else
			{
				set_pdata<int8>(pObject, data.fieldOffset, static_cast<uint8>(value), element);
			}
			break;
		}
		case FieldType::FIELD_BOOLEAN:
		{
			set_pdata<bool>(pObject, data.fieldOffset, value != 0, element);
			break;
		}
	}
}

cell GetDataFloat(void *pObject, TypeDescription &data, int element)
{
	return amx_ftoc(get_pdata<float>(pObject, data.fieldOffset, element));
}

void SetDataFloat(void *pObject, TypeDescription &data, float value, int element)
{
	set_pdata<float>(pObject, data.fieldOffset, value, element);
}

void GetDataVector(void *pObject, TypeDescription &data, cell *pVector, int element)
{
	auto vector = get_pdata<Vector>(pObject, data.fieldOffset, element);

	pVector[0] = amx_ftoc(vector.x);
	pVector[1] = amx_ftoc(vector.y);
	pVector[2] = amx_ftoc(vector.z);
}

void SetDataVector(void *pObject, TypeDescription &data, cell *pVector, int element)
{
	Vector vector(amx_ctof(pVector[0]), amx_ctof(pVector[1]), amx_ctof(pVector[2]));

	set_pdata<Vector>(pObject, data.fieldOffset, vector, element);
}

cell GetDataEntity(void *pObject, TypeDescription &data, int element)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_CLASSPTR:
		{
			return TypeConversion.cbase_to_id(get_pdata<void*>(pObject, data.fieldOffset, element));
		}
		case FieldType::FIELD_ENTVARS:
		{
			return TypeConversion.entvars_to_id(get_pdata<entvars_t*>(pObject, data.fieldOffset, element));
		}
		case FieldType::FIELD_EDICT:
		{
			return TypeConversion.edict_to_id(get_pdata<edict_t*>(pObject, data.fieldOffset, element));
		}
		case FieldType::FIELD_EHANDLE:
		{
			return TypeConversion.edict_to_id(get_pdata<EHANDLE>(pObject, data.fieldOffset, element).Get());
		}
	}

	return 0;
}

void SetDataEntity(void *pObject, TypeDescription &data, int value, int element)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_CLASSPTR:
		{
			set_pdata<void*>(pObject, data.fieldOffset, value != -1 ? TypeConversion.id_to_cbase(value) : nullptr, element);
			break;
		}
		case FieldType::FIELD_ENTVARS:
		{
			set_pdata<entvars_t*>(pObject, data.fieldOffset, value != -1 ? TypeConversion.id_to_entvars(value) : nullptr, element);
			break;
		}
		case FieldType::FIELD_EDICT:
		{
			set_pdata<edict_t*>(pObject, data.fieldOffset, value != -1 ? TypeConversion.id_to_edict(value) : nullptr, element);
			break;
		}
		case FieldType::FIELD_EHANDLE:
		{
			get_pdata<EHANDLE>(pObject, data.fieldOffset, element).Set(value != -1 ? TypeConversion.id_to_edict(value) : nullptr);
			break;
		}
	}
}

char* GetDataString(void *pObject, TypeDescription &data, int element)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_STRING:
		{
			return get_pdata_direct<char*>(pObject, data.fieldOffset, element, data.fieldSize);
		}
		case FieldType::FIELD_STRINGPTR:
		{
			return get_pdata<char*>(pObject, data.fieldOffset, element);
		}
	}

	return nullptr;
}

cell SetDataString(void *pObject, TypeDescription &data, const char *value, int maxlen, int element)
{
	switch (data.fieldType)
	{
		case FieldType::FIELD_STRING:
		{
			auto buffer = get_pdata_direct<char*>(pObject, data.fieldOffset);
			return strncopy(buffer, value, ke::Min<int>(maxlen + 1, data.fieldSize));
		}
		case FieldType::FIELD_STRINGPTR:
		{
			auto buffer = get_pdata<char*>(pObject, data.fieldOffset, element);

			if (!buffer || maxlen > static_cast<int>(strlen(buffer)))
			{
				if (buffer)
				{
					free(buffer);
				}

				buffer = reinterpret_cast<char*>(malloc(maxlen + 1));
				set_pdata<char*>(pObject, data.fieldOffset, buffer, element);
			}

			return strncopy(buffer, value, maxlen + 1);
		}
	}

	return 0;
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

	return GetData(TypeConversion.id_to_edict(entity)->pvPrivateData, data, element);
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

	SetData(TypeConversion.id_to_edict(entity)->pvPrivateData, data, params[4], element);

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

	return GetDataFloat(TypeConversion.id_to_edict(entity)->pvPrivateData, data, element);
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

	SetDataFloat(TypeConversion.id_to_edict(entity)->pvPrivateData, data, amx_ctof(params[4]), element);

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

	GetDataVector(TypeConversion.id_to_edict(entity)->pvPrivateData, data, MF_GetAmxAddr(amx, params[4]), element);

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

	SetDataVector(TypeConversion.id_to_edict(entity)->pvPrivateData, data, MF_GetAmxAddr(amx, params[4]), element);

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

	return GetDataEntity(TypeConversion.id_to_edict(entity)->pvPrivateData, data, element);
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

	SetDataEntity(TypeConversion.id_to_edict(entity)->pvPrivateData, data, value, element);

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

	auto string = GetDataString(TypeConversion.id_to_edict(entity)->pvPrivateData, data, element);

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

	return SetDataString(TypeConversion.id_to_edict(entity)->pvPrivateData, data, value, length, element);
}



// native any:get_gamerules_int(const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_gamerules_int(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Integer, GAMERULES);

	int element = params[3];
	CHECK_ELEMENT(element);

	return GetData(GameRulesAddress, data, element);
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

	SetData(GameRulesAddress, data, params[3], element);

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

	return GetDataFloat(GameRulesAddress, data, element);
}

// native set_gamerules_float(const class[], const member[], Float:value, element = 0);
static cell AMX_NATIVE_CALL set_gamerules_float(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::Float, GAMERULES);

	int element = params[4];
	CHECK_ELEMENT(element);

	SetDataFloat(GameRulesAddress, data, amx_ctof(params[3]), element);

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

	GetDataVector(GameRulesAddress, data, MF_GetAmxAddr(amx, params[3]), element);

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

	GetDataVector(GameRulesAddress, data, MF_GetAmxAddr(amx, params[3]), element);

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

	return GetDataEntity(GameRulesAddress, data, element);
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

	SetDataEntity(GameRulesAddress, data, params[3], element);

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

	auto string = GetDataString(GameRulesAddress, data, element);

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

	return SetDataString(GameRulesAddress, data, value, length, element);
}



// native get_member_size(const class[], const member[]);
static cell AMX_NATIVE_CALL get_member_size(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None, ALL);

	return data.fieldSize;
}

// native find_member_info(const class[], const member[], &FieldType:type = FIELD_NONE, &arraysize = 0, &bool:unsigned = false);
static cell AMX_NATIVE_CALL find_member_info(AMX *amx, cell *params)
{
	CHECK_GAMERULES();

	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None, ALL);

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

	{ "get_member_size"     , get_member_size      },
	{ "find_member_info"    , find_member_info     },

	{ nullptr              , nullptr      }
};
