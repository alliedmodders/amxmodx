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

#define GET_TYPE_DESCRIPTION(position, data, baseType)                                     \
	int classLength, memberLength;                                                         \
	const char *className  = MF_GetAmxString(amx, params[position], 0, &classLength);  \
	const char *memberName = MF_GetAmxString(amx, params[position + 1], 1, &memberLength); \
	if (!classLength || !memberLength)                                                     \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Either class (\"%s\") or member (\"%s\") is empty", className, memberName); \
		return 0;                                                                          \
	}                                                                                      \
	else if (!CommonConfig->GetOffsetByClass(className, memberName, &data))                \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Could not find class \"%s\" and/or member \"%s\" in gamedata", className, memberName); \
		return 0;                                                                          \
	}                                                                                      \
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

// native any:get_ent_data(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Integer);

	int element = params[4];
	CHECK_ELEMENT(element);

	auto pEntity = TypeConversion.id_to_edict(entity);

	switch (data.fieldType)
	{
		case FieldType::FIELD_INTEGER:
		case FieldType::FIELD_STRINGINT:
		{
			return get_pdata<int32>(pEntity, data.fieldOffset, element);
		}
		case FieldType::FIELD_CLASS:
		case FieldType::FIELD_STRUCTURE:
		{
			return reinterpret_cast<cell>(reinterpret_cast<int8*>(pEntity->pvPrivateData) + data.fieldOffset);
		}
		case FieldType::FIELD_POINTER:
		case FieldType::FIELD_FUNCTION:
		{
			return reinterpret_cast<cell>(get_pdata<void*>(pEntity, data.fieldOffset, element));
		}
		case FieldType::FIELD_SHORT:
		{
			if (data.fieldUnsigned)
			{
				return get_pdata<uint16>(pEntity, data.fieldOffset, element);
			}
			else
			{
				return get_pdata<int16>(pEntity, data.fieldOffset, element);
			}
		}
		case FieldType::FIELD_CHARACTER:
		{
			if (data.fieldUnsigned)
			{
				return get_pdata<uint8>(pEntity, data.fieldOffset, element);
			}
			else
			{
				return get_pdata<int8>(pEntity, data.fieldOffset, element);
			}
		}
		case FieldType::FIELD_BOOLEAN:
		{
			return get_pdata<bool>(pEntity, data.fieldOffset, element) ? 1 : 0;
		}
	}

	return 0;
}

// native set_ent_data(entity, const class[], const member[], any:value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Integer);

	int element = params[5];
	CHECK_ELEMENT(element);

	auto pEntity = TypeConversion.id_to_edict(entity);
	auto value = params[4];

	switch (data.fieldType)
	{
		case FieldType::FIELD_INTEGER:
		case FieldType::FIELD_STRINGINT:
		{
			set_pdata<int32>(pEntity, data.fieldOffset, static_cast<int32>(value), element);
			break;
		}
		case FieldType::FIELD_CLASS:
		case FieldType::FIELD_STRUCTURE:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Setting directly to a class or structure address is not available");
			return 0;
		}
		case FieldType::FIELD_POINTER:
		case FieldType::FIELD_FUNCTION:
		{
			set_pdata<void*>(pEntity, data.fieldOffset, reinterpret_cast<void*>(value), element);
			break;
		}
		case FieldType::FIELD_SHORT:
		{
			if (data.fieldUnsigned)
			{
				set_pdata<uint16>(pEntity, data.fieldOffset, static_cast<uint16>(value), element);
			}
			else
			{
				set_pdata<int16>(pEntity, data.fieldOffset, static_cast<uint16>(value), element);
			}
			break;
		}
		case FieldType::FIELD_CHARACTER:
		{
			if (data.fieldUnsigned)
			{
				set_pdata<uint8>(pEntity, data.fieldOffset, static_cast<uint8>(value), element);
			}
			else
			{
				set_pdata<int8>(pEntity, data.fieldOffset, static_cast<uint8>(value), element);
			}
			break;
		}
		case FieldType::FIELD_BOOLEAN:
		{
			set_pdata<bool>(pEntity, data.fieldOffset, value != 0, element);
			break;
		}
	}

	return 0;
}


// native Float:get_ent_data_float(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_float(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Float);

	int element = params[4];
	CHECK_ELEMENT(element);

	return amx_ftoc(get_pdata<float>(TypeConversion.id_to_edict(entity), data.fieldOffset, element));
}

// native set_ent_data_float(entity, const classname[], const member[], Float:value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data_float(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Float);

	int element = params[5];
	CHECK_ELEMENT(element);

	set_pdata<float>(TypeConversion.id_to_edict(entity), data.fieldOffset, amx_ctof(params[4]), element);

	return 1;
}


// native get_ent_data_vector(entity, const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_vector(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Vector);

	int element = params[5];
	CHECK_ELEMENT(element);

	auto refvec = MF_GetAmxAddr(amx, params[4]);
	auto vector = get_pdata<Vector>(TypeConversion.id_to_edict(entity), data.fieldOffset, element);

	refvec[0] = amx_ftoc(vector.x);
	refvec[1] = amx_ftoc(vector.y);
	refvec[2] = amx_ftoc(vector.z);

	return 1;
}

// native set_ent_data_vector(entity, const class[], const member[], Float:value[3], element = 0);
static cell AMX_NATIVE_CALL set_ent_data_vector(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Vector);

	int element = params[5];
	CHECK_ELEMENT(element);

	auto refvec = MF_GetAmxAddr(amx, params[4]);
	Vector vector(amx_ctof(refvec[0]), amx_ctof(refvec[1]), amx_ctof(refvec[2]));

	set_pdata<Vector>(TypeConversion.id_to_edict(entity), data.fieldOffset, vector, element);

	return 1;
}


// native get_ent_data_entity(entity, const class[], const member[], element = 0);
static cell AMX_NATIVE_CALL get_ent_data_entity(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Entity);

	int element = params[4];
	CHECK_ELEMENT(element);

	auto pEntity = TypeConversion.id_to_edict(entity);

	switch (data.fieldType)
	{
		case FieldType::FIELD_CLASSPTR:
		{
			return TypeConversion.cbase_to_id(get_pdata<void*>(pEntity, data.fieldOffset, element));
		}
		case FieldType::FIELD_ENTVARS:
		{
			return TypeConversion.entvars_to_id(get_pdata<entvars_t*>(pEntity, data.fieldOffset, element));
		}
		case FieldType::FIELD_EDICT:
		{
			return TypeConversion.edict_to_id(get_pdata<edict_t*>(pEntity, data.fieldOffset, element));
		}
		case FieldType::FIELD_EHANDLE:
		{
			return TypeConversion.edict_to_id(get_pdata<EHANDLE>(pEntity, data.fieldOffset, element).Get());
		}
	}

	return 0;
}

// native set_ent_data_entity(entity, const class[], const member[], value, element = 0);
static cell AMX_NATIVE_CALL set_ent_data_entity(AMX *amx, cell *params)
{
	int entity = params[1];
	int value  = params[4];

	CHECK_ENTITY(entity);

	if (value != -1)
	{
		CHECK_ENTITY(value);
	}

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::Entity);

	int element = params[5];
	CHECK_ELEMENT(element);

	auto pEntity = TypeConversion.id_to_edict(entity);

	switch (data.fieldType)
	{
		case FieldType::FIELD_CLASSPTR:
		{
			set_pdata<void*>(pEntity, data.fieldOffset, value != -1 ? TypeConversion.id_to_cbase(value) : nullptr, element);
			break;
		}
		case FieldType::FIELD_ENTVARS:
		{
			set_pdata<entvars_t*>(pEntity, data.fieldOffset, value != -1 ? TypeConversion.id_to_entvars(value) : nullptr, element);
			break;
		}
		case FieldType::FIELD_EDICT:
		{
			set_pdata<edict_t*>(pEntity, data.fieldOffset, value != -1 ? TypeConversion.id_to_edict(value) : nullptr, element);
			break;
		}
		case FieldType::FIELD_EHANDLE:
		{
			get_pdata<EHANDLE>(pEntity, data.fieldOffset, element).Set(value != -1 ? TypeConversion.id_to_edict(value) : nullptr);
			break;
		}
	}

	return 0;
}


// native get_ent_data_string(entity, const class[], const member[], value[], maxlen, element = 0);
static cell AMX_NATIVE_CALL get_ent_data_string(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::String);

	int element = params[6];
	CHECK_ELEMENT(element);

	auto pEntity = TypeConversion.id_to_edict(entity);

	cell buffer = params[4];
	int  maxlen = params[5];

	switch (data.fieldType)
	{
		case FieldType::FIELD_STRING:
		{
			maxlen = ke::Min<int>(maxlen, static_cast<int>(data.fieldSize));
			char *string = get_pdata_direct<char*>(pEntity, data.fieldOffset, element, data.fieldSize);

			return MF_SetAmxStringUTF8Char(amx, buffer, string ? string : "", string ? strlen(string) : 0, maxlen);
		}
		case FieldType::FIELD_STRINGPTR:
		{
			char *string = get_pdata<char*>(pEntity, data.fieldOffset, element);

			return MF_SetAmxStringUTF8Char(amx, buffer, string ? string : "", string ? strlen(string) : 0, maxlen);
		}
	}

	return 0;
}

// native set_ent_data_string(entity, const class[], const member[], const value[], element = 0);
static cell AMX_NATIVE_CALL set_ent_data_string(AMX *amx, cell *params)
{
	int entity = params[1];
	CHECK_ENTITY(entity);

	TypeDescription data;
	GET_TYPE_DESCRIPTION(2, data, BaseFieldType::String);

	int element = params[5];
	CHECK_ELEMENT(element);

	auto pEntity = TypeConversion.id_to_edict(entity);

	int length;
	const char *value = MF_GetAmxString(amx, params[4], 0, &length);

	switch (data.fieldType)
	{
		case FieldType::FIELD_STRING:
		{
			auto buffer = reinterpret_cast<char*>(pEntity->pvPrivateData) + data.fieldOffset;
			return strncopy(buffer, value, ke::Min<int>(length + 1, data.fieldSize));
		}
		case FieldType::FIELD_STRINGPTR:
		{
			auto buffer = get_pdata<char*>(pEntity, data.fieldOffset, element);

			if (!buffer || length > static_cast<int>(strlen(buffer)))
			{
				if (buffer)
				{
					free(buffer);
				}

				buffer = reinterpret_cast<char*>(malloc(length + 1));
				set_pdata<char*>(pEntity, data.fieldOffset, buffer, element);
			}

			return strncopy(buffer, value, length + 1);
		}
	}

	return 0;
}


// native get_ent_data_size(const class[], const member[]);
static cell AMX_NATIVE_CALL get_ent_data_size(AMX *amx, cell *params)
{
	TypeDescription data;
	GET_TYPE_DESCRIPTION(0, data, BaseFieldType::None);

	return data.fieldSize;
}

// native find_ent_data_info(const class[], const member[], &FieldType:type = FIELD_NONE, &arraysize = 0, &bool:unsigned = false);
static cell AMX_NATIVE_CALL find_ent_data_info(AMX *amx, cell *params)
{
	TypeDescription data;
	GET_TYPE_DESCRIPTION(1, data, BaseFieldType::None);

	*MF_GetAmxAddr(amx, params[3]) = static_cast<cell>(data.fieldType);
	*MF_GetAmxAddr(amx, params[4]) = ke::Max<int>(0, data.fieldSize);
	*MF_GetAmxAddr(amx, params[5]) = data.fieldUnsigned != 0;

	return data.fieldOffset;
}


AMX_NATIVE_INFO pdata_gc_natives[] =
{
	{ "get_ent_data"       , get_ent_data        },
	{ "set_ent_data"       , set_ent_data        },

	{ "get_ent_data_float" , get_ent_data_float  },
	{ "set_ent_data_float" , set_ent_data_float  },

	{ "get_ent_data_vector", get_ent_data_vector },
	{ "set_ent_data_vector", set_ent_data_vector },

	{ "get_ent_data_entity", get_ent_data_entity },
	{ "set_ent_data_entity", set_ent_data_entity },

	{ "get_ent_data_string", get_ent_data_string },
	{ "set_ent_data_string", set_ent_data_string },

	{ "get_ent_data_size"  , get_ent_data_size   },
	{ "find_ent_data_info" , find_ent_data_info  },

	{ nullptr              , nullptr      }
};
