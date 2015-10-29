//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _PDATA_SHARED_H_
#define _PDATA_SHARED_H_

#include <amxxmodule.h>
#include <IGameConfigs.h>
#include <HLTypeConversion.h>
#include <amtl/am-algorithm.h>

extern HLTypeConversion TypeConversion;

enum class BaseFieldType
{
	None,
	Integer,
	Float,
	Vector,
	Entity,
	String,
};

#define GET_TYPE_DESCRIPTION(position, data, conf)                                         \
	int classLength, memberLength;                                                         \
	char const *className = MF_GetAmxString(amx, params[position], 0, &classLength);       \
	char const *memberName = MF_GetAmxString(amx, params[position + 1], 1, &memberLength); \
	if (!classLength || !memberLength)                                                     \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Either class (\"%s\") or member (\"%s\") is empty", className, memberName); \
		return 0;                                                                          \
	}                                                                                      \
	else if (!conf->GetOffsetByClass(className, memberName, &data))                        \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "Could not find class \"%s\" and/or member \"%s\" in gamedata", className, memberName); \
		return 0;                                                                          \
	}                                                                                      \
	else if (data.fieldOffset < 0)                                                         \
	{                                                                                      \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset %d retrieved from \"%s\" member", data.fieldOffset, memberName); \
			return 0;                                                                      \
	}

#define CHECK_DATA(data, element, baseType)                                                \
	if (baseType > BaseFieldType::None && baseType != PvData::GetBaseDataType(data))       \
	{                                                                                      \
			MF_LogError(amx, AMX_ERR_NATIVE, "Data field is not %s-based", PvData::GetBaseTypeName(baseType)); \
			return 0;                                                                      \
	}                                                                                      \
	else if (element < 0 || (element > 0 && element >= data.fieldSize))                    \
	{                                                                                      \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid element index %d, value must be between 0 and %d", element, data.fieldSize); \
			return 0;                                                                      \
	}                                                                                      \
	else if (element > 0 && !data.fieldSize)                                               \
	{                                                                                      \
			MF_LogError(amx, AMX_ERR_NATIVE, "Member \"%s\" is not an array. Element %d is invalid.", memberName, element); \
			return 0;                                                                      \
	}

#define CHECK_GAMERULES()                                                                  \
	if (!GameRulesAddress)                                                                 \
	{                                                                                      \
		MF_LogError(amx, AMX_ERR_NATIVE, "%s is disabled. Check your AMXX log.", __FUNCTION__);  \
		return 0;                                                                          \
	}

class PvData
{
public:

	static cell GetInt(int index, TypeDescription &data, int element)
	{
		return GetInt(TypeConversion.id_to_edict(index)->pvPrivateData, data, element);
	}

	static cell GetInt(void *pObject, TypeDescription &data, int element)
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


	static void SetInt(int index, TypeDescription &data, cell value, int element)
	{
		SetInt(TypeConversion.id_to_edict(index)->pvPrivateData, data, value, element);
	}

	static void SetInt(void *pObject, TypeDescription &data, cell value, int element)
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
					set_pdata<int16>(pObject, data.fieldOffset, static_cast<int16>(value), element);
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
					set_pdata<int8>(pObject, data.fieldOffset, static_cast<int8>(value), element);
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


	static cell GetFloat(int index, TypeDescription &data, int element)
	{
		return GetFloat(TypeConversion.id_to_edict(index)->pvPrivateData, data, element);
	}

	static cell GetFloat(void *pObject, TypeDescription &data, int element)
	{
		return amx_ftoc(get_pdata<float>(pObject, data.fieldOffset, element));
	}


	static void SetFloat(int index, TypeDescription &data, float value, int element)
	{
		SetFloat(TypeConversion.id_to_edict(index)->pvPrivateData, data, value, element);
	}

	static void SetFloat(void *pObject, TypeDescription &data, float value, int element)
	{
		set_pdata<float>(pObject, data.fieldOffset, value, element);
	}


	static void GetVector(int index, TypeDescription &data, cell *pVector, int element)
	{
		return GetVector(TypeConversion.id_to_edict(index)->pvPrivateData, data, pVector, element);
	}

	static void GetVector(void *pObject, TypeDescription &data, cell *pVector, int element)
	{
		auto vector = get_pdata<Vector>(pObject, data.fieldOffset, element);

		pVector[0] = amx_ftoc(vector.x);
		pVector[1] = amx_ftoc(vector.y);
		pVector[2] = amx_ftoc(vector.z);
	}


	static void SetVector(int index, TypeDescription &data, cell *pVector, int element)
	{
		SetVector(TypeConversion.id_to_edict(index)->pvPrivateData, data, pVector, element);
	}

	static void SetVector(void *pObject, TypeDescription &data, cell *pVector, int element)
	{
		Vector vector(amx_ctof(pVector[0]), amx_ctof(pVector[1]), amx_ctof(pVector[2]));

		set_pdata<Vector>(pObject, data.fieldOffset, vector, element);
	}


	static cell GetEntity(int index, TypeDescription &data, int element)
	{
		return GetEntity(TypeConversion.id_to_edict(index)->pvPrivateData, data, element);
	}

	static cell GetEntity(void *pObject, TypeDescription &data, int element)
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


	static void SetEntity(int index, TypeDescription &data, int value, int element)
	{
		SetEntity(TypeConversion.id_to_edict(index)->pvPrivateData, data, value, element);
	}

	static void SetEntity(void *pObject, TypeDescription &data, int value, int element)
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


	static char* GetString(int index, TypeDescription &data, int element)
	{
		return GetString(TypeConversion.id_to_edict(index)->pvPrivateData, data, element);
	}

	static char* GetString(void *pObject, TypeDescription &data, int element)
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


	static cell SetString(int index, TypeDescription &data, const char *value, int maxlen, int element)
	{
		return SetString(TypeConversion.id_to_edict(index)->pvPrivateData, data, value, maxlen, element);
	}

	static cell SetString(void *pObject, TypeDescription &data, const char *value, int maxlen, int element)
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

public:

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

	static const char* GetBaseTypeName(BaseFieldType baseType)
	{
		static const char *BaseFieldTypeName[] =
		{
			"none",
			"integer",
			"float",
			"vector",
			"entity",
			"string",
		};

		return BaseFieldTypeName[static_cast<size_t>(baseType)];
	}
};

#endif // _PDATA_SHARED_H_
