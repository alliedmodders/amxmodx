// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// JSON Natives
//

#include "JsonMngr.h"

ke::UniquePtr<JSONMngr> JsonMngr;

//native JSON:json_parse(const string[], bool:is_file = false, bool:with_comments = false);
static cell AMX_NATIVE_CALL amxx_json_parse(AMX *amx, cell *params)
{
	int len;
	auto string = MF_GetAmxString(amx, params[1], 0, &len);
	auto is_file = params[2] != 0;

	if (is_file)
	{
		char path[256];
		string = MF_BuildPathnameR(path, sizeof(path), "%s", string);
	}

	JS_Handle handle;
	auto result = JsonMngr->Parse(string, &handle, is_file, params[3] != 0);

	return (result) ? handle : -1;
}

//native bool:json_equals(const JSON:value1, const JSON:value2);
static cell AMX_NATIVE_CALL amxx_json_equals(AMX *amx, cell *params)
{
	auto value1 = params[1], value2 = params[2];
	//For check against Invalid_JSON
	if (value1 == -1 || value2 == -1)
	{
		return value1 == value2;
	}

	if (!JsonMngr->IsValidHandle(value1))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value1);
		return 0;
	}

	if (!JsonMngr->IsValidHandle(value2))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value2);
		return 0;
	}

	return JsonMngr->AreValuesEquals(value1, value2);
}

//native bool:json_validate(const JSON:schema, const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_validate(AMX *amx, cell *params)
{
	auto schema = params[1], value = params[2];
	if (!JsonMngr->IsValidHandle(schema))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON schema! %d", schema);
		return 0;
	}

	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	return JsonMngr->IsValueValid(schema, value);
}

//native JSON:json_get_parent(const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_get_parent(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return -1;
	}

	JS_Handle parent;
	auto result = JsonMngr->GetValueParent(value, &parent);

	return (result) ? parent : -1;
}

//native JSONType:json_get_type(const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_get_type(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return static_cast<cell>(JSONTypeError);
	}

	return JsonMngr->GetHandleJSONType(value);
}

//native JSON:json_init_object();
static cell AMX_NATIVE_CALL amxx_json_init_object(AMX *amx, cell *params)
{
	JS_Handle handle;
	auto result = JsonMngr->InitObject(&handle);

	return (result) ? handle : -1;
}

//native JSON:json_init_array();
static cell AMX_NATIVE_CALL amxx_json_init_array(AMX *amx, cell *params)
{
	JS_Handle handle;
	auto result = JsonMngr->InitArray(&handle);

	return (result) ? handle : -1;
}

//native JSON:json_init_string(const value[]);
static cell AMX_NATIVE_CALL amxx_json_init_string(AMX *amx, cell *params)
{
	int len;
	JS_Handle handle;
	auto result = JsonMngr->InitString(MF_GetAmxString(amx, params[1], 0, &len), &handle);

	return (result) ? handle : -1;
}

//native JSON:json_init_number(value);
static cell AMX_NATIVE_CALL amxx_json_init_number(AMX *amx, cell *params)
{
	JS_Handle handle;
	auto result = JsonMngr->InitNum(params[1], &handle);

	return (result) ? handle : -1;
}

//native JSON:json_init_real(Float:value);
static cell AMX_NATIVE_CALL amxx_json_init_real(AMX *amx, cell *params)
{
	JS_Handle handle;
	auto result = JsonMngr->InitNum(amx_ctof(params[1]), &handle);

	return (result) ? handle : -1;
}

//native JSON:json_init_bool(bool:value);
static cell AMX_NATIVE_CALL amxx_json_init_bool(AMX *amx, cell *params)
{
	JS_Handle handle;
	auto result = JsonMngr->InitBool(params[1] != 0, &handle);

	return (result) ? handle : -1;
}

//native JSON:json_init_null();
static cell AMX_NATIVE_CALL amxx_json_init_null(AMX *amx, cell *params)
{
	JS_Handle handle;
	auto result = JsonMngr->InitNull(&handle);

	return (result) ? handle : -1;
}

//native JSON:json_deep_copy(const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_deep_copy(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return -1;
	}

	JS_Handle clonedHandle;
	auto result = JsonMngr->DeepCopyValue(value, &clonedHandle);

	return (result) ? clonedHandle : -1;
}

//native bool:json_free(&JSON:value);
static cell AMX_NATIVE_CALL amxx_json_free(AMX *amx, cell *params)
{
	auto value = MF_GetAmxAddr(amx, params[1]);
	if (!JsonMngr->IsValidHandle(*value))
	{
		return 0;
	}

	JsonMngr->Free(*value);

	*value = -1;

	return 1;
}

//native json_get_string(const JSON:value, buffer[], maxlen);
static cell AMX_NATIVE_CALL amxx_json_get_string(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	auto string = JsonMngr->ValueToString(value);

	return MF_SetAmxStringUTF8Char(amx, params[2], string, strlen(string), params[3]);
}

//native json_get_number(const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_get_number(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	return static_cast<cell>(JsonMngr->ValueToNum(value));
}

//native Float:json_get_real(const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_get_real(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	auto result = static_cast<float>(JsonMngr->ValueToNum(value));

	return amx_ftoc(result);
}

//native bool:json_get_bool(const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_get_bool(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	return JsonMngr->ValueToBool(value);
}

//native JSON:json_array_get_value(const JSON:array, index);
static cell AMX_NATIVE_CALL amxx_json_array_get_value(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return -1;
	}

	JS_Handle handle;
	auto result = JsonMngr->ArrayGetValue(array, params[2], &handle);

	return (result) ? handle : -1;
}

//native json_array_get_string(const JSON:array, index, buffer[], maxlen);
static cell AMX_NATIVE_CALL amxx_json_array_get_string(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	auto string = JsonMngr->ArrayGetString(array, params[2]);

	return MF_SetAmxStringUTF8Char(amx, params[3], string, strlen(string), params[4]);
}

//native json_array_get_number(const JSON:array, index);
static cell AMX_NATIVE_CALL amxx_json_array_get_number(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return static_cast<cell>(JsonMngr->ArrayGetNum(array, params[2]));
}

//native Float:json_array_get_real(const JSON:array, index);
static cell AMX_NATIVE_CALL amxx_json_array_get_real(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	auto result = static_cast<float>(JsonMngr->ArrayGetNum(array, params[2]));

	return amx_ftoc(result);
}

//native bool:json_array_get_bool(const JSON:array, index);
static cell AMX_NATIVE_CALL amxx_json_array_get_bool(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayGetBool(array, params[2]);
}

//native json_array_get_count(const JSON:array);
static cell AMX_NATIVE_CALL amxx_json_array_get_count(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayGetCount(array);
}

//native bool:json_array_replace_value(JSON:array, index, const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_array_replace_value(AMX *amx, cell *params)
{
	auto array = params[1], value = params[3];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	return JsonMngr->ArrayReplaceValue(array, params[2], value);
}

//native bool:json_array_replace_string(JSON:array, index, const string[]);
static cell AMX_NATIVE_CALL amxx_json_array_replace_string(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	int len;
	auto string = MF_GetAmxString(amx, params[3], 0, &len);

	return JsonMngr->ArrayReplaceString(array, params[2], string);
}

//native bool:json_array_replace_number(JSON:array, index, number);
static cell AMX_NATIVE_CALL amxx_json_array_replace_number(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayReplaceNum(array, params[2], params[3]);
}

//native bool:json_array_replace_real(JSON:array, index, Float:number);
static cell AMX_NATIVE_CALL amxx_json_array_replace_real(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayReplaceNum(array, params[2], amx_ctof(params[3]));
}

//native bool:json_array_replace_bool(JSON:array, index, bool:boolean);
static cell AMX_NATIVE_CALL amxx_json_array_replace_bool(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayReplaceBool(array, params[2], params[3] != 0);
}

//native bool:json_array_replace_null(JSON:array, index);
static cell AMX_NATIVE_CALL amxx_json_array_replace_null(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayReplaceNull(array, params[2]);
}

//native bool:json_array_append_value(JSON:array, const JSON:value);
static cell AMX_NATIVE_CALL amxx_json_array_append_value(AMX *amx, cell *params)
{
	auto array = params[1], value = params[2];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	return JsonMngr->ArrayAppendValue(array, value);
}

//native bool:json_array_append_string(JSON:array, const string[]);
static cell AMX_NATIVE_CALL amxx_json_array_append_string(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	int len;
	return JsonMngr->ArrayAppendString(array, MF_GetAmxString(amx, params[2], 0, &len));
}

//native bool:json_array_append_number(JSON:array, number);
static cell AMX_NATIVE_CALL amxx_json_array_append_number(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayAppendNum(array, params[2]);
}

//native bool:json_array_append_real(JSON:array, Float:number);
static cell AMX_NATIVE_CALL amxx_json_array_append_real(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayAppendNum(array, amx_ctof(params[2]));
}

//native bool:json_array_append_bool(JSON:array, bool:boolean);
static cell AMX_NATIVE_CALL amxx_json_array_append_bool(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayAppendBool(array, params[2] != 0);
}

//native bool:json_array_append_null(JSON:array);
static cell AMX_NATIVE_CALL amxx_json_array_append_null(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayAppendNull(array);
}

//native bool:json_array_remove(JSON:array, index);
static cell AMX_NATIVE_CALL amxx_json_array_remove(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayRemove(array, params[2]);
}

//native bool:json_array_clear(JSON:array);
static cell AMX_NATIVE_CALL amxx_json_array_clear(AMX *amx, cell *params)
{
	auto array = params[1];
	if (!JsonMngr->IsValidHandle(array, Handle_Array))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON array! %d", array);
		return 0;
	}

	return JsonMngr->ArrayClear(array);
}

//native JSON:json_object_get_value(const JSON:object, const name[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_get_value(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return -1;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	JS_Handle handle;
	auto result = JsonMngr->ObjectGetValue(object, name, &handle, params[3] != 0);

	return (result) ? handle : -1;
}

//native json_object_get_string(const JSON:object, const name[], buffer[], maxlen, bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_get_string(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);
	auto string = JsonMngr->ObjectGetString(object, name, params[5] != 0);

	return MF_SetAmxStringUTF8Char(amx, params[3], string, strlen(string), params[4]);
}

//native json_object_get_number(const JSON:object, const name[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_get_number(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return static_cast<cell>(JsonMngr->ObjectGetNum(object, name, params[3] != 0));
}

//native Float:json_object_get_real(const JSON:object, const name[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_get_real(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);
	auto result = static_cast<float>(JsonMngr->ObjectGetNum(object, name, params[3] != 0));

	return amx_ftoc(result);
}

//native bool:json_object_get_bool(const JSON:object, const name[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_get_bool(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectGetBool(object, name, params[3] != 0);
}

//native json_object_get_count(const JSON:object);
static cell AMX_NATIVE_CALL amxx_json_object_get_count(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	return JsonMngr->ObjectGetCount(object);
}

//native json_object_get_name(const JSON:object, index, buffer[], maxlen);
static cell AMX_NATIVE_CALL amxx_json_object_get_name(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	auto name = JsonMngr->ObjectGetName(object, params[2]);

	return MF_SetAmxStringUTF8Char(amx, params[3], name, strlen(name), params[4]);
}

//native JSON:amxx_json_object_get_value_at(const JSON:object, index);
static cell AMX_NATIVE_CALL amxx_json_object_get_value_at(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return -1;
	}

	JS_Handle valueHandle;
	auto result = JsonMngr->ObjectGetValueAt(object, params[2], &valueHandle);

	return (result) ? valueHandle : -1;
}

//native bool:json_object_has_value(const JSON:object, const name[], JSONType:type = JSONError, bool:dot_not = false);
static cell AMX_NATIVE_CALL amxx_json_object_has_value(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectHasValue(object, name, static_cast<JSONType>(params[3]), params[4] != 0);
}

//native bool:json_object_set_value(JSON:object, const name[], JSON:value, bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_set_value(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	if (!JsonMngr->IsValidHandle(params[3]))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", params[3]);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectSetValue(object, name, params[3], params[4] != 0);
}

//native bool:json_object_set_string(JSON:object, const name[], const string[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_set_string(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);
	auto string = MF_GetAmxString(amx, params[3], 1, &len);

	return JsonMngr->ObjectSetString(object, name, string, params[4] != 0);
}

//native bool:json_object_set_number(JSON:object, const name[], number, bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_set_number(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectSetNum(object, name, params[3], params[4] != 0);
}

//native bool:json_object_set_real(JSON:object, const name[], Float:number, bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_set_real(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectSetNum(object, name, amx_ctof(params[3]), params[4] != 0);
}

//native bool:json_object_set_bool(JSON:object, const name[], bool:boolean, bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_set_bool(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectSetBool(object, name, params[3] != 0, params[4] != 0);
}

//native bool:json_object_set_null(JSON:object, const name[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_set_null(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectSetNull(object, name, params[3] != 0);
}

//native bool:json_object_remove(JSON:object, const name[], bool:dotfunc = false);
static cell AMX_NATIVE_CALL amxx_json_object_remove(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	int len;
	auto name = MF_GetAmxString(amx, params[2], 0, &len);

	return JsonMngr->ObjectRemove(object, name, params[3] != 0);
}

//native bool:json_object_clear(JSON:object);
static cell AMX_NATIVE_CALL amxx_json_object_clear(AMX *amx, cell *params)
{
	auto object = params[1];
	if (!JsonMngr->IsValidHandle(object, Handle_Object))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON object! %d", object);
		return 0;
	}

	return JsonMngr->ObjectClear(object);
}

//native json_serial_size(const JSON:value, bool:pretty = false, bool:with_null_byte = false);
static cell AMX_NATIVE_CALL amxx_json_serial_size(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	auto size = JsonMngr->SerialSize(value, params[2] != 0);

	return (params[3]) ? size : size - 1;
}

//native json_serial_to_string(const JSON:value, buffer[], maxlen, bool:pretty = false);
static cell AMX_NATIVE_CALL amxx_json_serial_to_string(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	auto result = JsonMngr->SerialToString(value, params[4] != 0);
	auto written = (result) ? MF_SetAmxStringUTF8Char(amx, params[2], result, strlen(result), params[3]) : 0;

	JsonMngr->FreeString(result);

	return written;
}

//native bool:json_serial_to_file(const JSON:value, const file[], bool:pretty = false);
static cell AMX_NATIVE_CALL amxx_json_serial_to_file(AMX *amx, cell *params)
{
	auto value = params[1];
	if (!JsonMngr->IsValidHandle(value))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid JSON value! %d", value);
		return 0;
	}

	int len;
	auto file = MF_GetAmxString(amx, params[2], 0, &len);

	char path[256];
	MF_BuildPathnameR(path, sizeof(path), "%s", file);

	return JsonMngr->SerialToFile(value, path, params[3] != 0);
}

AMX_NATIVE_INFO JsonNatives[] =
{
	{ "json_parse",                     amxx_json_parse },
	{ "json_equals",                    amxx_json_equals },
	{ "json_validate",                  amxx_json_validate },
	{ "json_get_parent",                amxx_json_get_parent },
	{ "json_get_type",                  amxx_json_get_type },
	{ "json_init_object",               amxx_json_init_object },
	{ "json_init_array",                amxx_json_init_array },
	{ "json_init_string",               amxx_json_init_string },
	{ "json_init_number",               amxx_json_init_number },
	{ "json_init_real",                 amxx_json_init_real },
	{ "json_init_bool",                 amxx_json_init_bool },
	{ "json_init_null",                 amxx_json_init_null },
	{ "json_deep_copy",                 amxx_json_deep_copy },
	{ "json_free",                      amxx_json_free },
	{ "json_get_string",                amxx_json_get_string },
	{ "json_get_number",                amxx_json_get_number },
	{ "json_get_real",                  amxx_json_get_real },
	{ "json_get_bool",                  amxx_json_get_bool },
	{ "json_array_get_value",           amxx_json_array_get_value },
	{ "json_array_get_string",          amxx_json_array_get_string },
	{ "json_array_get_count",           amxx_json_array_get_count },
	{ "json_array_get_number",          amxx_json_array_get_number },
	{ "json_array_get_real",            amxx_json_array_get_real },
	{ "json_array_get_bool",            amxx_json_array_get_bool },
	{ "json_array_replace_value",       amxx_json_array_replace_value },
	{ "json_array_replace_string",      amxx_json_array_replace_string },
	{ "json_array_replace_number",      amxx_json_array_replace_number },
	{ "json_array_replace_real",        amxx_json_array_replace_real },
	{ "json_array_replace_bool",        amxx_json_array_replace_bool },
	{ "json_array_replace_null",        amxx_json_array_replace_null },
	{ "json_array_append_value",        amxx_json_array_append_value },
	{ "json_array_append_string",       amxx_json_array_append_string },
	{ "json_array_append_number",       amxx_json_array_append_number },
	{ "json_array_append_real",         amxx_json_array_append_real },
	{ "json_array_append_bool",         amxx_json_array_append_bool },
	{ "json_array_append_null",         amxx_json_array_append_null },
	{ "json_array_remove",              amxx_json_array_remove },
	{ "json_array_clear",               amxx_json_array_clear },
	{ "json_object_get_value",          amxx_json_object_get_value },
	{ "json_object_get_string",         amxx_json_object_get_string },
	{ "json_object_get_number",         amxx_json_object_get_number },
	{ "json_object_get_real",           amxx_json_object_get_real },
	{ "json_object_get_bool",           amxx_json_object_get_bool },
	{ "json_object_get_count",          amxx_json_object_get_count },
	{ "json_object_get_name",           amxx_json_object_get_name },
	{ "json_object_get_value_at",       amxx_json_object_get_value_at },
	{ "json_object_has_value",          amxx_json_object_has_value },
	{ "json_object_set_value",          amxx_json_object_set_value },
	{ "json_object_set_string",         amxx_json_object_set_string },
	{ "json_object_set_number",         amxx_json_object_set_number },
	{ "json_object_set_real",           amxx_json_object_set_real },
	{ "json_object_set_bool",           amxx_json_object_set_bool },
	{ "json_object_set_null",           amxx_json_object_set_null },
	{ "json_object_remove",             amxx_json_object_remove },
	{ "json_object_clear",              amxx_json_object_clear },
	{ "json_serial_size",               amxx_json_serial_size },
	{ "json_serial_to_string",          amxx_json_serial_to_string },
	{ "json_serial_to_file",            amxx_json_serial_to_file },
	{ nullptr,                          nullptr }
};

void OnAmxxAttach()
{
	JsonMngr = ke::MakeUnique<JSONMngr>();

	MF_AddNatives(JsonNatives);
	//MF_AddInterface(JsonMngr.get());
}
