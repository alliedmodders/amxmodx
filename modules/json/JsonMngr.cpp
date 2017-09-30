// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// JSON Class
//

#include "JsonMngr.h"

JSONMngr::~JSONMngr()
{
	for (auto &i : m_Handles)
	{
		if (i)
		{
			_FreeHandle(i);
		}
	}
}

JS_Handle JSONMngr::_MakeHandle(void *value, JSONHandleType type, bool must_be_freed)
{
	JS_Handle id;

	if (!m_OldHandles.empty())
	{
		id = m_OldHandles.popFrontCopy();
		m_Handles[id] = ke::AutoPtr<JSONHandle>(new JSONHandle);
	}
	else
	{
		m_Handles.append(ke::AutoPtr<JSONHandle>(new JSONHandle));
		id = m_Handles.length() - 1;
	}

	switch (type)
	{
		case Handle_Value:
		{
			auto getHandleType = [this](JSON_Value *jsvalue, JS_Handle id)
			{
				if (!(m_Handles[id]->m_pArray = json_value_get_array(jsvalue)))
				{
					m_Handles[id]->m_pObject = json_value_get_object(jsvalue);
				}
			};

			auto JSValue = m_Handles[id]->m_pValue = static_cast<JSON_Value *>(value);
			getHandleType(JSValue, id);
			break;
		}
		case Handle_Array:
		{
			auto JSArray = m_Handles[id]->m_pArray = static_cast<JSON_Array *>(value);
			m_Handles[id]->m_pValue = json_array_get_wrapping_value(JSArray);
			break;
		}
		case Handle_Object:
		{
			auto JSObject = m_Handles[id]->m_pObject = static_cast<JSON_Object *>(value);
			m_Handles[id]->m_pValue = json_object_get_wrapping_value(JSObject);
			break;
		}
	}

	m_Handles[id]->m_bMustBeFreed = must_be_freed;

	return id;
}

void JSONMngr::_FreeHandle(ke::AutoPtr<JSONHandle> &ptr)
{
	if (ptr->m_bMustBeFreed && ptr->m_pValue)
	{
		json_value_free(ptr->m_pValue);
	}
}

void JSONMngr::Free(JS_Handle id)
{
	auto handle = ke::Move(m_Handles[id]);

	if (!handle)
	{
		return;
	}

	_FreeHandle(handle);
	m_OldHandles.append(id);
}

bool JSONMngr::IsValidHandle(JS_Handle handle, JSONHandleType type)
{
	if (handle >= m_Handles.length() || !m_Handles[handle])
	{
		return false;
	}

	switch (type)
	{
		case Handle_Array: return m_Handles[handle]->m_pArray != nullptr;
		case Handle_Object: return m_Handles[handle]->m_pObject != nullptr;
		default: return true;
	}
}

bool JSONMngr::GetValueParent(JS_Handle value, JS_Handle *parent)
{
	auto JSParent = json_value_get_parent(m_Handles[value]->m_pValue);
	if (!JSParent)
	{
		return false;
	}

	if (parent)
	{
		*parent = _MakeHandle(JSParent, Handle_Value);
	}

	return true;
}

bool JSONMngr::InitObject(JS_Handle *handle)
{
	auto JSObject = json_value_get_object(json_value_init_object());
	if (!JSObject)
	{
		return false;
	}

	*handle = _MakeHandle(JSObject, Handle_Object, true);
	return true;
}

bool JSONMngr::InitArray(JS_Handle *handle)
{
	auto JSArray = json_value_get_array(json_value_init_array());
	if (!JSArray)
	{
		return false;
	}

 	*handle = _MakeHandle(JSArray, Handle_Array, true);
	return true;
}

bool JSONMngr::InitString(const char *string, JS_Handle *handle)
{
	auto JSValue = json_value_init_string(string);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value, true);
	return true;
}

bool JSONMngr::InitNum(double number, JS_Handle *handle)
{
	auto JSValue = json_value_init_number(number);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value, true);
	return true;
}

bool JSONMngr::InitBool(bool boolean, JS_Handle *handle)
{
	auto JSValue = json_value_init_boolean(boolean);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value, true);
	return true;
}

bool JSONMngr::InitNull(JS_Handle *handle)
{
	auto JSValue = json_value_init_null();
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value, true);
	return true;
}

bool JSONMngr::Parse(const char *string, JS_Handle *handle, bool is_file, bool with_comments)
{
	auto jsonFunc = (!with_comments) ? json_parse_string : json_parse_string_with_comments;
	if (is_file)
	{
		jsonFunc = (!with_comments) ? json_parse_file : json_parse_file_with_comments;
	}

	auto JSValue = jsonFunc(string);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value, true);
	return true;
}

bool JSONMngr::DeepCopyValue(JS_Handle value, JS_Handle *handle)
{
	auto JSValue = json_value_deep_copy(m_Handles[value]->m_pValue);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value, true);
	return true;
}

const char *JSONMngr::ValueToString(JS_Handle value)
{
	auto string = json_value_get_string(m_Handles[value]->m_pValue);
	return (string) ? string : "";
}

bool JSONMngr::ArrayGetValue(JS_Handle array, size_t index, JS_Handle *handle)
{
	auto JSValue = json_array_get_value(m_Handles[array]->m_pArray, index);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value);
	return true;
}

const char *JSONMngr::ArrayGetString(JS_Handle array, size_t index)
{
	auto string = json_array_get_string(m_Handles[array]->m_pArray, index);
	return (string) ? string : "";
}

bool JSONMngr::ArrayReplaceValue(JS_Handle array, size_t index, JS_Handle value)
{
	auto JSValue = m_Handles[value]->m_pValue;

	//We cannot assign the same value to the different arrays or objects
	//So if value is already assigned somewhere else let's create a copy of it
	if (json_value_get_parent(JSValue))
	{
		JSValue = json_value_deep_copy(JSValue);
	}
	else
	{
		//Parson will take care of freeing child values
		m_Handles[value]->m_bMustBeFreed = false;
	}
	return json_array_replace_value(m_Handles[array]->m_pArray, index, JSValue) == JSONSuccess;
}

bool JSONMngr::ArrayAppendValue(JS_Handle array, JS_Handle value)
{
	auto JSValue = m_Handles[value]->m_pValue;

	//We cannot assign the same value to the different arrays or objects
	//So if value is already assigned somewhere else let's create a copy of it
	if (json_value_get_parent(JSValue))
	{
		JSValue = json_value_deep_copy(JSValue);
	}
	else
	{
		//Parson will take care of freeing child values
		m_Handles[value]->m_bMustBeFreed = false;
	}
	return json_array_append_value(m_Handles[array]->m_pArray, JSValue) == JSONSuccess;
}

bool JSONMngr::ObjectGetValue(JS_Handle object, const char *name, JS_Handle *handle, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto JSValue = (!dotfunc) ? json_object_get_value(JSObject, name) :
					json_object_dotget_value(JSObject, name);

	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value);
	return true;
}

const char *JSONMngr::ObjectGetString(JS_Handle object, const char *name, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto string = (!dotfunc) ? json_object_get_string(JSObject, name) :
					json_object_dotget_string(JSObject, name);

	return (string) ? string : "";
}

double JSONMngr::ObjectGetNum(JS_Handle object, const char *name, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	return (!dotfunc) ? json_object_get_number(JSObject, name) :
					json_object_dotget_number(JSObject, name);
}

bool JSONMngr::ObjectGetBool(JS_Handle object, const char *name, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto result = (!dotfunc) ? json_object_get_boolean(JSObject, name) :
					json_object_dotget_boolean(JSObject, name);

	return result == 1;
}

const char *JSONMngr::ObjectGetName(JS_Handle object, size_t index)
{
	auto string = json_object_get_name(m_Handles[object]->m_pObject, index);
	return (string) ? string : "";
}

bool JSONMngr::ObjectGetValueAt(JS_Handle object, size_t index, JS_Handle *handle)
{
	auto JSValue = json_object_get_value_at(m_Handles[object]->m_pObject, index);
	if (!JSValue)
	{
		return false;
	}

	*handle = _MakeHandle(JSValue, Handle_Value);
	return true;
}

bool JSONMngr::ObjectHasValue(JS_Handle object, const char *name, JSONType type, bool dotfunc)
{
	int result;
	auto JSObject = m_Handles[object]->m_pObject;

	if (type == JSONTypeError)
	{
		result = (!dotfunc) ? json_object_has_value(JSObject, name) :
					json_object_dothas_value(JSObject, name);
	}
	else
	{
		result = (!dotfunc) ? json_object_has_value_of_type(JSObject, name, type) :
					json_object_dothas_value_of_type(JSObject, name, type);
	}
	return result == 1;
}

bool JSONMngr::ObjectSetValue(JS_Handle object, const char *name, JS_Handle value, bool dotfunc)
{
	auto JSValue = m_Handles[value]->m_pValue;

	//We cannot assign the same value to the different arrays or objects
	//So if value is already assigned somewhere else let's create a copy of it
	if (json_value_get_parent(JSValue))
	{
		JSValue = json_value_deep_copy(JSValue);
	}
	else
	{
		//Parson will take care of freeing child values
		m_Handles[value]->m_bMustBeFreed = false;
	}

	auto JSObject = m_Handles[object]->m_pObject;
	auto JSResult = (!dotfunc) ? json_object_set_value(JSObject, name, JSValue) :
					json_object_dotset_value(JSObject, name, JSValue);

	return JSResult == JSONSuccess;
}

bool JSONMngr::ObjectSetString(JS_Handle object, const char *name, const char *string, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto JSResult = (!dotfunc) ? json_object_set_string(JSObject, name, string) :
					json_object_dotset_string(JSObject, name, string);

	return JSResult == JSONSuccess;
}

bool JSONMngr::ObjectSetNum(JS_Handle object, const char *name, double number, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto JSResult = (!dotfunc) ? json_object_set_number(JSObject, name, number) :
					json_object_dotset_number(JSObject, name, number);

	return JSResult == JSONSuccess;
}

bool JSONMngr::ObjectSetBool(JS_Handle object, const char *name, bool boolean, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto JSResult = (!dotfunc) ? json_object_set_boolean(JSObject, name, boolean) :
					json_object_dotset_boolean(JSObject, name, boolean);

	return JSResult == JSONSuccess;
}

bool JSONMngr::ObjectSetNull(JS_Handle object, const char *name, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto JSResult = (!dotfunc) ? json_object_set_null(JSObject, name) :
					json_object_dotset_null(JSObject, name);

	return JSResult == JSONSuccess;
}

bool JSONMngr::ObjectRemove(JS_Handle object, const char *name, bool dotfunc)
{
	auto JSObject = m_Handles[object]->m_pObject;
	auto JSResult = (!dotfunc) ? json_object_remove(JSObject, name) :
					json_object_dotremove(JSObject, name);

	return JSResult == JSONSuccess;
}

size_t JSONMngr::SerialSize(JS_Handle value, bool pretty)
{
	auto JSValue = m_Handles[value]->m_pValue;
	return (!pretty) ? json_serialization_size(JSValue) :
					json_serialization_size_pretty(JSValue);
}

bool JSONMngr::SerialToBuffer(JS_Handle value, char *buffer, size_t size, bool pretty)
{
	auto JSValue = m_Handles[value]->m_pValue;
	auto JSResult = (!pretty) ? json_serialize_to_buffer(JSValue, buffer, size) :
					json_serialize_to_buffer_pretty(JSValue, buffer, size);

	return JSResult == JSONSuccess;
}

bool JSONMngr::SerialToFile(JS_Handle value, const char *filepath, bool pretty)
{
	auto JSValue = m_Handles[value]->m_pValue;
	auto JSResult = (!pretty) ? json_serialize_to_file(JSValue, filepath) :
					json_serialize_to_file_pretty(JSValue, filepath);

	return JSResult == JSONSuccess;
}

char *JSONMngr::SerialToString(JS_Handle value, bool pretty)
{
	auto JSValue = m_Handles[value]->m_pValue;
	auto result = (!pretty) ? json_serialize_to_string(JSValue) :
					json_serialize_to_string_pretty(JSValue);

	return (result) ? result : nullptr;
}
