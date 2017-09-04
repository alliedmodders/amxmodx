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

#include <parson.h>
#include <amtl/am-vector.h>
#include <amtl/am-autoptr.h>
#include <amtl/am-uniqueptr.h>
#include <amtl/am-deque.h>
#include "amxxmodule.h"
#include "IJsonMngr.h"

using namespace AMXX;

class JSONMngr : public IJSONMngr
{
	public:

	JSONMngr() = default;
	virtual ~JSONMngr();

	// Handles
	virtual bool IsValidHandle(JS_Handle id, JSONHandleType type = Handle_Value);
	virtual void Free(JS_Handle id);
	virtual inline JSONType GetHandleJSONType(JS_Handle value)
	{
		return static_cast<JSONType>(json_value_get_type(m_Handles[value]->m_pValue));
	}

	// Parsing
	virtual bool Parse(const char *string, JS_Handle *handle, bool is_file, bool with_comments);

	// Comapring
	virtual inline bool AreValuesEquals(JS_Handle value1, JS_Handle value2)
	{
		// to avoid ms compiler warning
		return json_value_equals(m_Handles[value1]->m_pValue, m_Handles[value2]->m_pValue) == 1;
	}

	// Validating
	virtual inline bool IsValueValid(JS_Handle schema, JS_Handle value)
	{
		return json_validate(m_Handles[schema]->m_pValue, m_Handles[value]->m_pValue) == JSONSuccess;
	}

	// Accessing parent value
	virtual bool GetValueParent(JS_Handle value, JS_Handle *parent);

	// Init functions
	virtual bool InitObject(JS_Handle *handle);
	virtual bool InitArray(JS_Handle *handle);
	virtual bool InitString(const char *string, JS_Handle *handle);
	virtual bool InitNum(double number, JS_Handle *handle);
	virtual bool InitBool(bool boolean, JS_Handle *handle);
	virtual bool InitNull(JS_Handle *handle);

	// Copying
	virtual bool DeepCopyValue(JS_Handle value, JS_Handle *handle);

	// Convert functions
	virtual const char *ValueToString(JS_Handle value);
	virtual inline double ValueToNum(JS_Handle value)
	{
		return json_value_get_number(m_Handles[value]->m_pValue);
	}
	virtual inline bool ValueToBool(JS_Handle value)
	{
		return json_value_get_boolean(m_Handles[value]->m_pValue) == 1;
	}

	// Wrappers for Array API
	virtual bool ArrayGetValue(JS_Handle array, size_t index, JS_Handle *handle);
	virtual const char *ArrayGetString(JS_Handle array, size_t index);
	virtual inline bool ArrayGetBool(JS_Handle array, size_t index)
	{
		return json_array_get_boolean(m_Handles[array]->m_pArray, index) == 1;
	}
	virtual bool ArrayReplaceValue(JS_Handle array, size_t index, JS_Handle value);
	virtual bool ArrayAppendValue(JS_Handle array, JS_Handle value);

	virtual inline double ArrayGetNum(JS_Handle array, size_t index)
	{
		return json_array_get_number(m_Handles[array]->m_pArray, index);
	}
	virtual inline size_t ArrayGetCount(JS_Handle array)
	{
		return json_array_get_count(m_Handles[array]->m_pArray);
	}
	virtual inline bool ArrayReplaceString(JS_Handle array, size_t index, const char *string)
	{
		return json_array_replace_string(m_Handles[array]->m_pArray, index, string) == JSONSuccess;
	}
	virtual inline bool ArrayReplaceNum(JS_Handle array, size_t index, double number)
	{
		return json_array_replace_number(m_Handles[array]->m_pArray, index, number) == JSONSuccess;
	}
	virtual inline bool ArrayReplaceBool(JS_Handle array, size_t index, bool boolean)
	{
		return json_array_replace_boolean(m_Handles[array]->m_pArray, index, boolean) == JSONSuccess;
	}
	virtual inline bool ArrayReplaceNull(JS_Handle array, size_t index)
	{
		return json_array_replace_null(m_Handles[array]->m_pArray, index) == JSONSuccess;
	}
	virtual inline bool ArrayAppendString(JS_Handle array, const char *string)
	{
		return json_array_append_string(m_Handles[array]->m_pArray, string) == JSONSuccess;
	}
	virtual inline bool ArrayAppendNum(JS_Handle array, double number)
	{
		return json_array_append_number(m_Handles[array]->m_pArray, number) == JSONSuccess;
	}
	virtual inline bool ArrayAppendBool(JS_Handle array, bool boolean)
	{
		return json_array_append_boolean(m_Handles[array]->m_pArray, boolean) == JSONSuccess;
	}
	virtual inline bool ArrayAppendNull(JS_Handle array)
	{
		return json_array_append_null(m_Handles[array]->m_pArray) == JSONSuccess;
	}
	virtual inline bool ArrayRemove(JS_Handle array, size_t index)
	{
		return json_array_remove(m_Handles[array]->m_pArray, index) == JSONSuccess;
	}
	virtual inline bool ArrayClear(JS_Handle array)
	{
		return json_array_clear(m_Handles[array]->m_pArray) == JSONSuccess;
	}

	// Wrappers for Object API

	// Get functions
	virtual bool ObjectGetValue(JS_Handle object, const char *name, JS_Handle *handle, bool dot_not);
	virtual const char *ObjectGetString(JS_Handle object, const char *name, bool dot_not);
	virtual double ObjectGetNum(JS_Handle object, const char *name, bool dot_not);
	virtual bool ObjectGetBool(JS_Handle object, const char *name, bool dot_not);
	virtual inline size_t ObjectGetCount(JS_Handle object)
	{
		return json_object_get_count(m_Handles[object]->m_pObject);
	}
	virtual const char *ObjectGetName(JS_Handle object, size_t index);
	virtual bool ObjectGetValueAt(JS_Handle object, size_t index, JS_Handle *handle);
	virtual bool ObjectHasValue(JS_Handle object, const char *name, JSONType type, bool dot_not);

	// Set functions
	virtual bool ObjectSetValue(JS_Handle object, const char *name, JS_Handle value, bool dot_not);
	virtual bool ObjectSetString(JS_Handle object, const char *name, const char *string, bool dot_not);
	virtual bool ObjectSetNum(JS_Handle object, const char *name, double number, bool dot_not);
	virtual bool ObjectSetBool(JS_Handle object, const char *name, bool boolean, bool dot_not);
	virtual bool ObjectSetNull(JS_Handle object, const char *name, bool dot_not);

	// Remove functions
	virtual bool ObjectRemove(JS_Handle object, const char *name, bool dot_not);
	virtual inline bool ObjectClear(JS_Handle object)
	{
		return json_object_clear(m_Handles[object]->m_pObject) == JSONSuccess;
	}

	// Serialization API
	virtual size_t SerialSize(JS_Handle value, bool pretty);
	virtual bool SerialToBuffer(JS_Handle value, char *buffer, size_t size, bool pretty);
	virtual bool SerialToFile(JS_Handle value, const char *filepath, bool pretty);
	virtual char *SerialToString(JS_Handle value, bool pretty);
	virtual inline void FreeString(char *string)
	{
		json_free_serialized_string(string);
	}

	private:

	struct JSONHandle
	{
		JSON_Value  *m_pValue;          //Store an pointer to a value
		JSON_Array  *m_pArray;          //Store an pointer to an array
		JSON_Object *m_pObject;         //Store an pointer to an object
		bool         m_bMustBeFreed;    //Must be freed using json_value_free()?
	};

	JS_Handle _MakeHandle(void *value, JSONHandleType type, bool must_be_freed = false);
	void _FreeHandle(ke::AutoPtr<JSONHandle> &ptr);

	ke::Vector<ke::AutoPtr<JSONHandle>> m_Handles;
	ke::Deque<JS_Handle> m_OldHandles;
};

extern ke::UniquePtr<JSONMngr> JsonMngr;
