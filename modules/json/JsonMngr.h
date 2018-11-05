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

#include <amxxmodule.h>
#include <parson.h>
#include <amtl/am-vector.h>
#include <amtl/am-autoptr.h>
#include <amtl/am-uniqueptr.h>
#include <amtl/am-deque.h>

#include "IJsonMngr.h"

using namespace AMXX;

class JSONMngr : public IJSONMngr
{
	public:

	JSONMngr() = default;
	virtual ~JSONMngr();

	// Handles
	bool IsValidHandle(JS_Handle id, JSONHandleType type = Handle_Value) override;
	void Free(JS_Handle id) override;
	inline JSONType GetHandleJSONType(JS_Handle value) override
	{
		return static_cast<JSONType>(json_value_get_type(m_Handles[value]->m_pValue));
	}

	// Parsing
	bool Parse(const char *string, JS_Handle *handle, bool is_file, bool with_comments) override;

	// Comapring
	inline bool AreValuesEquals(JS_Handle value1, JS_Handle value2) override
	{
		// to avoid ms compiler warning
		return json_value_equals(m_Handles[value1]->m_pValue, m_Handles[value2]->m_pValue) == 1;
	}

	// Validating
	inline bool IsValueValid(JS_Handle schema, JS_Handle value) override
	{
		return json_validate(m_Handles[schema]->m_pValue, m_Handles[value]->m_pValue) == JSONSuccess;
	}

	// Accessing parent value
	bool GetValueParent(JS_Handle value, JS_Handle *parent) override;

	// Init functions
	bool InitObject(JS_Handle *handle) override;
	bool InitArray(JS_Handle *handle) override;
	bool InitString(const char *string, JS_Handle *handle) override;
	bool InitNum(double number, JS_Handle *handle) override;
	bool InitBool(bool boolean, JS_Handle *handle) override;
	bool InitNull(JS_Handle *handle) override;

	// Copying
	bool DeepCopyValue(JS_Handle value, JS_Handle *handle) override;

	// Convert functions
	const char *ValueToString(JS_Handle value) override;
	inline double ValueToNum(JS_Handle value) override
	{
		return json_value_get_number(m_Handles[value]->m_pValue);
	}
	inline bool ValueToBool(JS_Handle value) override
	{
		return json_value_get_boolean(m_Handles[value]->m_pValue) == 1;
	}

	// Wrappers for Array API
	bool ArrayGetValue(JS_Handle array, size_t index, JS_Handle *handle) override;
	const char *ArrayGetString(JS_Handle array, size_t index) override;
	inline bool ArrayGetBool(JS_Handle array, size_t index) override
	{
		return json_array_get_boolean(m_Handles[array]->m_pArray, index) == 1;
	}
	bool ArrayReplaceValue(JS_Handle array, size_t index, JS_Handle value) override;
	bool ArrayAppendValue(JS_Handle array, JS_Handle value) override;

	inline double ArrayGetNum(JS_Handle array, size_t index) override
	{
		return json_array_get_number(m_Handles[array]->m_pArray, index);
	}
	inline size_t ArrayGetCount(JS_Handle array) override
	{
		return json_array_get_count(m_Handles[array]->m_pArray);
	}
	inline bool ArrayReplaceString(JS_Handle array, size_t index, const char *string) override
	{
		return json_array_replace_string(m_Handles[array]->m_pArray, index, string) == JSONSuccess;
	}
	inline bool ArrayReplaceNum(JS_Handle array, size_t index, double number) override
	{
		return json_array_replace_number(m_Handles[array]->m_pArray, index, number) == JSONSuccess;
	}
	inline bool ArrayReplaceBool(JS_Handle array, size_t index, bool boolean) override
	{
		return json_array_replace_boolean(m_Handles[array]->m_pArray, index, boolean) == JSONSuccess;
	}
	inline bool ArrayReplaceNull(JS_Handle array, size_t index) override
	{
		return json_array_replace_null(m_Handles[array]->m_pArray, index) == JSONSuccess;
	}
	inline bool ArrayAppendString(JS_Handle array, const char *string) override
	{
		return json_array_append_string(m_Handles[array]->m_pArray, string) == JSONSuccess;
	}
	inline bool ArrayAppendNum(JS_Handle array, double number) override
	{
		return json_array_append_number(m_Handles[array]->m_pArray, number) == JSONSuccess;
	}
	inline bool ArrayAppendBool(JS_Handle array, bool boolean) override
	{
		return json_array_append_boolean(m_Handles[array]->m_pArray, boolean) == JSONSuccess;
	}
	inline bool ArrayAppendNull(JS_Handle array) override
	{
		return json_array_append_null(m_Handles[array]->m_pArray) == JSONSuccess;
	}
	inline bool ArrayRemove(JS_Handle array, size_t index) override
	{
		return json_array_remove(m_Handles[array]->m_pArray, index) == JSONSuccess;
	}
	inline bool ArrayClear(JS_Handle array) override
	{
		return json_array_clear(m_Handles[array]->m_pArray) == JSONSuccess;
	}

	// Wrappers for Object API

	// Get functions
	bool ObjectGetValue(JS_Handle object, const char *name, JS_Handle *handle, bool dotfunc) override;
	const char *ObjectGetString(JS_Handle object, const char *name, bool dotfunc) override;
	double ObjectGetNum(JS_Handle object, const char *name, bool dotfunc) override;
	bool ObjectGetBool(JS_Handle object, const char *name, bool dotfunc) override;
	inline size_t ObjectGetCount(JS_Handle object) override
	{
		return json_object_get_count(m_Handles[object]->m_pObject);
	}
	const char *ObjectGetName(JS_Handle object, size_t index) override;
	bool ObjectGetValueAt(JS_Handle object, size_t index, JS_Handle *handle) override;
	bool ObjectHasValue(JS_Handle object, const char *name, JSONType type, bool dotfunc) override;

	// Set functions
	bool ObjectSetValue(JS_Handle object, const char *name, JS_Handle value, bool dotfunc) override;
	bool ObjectSetString(JS_Handle object, const char *name, const char *string, bool dotfunc) override;
	bool ObjectSetNum(JS_Handle object, const char *name, double number, bool dotfunc) override;
	bool ObjectSetBool(JS_Handle object, const char *name, bool boolean, bool dotfunc) override;
	bool ObjectSetNull(JS_Handle object, const char *name, bool dotfunc) override;

	// Remove functions
	bool ObjectRemove(JS_Handle object, const char *name, bool dotfunc) override;
	inline bool ObjectClear(JS_Handle object) override
	{
		return json_object_clear(m_Handles[object]->m_pObject) == JSONSuccess;
	}

	// Serialization API
	size_t SerialSize(JS_Handle value, bool pretty) override;
	bool SerialToBuffer(JS_Handle value, char *buffer, size_t size, bool pretty) override;
	bool SerialToFile(JS_Handle value, const char *filepath, bool pretty) override;
	char *SerialToString(JS_Handle value, bool pretty) override;
	inline void FreeString(char *string) override
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
