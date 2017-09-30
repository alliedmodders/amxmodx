// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// JSON Interface
//

//#include <InterfaceSys.h>

namespace AMXX
{
	/**
	 * @brief Lists of possible handle types.
	 */
	enum JSONHandleType
	{
		Handle_Value = 0,
		Handle_Array,
		Handle_Object
	};

	/**
	 * @brief Lists of possible JSON types.
	 */
	enum JSONType
	{
		JSONTypeError   = -1,
		JSONTypeNull    = 1,
		JSONTypeString  = 2,
		JSONTypeNumber  = 3,
		JSONTypeObject  = 4,
		JSONTypeArray   = 5,
		JSONTypeBoolean = 6
	};

	/**
	 * @brief Represents a Handle ID.
	 */
	typedef size_t JS_Handle;

	/**
	 * @brief Provides functions for managing JSON.
	 */
	class IJSONMngr/* : public AMXXInterface*/
	{
		public:

			//virtual unsigned int GetInterfaceVersion() override final { return 1; }
			//virtual const char *GetInterfaceName() override final { return "IJsonMngr"; }

			virtual ~IJSONMngr() {};

			/**
			* @brief                  Checks if handle with specified type is valid.
			*
			* @param id               JSON handle
			* @param type             Handle's type
			*
			* @return                 True if handle is valid, false otherwise
			*/
			virtual bool IsValidHandle(JS_Handle id, JSONHandleType type = Handle_Value) = 0;

			/**
			* @brief                  Frees handle.
			*
			* @param id               JSON handle
			*
			* @noreturn
			*/
			virtual void Free(JS_Handle id) = 0;

			/**
			* @brief                  Gets JSON type of passed handle.
			*
			* @param value            JSON handle
			*
			* @return                 JSON type or JSONTypeError if error occurred
			*/
			virtual JSONType GetHandleJSONType(JS_Handle value) = 0;

			/**
			* @brief                  Parses JSON string or a file that contains JSON.
			*
			* @note                   Handle needs to be freed using Free().
			*
			* @param string           String to parse
			* @param handle           Address to variable where value's handle will be stored
			* @param is_file          True to treat string param as filename, false otherwise
			* @param with_comments    True if parsing JSON includes comments (it will ignore them), false otherwise
			*
			* @return                 True if succeed, false otherwise
			*/
			virtual bool Parse(const char *string, JS_Handle *handle, bool is_file = false, bool with_comments = false) = 0;

			/**
			* @brief                  Checks if the first value is the same as the second one.
			*
			* @param value1           JSON handle
			* @param value2           JSON handle
			*
			* @return                 True if they are the same, false otherwise
			*/
			virtual bool AreValuesEquals(JS_Handle value1, JS_Handle value2) = 0;

			/**
			 * @brief                 Validates json by checking if object have identically named
			 *                        fields with matching types.
			 *
			 * @note                  Schema {"name":"", "age":0} will validate
			 *                        {"name":"Joe", "age":25} and {"name":"Joe", "age":25, "gender":"m"},
			 *                        but not {"name":"Joe"} or {"name":"Joe", "age":"Cucumber"}.
			 *
			 * @note                  In case of arrays, only first value in schema is checked against
			 *                        all values in tested array.
			 *
			 * @note                  Empty objects ({}) validate all objects,
			 *                        empty arrays ([]) validate all arrays,
			 *                        null validates values of every type.
			 *
			 * @param schema          JSON handle
			 * @param value           JSON handle
			 *
			 * @return                True if passed value is valid, false otherwise
			 */
			virtual bool IsValueValid(JS_Handle schema, JS_Handle value) = 0;

			/**
			 * @brief                 Checks if value has parent and assigns it to variable (if provided).
			 *
			 * @note                  Parent's handle needs to be freed using Free().
			 *
			 * @param value           JSON handle
			 * @param parent          Address to variable where parent's handle will be stored
			 *
			 * @return                True if value has parent, false otherwise
			 */
			virtual bool GetValueParent(JS_Handle value, JS_Handle *parent = nullptr) = 0;

			/**
			 * @brief                 Inits an empty object.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool InitObject(JS_Handle *handle) = 0;

			/**
			 * @brief                 Inits an empty array.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool InitArray(JS_Handle *handle) = 0;

			/**
			 * @brief                 Inits string data.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param string          String that the handle will be initialized with
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool InitString(const char *string, JS_Handle *handle) = 0;

			/**
			 * @brief                 Inits a number.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param number          Number that the handle will be initialized with
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool InitNum(double number, JS_Handle *handle) = 0;

			/**
			 * @brief                 Inits a boolean value.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param boolean         Boolean value that the handle will be initialized with
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool InitBool(bool boolean, JS_Handle *handle) = 0;

			/**
			 * @brief                 Inits a null.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool InitNull(JS_Handle *handle) = 0;

			/**
			 * @brief                 Creates deep copy of passed value.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param value           JSON handle to be copied
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool DeepCopyValue(JS_Handle value, JS_Handle *handle) = 0;

			/**
			 * @brief                 Gets a string data.
			 *
			 * @param value           JSON handle
			 *
			 * @return                String data
			 */
			virtual const char *ValueToString(JS_Handle value) = 0;

			/**
			 * @brief                 Gets a number.
			 *
			 * @param value           JSON handle
			 *
			 * @return                Number
			 */
			virtual double ValueToNum(JS_Handle value) = 0;

			/**
			 * @brief                 Gets a boolean value.
			 *
			 * @param value           JSON handle
			 *
			 * @return                Boolean value
			 */
			virtual bool ValueToBool(JS_Handle value) = 0;

			//JSON Array API

			//Get functions

			/**
			 * @brief                 Gets a value from the array.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayGetValue(JS_Handle array, size_t index, JS_Handle *handle) = 0;

			/**
			 * @brief                 Gets string data from the array.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 *
			 * @return                String data
			 */
			virtual const char *ArrayGetString(JS_Handle array, size_t index) = 0;

			/**
			 * @brief                 Gets a number from the array.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 *
			 * @return                Number
			 */
			virtual double ArrayGetNum(JS_Handle array, size_t index) = 0;

			/**
			 * @brief                 Gets a boolean value from the array.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 *
			 * @return                Boolean value
			 */
			virtual bool ArrayGetBool(JS_Handle array, size_t index) = 0;

			/**
			 * @brief                 Gets count of the elements in the array.
			 *
			 * @param array           JSON handle
			 *
			 * @return                Number of elements in the array
			 */
			virtual size_t ArrayGetCount(JS_Handle array) = 0;

			//Set functions

			/**
			 * @brief                 Replaces an element in the array with value.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 * @param value           JSON handle to set
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayReplaceValue(JS_Handle array, size_t index, JS_Handle value) = 0;

			/**
			 * @brief                 Replaces an element in the array with string.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 * @param string          String to copy
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayReplaceString(JS_Handle array, size_t index, const char *string) = 0;

			/**
			 * @brief                 Replaces an element in the array with number.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 * @param number          Number to set
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayReplaceNum(JS_Handle array, size_t index, double number) = 0;

			/**
			 * @brief                 Replaces an element in the array with boolean value.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 * @param boolean         Boolean value to set
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayReplaceBool(JS_Handle array, size_t index, bool boolean) = 0;

			/**
			 * @brief                 Replaces an element in the array with null.
			 *
			 * @param array           JSON handle
			 * @param index           Position in the array (starting from 0)
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayReplaceNull(JS_Handle array, size_t index) = 0;

			/**
			 * @brief                 Appends a value in the array.
			 *
			 * @param array           JSON handle
			 * @param value           JSON handle
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayAppendValue(JS_Handle array, JS_Handle value) = 0;

			/**
			 * @brief                 Appends string data in the array.
			 *
			 * @param array           JSON handle
			 * @param string          String to copy
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayAppendString(JS_Handle array, const char *string) = 0;

			/**
			 * @brief                 Appends a number in the array.
			 *
			 * @param array           JSON handle
			 * @param string          Number to set
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayAppendNum(JS_Handle array, double number) = 0;

			/**
			 * @brief                 Appends a boolean value in the array.
			 *
			 * @param array           JSON handle
			 * @param boolean         Boolean value to set
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayAppendBool(JS_Handle array, bool boolean) = 0;

			/**
			 * @brief                 Appends a null in the array.
			 *
			 * @param array           JSON handle
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayAppendNull(JS_Handle array) = 0;

			//Remove functions

			/**
			 * @brief                 Removes an element from the array.
			 *
			 * @note                  Order of values in array may change during execution.
			 *
			 * @param array           JSON handle
			 * @param position        Position in the array (starting from 0)
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayRemove(JS_Handle array, size_t index) = 0;

			/**
			 * @brief                 Removes all elements from the array.
			 *
			 * @param array           JSON handle
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ArrayClear(JS_Handle array) = 0;

			//Wrappers for Object API

			//Get functions

			/**
			 * @brief                 Gets a value from the object.
			 *
			 * @note                  Handle needs to be freed using Free().
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param handle          Address to variable where value's handle will be stored
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectGetValue(JS_Handle object, const char *name, JS_Handle *handle, bool dotfunc = false) = 0;

			/**
			 * @brief                 Gets string data from the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                String data
			 */
			virtual const char *ObjectGetString(JS_Handle object, const char *name, bool dotfunc = false) = 0;

			/**
			 * @brief                 Gets a number from the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                Number
			 */
			virtual double ObjectGetNum(JS_Handle object, const char *name, bool dotfunc = false) = 0;

			/**
			 * @brief                 Gets a boolean value from the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                Boolean value
			 */
			virtual bool ObjectGetBool(JS_Handle object, const char *name, bool dotfunc = false) = 0;

			/**
			 * @brief                 Gets count of the keys in the object.
			 *
			 * @param object          JSON handle
			 *
			 * @return                Keys count
			 */
			virtual size_t ObjectGetCount(JS_Handle object) = 0;

			/**
			 * @brief                 Gets name of the object's key.
			 *
			 * @param object          JSON handle
			 * @param index           Position from which get key name
			 *
			 * @return                Key name
			 */
			virtual const char *ObjectGetName(JS_Handle object, size_t index) = 0;

			/**
			 * @brief                 Gets a value at the specified position from the object.
			 *
			 * @note                  Handle needs to be freed using Free().
			 *
			 * @param object          JSON handle
			 * @param index           Position from which get key name
			 * @param handle          Address to variable where value's handle will be stored
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectGetValueAt(JS_Handle object, size_t index, JS_Handle *handle) = 0;

			/**
			 * @brief                 Checks if object has a value with a specific name and type.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param type            Type of value, if JSONTypeError type will not be checked
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if has, false if not
			 */
			virtual bool ObjectHasValue(JS_Handle object, const char *name, JSONType type = JSONTypeError, bool dotfunc = false) = 0;

			//Set functions

			/**
			 * @brief                 Sets a value in the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 * @note                  It also removes the old value if any.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param value           JSON handle
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectSetValue(JS_Handle object, const char *name, JS_Handle value, bool dotfunc = false) = 0;

			/**
			 * @brief                 Sets string data in the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 * @note                  It also removes the old value if any.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param string          String to copy
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectSetString(JS_Handle object, const char *name, const char *string, bool dotfunc = false) = 0;

			/**
			 * @brief                 Sets a number in the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 * @note                  It also removes the old value if any.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param number          Number to set
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectSetNum(JS_Handle object, const char *name, double number, bool dotfunc = false) = 0;

			/**
			 * @brief                 Sets a boolean value in the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 * @note                  It also removes the old value if any.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param boolean         Boolean value to set
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectSetBool(JS_Handle object, const char *name, bool boolean, bool dotfunc = false) = 0;

			/**
			 * @brief                 Sets a null in the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 * @note                  It also removes the old value if any.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectSetNull(JS_Handle object, const char *name, bool dotfunc = false) = 0;

			//Remove functions

			/**
			 * @brief                 Removes a key and its value in the object.
			 *
			 * @note                  If dot notation is used some values may be inaccessible
			 *                        because valid names in JSON can contain dots.
			 *
			 * @param object          JSON handle
			 * @param name            Key name
			 * @param dotfunc         True to use dot notation, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectRemove(JS_Handle object, const char *name, bool dotfunc = false) = 0;

			/**
			 * @brief                 Removes all keys and their values in the object.
			 *
			 * @param object          JSON handle
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool ObjectClear(JS_Handle object) = 0;

			//Serialization API

			/**
			 * @brief                 Gets size of serialization.
			 *
			 * @param value           JSON handle
			 * @param pretty          True to count size for pretty format, false to not
			 *
			 * @return                Size of serialized string
			 */
			virtual size_t SerialSize(JS_Handle value, bool pretty) = 0;

			/**
			 * @brief                 Copies serialized string to the buffer.
			 *
			 * @note                  The buffer must be large enough or function will
			 *                        fail.
			 *
			 * @param value           JSON handle
			 * @param buffer          Buffer to copy string to
			 * @param size            Size of the buffer
			 * @param pretty          True to format pretty JSON string, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool SerialToBuffer(JS_Handle value, char *buffer, size_t size, bool pretty) = 0;

			/**
			 * @brief                 Copies serialized string to the file.
			 *
			 * @param value           JSON handle
			 * @param filepath        Path to the file
			 * @param pretty          True to format pretty JSON string, false to not
			 *
			 * @return                True if succeed, false otherwise
			 */
			virtual bool SerialToFile(JS_Handle value, const char *filepath, bool pretty) = 0;

			/**
			 * @brief                 Returns serialized string.
			 *
			 * @note                  Must be freed using FreeString().
			 *
			 * @param value           JSON handle
			 * @param pretty          True to format pretty JSON string, false to not
			 *
			 * @return                Serialized string, nullptr if failed
			 */
			virtual char *SerialToString(JS_Handle value, bool pretty) = 0;

			/**
			 * @brief                 Frees serialized string.
			 *
			 * @param string          Pointer to serialized string
			 *
			 * @noreturn
			 */
			virtual void FreeString(char *string) = 0;
	};
}
