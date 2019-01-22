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

#if defined _json_included
	#endinput
#endif
#define _json_included

#pragma reqlib json
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib json
#endif

/*
 * JSON types
 */
enum JSONType
{
	JSONError   = -1,
	JSONNull    = 1,
	JSONString  = 2,
	JSONNumber  = 3,
	JSONObject  = 4,
	JSONArray   = 5,
	JSONBoolean = 6
};

/*
 * JSON invalid handle
 */
enum JSON
{
	Invalid_JSON = -1
}

/**
 * Helper macros for checking type
 */
#define json_is_object(%1)   (%1 != Invalid_JSON && json_get_type(%1) == JSONObject)
#define json_is_array(%1)    (%1 != Invalid_JSON && json_get_type(%1) == JSONArray)
#define json_is_string(%1)   (%1 != Invalid_JSON && json_get_type(%1) == JSONString)
#define json_is_number(%1)   (%1 != Invalid_JSON && json_get_type(%1) == JSONNumber)
#define json_is_bool(%1)     (%1 != Invalid_JSON && json_get_type(%1) == JSONBoolean)
#define json_is_null(%1)     (%1 != Invalid_JSON && json_get_type(%1) == JSONNull)
#define json_is_true(%1)     (%1 != Invalid_JSON && json_is_bool(%1) && json_get_bool(%1))
#define json_is_false(%1)    (%1 != Invalid_JSON && json_is_bool(%1) && !json_get_bool(%1))

/**
 * Parses JSON string or a file that contains JSON.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param string            String to parse
 * @param is_file           True to treat string param as filename, false otherwise
 * @param with_comments     True if parsing JSON includes comments (it will ignore them), false otherwise
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_parse(const string[], bool:is_file = false, bool:with_comments = false);

/**
 * Checks if the first value is the same as the second one.
 *
 * @param value1            JSON handle
 * @param value2            JSON handle
 *
 * @return                  True if they are the same, false otherwise
 * @error                   If passed value is not a valid handle
 */
native bool:json_equals(const JSON:value1, const JSON:value2);

/**
 * Validates json by checking if object have identically named
 * fields with matching types.
 *
 * @note                    Schema {"name":"", "age":0} will validate
 *                          {"name":"Joe", "age":25} and {"name":"Joe", "age":25, "gender":"m"},
 *                          but not {"name":"Joe"} or {"name":"Joe", "age":"Cucumber"}.
 *
 * @note                    In case of arrays, only first value in schema
 *                          is checked against all values in tested array.
 *
 * @note                    Empty objects ({}) validate all objects,
 *                          empty arrays ([]) validate all arrays,
 *                          null validates values of every type.
 *
 * @param schema            JSON handle
 * @param value             JSON handle
 *
 * @return                  True if passed value is valid, false otherwise
 * @error                   If a schema handle or value handle is invalid
 */
native bool:json_validate(const JSON:schema, const JSON:value);

/**
 * Gets value's parent handle.
 *
 * @note                  Parent's handle needs to be freed using json_free() native.
 *
 * @param value           JSON handle
 *
 * @return                Parent's handle
 */
native JSON:json_get_parent(const JSON:value);

/**
 * Gets JSON type of passed value.
 *
 * @param value             JSON handle
 *
 * @return                  JSON type (JSONType constants)
 * @error                   If a value handle is invalid
 */
native JSONType:json_get_type(const JSON:value);

/**
 * Inits an empty object.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_object();

/**
 * Inits an empty array.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_array();

/**
 * Inits string data.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param value             String that the handle will be initialized with
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_string(const value[]);

/**
 * Inits a number.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param value             Integer number that the handle will be initialized with
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_number(value);

/**
 * Inits a real number.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param value             Real number that the handle will be initialized with
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_real(Float:value);

/**
 * Inits a boolean value.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param value             Boolean value that the handle will be initialized with
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_bool(bool:value);

/**
 * Inits a null.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 */
native JSON:json_init_null();

/**
 * Creates deep copy of passed value.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param value             JSON handle to be copied
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 * @error                   If passed value is not a valid handle
 */
native JSON:json_deep_copy(const JSON:value);

/**
 * Frees handle.
 *
 * @param handle            JSON handle to be freed
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid handle
 */
native bool:json_free(&JSON:handle);

/**
 * Gets string data.
 *
 * @param value             JSON handle
 * @param buffer            Buffer to copy string to
 * @param maxlen            Maximum size of the buffer
 *
 * @return                  The number of cells written to the buffer
 * @error                   If passed value is not a valid handle
 */
native json_get_string(const JSON:value, buffer[], maxlen);

/**
 * Gets a number.
 *
 * @param value             JSON handle
 *
 * @return                  Number
 * @error                   If passed value is not a valid handle
 */
native json_get_number(const JSON:value);

/**
 * Gets a real number.
 *
 * @param value             JSON handle
 *
 * @return                  Real number
 * @error                   If passed value is not a valid handle
 */
native Float:json_get_real(const JSON:value);

/**
 * Gets a boolean value.
 *
 * @param value             JSON handle
 *
 * @return                  Boolean value
 * @error                   If passed value is not a valid handle
 */
native bool:json_get_bool(const JSON:value);

/**
 * Gets a value from the array.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param array             Array handle
 * @param index             Position in the array (starting from 0)
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 * @error                   If passed handle is not a valid array
 */
native JSON:json_array_get_value(const JSON:array, index);

/**
 * Gets string data from the array.
 *
 * @param array             Array handle
 * @param index             Position in the array (starting from 0)
 * @param buffer            Buffer to copy string to
 * @param maxlen            Maximum size of the buffer
 *
 * @return                  The number of cells written to the buffer
 * @error                   If passed handle is not a valid array
 */
native json_array_get_string(const JSON:array, index, buffer[], maxlen);

/**
 * Gets a number from the array.
 *
 * @param array             Array handle
 * @param index             Position in the array (starting from 0)
 *
 * @return                  The number as integer
 * @error                   If passed handle is not a valid array
 */
native json_array_get_number(const JSON:array, index);

/**
 * Gets a real number from the array.
 *
 * @param array             Array handle
 * @param index             Position in the array (starting from 0)
 *
 * @return                  The number as float
 * @error                   If passed handle is not a valid array
 */
native Float:json_array_get_real(const JSON:array, index);

/**
 * Gets a boolean value from the array.
 *
 * @param array             Array handle
 * @param index             Position in the array (starting from 0)
 *
 * @return                  Boolean value
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_get_bool(const JSON:array, index);

/**
 * Gets count of the elements in the array.
 *
 * @param array             Array handle
 *
 * @return                  Number of elements in the array
 * @error                   If passed handle is not a valid array
 */
native json_array_get_count(const JSON:array);

/**
 * Replaces an element in the array with value.
 *
 * @param array             Array handle
 * @param index             Position in the array to be replaced
 * @param value             JSON handle to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_replace_value(JSON:array, index, const JSON:value);

/**
 * Replaces an element in the array with string data.
 *
 * @param array             Array handle
 * @param index             Position in the array to be replaced
 * @param string            String to copy
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_replace_string(JSON:array, index, const string[]);

/**
 * Replaces an element in the array with number.
 *
 * @param array             Array handle
 * @param index             Position in the array to be replaced
 * @param number            Number to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_replace_number(JSON:array, index, number);

/**
 * Replaces an element in the array with real number.
 *
 * @param array             Array handle
 * @param index             Position in the array to be replaced
 * @param number            Real number to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_replace_real(JSON:array, index, Float:number);

/**
 * Replaces an element in the array with boolean value.
 *
 * @param array             Array handle
 * @param index             Position in the array to be replaced
 * @param boolean           Boolean value to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_replace_bool(JSON:array, index, bool:boolean);

/**
 * Replaces an element in the array with null.
 *
 * @param array             Array handle
 * @param index             Position in the array to be replaced
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_replace_null(JSON:array, index);

/**
 * Appends a value in the array.
 *
 * @param array             Array handle
 * @param value             JSON handle to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_append_value(JSON:array, const JSON:value);

/**
 * Appends string data in the array.
 *
 * @param array             Array handle
 * @param string            String to copy
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_append_string(JSON:array, const string[]);

/**
 * Appends a number in the array.
 *
 * @param array             Array handle
 * @param number            Number to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_append_number(JSON:array, number);

/**
 * Appends a real number in the array.
 *
 * @param array             Array handle
 * @param number            Real number to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_append_real(JSON:array, Float:number);

/**
 * Appends a boolean value in the array.
 *
 * @param array             Array handle
 * @param boolean           Boolean value to set
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_append_bool(JSON:array, bool:boolean);

/**
 * Appends a null in the array.
 *
 * @param array             Array handle
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_append_null(JSON:array);

/**
 * Removes an element from the array.
 *
 * @note                    Order of values in array may change during execution.
 *
 * @param array             Array handle
 * @param index             Position in the array (starting from 0)
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_remove(JSON:array, index);

/**
 * Removes all elements from the array.
 *
 * @param array             Array handle
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid array
 */
native bool:json_array_clear(JSON:array);

/**
 * Gets a value from the object.
 *
 * @note                    Needs to be freed using json_free() native.
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 * @error                   If passed handle is not a valid object
 */
native JSON:json_object_get_value(const JSON:object, const name[], bool:dot_not = false);

/**
 * Gets string data from the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param buffer            Buffer to copy string to
 * @param maxlen            Maximum size of the buffer
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  The number of cells written to the buffer
 * @error                   If passed handle is not a valid object
 */
native json_object_get_string(const JSON:object, const name[], buffer[], maxlen, bool:dot_not = false);

/**
 * Gets a number from the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  Number
 * @error                   If passed handle is not a valid object
 */
native json_object_get_number(const JSON:object, const name[], bool:dot_not = false);

/**
 * Gets a real number from the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  Real number
 * @error                   If passed handle is not a valid object
 */
native Float:json_object_get_real(const JSON:object, const name[], bool:dot_not = false);

/**
 * Gets a boolean value from the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  Boolean value
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_get_bool(const JSON:object, const name[], bool:dot_not = false);

/**
 * Gets count of the keys in the object.
 *
 * @param object            Object handle
 *
 * @return                  Keys count
 * @error                   If passed handle is not a valid object
 */
native json_object_get_count(const JSON:object);

/**
 * Gets name of the object's key.
 *
 * @param object            Object handle
 * @param index             Position from which get key name
 * @param buffer            Buffer to copy string to
 * @param maxlen            Maximum size of the buffer
 *
 * @return                  The number of cells written to the buffer
 * @error                   If passed handle is not a valid object
 */
native json_object_get_name(const JSON:object, index, buffer[], maxlen);

/**
 * Gets a value at the specified position from the object.
 *
 * @note                    Needs to be freed using json_free() native.
 *
 * @param object            Object handle
 * @param index             Position from which get key name
 *
 * @return                  JSON handle, Invalid_JSON if error occurred
 * @error                   If passed handle is not a valid object
 */
native JSON:json_object_get_value_at(const JSON:object, index);

/**
 * Checks if the object has a value with a specific name and type.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param type              Type of value, if JSONError type will not be checked
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if has, false if not
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_has_value(const JSON:object, const name[], JSONType:type = JSONError, bool:dot_not = false);

/**
 * Sets a value in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 * @note                    It also removes the old value if any.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param value             JSON handle to set
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_set_value(JSON:object, const name[], const JSON:value, bool:dot_not = false);

/**
 * Sets string data in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 * @note                    It also removes the old value if any.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param string            String to copy
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_set_string(JSON:object, const name[], const string[], bool:dot_not = false);

/**
 * Sets a number in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 * @note                    It also removes the old value if any.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param number            Number to set
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_set_number(JSON:object, const name[], number, bool:dot_not = false);

/**
 * Sets a real number in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 * @note                    It also removes the old value if any.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param number            Real number to set
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_set_real(JSON:object, const name[], Float:number, bool:dot_not = false);

/**
 * Sets a boolean value in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 * @note                    It also removes the old value if any.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param boolean           Boolean value to set
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_set_bool(JSON:object, const name[], bool:boolean, bool:dot_not = false);

/**
 * Sets a null in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 * @note                    It also removes the old value if any.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_set_null(JSON:object, const name[], bool:dot_not = false);

/**
 * Removes a key and its value in the object.
 *
 * @note                    If dot notation is used some values may be inaccessible
 *                          because valid names in JSON can contain dots.
 *
 * @param object            Object handle
 * @param name              Key name
 * @param dot_not           True to use dot notation, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_remove(JSON:object, const name[], bool:dot_not = false);

/**
 * Removes all keys and their values in the object.
 *
 * @param object            Object handle
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid object
 */
native bool:json_object_clear(JSON:object);

/**
 * Gets size of serialization.
 *
 * @param value             JSON handle
 * @param pretty            True to count size for pretty format, false to not
 * @param null_byte         True to include null byte, false to not
 *
 * @return                  Size of serialized string
 * @error                   If passed handle is not a valid value
 */
native json_serial_size(const JSON:value, bool:pretty = false, bool:null_byte = false);

/**
 * Copies serialized string to the buffer.
 *
 * @param value             JSON handle
 * @param buffer            Buffer to copy string to
 * @param maxlen            Maximum size of the buffer
 * @param pretty            True to format pretty JSON string, false to not
 *
 * @return                  The number of cells written to the buffer
 * @error                   If passed handle is not a valid value
 */
native json_serial_to_string(const JSON:value, buffer[], maxlen, bool:pretty = false);

/**
 * Copies serialized string to the file.
 *
 * @param value             JSON handle
 * @param file              Path to the file
 * @param pretty            True to format pretty JSON string, false to not
 *
 * @return                  True if succeed, false otherwise
 * @error                   If passed handle is not a valid value
 */
native bool:json_serial_to_file(const JSON:value, const file[], bool:pretty = false);
