// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>
#include <json>

//For encoding
new buffer[500];

public plugin_init()
{
	register_plugin("JSON Test", "1.0", "Ni3znajomy");
	register_srvcmd("json_test_encode", "cmdJSONTestEncode");
	register_srvcmd("json_test_decode", "cmdJSONTestDecode");
	register_srvcmd("json_test_replace", "cmdJSONTestReplace");
	register_srvcmd("json_test_validate", "cmdJSONTestValidate");
	register_srvcmd("json_test_has_key", "cmdJSONTestHasKey");
	register_srvcmd("json_test_remove", "cmdJSONTestRemove");
}

public cmdJSONTestEncode()
{
	if (read_argc() < 3)
	{
		server_print("Usage: json_test_encode <use init funcs> <use pretty format>");
		return;
	}
	// Use init funcs?
	new bool:init = bool:read_argv_int(1);

	//Pretty
	new bool:pretty = bool:read_argv_int(2);

	// Create root array
	new JSON:root_array = json_init_array();
	// Init & setup object
	new JSON:object = json_init_object();
	if (!init)
	{
		json_object_set_string(object, "string", "https://alliedmods.net");
		json_object_set_number(object, "number", 45);
		json_object_set_real(object, "real", -5.31);
		json_object_set_null(object, "null.null");
		json_object_set_bool(object, "bool.true", true, true);
	}
	else
	{
		ObjectSetKey(object, "string", json_init_string("https://alliedmods.net"));
		ObjectSetKey(object, "number", json_init_number(45));
		ObjectSetKey(object, "real", json_init_real(-5.31));
		ObjectSetKey(object, "null.null", json_init_null());
		ObjectSetKey(object, "bool.true", json_init_bool(true), true);
	}

	// Add object to root array
	ArrayAppendValue(root_array, object);

	new JSON:parent = json_get_parent(root_array);
	if (parent != Invalid_JSON)
	{
		server_print("Root array has parent!");
		json_free(parent);
		json_free(root_array);
		return;
	}

	// Append some values to root array
	if (!init)
	{
		json_array_append_real(root_array, -31.1);
		json_array_append_number(root_array, -42);
		json_array_append_bool(root_array, false);
		json_array_append_null(root_array);
	}
	else
	{
		ArrayAppendValue(root_array, json_init_real(-31.1));
		ArrayAppendValue(root_array, json_init_number(-42));
		ArrayAppendValue(root_array, json_init_bool(false));
		ArrayAppendValue(root_array, json_init_null());
	}

	// Serialiaze to file and to buffer
	json_serial_to_file(root_array, "json_encode_test_to_file.txt", pretty);
	json_serial_to_string(root_array, buffer, charsmax(buffer), pretty);

	// Put buffer's content to file
	new file = fopen("json_encode_test_to_string.txt", "wt");
	if (!file)
	{
		server_print("Couldn't create file");
		return;
	}

	fputs(file, buffer);
	fclose(file);

	server_print("Encoding done (%d bytes)", json_serial_size(root_array, pretty));
	json_free(root_array);
}

public cmdJSONTestDecode()
{
	// Check if encode command was run
	if (!strlen(buffer))
	{
		server_print("Run ^"json_test_encode^" first!");
		return;
	}

	new JSON:root_array = json_parse(buffer);
	new JSON:for_compare = json_parse("json_encode_test_to_file.txt", true);

	// Check if they are the same
	if (root_array != for_compare || !json_is_array(root_array))
	{
		if (root_array != Invalid_JSON)
			json_free(root_array);

		if (for_compare != Invalid_JSON)
			json_free(for_compare);

		server_print("Root value is not array!");
		return;
	}

	// We don't need this anymore
	json_free(for_compare);

	DecodeArray(root_array);

	json_free(root_array);
}

//Creating inversed root array
public cmdJSONTestReplace()
{
	// Check if encode command was run
	if (!strlen(buffer))
	{
		server_print("Run ^"json_test_encode^" first!");
		return;
	}

	new JSON:root_array_orig = json_parse(buffer);
	new JSON:root_array_copy = json_deep_copy(root_array_orig);
	json_free(root_array_orig);

	//Replace null with object
	new JSON:object = json_array_get_value(root_array_copy, 0);
	json_array_replace_value(root_array_copy, 4, object);
	json_free(object);

	//Replace object with null
	json_array_replace_null(root_array_copy, 0);

	//Replace bool with real and vice versa
	new Float:realnum = json_array_get_real(root_array_copy, 1);
	new bool:boolval = json_array_get_bool(root_array_copy, 3);
	json_array_replace_real(root_array_copy, 3, realnum);
	json_array_replace_bool(root_array_copy, 1, boolval);

	//Replace number with random
	json_array_replace_number(root_array_copy, 2, random(42));

	json_serial_to_file(root_array_copy, "json_replace_test.txt", true);

	json_free(root_array_copy);

	server_print("Values replaced!");
}

public cmdJSONTestValidate()
{
	// Check if encode command was run
	if (!strlen(buffer))
	{
		server_print("Run ^"json_test_encode^" first!");
		return;
	}

	if (read_argc() < 2)
	{
		server_print("Usage: json_test_validate <success>");
		return;
	}

	//Should validating be succeed?
	new bool:success = bool:read_argv_int(1);

	//Create schema
	new JSON:schema = json_init_object();

	if (success)
		json_object_set_string(schema, "string", "");
	else
		json_object_set_real(schema, "string", 0.0);

	json_object_set_number(schema, "number", 0);
	json_object_set_null(schema, "real");   //Null validate all types

	new JSON:root_array = json_parse(buffer);   //Get root array
	new JSON:object = json_array_get_value(root_array, 0);  //Get object from it

	new bool:result = json_validate(schema, object);	//Validate object with our schema

	server_print("Validate %d! (result: %d)", result, result == success);

	json_free(object);
	json_free(schema);
	json_free(root_array);
}

public cmdJSONTestHasKey()
{
	// Check if encode command was run
	if (!strlen(buffer))
	{
		server_print("Run ^"json_test_encode^" first!");
		return;
	}

	if (read_argc() < 2)
	{
		server_print("Usage: json_test_has_key <key name> <dotnot> <type>");
		server_print("Available types:^nn - null^ns - string^nr - number^no - object^na - array^nb - boolean");
		return;
	}

	//Get root array
	new JSON:root_array = json_parse(buffer);
	new JSON:object = json_array_get_value(root_array, 0);  //Get object
	new keyname[32], type[10];
	new JSONType:jtype = JSONError;

	read_argv(1, keyname, charsmax(keyname));   //Key name that have to be found
	new bool:dotnot = bool:read_argv_int(2);	//Use dot natation? (optional)
	read_argv(3, type, charsmax(type));			//Type of searched value (optional)

	//Get type
	switch(type[0])
	{
		case 'n': jtype = JSONNull;
		case 's': jtype = JSONString;
		case 'r': jtype = JSONNumber;
		case 'o': jtype = JSONObject;
		case 'a': jtype = JSONArray;
		case 'b': jtype = JSONBoolean;
	}

	new bool:found = json_object_has_value(object, keyname, jtype, dotnot);

	if (jtype == JSONError)
		server_print("Key %s%s!%s Found!", keyname, (dotnot) ? " using dotnot" : "", (found) ? "" : " Not");
	else
	{
		GetTypeName(jtype, type, charsmax(type));   //Get type as string
		server_print("Key %s (type: %s)%s!%s Found!", keyname, type, (dotnot) ? " using dotnot" : "", (found) ? "" : " Not");
	}

	json_free(object);
	json_free(root_array);
}

public cmdJSONTestRemove()
{
	// Check if encode command was run
	if (!strlen(buffer))
	{
		server_print("Run ^"json_test_encode^" first!");
		return;
	}

	if (read_argc() < 3)
	{
		server_print("Usage: json_test_has_key <type> <index/key name> <dotnot>");
		server_print("Available types:^na - array^no - object");
		return;
	}

	//Get root array
	new JSON:root_array = json_parse(buffer);
	new type[10], bool:is_object, bool:success;

	read_argv(1, type, charsmax(type));			//Type
	is_object = type[0] == 'o';					//Is type object?
	if (!is_object && type[0] != 'a')			//If not, is it array?
	{
		server_print("Unknown type: %c", type[0]);  //Fail, unknown type
		return;
	}

	if (is_object)
	{
		new keyname[32];
		new JSON:object = json_array_get_value(root_array, 0);

		read_argv(2, keyname, charsmax(keyname)); //Key to delete or "clear" if object have to be cleared
		new bool:dotnot = bool:read_argv_int(3); //Use dot notation?

		if (equal(keyname, "clear"))
			success = json_object_clear(object);
		else
			success = json_object_remove(object, keyname, dotnot);

		json_free(object);
	}
	else
	{
		new index = read_argv_int(2); //Entry to delete or -1 if array have to be cleared

		if (index == -1)
			success = json_array_clear(root_array);
		else
			success = json_array_remove(root_array, index);
	}

	//Dump result
	json_serial_to_file(root_array, "json_remove_test.txt", true); //Use pretty format for better view

	json_free(root_array);

	server_print("Removing %s! (Results dumped)", (success) ? "succeed" : "failed");
}

ObjectSetKey(JSON:object, const key[], JSON:node, bool:dot_not = false)
{
	json_object_set_value(object, key, node, dot_not);
	json_free(node);
}

ArrayAppendValue(JSON:array, JSON:node)
{
	json_array_append_value(array, node);
	json_free(node);
}

DecodeArray(&JSON:array)
{
	//for storing string data
	new tempbuf[2][100];
	new JSON:array_value, JSON:parent_value;
	for (new i = 0; i < json_array_get_count(array); i++)
	{
		array_value = json_array_get_value(array, i);
		parent_value = json_get_parent(array_value);

		if (parent_value != array)
		{
			json_free(parent_value);
			json_free(array_value);

			server_print("[Array] Parent value differs!");
			break;
		}

		json_free(parent_value);

		switch (json_get_type(array_value))
		{
			case JSONNull: server_print("Array Index %d (Null)", i);
			case JSONString:
			{
				json_get_string(array_value, tempbuf[0], charsmax(tempbuf[]));
				json_array_get_string(array, i, tempbuf[1], charsmax(tempbuf[]));

				server_print("Array Index %d (String) value: %s | index: %s", i, tempbuf[0], tempbuf[1]);
			}
			case JSONNumber:
			{
				new num1 = json_get_number(array_value), num2 = json_array_get_number(array, i);
				new Float:num3 = json_get_real(array_value), Float:num4 = json_array_get_real(array, i);

				server_print("Array Index %d (Number/Real) value: %d/%f | index: %d/%f", i, num1, num3, num2, num4);
			}
			case JSONObject:
			{
				new iCount = json_object_get_count(array_value);
				server_print("Array Index %d (Object) %d elements", i, iCount);

				DecodeObject(array_value);
			}
			case JSONArray:
			{
				new iCount = json_array_get_count(array_value);
				server_print("Array Index %d (Array) %d elements", i, iCount);

				DecodeArray(array_value);
			}
			case JSONBoolean:
			{
				new bool:val1 = json_get_bool(array_value), bool:val2 = json_array_get_bool(array, i);
				server_print("Array Index %d (Bool) value %d | index %d", i, val1, val2);
			}
		}

		json_free(array_value);
	}
}

DecodeObject(&JSON:object)
{
	//for storing string data
	new tempbuf[2][100];
	new key[30];
	new JSON:obj_value, JSON:parent_value;
	for (new i = 0; i < json_object_get_count(object); i++)
	{
		json_object_get_name(object, i, key, charsmax(key));
		obj_value = json_object_get_value_at(object, i);
		parent_value = json_get_parent(obj_value);

		if (parent_value != object)
		{
			json_free(parent_value);
			json_free(obj_value);

			server_print("[Object] Parent value differs!");
			break;
		}

		json_free(parent_value);

		switch (json_get_type(obj_value))
		{
			case JSONNull: server_print("   Object Key ^"%s^" (Null)", key);
			case JSONString:
			{
				json_get_string(obj_value, tempbuf[0], charsmax(tempbuf[]));
				json_object_get_string(object, key, tempbuf[1], charsmax(tempbuf[]));

				server_print("   Object Key ^"%s^" (String) value: %s | key: %s", key, tempbuf[0], tempbuf[1]);
			}
			case JSONNumber:
			{
				new num1 = json_get_number(obj_value), num2 = json_object_get_number(object, key);
				new Float:num3 = json_get_real(obj_value), Float:num4 = json_object_get_real(object, key);

				server_print("   Object Key ^"%s^" (Number/Real) value: %d/%f | key: %d/%f", key, num1, num3, num2, num4);
			}
			case JSONObject:
			{
				new iCount = json_object_get_count(obj_value);
				server_print("   Object Key ^"%s^" (Object) %d elements", key, iCount);

				//Let's get its value by dot notation
				if (equal(key, "bool"))
				{
					//dotnot
					new bool:val1 = json_object_get_bool(object, "bool.true", true);
					new bool:val2 = json_object_get_bool(obj_value, "true");
					json_object_get_name(obj_value, 0, key, charsmax(key));

					server_print("      Object Key ^"%s^" (Bool) dot %d | nodot: %d", key, val1, val2);
				}
				else
					DecodeObject(obj_value);
			}
			case JSONArray:
			{
				new iCount = json_array_get_count(obj_value);
				server_print("   Object Key ^"%s^" (Array) %d elements", key, iCount);

				DecodeArray(obj_value);
			}
			case JSONBoolean:
			{
				new bool:val1 = json_get_bool(obj_value), bool:val2 = json_object_get_bool(object, key);
				server_print("   Object Key ^"%s^" (Bool) value %d | index %d", key, val1, val2);
			}
		}

		json_free(obj_value);
	}
}

GetTypeName(JSONType:type, buffer[], maxlen)
{
	switch(type)
	{
		case JSONNull: copy(buffer, maxlen, "Null");
		case JSONString: copy(buffer, maxlen, "String");
		case JSONNumber: copy(buffer, maxlen, "Number");
		case JSONObject: copy(buffer, maxlen, "Object");
		case JSONArray: copy(buffer, maxlen, "Array");
		case JSONBoolean: copy(buffer, maxlen, "Boolean");
	}
}
