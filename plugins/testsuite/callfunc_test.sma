// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

public plugin_init()
{
	register_plugin("callfunc test", "1.0", "BAILOPAN")
	
	register_srvcmd("test_callfunc", "Command_Callfunc")
}

public OnCallfuncReceived(num, str[], &val, array[], array2[], size, hello2[1])
{
	server_print("num = %d (expected: %d)", num, 5)
	server_print("str[] = ^"%s^" (expected: %s)", str, "Gaben")

	server_print("val = %d (expected %d, setting to %d)", val, 62, 15)
	val = 15
	server_print("printing %d elements of array[] (expected: %d)", size, 6)
	for (new i=0; i<size; i++)
	{
		server_print("array[%d] = %d (expected: %d)", i, array[i], i)
	}
	for (new i=0; i<size; i++)
	{
		server_print("array2[%d] = %d (expected: %d)", i, array[i], i)
	}
	array[0] = 5
	array2[1] = 6
	hello2[0] = 25
}

public Command_Callfunc()
{
	new a = 62
	new hello[] = {0,1,2,3,4,5}
	new hello2[] = {9}
	new pm = 6
	new err
	
	if ((err=callfunc_begin("OnCallfuncReceived")) < 1)
	{
		server_print("Failed to call callfunc_begin()! Error: %d", err)
		
		return PLUGIN_HANDLED
	}
	callfunc_push_int(5)
	callfunc_push_str("Gaben")
	callfunc_push_intrf(a)
	callfunc_push_array(hello, pm)
	callfunc_push_array(hello, pm)
	callfunc_push_int(pm)
	callfunc_push_array(hello2, 1, false)
	callfunc_end()
	
	server_print("a = %d (expected: %d)", a, 15)
	server_print("hello[0] = %d (expected: %d)", hello[0], 5)
	server_print("hello[1] = %d (expected: %d)", hello[1], 6)
	server_print("hello2[0] = %d (expected: %d)", hello2[0], 9)
	
	return PLUGIN_HANDLED
}
