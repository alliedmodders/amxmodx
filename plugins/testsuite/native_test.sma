// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

native Factorial(num)

public __Factorial(id, num)
{
	new num = get_param(1)
	if (num == 0)
	{
		return 1
	}
	
	return num * Factorial(num - 1)
}

public plugin_natives()
{
	register_native("Factorial", "__Factorial")
}

public plugin_init()
{
	register_plugin("Native Test", "1.0", "BAILOPAN")
	register_srvcmd("test_native1", "Command_TestNative1")
}

public Command_TestNative1()
{
	new num = Factorial(6)
	server_print("Factorial of 6 is: %d", num)
}
