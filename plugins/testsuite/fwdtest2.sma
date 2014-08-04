// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new g_id

native test_createforward(function[])
native test_executeforward()

public plugin_init()
{
	g_id = register_plugin("Forward Test (Client)", "1.0", "Belsebub")
	
	register_srvcmd("fwd_test1", "Test_Forward1")
}

public Test_Forward1()
{
	server_print("Executing forward ^"gaben^" (I'm %d)", g_id)
	test_createforward("gaben")
	test_executeforward()	
}

public gaben()
{
	server_print("gaben executed (I'm %d)", g_id)
}
