// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new g_iTestValue1 = 0
new Float:g_fTestValue2 = 0.0;

public plugin_init()
{
	register_plugin("Forward Test (Reference) (2)", "1.0", "Ni3znajomy");

	register_srvcmd("fwdref_set_test_values", "cmdSetTestValues");
}

public cmdSetTestValues()
{
	new sTestValue[10];

	read_argv(1, sTestValue, charsmax(sTestValue));
	g_iTestValue1 = str_to_num(sTestValue);
	
	read_argv(2, sTestValue, charsmax(sTestValue));
	g_fTestValue2 = str_to_float(sTestValue);

	server_print("PLUGIN2: TEST VALUES: val1 = %i | val2 = %f", g_iTestValue1, g_fTestValue2);
}

public multi_forward_reference(&val1, &Float:val2)
{
	server_print("PLUGIN2: MULTI FORWARD START: val1 = %i | val2 = %f", val1, val2);

	val1 = g_iTestValue1;
	val2 = g_fTestValue2;

	server_print("PLUGIN2: MULTI FORWARD END: val1 = %i | val2 = %f", val1, val2);
}

public one_forward_reference(&val1, &Float:val2)
{
	server_print("PLUGIN2: ONE FORWARD START: val1 = %i | val2 = %f", val1, val2);

	val1 = g_iTestValue1;
	val2 = g_fTestValue2;

	server_print("PLUGIN2: ONE FORWARD END: val1 = %i | val2 = %f", val1, val2);
}


