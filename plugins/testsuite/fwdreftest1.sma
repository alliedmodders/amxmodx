// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new g_hMultiForward;
new g_hOneForward;

public plugin_init()
{
	register_plugin("Forward Test (Reference) (1)", "1.0", "Ni3znajomy");

	g_hMultiForward = CreateMultiForward("multi_forward_reference", ET_IGNORE, FP_VAL_BYREF, FP_VAL_BYREF);
	g_hOneForward = CreateOneForward(find_plugin_byfile("fwdreftest2.amxx"), "one_forward_reference", FP_VAL_BYREF, FP_VAL_BYREF);

	register_srvcmd("fwdref_multi_test", "cmdForwardRefMultiTest");
	register_srvcmd("fwdref_one_test", "cmdForwardRefOneTest");
}

public cmdForwardRefMultiTest()
{
	new sTestValue[10];

	read_argv(1, sTestValue, charsmax(sTestValue));
	new iTestValue1 = str_to_num(sTestValue);
	
	read_argv(2, sTestValue, charsmax(sTestValue));
	new Float:fTestValue2 = str_to_float(sTestValue);

	server_print("PLUGIN1: MULTI FORWARD START: val1 = %i | val2 = %f", iTestValue1, fTestValue2);
	new dump;
	ExecuteForward(g_hMultiForward, dump, iTestValue1, fTestValue2);
	server_print("PLUGIN1: MULTI FORWARD END: val1 = %i | val2 = %f", iTestValue1, fTestValue2);
}

public cmdForwardRefOneTest()
{
	new sTestValue[10];

	read_argv(1, sTestValue, charsmax(sTestValue));
	new iTestValue1 = str_to_num(sTestValue);
	
	read_argv(2, sTestValue, charsmax(sTestValue));
	new Float:fTestValue2 = str_to_float(sTestValue);

	server_print("PLUGIN1: ONE FORWARD START: val1 = %i | val2 = %f", iTestValue1, fTestValue2);
	new dump;
	ExecuteForward(g_hOneForward, dump, iTestValue1, fTestValue2);
	server_print("PLUGIN1: ONE FORWARD END: val1 = %i | val2 = %f", iTestValue1, fTestValue2);
}





