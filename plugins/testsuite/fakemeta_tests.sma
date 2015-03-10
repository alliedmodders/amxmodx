// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>
#include <fakemeta>

public plugin_init()
{
	register_plugin("Fakemeta Tests", "1.0", "BAILOPAN")
	register_forward(FM_ServerDeactivate, "Hook_ServerDeactivate")
}

public Hook_ServerDeactivate()
{
	server_print("[FAKEMETA TEST] ServerDeactivate() at %f", get_gametime())
}

public plugin_end()
{
	server_print("[FAKEMETA TEST] plugin_end() at %f", get_gametime())
}
