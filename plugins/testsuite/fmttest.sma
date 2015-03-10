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
	register_plugin("Format Test", "1.0", "BAILOPAN")
	
	register_srvcmd("test_format", "Command_TestFormat")
	register_srvcmd("test_replace", "Command_TestReplace")
}

public gabprint(const fmt[], ...)
{
	static buffer[2048]
	vformat(buffer, 2047, fmt, 2)
	
	server_print("%s", buffer)
}

public Command_TestFormat()
{
	server_print("Printing -1 with d: %d", -1)
	server_print("Printing -1 with u: %u", -1)
	server_print("Printing (1<<31) with d: %d", (1<<31))
	server_print("Printing (1<<31) with u: %u", (1<<31))
	server_print("Printing 1 with d: %d", 1)
	server_print("Printing 1 with u: %u", 1)
}

public Command_TestReplace()
{
	new message[192] = "^"@test^""
	
	replace_all(message, 191, "^"", "")
	server_print("Got: %s (expected: %s)", message, "@test")
	
	copy(message, 191, "test")
	replace_all(message, 191, "t", "tt")
	server_print("Got: %s (expected: %s)", message, "ttestt")
	
	replace_all(message, 191, "tt", "")
	server_print("Got: %s (expected: %s)", message, "es")
	
	copy(message, 191, "good boys do fine always")
	replace_all(message, 191, " ", "-----")
	server_print("Got %s (expected: %s)", message, "good-----boys-----do-----fine-----always")
	
	copy(message, 191, "-----")
	replace_all(message, 191, "-", "")
	server_print("Got ^"%s%^" (expected: ^"%s%^")", message, "")
	
	copy(message, 191, "-----")
	replace_all(message, 191, "--", "")
	server_print("Got ^"%s%^" (expected: ^"%s%^")", message, "-")
	
	copy(message, 191, "aaaa")
	replace_all(message, 191, "a", "Aaa")
	server_print("Got %s (expected: %s)", message, "AaaAaaAaaAaa")
}
