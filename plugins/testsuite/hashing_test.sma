// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx>

new const g_hashTypes[HashType][] =
{
	"CRC32",
	"MD5",
	"SHA1",
	"SHA256",
	"SHA3 224",
	"SHA3 256",
	"SHA3 384",
	"SHA3 512",
	"Keccak 224",
	"Keccak 256",
	"Keccak 384",
	"Keccak 512"
};

public plugin_init()
{
	register_plugin("Hashing Test", "1.0", "Hattrick (Claudiu HKS)");
	register_srvcmd("hash_string", "cmdHashString");
	register_srvcmd("hash_file", "cmdHashFile");
}

public cmdHashString()
{
	if (read_argc() < 2)
	{
		server_print("Specify string to be hashed.");
		return PLUGIN_HANDLED;
	}

	new String[256], Output[256], HashType:Type;
	read_argv(1, String, charsmax(String));

	log_amx("Hashing string %s...", String);
	log_amx("-----------------------------------");

	for (Type = Hash_Crc32; Type < any:sizeof g_hashTypes; Type++)
	{
		hash_string(String, Type, Output, charsmax(Output));
		log_amx("%s :  %s", g_hashTypes[Type], Output);
	}

	return PLUGIN_HANDLED;
}

public cmdHashFile()
{
	if (read_argc() < 2)
	{
		server_print("Specify file to be hashed.");
		return PLUGIN_HANDLED;
	}

	new File[256], Output[256], HashType:Type;
	read_argv(1, File, charsmax(File));

	log_amx("Hashing file %s...", File);
	log_amx("-----------------------------------");

	for (Type = Hash_Crc32; Type < any:sizeof g_hashTypes; Type++)
	{
		hash_file(File, Type, Output, charsmax(Output));
		log_amx("%s :  %s", g_hashTypes[Type], Output);
	}

	return PLUGIN_HANDLED;
}
