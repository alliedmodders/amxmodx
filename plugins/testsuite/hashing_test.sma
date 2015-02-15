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
	register_plugin("Hashing Test", "1.0", "Hattrick (Claudiu HKS)");
}

public client_command(Id)
{
	new Command[64], StringOrFile[8], Data[64], HashTypeStr[4], Output[256], HashType:Type;

	if (is_user_connected(Id) && !is_user_bot(Id) && !is_user_hltv(Id))
	{
		read_argv(0, Command, charsmax(Command));
		read_argv(1, StringOrFile, charsmax(StringOrFile));
		read_argv(2, Data, charsmax(Data));
		read_argv(3, HashTypeStr, charsmax(HashTypeStr));

		if (equali(Command, "Hash"))
		{
			if (equali(StringOrFile, "File"))
			{
				Type = HashType:str_to_num(HashTypeStr);

				hash_file(Data, Type, Output, charsmax(Output));
				log_amx("Original: %s Hashed: %s", Data, Output);
			}

			else if (equali(StringOrFile, "String"))
			{
				Type = HashType:str_to_num(HashTypeStr);

				hash_string(Data, Type, Output, charsmax(Output));
				log_amx("Original: %s Hashed: %s", Data, Output);
			}
		}
	}
}
