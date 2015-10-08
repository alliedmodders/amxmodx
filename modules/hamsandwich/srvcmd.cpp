// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#include "amxxmodule.h"
#include <amtl/am-vector.h>
#include "ham_const.h"
#include "hooklist.h"
#include "offsets.h"
#include "hook.h"

extern hook_t hooklist[];
extern ke::Vector<Hook *> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];

void HamCommand(void)
{
	const char *cmd=CMD_ARGV(1);

	if (strcmp(cmd, "list")==0)
	{
		unsigned int Total=0;

		MF_PrintSrvConsole("%-24s | %10s\n","Name","Set","Value");
		MF_PrintSrvConsole("------------------------------------\n");
		MF_PrintSrvConsole("%-24s | %10d\n", "pev", Offsets.GetPev());
		MF_PrintSrvConsole("%-24s | %10d\n", "base", Offsets.GetBase());

		if (Offsets.IsPevSet())
		{
			Total++;
		}
		if (Offsets.IsBaseSet())
		{
			Total++;
		}

		int count=2;
		for (int i=0; i<HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
		{


			if (hooklist[i].isset != 0)
			{
				MF_PrintSrvConsole("%-24s | %10d\n", hooklist[i].name, hooklist[i].vtid);
				Total++;
				count++;
			}

			if (count >= 5)
			{
				count = 0;
				MF_PrintSrvConsole("------------------------------------\n");

			}


		}
		MF_PrintSrvConsole("\n%u keys, %u set.\n\n", HAM_LAST_ENTRY_DONT_USE_ME_LOL, Total);
		return;
	}
	else if (strcmp(cmd, "hooks")==0)
	{
		MF_PrintSrvConsole("%-24s | %-27s | %10s | %10s\n", "Key", "Classname", "Pre", "Post");
		MF_PrintSrvConsole("--------------------------------------------------------------------------------\n");
		unsigned int ForwardCount=0;
		unsigned int HookCount=0;
		int count = 0;
		for (int i=0; i<HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
		{
			for (size_t j = 0; j < hooks[i].length(); ++j)
			{
				HookCount++;
				ForwardCount += hooks[i].at(j)->pre.length() + hooks[i].at(j)->post.length();

				MF_PrintSrvConsole("%-24s | %-27s | %10d | %10d\n", hooklist[i].name, hooks[i].at(j)->ent, hooks[i].at(j)->pre.length(), hooks[i].at(j)->post.length());
				if (count >= 5)
				{
					MF_PrintSrvConsole("--------------------------------------------------------------------------------\n");
				}
			}
		}
		MF_PrintSrvConsole("\n%u hooks, %u forwards.\n\n", HookCount, ForwardCount);
		return;
	}

	// Unknown command
	MF_PrintSrvConsole("Usage: ham < command > [ argument ]\n");
	MF_PrintSrvConsole("Commands:\n");
	MF_PrintSrvConsole("   %-22s - %s\n", "list", "list all keys and their values from the config file.");
	MF_PrintSrvConsole("   %-22s - %s\n", "hooks", "list all active hooks");
}
