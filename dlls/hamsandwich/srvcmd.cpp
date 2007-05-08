#include "sdk/amxxmodule.h"

#include <stdarg.h>

#include "CVector.h"

#include "ham_const.h"
#include "hooklist.h"
#include "offsets.h"
#include "forward.h"
#include "hook.h"


extern hook_t hooklist[];
extern CVector<Hook *> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];

void print_srvconsole(char *fmt, ...)
{
	va_list argptr;
	static char string[384];
	va_start(argptr, fmt);
	vsnprintf(string, sizeof(string) - 1, fmt, argptr);
	string[sizeof(string) - 1] = '\0';
	va_end(argptr);
	
	SERVER_PRINT(string);
}



void HamCommand(void)
{
	const char *cmd=CMD_ARGV(1);

	if (strcmp(cmd, "list")==0)
	{
		unsigned int Total=0;
		print_srvconsole("%-24s | %3s | %10s\n","Name","Set","Value");
		print_srvconsole("-------------------------------------------\n");
		print_srvconsole("%-24s |  %s  | %10d\n", "pev", Offsets.IsPevSet() == 0 ? "n" : "y", Offsets.GetPev());
		print_srvconsole("%-24s |  %s  | %10d\n", "base", Offsets.IsBaseSet() == 0 ? "n" : "y", Offsets.GetBase());

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
			print_srvconsole("%-24s |  %s  | %10d\n", hooklist[i].name, hooklist[i].isset == 0 ? "n" : "y", hooklist[i].vtid);

			if (hooklist[i].isset != 0)
			{
				Total++;
			}
			count++;

			if (count >= 5)
			{
				count = 0;
				print_srvconsole("-------------------------------------------\n");

			}


		}
		print_srvconsole("\n%u keys, %u set.\n\n", HAM_LAST_ENTRY_DONT_USE_ME_LOL, Total);
		return;
	}
	else if (strcmp(cmd, "hooks")==0)
	{
		print_srvconsole("%-24s | %-27s | %10s | %10s\n", "Key", "Classname", "Pre", "Post");
		print_srvconsole("--------------------------------------------------------------------------------\n");
		unsigned int ForwardCount=0;
		unsigned int HookCount=0;
		int count = 0;
		for (int i=0; i<HAM_LAST_ENTRY_DONT_USE_ME_LOL; i++)
		{
			CVector<Hook *>::iterator end=hooks[i].end();

			for (CVector<Hook *>::iterator j=hooks[i].begin();
				 j!=end;
				 ++j)
			{
				HookCount++;
				ForwardCount+=(*j)->pre.size() + (*j)->post.size();

				print_srvconsole("%-24s | %-27s | %10d | %10d\n",hooklist[i].name, (*j)->ent, (*j)->pre.size(), (*j)->post.size());
				if (count >= 5)
				{
					print_srvconsole("--------------------------------------------------------------------------------\n");
				}
			}
		}
		print_srvconsole("\n%u hooks, %u forwards.\n\n", HookCount, ForwardCount);
		return;
	}

	// Unknown command
	print_srvconsole("Usage: ham < command > [ argument ]\n");
	print_srvconsole("Commands:\n");
	print_srvconsole("   %-22s - %s\n", "list", "list all keys and their values from the config file.");
	print_srvconsole("   %-22s - %s\n", "hooks", "list all active hooks");
}

