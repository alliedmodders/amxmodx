/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"
#include "svn_version.h"

void amx_command()
{
	const char* cmd = CMD_ARGV(1);
		
	if (!strcmp(cmd, "plugins") || !strcmp(cmd, "list"))
	{

		print_srvconsole("Currently loaded plugins:\n");
		print_srvconsole("       %-23.22s %-11.10s %-17.16s %-16.15s %-9.8s\n", "name", "version", "author", "file", "status");

		int plugins = 0;
		int	running = 0;

		CPluginMngr::iterator a = g_plugins.begin();
			
		while (a) 
		{
			++plugins;
			if ((*a).isValid() && !(*a).isPaused()) 
				++running;

			print_srvconsole(" [%3d] %-23.22s %-11.10s %-17.16s %-16.15s %-9.8s\n", plugins, (*a).getTitle(), (*a).getVersion(), (*a).getAuthor(), (*a).getName(), (*a).getStatus());
			++a;
		}

		a = g_plugins.begin();

		int num = 0;
		while (a)
		{
			num++;
			if ((*a).getStatusCode() == ps_bad_load)
			{
				//error
				print_srvconsole("(%3d) Load fails: %s\n", num, (*a).getError());
			} else if ( (*a).getStatusCode() == ps_error) {
				//error
				print_srvconsole("(%3d) Error: %s\n", num, (*a).getError());
			}
			++a;
		}

		print_srvconsole("%d plugins, %d running\n", plugins, running);
	}
	else if (!strcmp(cmd, "pause") && CMD_ARGC() > 2) 
	{
		const char* sPlugin = CMD_ARGV(2);

		CPluginMngr::CPlugin *plugin = g_plugins.findPlugin(sPlugin);

		if (plugin && plugin->isValid()) 
		{
			if (plugin->isPaused())
			{
				if (plugin->isStopped())
				{
					print_srvconsole("Plugin \"%s\" is stopped and may not be paused.\n",plugin->getName());
				}
				else
				{
					print_srvconsole("Plugin \"%s\" is already paused.\n",plugin->getName());
				}
			}
			else
			{
				plugin->pausePlugin();
				print_srvconsole("Paused plugin \"%s\"\n", plugin->getName());
			}
		}
		else 
		{
			print_srvconsole("Couldn't find plugin matching \"%s\"\n", sPlugin);
		}
	}
	else if (!strcmp(cmd, "unpause") && CMD_ARGC() > 2) 
	{
		const char* sPlugin = CMD_ARGV(2);

		CPluginMngr::CPlugin *plugin = g_plugins.findPlugin(sPlugin);

		if (plugin && plugin->isValid() && plugin->isPaused()) 
		{
			if (plugin->isStopped())
			{
				print_srvconsole("Plugin \"%s\" is stopped and may not be unpaused.\n", plugin->getName());
			}
			else
			{
				plugin->unpausePlugin();
				print_srvconsole("Unpaused plugin \"%s\"\n", plugin->getName());
			}
		}
		else if (!plugin)
		{
			print_srvconsole("Couldn't find plugin matching \"%s\"\n", sPlugin);
		} else {
			print_srvconsole("Plugin %s can't be unpaused right now.\n", sPlugin);
		}
	}
	else if (!strcmp(cmd, "cvars")) 
	{
		print_srvconsole("Registered cvars:\n");
		print_srvconsole("       %-24.23s %-24.23s %-16.15s\n", "name", "value", "plugin");

		int ammount = 0;

		if (CMD_ARGC() > 2) // Searching for cvars registered to a plugin
		{
			const char* targetname = CMD_ARGV(2);
			size_t len = strlen(targetname);
			for (CList<CCVar>::iterator a = g_cvars.begin(); a; ++a)
			{
				if (strncmp((*a).getPluginName(), targetname, len) == 0)
				{
					print_srvconsole(" [%3d] %-24.23s %-24.23s %-16.15s\n", ++ammount, (*a).getName(), CVAR_GET_STRING((*a).getName()), (*a).getPluginName());
				}
			}
		}
		else // No search
		{
			for (CList<CCVar>::iterator a = g_cvars.begin(); a; ++a)
			{
				print_srvconsole(" [%3d] %-24.23s %-24.23s %-16.15s\n", ++ammount, (*a).getName(), CVAR_GET_STRING((*a).getName()), (*a).getPluginName());
			}
		}
		
		print_srvconsole("%d cvars\n", ammount);
	}
	else if (!strcmp(cmd, "cmds")) 
	{
		print_srvconsole("Registered commands:\n");
		print_srvconsole("       %-24.23s %-16.15s %-8.7s %-16.15s\n", "name", "access", "type", "plugin");
				
		int ammount = 0;
		char access[32];

		CmdMngr::iterator a = g_commands.begin(CMD_ConsoleCommand);

		if (CMD_ARGC() > 2) // Searching for commands registered to a plugin
		{
			const char* targetname = CMD_ARGV(2);
			size_t len = strlen(targetname);
			while (a)
			{
				if (strncmp((*a).getPlugin()->getName(), targetname, len) == 0)
				{
					UTIL_GetFlags(access, (*a).getFlags());
					print_srvconsole(" [%3d] %-24.23s %-16.15s %-8.7s %-16.15s\n", ++ammount, (*a).getCmdLine(), access, (*a).getCmdType(), (*a).getPlugin()->getName());
				}
				++a;
			}
		}
		else // No search
		{
			while (a)
			{
				UTIL_GetFlags(access, (*a).getFlags());
				print_srvconsole(" [%3d] %-24.23s %-16.15s %-8.7s %-16.15s\n", ++ammount, (*a).getCmdLine(), access, (*a).getCmdType(), (*a).getPlugin()->getName());
				++a;
			}
		}
		print_srvconsole("%d commands\n",ammount);
	}
	else if (!strcmp(cmd, "version")) 
	{
		print_srvconsole("%s %s (%s)\n", Plugin_info.name, Plugin_info.version, Plugin_info.url);
		print_srvconsole("Authors:\n\tDavid \"BAILOPAN\" Anderson, Pavol \"PM OnoTo\" Marko\n");
		print_srvconsole("\tFelix \"SniperBeamer\" Geyer, Jonny \"Got His Gun\" Bergstrom\n");
		print_srvconsole("\tLukasz \"SidLuke\" Wlasinski, Christian \"Basic-Master\" Hammacher\n");
		print_srvconsole("\tBorja \"faluco\" Ferrer, Scott \"Damaged Soul\" Ehlert\n");
		print_srvconsole("Compiled: %s\n", __DATE__ ", " __TIME__);
		print_srvconsole("Build ID: %s\n", SVN_BUILD_ID);
#if defined JIT && !defined ASM32
		print_srvconsole("Core mode: JIT Only\n");
#elif !defined JIT && defined ASM32
		print_srvconsole("Core mode: ASM32 Only\n");
#elif defined JIT && defined ASM32
		print_srvconsole("Core mode: JIT+ASM32\n");
#else
		print_srvconsole("Core mode: Normal\n");
#endif
	}
	else if (!strcmp(cmd, "modules"))
	{
		print_srvconsole("Currently loaded modules:\n");
		print_srvconsole("      %-23.22s %-11.10s %-20.19s %-11.10s\n", "name", "version", "author", "status");

		int running = 0;
		int modules = 0;

		CList<CModule,const char *>::iterator a = g_modules.begin();

		while (a)
		{
			if ((*a).getStatusValue() == MODULE_LOADED)
				++running;
			++modules;

			print_srvconsole(" [%2d] %-23.22s %-11.10s %-20.19s %-11.10s\n", modules, (*a).getName(), (*a).getVersion(), (*a).getAuthor(), (*a).getStatus());
			++a;
		}

		print_srvconsole("%d modules, %d correct\n", modules, running);
	}
	else if (!strcmp(cmd, "gpl"))
	{
		print_srvconsole("AMX Mod X\n");
		print_srvconsole("\n");
		print_srvconsole(" by the AMX Mod X Development Team\n");
		print_srvconsole("  originally developed by OLO\n");
		print_srvconsole("\n");
		print_srvconsole("\n");
		print_srvconsole("  This program is free software; you can redistribute it and/or modify it\n");
		print_srvconsole("  under the terms of the GNU General Public License as published by the\n");
		print_srvconsole("  Free Software Foundation; either version 2 of the License, or (at\n");
		print_srvconsole("  your option) any later version.\n");
		print_srvconsole("\n");
		print_srvconsole("  This program is distributed in the hope that it will be useful, but\n");
		print_srvconsole("  WITHOUT ANY WARRANTY; without even the implied warranty of\n");
		print_srvconsole("  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU\n");
		print_srvconsole("  General Public License for more details.\n");
		print_srvconsole("\n");
		print_srvconsole("  You should have received a copy of the GNU General Public License\n");
		print_srvconsole("  along with this program; if not, write to the Free Software Foundation,\n");
		print_srvconsole("  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA\n");
		print_srvconsole("\n");
		print_srvconsole("  In addition, as a special exception, the author gives permission to\n");
		print_srvconsole("  link the code of this program with the Half-Life Game Engine (\"HL\n");
		print_srvconsole("  Engine\") and Modified Game Libraries (\"MODs\") developed by Valve,\n");
		print_srvconsole("  L.L.C (\"Valve\"). You must obey the GNU General Public License in all\n");
		print_srvconsole("  respects for all of the code used other than the HL Engine and MODs\n");
		print_srvconsole("  from Valve. If you modify this file, you may extend this exception\n");
		print_srvconsole("  to your version of the file, but you are not obligated to do so. If\n");
		print_srvconsole("  you do not wish to do so, delete this exception statement from your\n");
		print_srvconsole("  version.\n");
		print_srvconsole("\n");
	}
	else if (!strcmp(cmd, "\x74\x75\x72\x74\x6C\x65"))		// !! Hidden Command :D !!
	{
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2E\x2E\x3A\x3A\x3E\x3E\x3A\x3A\x3B\x3E\x5E\x27\x2E\x27\x27\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x27\x3A\x3A\x3F\x3D\x3E\x3E\x3E\x3E\x3E\x3D\x3F\x3E\x78\x2B\x3F\x3E\x3E\x3E\x3D\x3E\x3F\x2B\x3F\x3E\x3B\x2E\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x2E\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x27\x2C\x3A\x3E\x3B\x3F\x3D\x3E\x3B\x2E\x27\x5E\x5E\x3B\x3B\x2C\x3A\x3F\x3F\x3D\x78\x3F\x3B\x3E\x3A\x3B\x3A\x5E\x3B\x3D\x3E\x2B\x2B\x2B\x2B\x3D\x2C\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x2C\x3E\x37\x24\x24\x78\x3D\x3D\x3D\x3F\x3A\x27\x20\x20\x20\x20\x20\x20\x20\x2E\x3A\x3B\x3D\x3E\x3A\x3A\x3A\x3A\x3F\x3F\x3F\x3E\x5E\x2C\x2E\x2E\x2C\x2C\x2C\x2C\x3A\x3B\x3D\x3D\x3B\x5E\x2C\x2C\x2C\x3A\x5E\x3A\x3F\x3F\x3E\x3D\x3D\x3E\x3E\x2B\x3B\x27\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x2C\x3D\x2B\x3E\x2C\x5E\x3D\x79\x24\x33\x78\x33\x24\x5A\x24\x3B\x20\x20\x3A\x3E\x2B\x3E\x3D\x3F\x5E\x2C\x2C\x2C\x5E\x5E\x3E\x3D\x3E\x3B\x3B\x3A\x5E\x5E\x3E\x3F\x3D\x2B\x37\x3D\x3F\x3E\x3E\x3E\x3F\x3D\x3F\x3F\x3D\x3D\x3D\x3D\x3E\x3F\x3D\x3E\x3E\x3E\x3D\x5A\x78\x3E\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x3D\x5A\x24\x37\x78\x66\x68\x78\x5A\x5A\x24\x79\x79\x71\x23\x23\x4D\x71\x3B\x3A\x3B\x3A\x3E\x3B\x3B\x2C\x5E\x3E\x3F\x3D\x3F\x3A\x2C\x2C\x3A\x3B\x3B\x3E\x3E\x3D\x2B\x3D\x3E\x3D\x3B\x3A\x3E\x3D\x2B\x3D\x2B\x37\x2B\x3D\x2B\x37\x37\x2B\x2B\x33\x33\x33\x37\x37\x24\x5A\x79\x3A\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x5E\x2B\x5A\x2B\x3E\x3D\x37\x5A\x66\x40\x40\x23\x40\x48\x23\x23\x23\x38\x5E\x3B\x3D\x3F\x2B\x3E\x3B\x3E\x5E\x5E\x2C\x27\x2E\x27\x2E\x2E\x5E\x3F\x3D\x3D\x3F\x3A\x3B\x3A\x3A\x3A\x5E\x5E\x3E\x3E\x3F\x3D\x37\x37\x3D\x3D\x37\x2B\x3D\x37\x2B\x37\x78\x24\x79\x38\x68\x45\x48\x79\x3E\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x2C\x3E\x3E\x78\x33\x68\x48\x23\x23\x40\x40\x48\x45\x66\x33\x20\x2C\x3A\x3E\x3E\x3E\x3B\x3B\x3A\x3A\x2C\x2E\x2C\x5E\x3A\x2C\x5E\x3B\x3E\x37\x37\x3F\x3B\x3A\x2E\x3A\x3A\x3B\x3D\x3B\x3B\x3D\x2B\x3D\x78\x33\x37\x3E\x3D\x3D\x2B\x37\x2B\x78\x78\x78\x78\x5A\x66\x71\x68\x38\x45\x27\x20\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x3A\x37\x37\x24\x66\x71\x45\x45\x71\x45\x3A\x3A\x2C\x5E\x3A\x3E\x3A\x3A\x3B\x3B\x5E\x3A\x2C\x5E\x5E\x2C\x2C\x5E\x3A\x3E\x2B\x33\x3D\x3E\x3A\x3A\x3A\x3D\x2B\x2B\x3D\x3F\x3F\x37\x37\x2B\x37\x3D\x3D\x5A\x33\x78\x33\x37\x78\x24\x5A\x33\x37\x38\x40\x71\x38\x66\x40\x2C\x20\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x5E\x2B\x5A\x45\x40\x5E\x5E\x5E\x3A\x2C\x3A\x3B\x3E\x3A\x5E\x5E\x2C\x2E\x2E\x2C\x5E\x3B\x3B\x3A\x2B\x3E\x3F\x3B\x3F\x3F\x3F\x3F\x3E\x3F\x3D\x37\x3B\x3B\x3D\x33\x2B\x3D\x3D\x78\x78\x5A\x78\x33\x78\x5A\x5A\x5A\x24\x71\x48\x79\x5A\x24\x79\x45\x3E\x20\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x27\x3B\x2C\x2C\x27\x5E\x5E\x2C\x3A\x5E\x3A\x3A\x5E\x3A\x3B\x3F\x3E\x3F\x3E\x3B\x3E\x3E\x3F\x3D\x2B\x37\x37\x2B\x2B\x3D\x2B\x37\x2B\x37\x37\x2B\x3B\x3D\x33\x2B\x2B\x37\x37\x2B\x3D\x78\x78\x66\x78\x78\x37\x33\x66\x78\x38\x23\x23\x27\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x3A\x3F\x3B\x5E\x2C\x3B\x3F\x3D\x3F\x3F\x3B\x3A\x3A\x3A\x3E\x3F\x3E\x3E\x3F\x3A\x3F\x33\x78\x78\x33\x24\x24\x33\x2B\x37\x78\x24\x78\x33\x3D\x2B\x2B\x5A\x24\x78\x24\x78\x33\x33\x24\x5A\x79\x24\x24\x24\x68\x45\x48\x38\x68\x45\x40\x3E\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2C\x3A\x3E\x3F\x37\x3D\x3E\x3F\x2B\x3F\x3F\x3E\x3F\x3F\x3F\x3D\x3F\x3E\x3F\x3D\x37\x2B\x3E\x3E\x2B\x37\x37\x33\x37\x33\x78\x33\x33\x33\x78\x37\x37\x37\x78\x5A\x78\x5A\x79\x79\x5A\x24\x79\x79\x79\x79\x79\x68\x71\x38\x38\x71\x23\x23\x45\x37\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x5E\x2B\x2B\x3F\x2B\x78\x40\x38\x3F\x3B\x3E\x3B\x3B\x3E\x3F\x37\x2B\x3F\x3F\x3D\x3D\x3E\x3F\x2B\x37\x37\x37\x37\x33\x33\x78\x78\x33\x37\x24\x5A\x78\x5A\x5A\x78\x24\x33\x3D\x37\x37\x37\x78\x24\x5A\x78\x37\x37\x78\x66\x79\x66\x71\x66\x40\x45\x40\x3A\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2B\x3A\x3F\x2B\x3D\x2B\x79\x23\x79\x3B\x2C\x3A\x3A\x3A\x37\x78\x3F\x3E\x3B\x3E\x3B\x3E\x3D\x37\x24\x33\x37\x33\x37\x78\x78\x33\x24\x68\x79\x33\x24\x78\x2B\x33\x33\x5A\x79\x24\x5A\x79\x24\x5A\x37\x24\x5A\x5A\x66\x38\x66\x79\x66\x40\x71\x45\x48\x5A\x3A\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x5E\x37\x3D\x37\x37\x33\x37\x37\x66\x45\x5A\x3F\x5E\x5E\x78\x37\x3D\x3F\x3E\x3B\x3B\x3E\x2B\x2B\x24\x78\x37\x2B\x37\x2B\x37\x78\x78\x71\x79\x33\x33\x24\x24\x78\x24\x5A\x3F\x37\x78\x24\x78\x79\x66\x5A\x78\x79\x66\x79\x68\x79\x66\x5A\x33\x3F\x3D\x3D\x20\x20\x20\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x5E\x2B\x3D\x3F\x37\x37\x78\x33\x5A\x4E\x4D\x23\x38\x33\x3F\x3E\x3B\x3E\x3B\x3D\x3D\x33\x66\x24\x78\x33\x2B\x78\x24\x5A\x24\x5A\x71\x79\x78\x33\x33\x78\x79\x5A\x5A\x33\x66\x24\x78\x78\x24\x79\x5A\x24\x79\x5A\x37\x66\x24\x3D\x3B\x66\x23\x4D\x4D\x4D\x79\x3B\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x3A\x33\x37\x37\x24\x78\x66\x79\x48\x4D\x4D\x4D\x4D\x23\x71\x68\x5A\x24\x5A\x79\x68\x68\x5A\x5A\x24\x79\x66\x68\x78\x5A\x4E\x45\x66\x66\x45\x45\x45\x24\x5A\x40\x71\x68\x5A\x68\x5A\x37\x66\x79\x78\x37\x78\x37\x68\x38\x38\x71\x48\x40\x23\x45\x3A\x3D\x37\x45\x27\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x37\x37\x78\x37\x33\x38\x45\x45\x71\x20\x5E\x3D\x2B\x3F\x2B\x2B\x79\x71\x45\x48\x40\x45\x45\x45\x45\x45\x71\x40\x40\x71\x38\x38\x79\x66\x38\x68\x48\x48\x45\x66\x37\x2B\x3A\x37\x3F\x3B\x3A\x2C\x27\x2C\x27\x78\x4D\x23\x48\x48\x48\x79\x2B\x3A\x3F\x79\x27\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x3A\x78\x78\x24\x40\x4E\x4E\x4D\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x5E\x3E\x3E\x3F\x3E\x3E\x3E\x3E\x3B\x3B\x3B\x3A\x3A\x3F\x3E\x3A\x2E\x2E\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2E\x45\x4D\x40\x45\x78\x5E\x33\x68\x33\x2B\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x24\x48\x45\x48\x78\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2B\x4E\x40\x2B\x66\x33\x78\x20\x20\n");
		print_srvconsole("\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x20\x2B\x2C\x20\x3A\x20\x20\n");
	} else {
		print_srvconsole("Usage: amxx < command > [ argument ]\n");
		print_srvconsole("Commands:\n");
		print_srvconsole("   version                - display amxx version info\n");
		print_srvconsole("   gpl                    - print the license\n");
		print_srvconsole("   plugins                - list plugins currently loaded\n");
		print_srvconsole("   modules                - list modules currently loaded\n");
		print_srvconsole("   cvars [ plugin ]       - list cvars registered by plugins\n");
		print_srvconsole("   cmds [ plugin ]        - list commands registered by plugins\n");
		print_srvconsole("   pause < plugin >       - pause a running plugin\n");
		print_srvconsole("   unpause < plugin >     - unpause a previously paused plugin\n");
	}
}

void plugin_srvcmd()
{
	const char* cmd = CMD_ARGV(0);
				
	CmdMngr::iterator a = g_commands.srvcmdbegin();
			
	while (a)
	{
		if ((*a).matchCommand(cmd) && (*a).getPlugin()->isExecutable((*a).getFunction()))
		{
			cell ret = executeForwards((*a).getFunction(), static_cast<cell>(g_srvindex),
				static_cast<cell>((*a).getFlags()), static_cast<cell>((*a).getId()));
			if (ret) break;
		}
		++a;
	}
}
