// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include <amxmodx_version.h>

void amx_command()
{
	const char* cmd = CMD_ARGV(1);
	const char* search = nullptr;
	if (CMD_ARGC() > 2)
	{
		search = CMD_ARGV(2);
		// Ignore empty search criteria
		if (!(*search))
			search = nullptr;
	}

	if (!strcmp(cmd, "plugins") || !strcmp(cmd, "list"))
	{
		print_srvconsole("Currently loaded plugins:\n");
		print_srvconsole("       %-23.22s %-11.10s %-17.16s %-16.15s %-9.8s\n", "name", "version", "author", "file", "status");

		int plugins = 0;
		int	running = 0;

		CPluginMngr::iterator a = g_plugins.begin();

		while (a)
		{
			if (!search || stristr((*a).getTitle(), search) != nullptr || stristr((*a).getName(), search) != nullptr || stristr((*a).getAuthor(), search) != nullptr)
			{
				++plugins;
				if ((*a).isValid() && !(*a).isPaused())
					++running;

				print_srvconsole(" [%3d] %-23.22s %-11.10s %-17.16s %-16.15s %-9.8s\n", plugins, (*a).getTitle(), (*a).getVersion(), (*a).getAuthor(), (*a).getName(), (*a).getStatus());
			}
			++a;
		}

		a = g_plugins.begin();

		int num = 0;
		while (a)
		{
			if (!search || stristr((*a).getTitle(), search) != nullptr || stristr((*a).getName(), search) != nullptr || stristr((*a).getAuthor(), search) != nullptr)
			{
				num++;
				if ((*a).getStatusCode() == ps_bad_load)
					print_srvconsole("(%3d) Load fails: %s\n", num, (*a).getError());
				else if ((*a).getStatusCode() == ps_error)
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
					print_srvconsole("Plugin \"%s\" is stopped and may not be paused.\n", plugin->getName());
				}
				else
				{
					print_srvconsole("Plugin \"%s\" is already paused.\n", plugin->getName());
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
		}
		else {
			print_srvconsole("Plugin %s can't be unpaused right now.\n", sPlugin);
		}
	}
	else if (!strcmp(cmd, "cvars"))
	{
		g_CvarManager.OnConsoleCommand();
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
		print_srvconsole("%d commands\n", ammount);
	}
	else if (!strcmp(cmd, "version"))
	{
		print_srvconsole("%s %s (%s)\n", Plugin_info.name, Plugin_info.version, Plugin_info.url);
		print_srvconsole("Authors:\n\tDavid \"BAILOPAN\" Anderson, Pavol \"PM OnoTo\" Marko\n");
		print_srvconsole("\tFelix \"SniperBeamer\" Geyer, Jonny \"Got His Gun\" Bergstrom\n");
		print_srvconsole("\tLukasz \"SidLuke\" Wlasinski, Christian \"Basic-Master\" Hammacher\n");
		print_srvconsole("\tBorja \"faluco\" Ferrer, Scott \"DS\" Ehlert\n");
		print_srvconsole("Compiled: %s\n", AMXX_BUILD_TIME);
#if defined(AMXX_GENERATED_BUILD)
		print_srvconsole("Built from: https://github.com/alliedmodders/amxmodx/commit/%s\n", AMXX_SHA);
		print_srvconsole("Build ID: %s:%s\n", AMXX_LOCAL_REV, AMXX_SHA);
#endif
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

		for (auto &module : g_modules)
		{
			if (module->getStatusValue() == MODULE_LOADED)
				++running;
			++modules;

			print_srvconsole(" [%2d] %-23.22s %-11.10s %-20.19s %-11.10s\n", modules, module->getName(), module->getVersion(), module->getAuthor(), module->getStatus());
		}

		print_srvconsole("%d modules, %d correct\n", modules, running);
	}
	else if (!strcmp(cmd, "gpl"))
	{
		print_srvconsole("AMX Mod X\n");
		print_srvconsole("\n");
		print_srvconsole(" Based on AMX Mod by Aleksander Naszko (\"OLO\").\n");
		print_srvconsole(" Copyright (C) The AMX Mod X Development Team.\n");
		print_srvconsole("\n");
		print_srvconsole(" This software is licensed under the GNU General Public License, version 3 or\n");
		print_srvconsole(" higher. Additional exceptions apply. For full license details, see LICENSE.txt\n");
		print_srvconsole(" or visit:\n");
		print_srvconsole("      https://alliedmods.net/amxmodx-license\n");
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
	}
	else
	{
		print_srvconsole("Usage: amxx < command > [ argument ]\n");
		print_srvconsole("Commands:\n");
		print_srvconsole("   version                    - display amxx version info\n");
		print_srvconsole("   gpl                        - print the license\n");
		print_srvconsole("   plugins [ criteria ]       - list plugins currently loaded or ones matching given search criteria\n");
		print_srvconsole("   modules                    - list modules currently loaded\n");
		print_srvconsole("   cvars [ plugin ] [ index ] - list cvars handled by amxx or show information about a cvar if index is provided\n");
		print_srvconsole("   cmds [ plugin ]            - list commands registered by plugins\n");
		print_srvconsole("   pause < plugin >           - pause a running plugin\n");
		print_srvconsole("   unpause < plugin >         - unpause a previously paused plugin\n");
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
