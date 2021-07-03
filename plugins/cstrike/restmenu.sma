// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Restrict Weapons Plugin
//

#include <amxmodx>
#include <amxmisc>
#include <cstrike>

new const PluginName[] = "Restrict Weapons";

const MaxAliasNameLength    = 16;
const MaxItemNameLength     = 32;
const MaxMenuTitleLength    = 48;
const MaxCommandAliasLength = 12;
const MaxConfigFileLength   = 48;
const MaxConsoleLength      = 128;
const MaxMapLength          = 32;

new bool:BlockedItems[CSI_MAX_COUNT];
new bool:ModifiedItem;
new bool:ConfigsExecuted;

new MenuPosition[MAX_PLAYERS + 1];
new MenuHandle  [MAX_PLAYERS + 1] = { -1, ... };
new ConfigFilePath[PLATFORM_MAX_PATH];

new RestrictedBotWeapons[] = "00000000000000000000000000";
new RestrictedBotEquipAmmos[] = "000000000";

new CvarPointerAllowMapSettings;
new CvarPointerRestrictedWeapons;
new CvarPointerRestrictedEquipAmmos;

enum MenuTitle
{
	m_Title[MaxMenuTitleLength],
	m_Alias[MaxCommandAliasLength],
};

#define TITLE(%0)  "MENU_TITLE_" + #%0

new const MenuInfos[][MenuTitle] =
{
	{ TITLE(HANDGUNS)    , "pistol"  },
	{ TITLE(SHOTGUNS)    , "shotgun" },
	{ TITLE(SUBMACHINES) , "sub"     },
	{ TITLE(RIFLES)      , "rifle"   },
	{ TITLE(SNIPERS)     , "sniper"  },
	{ TITLE(MACHINE)     , "machine" },
	{ TITLE(EQUIPMENT)   , "equip"   },
	{ TITLE(AMMUNITION)  , "ammo"    },
}

enum MenuItem
{
	m_Index,
	m_Name[MaxItemNameLength],
};

#define ITEM(%0)  { CSI_%0, "MENU_ITEM_" + #%0 }
#define ITEM_NONE { CSI_NONE, "" }

new const ItemsInfos[][][MenuItem] =
{
	{ ITEM(USP)    , ITEM(GLOCK18) , ITEM(DEAGLE)   , ITEM(P228)     , ITEM(ELITE)       , ITEM(FIVESEVEN), ITEM_NONE , ITEM_NONE    },
	{ ITEM(M3)     , ITEM(XM1014)  , ITEM_NONE      , ITEM_NONE      , ITEM_NONE         , ITEM_NONE      , ITEM_NONE , ITEM_NONE    },
	{ ITEM(MP5NAVY), ITEM(TMP)     , ITEM(P90)      , ITEM(MAC10)    , ITEM(UMP45)       , ITEM_NONE      , ITEM_NONE , ITEM_NONE    },
	{ ITEM(AK47)   , ITEM(SG552)   , ITEM(M4A1)     , ITEM(GALIL)    , ITEM(FAMAS)       , ITEM(AUG)      , ITEM_NONE , ITEM_NONE    },
	{ ITEM(SCOUT)  , ITEM(AWP)     , ITEM(G3SG1)    , ITEM(SG550)    , ITEM_NONE         , ITEM_NONE      , ITEM_NONE , ITEM_NONE    },
	{ ITEM(M249)   , ITEM_NONE     , ITEM_NONE      , ITEM_NONE      , ITEM_NONE         , ITEM_NONE      , ITEM_NONE , ITEM_NONE    },
	{ ITEM(VEST)   , ITEM(VESTHELM), ITEM(FLASHBANG), ITEM(HEGRENADE), ITEM(SMOKEGRENADE), ITEM(DEFUSER)  , ITEM(NVGS), ITEM(SHIELD) },
	{ ITEM(PRIAMMO), ITEM(SECAMMO) , ITEM_NONE      , ITEM_NONE      , ITEM_NONE         , ITEM_NONE      , ITEM_NONE , ITEM_NONE    },
};


public plugin_init()
{
	register_plugin(PluginName, AMXX_VERSION_STR, "AMXX Dev Team");

	register_dictionary("restmenu.txt");
	register_dictionary("common.txt");

	register_clcmd( "amx_restmenu", "@ClientCommand_MainMenu" , ADMIN_CFG, .info = "REG_CMD_MENU", .info_ml = true);
	register_concmd("amx_restrict", "@ConsoleCommand_Restrict", ADMIN_CFG, .info = "REG_CMD_REST", .info_ml = true);

	CvarPointerAllowMapSettings     = register_cvar("amx_restrmapsettings", "0");
	CvarPointerRestrictedWeapons    = register_cvar("amx_restrweapons"    , RestrictedBotWeapons);
	CvarPointerRestrictedEquipAmmos = register_cvar("amx_restrequipammo"  , RestrictedBotEquipAmmos);
}

public OnConfigsExecuted()
{
	new const configFile[] = "weaprest";
	new const configFileExt[] = "ini";

	new configsDir[PLATFORM_MAX_PATH];
	get_configsdir(configsDir, charsmax(configsDir));

	if (get_pcvar_bool(CvarPointerAllowMapSettings))
	{
		new mapName[MaxMapLength];
		get_mapname(mapName, charsmax(mapName));

		formatex(ConfigFilePath, charsmax(ConfigFilePath), "%s/%s_%s.%s", configsDir, configFile, mapName, configFileExt);
	}
	else
	{
		formatex(ConfigFilePath, charsmax(ConfigFilePath), "%s/%s.%s", configsDir, configFile, configFileExt);
	}

	loadSettings(ConfigFilePath);

	ConfigsExecuted = true;
}

public CS_OnBuyAttempt(player, itemid)
{
	if (BlockedItems[itemid])
	{
		return blockcommand(player);
	}

	return PLUGIN_CONTINUE;
}

public blockcommand(const id) // Might be used by others plugins, so keep this for backward compatibility.
{
	client_print(id, print_center, "%l", "RESTRICTED_ITEM");
	return PLUGIN_HANDLED;
}

@ClientCommand_MainMenu(const id, const level, const cid)
{
	if (cmd_access(id, level, cid, 1))
	{
		displayMenu(id, MenuPosition[id] = -1);
	}

	return PLUGIN_HANDLED;
}

@ConsoleCommand_Restrict(const id, const level, const cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}

	new const argumentsCount = read_argc();

	if (argumentsCount <= 1) // Main command only, no arguments.
	{
		goto usage;
	}

	new action[8];
	new argumentIndex;
	new const actionLength = read_argv(++argumentIndex, action, charsmax(action)) - trim(action);

	if (!actionLength || !isalpha(action[0])) // Empty argument or first character is not a letter.
	{
		goto usage;
	}

	new const ch1 = char_to_lower(action[0]);
	new const ch2 = char_to_lower(action[1]);

	if (ch1 == 'o' && (ch2 == 'n' || ch2 == 'f'))  // [on]/[of]f
	{
		new const bool:restricted = (ch2 == 'n');
		new bool:valid;

		if (argumentsCount <= argumentIndex + 1) // No arguments, all items are concerned.
		{
			arrayset(BlockedItems, restricted, sizeof BlockedItems);
			console_print(id, "%l", restricted ? "EQ_WE_RES" : "EQ_WE_UNRES");

			ModifiedItem = valid = true;
			refreshMenus(level);
		}
		else // Either item type or specific alias
		{
			new commands[MaxConsoleLength];
			new itemName[MaxItemNameLength];
			new argument[MaxAliasNameLength];
			new position, class;
			new itemid, slot;
			new commandLength;

			while (argumentIndex < argumentsCount)
			{
				// Ignore if the argument is empty or the first character is not a letter.
				if ((commandLength = read_argv(++argumentIndex, commands, charsmax(commands)) - trim(commands)) <= 0 || !isalpha(commands[0]))
				{
					continue;
				}

				strtolower(commands);
				position = 0;

				// In case argument contains several input between quotes.
				while (position != commandLength && (position = argparse(commands, position, argument, charsmax(argument))) != -1)
				{
					if ((class = findMenuAliasId(argument)) != -1)
					{
						for (slot = 0; slot < sizeof ItemsInfos[] && (itemid = ItemsInfos[class][slot][m_Index]) != CSI_NONE; ++slot)
						{
							BlockedItems[itemid] = restricted;
						}

						console_print(id, "%l %l %l", MenuInfos[class], (class < 6) ? "HAVE_BEEN" : "HAS_BEEN", restricted ? "RESTRICTED" : "UNRESTRICTED");
						ModifiedItem = valid = true;
					}
					else if ((itemid = cs_get_item_id(argument)) != CSI_NONE)
					{
						BlockedItems[itemid] = restricted;
						findItemFullName(itemid, itemName, charsmax(itemName));

						console_print(id, "%l %l %l", itemName, "HAS_BEEN", restricted ? "RESTRICTED" : "UNRESTRICTED");
						ModifiedItem = valid = true;
					}
				}
			}

			if (!valid)
			{
				console_print(id, "%l", "NO_EQ_WE");
			}
			else
			{
				refreshMenus(level);
			}
		}

		if (ConfigsExecuted && valid)
		{
			show_activity_key("ADMIN_UPD_RES_1", "ADMIN_UPD_RES_2", fmt("%n", id));
			log_amx("%L", LANG_SERVER, "ADMIN_CMD_UPDATEDCFG", id);
		}
	}
	else if (ch1 == 'l' && ch2 == 'i')  // [li]st
	{
		// Items list.
		if (argumentsCount > argumentIndex + 1) // Available arguments.
		{
			new const selection = read_argv_int(++argumentIndex) - 1; // Index starts from 0.

			if (0 <= selection <= charsmax(ItemsInfos))
			{
				console_print(id, "^n----- %l: %l -----^n", "WEAP_RES", MenuInfos[selection][m_Title]);

				SetGlobalTransTarget(id);

				new alias[MaxAliasNameLength];
				new itemid;

				console_print(id, "  %-32.31s   %-10.9s   %-9.8s", fmt("%l", "NAME"), fmt("%l", "VALUE"), fmt("%l", "STATUS"));
				console_print(id, "");

				for (new slot = 0; slot < sizeof ItemsInfos[] && (itemid = ItemsInfos[selection][slot][m_Index]) != CSI_NONE; ++slot)
				{
					cs_get_item_alias(itemid, alias, charsmax(alias));

					console_print(id, "  %-32.31s   %-10.9s   %-9.8s", fmt("%l", ItemsInfos[selection][slot][m_Name]), alias
																	 , fmt("%l", BlockedItems[itemid] ? "ON" : "OFF"));
				}

				console_print(id, "");
				return PLUGIN_HANDLED;
			}
		}

		console_print(id, "^n----- %l -----^n", "WEAP_RES");

		// Item types list.
		for (new class = 0; class < sizeof MenuInfos; ++class)
		{
			console_print(id, "%3d: %l", class + 1, MenuInfos[class][m_Title]);
		}

		console_print(id, "^n----- %l -----^n", "REST_USE_HOW");
	}
	else if (ch1 == 's')  // [s]ave
	{
		// If 'save' is used in a per-map config file, the plugin config file is not yet known as it depends on
		// amx_restrmapsettings cvar value read after per-map configs are processed. Postponing the saving a little.
		if (!ConfigsExecuted)
		{
			const taskId = 424242;

			if (!task_exists(taskId))
			{
				set_task(0.1, "@Task_SaveConfig", taskId);
			}

			return PLUGIN_HANDLED;
		}

		new bool:saved = saveSettings(ConfigFilePath);

		if (saved)
		{
			ModifiedItem = false;
			refreshMenus(level, .displaySaveMessage = true);

			if (ConfigsExecuted)
			{
				log_amx("%L", LANG_SERVER, "ADMIN_CMD_SAVEDCFG", id, ConfigFilePath);
			}
		}

		console_print(id, "%l^n", saved ? "REST_CONF_SAVED" : "REST_COULDNT_SAVE", ConfigFilePath);
	}
	else if (ch1 == 'l' && ch2 == 'o')  // [lo]ad
	{
		if (argumentsCount <= argumentIndex + 1) // No argument
		{
			goto usage;
		}

		new argument[MaxConfigFileLength];
		read_argv(++argumentIndex, argument, charsmax(argument)) - trim(argument);

		new filepath[PLATFORM_MAX_PATH];
		new length = get_configsdir(filepath, charsmax(filepath));
		formatex(filepath[length], charsmax(filepath) - length, "/%s", argument);

		new bool:loaded = loadSettings(filepath);

		if (loaded)
		{
			arrayset(BlockedItems, false, sizeof BlockedItems);

			ModifiedItem = true;
			refreshMenus(level);

			if (ConfigsExecuted)
			{
				show_activity_key("ADMIN_UPD_RES_1", "ADMIN_UPD_RES_2", fmt("%n", id));
				log_amx("%L", LANG_SERVER, "ADMIN_CMD_LOADEDCFG", id, ConfigFilePath);
			}
		}

		console_print(id, "%l^n", loaded ? "REST_CONF_LOADED" : "REST_COULDNT_LOAD", filepath);
	}
	else
	{
		usage:
		console_print(id, "%l", "COM_REST_USAGE");
		console_print(id, "^n%l", "COM_REST_COMMANDS");
		console_print(id, "%l", "COM_REST_ON");
		console_print(id, "%l", "COM_REST_OFF");
		console_print(id, "%l", "COM_REST_ONV");
		console_print(id, "%l", "COM_REST_OFFV");
		console_print(id, "%l", "COM_REST_LIST");
		console_print(id, "%l", "COM_REST_SAVE");
		console_print(id, "%l^n", "COM_REST_LOAD");
		console_print(id, "%l^n", "COM_REST_VALUES");
		console_print(id, "%l^n", "COM_REST_TYPE");
	}

	return PLUGIN_HANDLED;
}

@Task_SaveConfig()
{
	server_cmd("amx_restrict save");
}

displayMenu(const id, const position)
{
	SetGlobalTransTarget(id);

	new menuTitle[MaxMenuTitleLength * 2];
	formatex(menuTitle, charsmax(menuTitle), "      \y%l", "REST_WEAP");

	new const menu = MenuHandle[id] = menu_create(menuTitle, "@OnMenuAction");

	if (position < 0)  // Main menu
	{
		for (new class = 0; class < sizeof MenuInfos; ++class)
		{
			menu_additem(menu, fmt("%l", MenuInfos[class][m_Title]));
		}
	}
	else // Sub-menus
	{
		menu_setprop(menu, MPROP_TITLE, fmt("%s  > \d%l", menuTitle, MenuInfos[position][m_Title]));

		for (new slot = 0, data[MenuItem], index; slot < sizeof ItemsInfos[]; ++slot)
		{
			data = ItemsInfos[position][slot];

			if ((index = data[m_Index]))
			{
				menu_additem(menu, fmt("%l\R%s%l", data[m_Name], BlockedItems[index] ? "\y" : "\r", BlockedItems[index] ? "ON" : "OFF"));
				continue;
			}

			menu_addblank2(menu);
		}
	}

	menu_addblank(menu, .slot = false);
	menu_additem(menu, fmt("%s%l \y\R%s", ModifiedItem ? "\y" : "\d", "SAVE_SET", ModifiedItem ? "*" : ""));

	if (position >= 0) // Inside a sub-menu
	{
		menu_addblank(menu, .slot = false);
		menu_additem(menu, fmt("%l", "BACK"));
	}
	else // Main menu
	{
		menu_setprop(menu, MPROP_EXITNAME, fmt("%l", "EXIT"));
		menu_setprop(menu, MPROP_EXIT, MEXIT_FORCE);                // Force an EXIT item since pagination is disabled.
	}

	menu_setprop(menu, MPROP_PERPAGE, 0);                           // Disable pagination.
	menu_setprop(menu, MPROP_NUMBER_COLOR, "      \r");             // Small QoL change to avoid menu overlapping with left icons.

	menu_display(id, menu);

	return menu;
}

@OnMenuAction(const id, const menu, const key)
{
	new position = MenuPosition[id];

	if (key >= 0)
	{
		switch (key + 1)
		{
			case 1 .. sizeof ItemsInfos[]:
			{
				if (position < 0)  // We are right now in the main menu, go to sub-menu.
				{
					position = key;
				}
				else  // We are in a sub-menu.
				{
					ModifiedItem = true;

					new const itemid = ItemsInfos[any:position][key][m_Index];
					BlockedItems[itemid] = !BlockedItems[itemid];

					restrictPodbotItem(itemid, .toggle = true);
					updatePodbotCvars();
				}
			}
			case sizeof ItemsInfos[] + 1:  // Save option.
			{
				if (saveSettings(ConfigFilePath))
				{
					show_activity_key("ADMIN_UPD_RES_1", "ADMIN_UPD_RES_2", fmt("%n", id));
					log_amx("%L", LANG_SERVER, "ADMIN_MENU_SAVEDCFG", id ,ConfigFilePath);

					ModifiedItem = false;
				}

				client_print(id, print_chat, "* %l", ModifiedItem ? "CONF_SAV_FAIL" : "CONF_SAV_SUC");
			}
			default:
			{
				position = -1;  // Back to main menu.
			}
		}
	}

	MenuHandle[id] = -1;

	menu_destroy(menu);

	if (position != MenuPosition[id] || key >= 0)
	{
		displayMenu(id, MenuPosition[id] = position);
	}

	return PLUGIN_HANDLED;
}

findAdminsWithMenu(playersList[MAX_PLAYERS], &playersCount, const commandLevel = -1)
{
	new player, adminsCount;
	new menu, newmenu;

	get_players(playersList, playersCount, "ch");

	for (new i = 0; i < playersCount; ++i)
	{
		player = playersList[i]
		
		if (player_menu_info(player, menu, newmenu) && newmenu != -1 && newmenu == MenuHandle[player])
		{
			if (commandLevel == -1 || access(player, commandLevel)) // extra safety
			{
				playersList[adminsCount++] = player;
			}
		}
	}

	playersCount = adminsCount;
}

refreshMenus(const commandLevel = 0, const bool:displaySaveMessage = false)
{
	new playersList[MAX_PLAYERS], playersCount;
	findAdminsWithMenu(playersList, playersCount, commandLevel);

	if (!playersCount)
	{
		return;
	}

	for (new i = 0, player; i < playersCount; ++i)
	{
		player = playersList[i]
		
		MenuHandle[player] = displayMenu(player, MenuPosition[player]);

		if (displaySaveMessage)
		{
			client_print(playersList[i], print_chat, "* %l (amx_restrict)", "CONF_SAV_SUC");
		}
	}
}

bool:saveSettings(const filename[])
{
	new const fp = fopen(filename, "wt");

	if (!fp)
	{
		return false;
	}

	fprintf(fp, "%L", LANG_SERVER, "CONFIG_FILE_HEADER", PluginName);

	new alias[MaxAliasNameLength];
	new itemid;
	new bool:showCategory;

	for (new class = 0, slot; class < sizeof ItemsInfos; ++class)
	{
		showCategory = true;

		for (slot = 0; slot < sizeof ItemsInfos[]; ++slot)
		{
			if ((itemid = ItemsInfos[class][slot][m_Index]) == CSI_NONE)
			{
				break;
			}

			if (BlockedItems[itemid])
			{
				if (showCategory)
				{
					showCategory = false;
					fprintf(fp, "^n; %l^n; -^n", MenuInfos[class][m_Title]);
				}

				cs_get_item_alias(itemid, alias, charsmax(alias));
				fprintf(fp, "%-16.15s ; %L^n", alias, LANG_SERVER, ItemsInfos[class][slot][m_Name]);
			}
		}
	}

	fclose(fp);

	return true;
}

bool:loadSettings(const filename[])
{
	new const fp = fopen(filename, "rt");

	if (!fp)
	{
		return false;
	}

	new lineRead[MaxAliasNameLength], alias[MaxAliasNameLength];
	new itemid, ch;

	arrayset(RestrictedBotEquipAmmos, '0', charsmax(RestrictedBotEquipAmmos));
	arrayset(RestrictedBotWeapons, '0', charsmax(RestrictedBotWeapons));

	while (fgets(fp, lineRead, charsmax(lineRead)))
	{
		trim(lineRead)

		if (!lineRead[0])
		{
			continue;
		}

		if ((ch = lineRead[0]) == ';' || ch == '/' || ch == '#')
		{
			continue;
		}

		if (parse(lineRead, alias, charsmax(alias)) == 1 && (itemid = cs_get_item_id(alias)) != CSI_NONE)
		{
			BlockedItems[itemid] = true;
			restrictPodbotItem(itemid);
		}
	}

	fclose (fp);

	updatePodbotCvars();

	return true;
}

findMenuAliasId(const name[])
{
	for (new i = 0; i < sizeof MenuInfos; ++i)
	{
		if (equal(name, MenuInfos[i][m_Alias]))
		{
			return i;
		}
	}

	return -1;
}

findItemFullName(const itemid, name[], const maxlen)
{
	for (new class = 0, slot; class < sizeof ItemsInfos; ++class)
	{
		for (slot = 0; slot < sizeof ItemsInfos[]; ++slot)
		{
			if (ItemsInfos[class][slot][m_Index] == itemid)
			{
				copy(name, maxlen, ItemsInfos[class][slot][m_Name]);
				return;
			}
		}
	}
}

restrictPodbotItem(const itemid, const bool:toggle = false)
{
	new const translatedItems[CSI_MAX_COUNT] =
	{
		// CSI ids -> string indexes of pb_restrweapons and pb_restrequipammo cvars. See podbot.cfg.
		-1, 4, -1, 20, 3, 8, -1, 12, 19, 4, 5, 6, 13, 23, 17, 18, 1, 2, 21, 9, 24, 7, 16, 10, 22, 2, 3, 15, 14, 0, 11, 0, 1, 5, 6, 25, 7, 8
	};

	new const index = translatedItems[itemid];

	if (index >= 0)
	{
		if ((itemid <= CSI_LAST_WEAPON && !(1 << itemid & CSI_ALL_GRENADES)) || itemid == CSI_SHIELD)
		{
			RestrictedBotWeapons[index] = toggle && RestrictedBotWeapons[index] == '1' ? '0' : '1';
		}
		else
		{
			RestrictedBotEquipAmmos[index] = toggle && RestrictedBotEquipAmmos[index] == '1' ? '0' : '1';
		}
	}
}

updatePodbotCvars()
{
	set_pcvar_string(CvarPointerRestrictedWeapons, RestrictedBotWeapons);
	set_pcvar_string(CvarPointerRestrictedEquipAmmos, RestrictedBotEquipAmmos);
}

