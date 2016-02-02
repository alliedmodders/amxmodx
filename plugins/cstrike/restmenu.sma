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

// Uncomment if you want to have seperate settings for each map
//#define MAPSETTINGS

#include <amxmodx>
#include <amxmisc>
#include <cstrike>

new const PluginName[] = "Restrict Weapons";

new bool:BlockedItems[CSI_MAX_COUNT];

#define MAXMENUPOS 34

new g_Position[MAX_PLAYERS + 1]
new g_Modified
new g_blockPos[112]
new g_saveFile[64]
new g_Restricted[] = "* This item is restricted *"

new RestrictedBotWeapons[] = "00000000000000000000000000";
new RestrictedBotEquipAmmos[] = "000000000";

new CvarPointerRestrictedWeapons;
new CvarPointerRestrictedEquipAmmos;

enum
{
	ItemType_Handguns,
	ItemType_Shotguns,
	ItemType_SubMachineGuns,
	ItemType_AssaultRifles,
	ItemType_SniperRifles,
	ItemType_MachineGuns,
	ItemType_Equipment,
	ItemType_Ammunition,
//  -
	MaxItemTypes
};



enum MenuTitle
{
	m_Title[24],
	m_Alias[12],
};

new const g_MenuTitle[MaxItemTypes][MenuTitle] =
{
	{ "MENU_TITLE_HANDGUNS"   , "pistol"  },
	{ "MENU_TITLE_SHOTGUNS"   , "shotgun" },
	{ "MENU_TITLE_SUBMACHINES", "sub"     },
	{ "MENU_TITLE_RIFLES"     , "rifle"   },
	{ "MENU_TITLE_SNIPERS"    , "sniper"  },
	{ "MENU_TITLE_MACHINE"    , "machine" },
	{ "MENU_TITLE_EQUIPMENT"  , "equip"   },
	{ "MENU_TITLE_AMMUNITION" , "ammo"    },
}

const MaxBuyMenuSlots = 8;

enum MenuItem
{
    m_Index,
    m_Name[32],
};

#define ITEM(%0)  { CSI_%0, "MENU_ITEM_" + #%0 }
#define ITEM_NONE { CSI_NONE, "" }

new const ItemsInfos[MaxItemTypes][MaxBuyMenuSlots][MenuItem] =
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

// First position is a position of menu (0 for ammo, 1 for pistols, 6 for equipment etc.)
// Second is a key for TERRORIST (all is key are minus one, 1 is 0, 2 is 1 etc.)
// Third is a key for CT
// Position with -1 doesn't exist

new g_Keys[MAXMENUPOS][3] =
{
	{1, 1, 1},	// H&K USP .45 Tactical
	{1, 0, 0},	// Glock18 Select Fire
	{1, 3, 3},	// Desert Eagle .50AE
	{1, 2, 2},	// SIG P228
	{1, 4, -1}, // Dual Beretta 96G Elite
	{1, -1, 4}, // FN Five-Seven
	{2, 0, 0},	// Benelli M3 Super90
	{2, 1, 1},	// Benelli XM1014
	{3, 1, 1},	// H&K MP5-Navy
	{3, -1, 0}, // Steyr Tactical Machine Pistol
	{3, 3, 3},	// FN P90
	{3, 0, -1}, // Ingram MAC-10
	{3, 2, 2},	// H&K UMP45
	{4, 1, -1}, // AK-47
	{4, 0, -1}, // Gali
	{4, -1, 0}, // Famas
	{4, 3, -1}, // Sig SG-552 Commando
	{4, -1, 2}, // Colt M4A1 Carbine
	{4, -1, 3}, // Steyr Aug
	{4, 2, 1},	// Steyr Scout
	{4, 4, 5},	// AI Arctic Warfare/Magnum
	{4, 5, -1}, // H&K G3/SG-1 Sniper Rifle
	{4, -1, 4}, // Sig SG-550 Sniper
	{5, 0, 0},	// FN M249 Para
	{6, 0, 0},	// Kevlar Vest
	{6, 1, 1},	// Kevlar Vest & Helmet
	{6, 2, 2},	// Flashbang
	{6, 3, 3},	// HE Grenade
	{6, 4, 4},	// Smoke Grenade
	{6, -1, 6}, // Defuse Kit
	{6, 5, 5},	// NightVision Goggles
	{6, -1, 7},	// Tactical Shield
	{0, 5, 5},	// Primary weapon ammo
	{0, 6, 6}	// Secondary weapon ammo
}

new g_WeaponNames[MAXMENUPOS][] =
{
	"H&K USP .45 Tactical", 
	"Glock18 Select Fire", 
	"Desert Eagle .50AE", 
	"SIG P228", 
	"Dual Beretta 96G Elite", 
	"FN Five-Seven", 
	"Benelli M3 Super90", 
	"Benelli XM1014", 
	"H&K MP5-Navy", 
	"Steyr Tactical Machine Pistol", 
	"FN P90", 
	"Ingram MAC-10", 
	"H&K UMP45", 
	"AK-47", 
	"Gali", 
	"Famas", 
	"Sig SG-552 Commando", 
	"Colt M4A1 Carbine", 
	"Steyr Aug", 
	"Steyr Scout", 
	"AI Arctic Warfare/Magnum", 
	"H&K G3/SG-1 Sniper Rifle", 
	"Sig SG-550 Sniper", 
	"FN M249 Para", 
	"Kevlar Vest", 
	"Kevlar Vest & Helmet", 
	"Flashbang", 
	"HE Grenade", 
	"Smoke Grenade", 
	"Defuse Kit", 
	"NightVision Goggles", 
	"Tactical Shield", 
	"Primary weapon ammo", 
	"Secondary weapon ammo"
}

new g_Aliases[MAXMENUPOS][] =
{
	"usp",		//Pistols
	"glock", 
	"deagle", 
	"p228", 
	"elites", 
	"fn57", 

	"m3",		//Shotguns
	"xm1014", 

	"mp5",		//SMG
	"tmp", 
	"p90", 
	"mac10", 
	"ump45", 

	"ak47",		//Rifles
	"galil", 
	"famas", 
	"sg552", 
	"m4a1", 
	"aug", 
	"scout", 
	"awp", 
	"g3sg1", 
	"sg550", 

	"m249",		//Machine Gun

	"vest",		//Equipment
	"vesthelm", 
	"flash", 
	"hegren", 
	"sgren", 
	"defuser", 
	"nvgs", 
	"shield", 

	"primammo", //Ammo
	"secammo"
}

public CS_OnBuyAttempt(player, itemid)
{
	if (BlockedItems[itemid])
	{
		client_print(player, print_center, "%s", g_Restricted);
		return PLUGIN_HANDLED;
	}
	
	return PLUGIN_CONTINUE;
}

positionBlocked(a)
{
	new m = g_Keys[a][0] * 8
	new d = (g_Keys[a][1] == -1) ? 0 : g_blockPos[m + g_Keys[a][1]]
	
	d += (g_Keys[a][2] == -1) ? 0 : g_blockPos[m + g_Keys[a][2] + 56]
	
	return d
}

findMenuAliasId(const name[])
{
	for (new i = 0; i < sizeof(g_MenuTitle); ++i)
	{
		if (equal(name, g_MenuTitle[i][m_Alias]))
		{
			return i;
		}
	}

	return -1;
}

findItemFullName(const itemid, name[], maxlen)
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

@ConsoleCommand_Restrict(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	new command[8];
	read_argv(1, command, charsmax(command));

	trim(command);
	strtolower(command);
	
	new const ch1 = command[0];
	new const ch2 = command[1];
	
	if (ch1 == 'o' && (ch2 == 'n' || ch2 == 'f'))  // [on]/[of]f
	{
		new const bool:status = (ch2 == 'n');
		new const numArgs = read_argc();

		if (numArgs <= 2) // No arguments, all items are concerned.
		{
			arrayset(BlockedItems, status, sizeof(BlockedItems));
			console_print(id, "%l", status ? "EQ_WE_RES" : "EQ_WE_UNRES");
			g_Modified = true;
		} 
		else // Either item type or specific alias
		{
			new commands[128];
			new itemName[64];
			new argument[16];
			new bool:found;
			new position, class;
			new itemid, slot;
			
			for (new argindex = 2; argindex < numArgs; ++argindex, position = 0)
			{
				read_argv(argindex, commands, charsmax(commands));

				trim(commands);
				strtolower(commands);
				
				// In case argument contains several input between quotes.
				while ((position = argparse(commands, position, argument, charsmax(argument))) != -1)
				{
					if ((class = findMenuAliasId(argument)) != -1)
					{
						for (slot = 0; slot < sizeof ItemsInfos[] && (itemid = ItemsInfos[class][slot][m_Index]) != CSI_NONE; ++slot)
						{
							BlockedItems[itemid] = status;
						}
						
						console_print(id, "%l %l %l", g_MenuTitle[class], (class < 6) ? "HAVE_BEEN" : "HAS_BEEN", status ? "RESTRICTED" : "UNRESTRICTED");
						g_Modified = found = true;
					}
					else if ((itemid = cs_get_item_id(argument)) != CSI_NONE)
					{
						BlockedItems[itemid] = status;
						findItemFullName(itemid, itemName, charsmax(itemName));
						
						console_print(id, "%l %l %l", itemName, "HAS_BEEN", status ? "RESTRICTED" : "UNRESTRICTED");
						g_Modified = found = true;
					}
				}
			}
			
			if (!found)
			{
				console_print(id, "%l", "NO_EQ_WE");
			}
		}
	}
	else if (ch1 == 'l' && ch2 == 'i')  // [li]st
	{
		new argument[2];
		new selection = -1;
		
		if (read_argv(2, argument, charsmax(argument)))
		{
			selection = clamp(strtol(argument), 1, sizeof ItemsInfos) - 1;
		}

		console_print(id, "^n----- %l: -----^n", "WEAP_RES");
		
		if (selection == -1) // Item types list.
		{
			for (new i = 0; i < sizeof g_MenuTitle; ++i)
			{
				console_print(id, "%3d: %l", i + 1, g_MenuTitle[i][m_Title]);
			}
			
			console_print(id, "^n----- %l: -----^n", "REST_USE_HOW");
		}
		else // Items list.
		{
			new langName[24], langValue[24], langStatus[24], langOnOff[24];
			new alias[16], itemid;
			
			LookupLangKey(langName, charsmax(langName), "NAME", id);
			LookupLangKey(langValue, charsmax(langValue), "VALUE", id);
			LookupLangKey(langStatus, charsmax(langStatus), "STATUS", id);
			
			console_print(id, "  %-32.31s   %-10.9s   %-9.8s", langName, langValue, langStatus)
		
			for (new slot = 0; slot < sizeof ItemsInfos[] && (itemid = ItemsInfos[selection][slot][m_Index]) != CSI_NONE; ++slot)
			{
				LookupLangKey(langOnOff, charsmax(langOnOff), BlockedItems[itemid] ? "ON" : "OFF", id);
				LookupLangKey(langName, charsmax(langName), ItemsInfos[selection][slot][m_Name], id);
				
				cs_get_item_alias(itemid, alias, charsmax(alias));
				
				console_print(id, "  %-32.31s   %-10.9s   %-9.8s", langName, alias, langOnOff);
			}
		}
	}
	else if(ch1 == 's')  // [s]ave
	{
		if (saveSettings(g_saveFile))
		{
			g_Modified = false;
		}
		
		console_print(id, "%l", g_Modified ? "REST_COULDNT_SAVE" : "REST_CONF_SAVED", g_saveFile);
	}
	else if (ch1 == 'l' && ch2 == 'o')  // [lo]ad
	{
		arrayset(BlockedItems, false, sizeof BlockedItems); 
		
		new argument[64];
		new length;

		length = read_argv(2, argument, charsmax(argument));
		length -= trim(argument);
		
		if (length)
		{
			new filepath[PLATFORM_MAX_PATH];
			length = get_configsdir(filepath, charsmax(filepath));

			formatex(filepath[length], charsmax(filepath) - length, "/%s", argument);
			
			if (loadSettings(filepath))
			{
				g_Modified = true;
			}
		}

		console_print(id, "%l", g_Modified ? "REST_CONF_LOADED" : "REST_COULDNT_LOAD", argument);
	}
	else 
	{
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

displayMenu(id, pos)
{
	new menuTitle[64], menuBody[128];
	new length = formatex(menuTitle, charsmax(menuTitle), "      \y%l", "REST_WEAP");
	
	new menu = menu_create(menuTitle, "@OnMenuAction");
	
	if (pos < 0)  // Main menu
	{
		for (new type = 0; type < sizeof(g_MenuTitle); ++type)
		{
			LookupLangKey(menuBody, charsmax(menuBody), g_MenuTitle[type][m_Title], id);
			menu_additem(menu, menuBody);
		}
	}
	else // Sub-menus
	{
		formatex(menuTitle[length], charsmax(menuTitle) - length, " > \d%l", g_MenuTitle[pos][m_Title]);
		menu_setprop(menu, MPROP_TITLE, menuTitle);

		for (new slot = 0, data[MenuItem]; slot < MaxBuyMenuSlots; ++slot)
		{
			data = ItemsInfos[pos][slot];

			if (data[m_Index])
			{
				formatex(menuBody, charsmax(menuBody), "%l\y\R%l", data[m_Name], BlockedItems[data[m_Index]] ? "ON" : "OFF");
				menu_additem(menu, menuBody);
				continue;
			}

			menu_addblank2(menu);
		}
	}

	formatex(menuBody, charsmax(menuBody), "%s%l \y\R%s", g_Modified ? "\y" : "\d", "SAVE_SET", g_Modified ? "*" : "");
	menu_addblank(menu, .slot = false); 
	menu_additem(menu, menuBody);

	formatex(menuBody, charsmax(menuBody), "%l", pos < 0 ? "EXIT" : "BACK");
	menu_setprop(menu, MPROP_EXITNAME, menuBody);       // If inside a sub-menu we want to 'back' to main menu.
	menu_setprop(menu, MPROP_PERPAGE, 0);               // Disable pagination.
	menu_setprop(menu, MPROP_EXIT, MEXIT_FORCE);        // Force an EXIT item since pagination is disabled.
	menu_setprop(menu, MPROP_NUMBER_COLOR, "      \r"); // Small QoL change to avoid menu overlapping with left icons.

	menu_display(id, menu);
}

@OnMenuAction(id, menu, key)
{
	new position = g_Position[id];
	
	switch (key + 1)
	{
		case 1 .. MaxBuyMenuSlots:
		{
			if (position < 0)  // We are right now in the main menu, go to sub-menu.
			{
				position = key;
			}
			else  // We are in a sub-menu.
			{
				g_Modified = true;

				new itemid = ItemsInfos[any:position][key][m_Index];
				BlockedItems[itemid] = !BlockedItems[itemid];

				restrictPodbotItem(itemid, .toggle = true);
				updatePodbotCvars();
			}
		}
		case MaxBuyMenuSlots + 1:  // Save option.
		{
			if (saveSettings(g_saveFile))
			{
				g_Modified = false;
			}
			
			client_print(id, print_chat, "* %l", g_Modified ? "CONF_SAV_FAIL" : "CONF_SAV_SUC");
		}
		default:
		{
			position = -1;  // Back to main menu.
		}
	}

	menu_destroy(menu);

	if (position != g_Position[id] || key >= 0)
	{
		displayMenu(id, g_Position[id] = position);
	}

	return PLUGIN_HANDLED;
}

public blockcommand(id)
{
	client_print(id, print_center, "%s", g_Restricted)
	return PLUGIN_HANDLED
}

@ClientCommand_MainMenu(id, level, cid)
{
	if (cmd_access(id, level, cid, 1))
	{
		displayMenu(id, g_Position[id] = -1);
	}
	
	return PLUGIN_HANDLED;
}

bool:saveSettings(const filename[])
{
	new fp = fopen(filename, "wt");

	if (!fp)
	{
		return false;
	}

	fprintf(fp, "%L", LANG_SERVER, "CONFIG_FILE_HEADER", PluginName);

	new alias[16];
	new itemid;
	new bool:showCategory;

	for (new i = 0, j; i < sizeof(ItemsInfos); ++i)
	{
		showCategory = true;
		
		for (j = 0; j < sizeof(ItemsInfos[]); ++j)
		{
			if ((itemid = ItemsInfos[i][j][m_Index]) == CSI_NONE)
			{
				break;
			}

			if (BlockedItems[itemid])
			{
				if (showCategory)
				{
					showCategory = false;
					fprintf(fp, "^n; %l^n; -^n", g_MenuTitle[i][m_Title]);
				}
				
				cs_get_item_alias(itemid, alias, charsmax(alias));
				fprintf(fp, "%-16.15s ; %L^n", alias, LANG_SERVER, ItemsInfos[i][j][m_Name]);
			}
		}
	}

	fclose(fp);

	return true;
}

bool:loadSettings(const filename[])
{
	new fp = fopen(filename, "rt");
	
	if (!fp)
	{
		return false;
	}

	new lineRead[16], alias[16];
	new length, ch;
	new itemid;

	arrayset(RestrictedBotEquipAmmos, '0', charsmax(RestrictedBotEquipAmmos));
	arrayset(RestrictedBotWeapons, '0', charsmax(RestrictedBotWeapons));
    
	while (!feof(fp))
	{
		length = fgets(fp, lineRead, charsmax(lineRead));
		length -= trim(lineRead);
		
		if (!length || (ch = lineRead[0]) == ';' || ch == '/' || ch == '#')
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

restrictPodbotItem(const itemid, const bool:toggle = false)
{
	new const translatedItems[CSI_MAX_COUNT] = 
	{ 
		// CSI ids -> string indexes of pb_restrweapons and pb_restrequipammo cvars. See podbot.cfg. 
		-1, 4, -1, 20, 3, 8, -1, 12, 19, 4, 5, 6, 13, 23, 17, 18, 1, 2, 21, 9, 24, 7, 16, 10, 22, 2, 3, 15, 14, 0, 11, 0, 1, 5, 6, 25, 7, 8
	};

	new index = translatedItems[itemid];
	
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

public plugin_init()
{
	register_plugin(PluginName, AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("restmenu.txt")
	register_dictionary("common.txt")

	register_clcmd("amx_restmenu", "@ClientCommand_MainMenu", ADMIN_CFG, "- displays weapons restriction menu");
	register_concmd("amx_restrict", "@ConsoleCommand_Restrict", ADMIN_CFG, "- displays help for weapons restriction");

	CvarPointerRestrictedWeapons    = register_cvar("amx_restrweapons"  , RestrictedBotWeapons);
	CvarPointerRestrictedEquipAmmos = register_cvar("amx_restrequipammo", RestrictedBotEquipAmmos);
    
	new configsDir[64];
	get_configsdir(configsDir, charsmax(configsDir));
#if defined MAPSETTINGS
	new mapname[32]
	get_mapname(mapname, charsmax(mapname))
	format(g_saveFile, charsmax(g_saveFile), "%s/weaprest_%s.ini", configsDir, mapname)
#else
	format(g_saveFile, charsmax(g_saveFile), "%s/weaprest.ini", configsDir)
#endif
	loadSettings(g_saveFile)
}
