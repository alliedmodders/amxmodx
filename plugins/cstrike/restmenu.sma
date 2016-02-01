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

new g_menusNames[7][] =
{
	"pistol", 
	"shotgun", 
	"sub", 
	"rifle", 
	"machine", 
	"equip", 
	"ammo"
}

new const g_MenuTitle[MaxItemTypes][] =
{
	"MENU_TITLE_HANDGUNS",
	"MENU_TITLE_SHOTGUNS",
	"MENU_TITLE_SUBMACHINES",
	"MENU_TITLE_RIFLES",
	"MENU_TITLE_SNIPERS",
	"MENU_TITLE_MACHINE",
	"MENU_TITLE_EQUIPMENT",
	"MENU_TITLE_AMMUNITION"
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

new g_menusSets[7][2] =
{
	{0, 6}, {6, 8}, {8, 13}, {13, 23}, {23, 24}, {24, 32}, {32, 34}
}

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

setWeapon(a, action)
{
	new b, m = g_Keys[a][0] * 8
	
	if (g_Keys[a][1] != -1)
	{
		b = m + g_Keys[a][1]
		
		if (action == 2)
			g_blockPos[b] = 1 - g_blockPos[b]
		else
			g_blockPos[b] = action
	}

	if (g_Keys[a][2] != -1)
	{
		b = m + g_Keys[a][2] + 56
		
		if (action == 2)
			g_blockPos[b] = 1 - g_blockPos[b]
		else
			g_blockPos[b] = action
	}
}

findMenuId(name[])
{
	for (new i = 0; i < sizeof(g_menusNames) ; ++i)
		if (equali(name, g_menusNames[i]))
			return i
	
	return -1
}

switchCommand(id, action)
{
	new c = read_argc()

	if (c < 3)
	{
		for (new x = 0; x < MAXMENUPOS; x++)
			setWeapon(x, action)		

		console_print(id, "%L", id, action ? "EQ_WE_RES" : "EQ_WE_UNRES")
		g_Modified = true
	} else {
		new arg[32], a
		new bool:found = false
		
		for (new b = 2; b < c; ++b)
		{
			read_argv(b, arg, charsmax(arg))
			
			if ((a = findMenuId(arg)) != -1)
			{
				c = g_menusSets[a][1]
				
				for (new i = g_menusSets[a][0]; i < c; ++i)
					setWeapon(i, action)
				
				console_print(id, "%s %L %L", g_MenuTitle[a], id, (a < 5) ? "HAVE_BEEN" : "HAS_BEEN", id, action ? "RESTRICTED" : "UNRESTRICTED")
				g_Modified = found = true
			}
			else if ((a = cs_get_item_id(arg)) != CSI_NONE)
			{
				g_Modified = found = true
				setWeapon(a, action)
				console_print(id, "%s %L %L", g_WeaponNames[a], id, "HAS_BEEN", id, action ? "RESTRICTED" : "UNRESTRICTED")
			}
		}

		if (!found)
			console_print(id, "%L", id, "NO_EQ_WE")
	}
}

positionBlocked(a)
{
	new m = g_Keys[a][0] * 8
	new d = (g_Keys[a][1] == -1) ? 0 : g_blockPos[m + g_Keys[a][1]]
	
	d += (g_Keys[a][2] == -1) ? 0 : g_blockPos[m + g_Keys[a][2] + 56]
	
	return d
}

public cmdRest(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	new cmd[8]
	
	read_argv(1, cmd, charsmax(cmd))
	
	if (equali("on", cmd))
		switchCommand(id, 1)
	else if (equali("off", cmd))
		switchCommand(id, 0)
	else if (equali("list", cmd))
	{
		new arg1[8]
		new	start = read_argv(2, arg1, charsmax(arg1)) ? str_to_num(arg1) : 1
		
		if (--start < 0)
			start = 0
		
		if (start >= MAXMENUPOS)
			start = MAXMENUPOS - 1
		
		new end = start + 10
		
		if (end > MAXMENUPOS)
			end = MAXMENUPOS
		
		new lName[16], lValue[16], lStatus[16], lOnOff[16]
		
		format(lName, charsmax(lName), "%L", id, "NAME")
		format(lValue, charsmax(lValue), "%L", id, "VALUE")
		format(lStatus, charsmax(lStatus), "%L", id, "STATUS")
		
		console_print(id, "^n----- %L: -----", id, "WEAP_RES")
		console_print(id, "     %-32.31s   %-10.9s   %-9.8s", lName, lValue, lStatus)
		
		if (start != -1)
		{
			for (new a = start; a < end; ++a)
			{
				format(lOnOff, charsmax(lOnOff), "%L", id, positionBlocked(a) ? "ON" : "OFF")
				console_print(id, "%3d: %-32.31s   %-10.9s   %-9.8s", a + 1, g_WeaponNames[a], g_Aliases[a], lOnOff)
			}
		}
		
		console_print(id, "----- %L -----", id, "REST_ENTRIES_OF", start + 1, end, MAXMENUPOS)
		
		if (end < MAXMENUPOS)
			console_print(id, "----- %L -----", id, "REST_USE_MORE", end + 1)
		else
			console_print(id, "----- %L -----", id, "REST_USE_BEGIN")
	}
	else if (equali("save", cmd))
	{
		if (saveSettings(g_saveFile))
		{
			console_print(id, "%L", id, "REST_CONF_SAVED", g_saveFile)
			g_Modified = false
		}
		else
			console_print(id, "%L", id, "REST_COULDNT_SAVE", g_saveFile)
	}
	else if (equali("load", cmd))
	{
		setc(g_blockPos, sizeof(g_blockPos), 0)	// Clear current settings
		new arg1[64]

		if (read_argv(2, arg1, charsmax(arg1)))
		{
			new configsdir[32]
			get_configsdir(configsdir, charsmax(configsdir))

			format(arg1, charsmax(arg1), "%s/%s", configsdir, arg1)
		}
		
		if (loadSettings(arg1))
		{
			console_print(id, "%L", id, "REST_CONF_LOADED", arg1)
			g_Modified = true
		}
		else
			console_print(id, "%L", id, "REST_COULDNT_LOAD", arg1)
	} else {
		console_print(id, "%L", id, "COM_REST_USAGE")
		console_print(id, "%L", id, "COM_REST_COMMANDS")
		console_print(id, "%L", id, "COM_REST_ON")
		console_print(id, "%L", id, "COM_REST_OFF")
		console_print(id, "%L", id, "COM_REST_ONV")
		console_print(id, "%L", id, "COM_REST_OFFV")
		console_print(id, "%L", id, "COM_REST_LIST")
		console_print(id, "%L", id, "COM_REST_SAVE")
		console_print(id, "%L", id, "COM_REST_LOAD")
		console_print(id, "%L", id, "COM_REST_VALUES")
		console_print(id, "%L", id, "COM_REST_TYPE")
	}

	return PLUGIN_HANDLED
}

displayMenu(id, pos)
{
	new menuTitle[64], menuBody[128];
	new length = formatex(menuTitle, charsmax(menuTitle), "      \y%l", "REST_WEAP");
	
	new menu = menu_create(menuTitle, "actionMenu");
	
	if (pos < 0)  // Main menu
	{
		for (new type = 0; type < sizeof(g_MenuTitle); ++type)
		{
			LookupLangKey(menuBody, charsmax(menuBody), g_MenuTitle[type], id);
			menu_additem(menu, menuBody);
		}
	}
	else // Sub-menus
	{
		formatex(menuTitle[length], charsmax(menuTitle) - length, " > \d%l", g_MenuTitle[pos]);
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

	formatex(menuBody, charsmax(menuBody), "%l \y\R%s", "SAVE_SET", g_Modified ? "*" : "");
	menu_addblank(menu, .slot = false); 
	menu_additem(menu, menuBody);

	formatex(menuBody, charsmax(menuBody), "%l", pos < 0 ? "EXIT" : "BACK");
	menu_setprop(menu, MPROP_EXITNAME, menuBody);       // If inside a sub-menu we want to 'back' to main menu.
	menu_setprop(menu, MPROP_PERPAGE, 0);               // Disable pagination.
	menu_setprop(menu, MPROP_EXIT, MEXIT_FORCE);        // Force an EXIT item since pagination is disabled.
	menu_setprop(menu, MPROP_NUMBER_COLOR, "      \r"); // Small QoL change to avoid menu overlapping with left icons.

	menu_display(id, menu);
}

public actionMenu(id, key)
{
	switch (key)
	{
		case 7:
		{
			if (saveSettings(g_saveFile))
			{
				g_Modified = false
				client_print(id, print_chat, "* %L", id, "CONF_SAV_SUC")
			}
			else
				client_print(id, print_chat, "* %L", id, "CONF_SAV_FAIL")

			displayMenu(id, g_Position[id])
		}
		case 8: displayMenu(id, ++g_Position[id])
		case 9: displayMenu(id, --g_Position[id])
		default:
		{
			setWeapon(g_Position[id] * 7 + key, 2)
			g_Modified = true
			displayMenu(id, g_Position[id])

			new a = g_Position[id] * 7 + key
			new sz[1]

			if (a < 24)
			{
				sz[0] = RestrictedBotWeapons[a + 1]
				RestrictedBotWeapons[a + 1] = (sz[0] == '0') ? '1' : '0'  // primary and secondary weapons
			}
			else if ((a >= 24) && (a < 31))
			{
				sz[0] = RestrictedBotEquipAmmos[a - 24]
				RestrictedBotEquipAmmos[a - 24] = (sz[0] == '0') ? '1' : '0'  // equipments
			}
			else if (a == 31)
			{
				sz[0] = RestrictedBotWeapons[25]
				RestrictedBotWeapons[25] = (sz[0] == '0') ? '1' : '0'  // shield
			}
			else if ((a > 31) && (a < 34))
			{
				sz[0] = RestrictedBotEquipAmmos[a - 25]
				RestrictedBotEquipAmmos[a - 25] = (sz[0] == '0') ? '1' : '0'   // primary and secondary ammo
			}
			set_pcvar_string(CvarPointerRestrictedWeapons, RestrictedBotWeapons);
			set_pcvar_string(CvarPointerRestrictedEquipAmmos, RestrictedBotEquipAmmos);
		}
	}

	return PLUGIN_HANDLED
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

saveSettings(filename[])
{
	if (file_exists(filename))
		delete_file(filename)

	if (!write_file(filename, "; Generated by Restrict Weapons Plugin. Do not modify!^n; value name"))
		return 0

	new text[64]

	for (new a = 0; a < MAXMENUPOS; ++a)
	{
		if (positionBlocked(a))
		{
			format(text, charsmax(text), "%-16.15s ; %s", g_Aliases[a], g_WeaponNames[a])
			write_file(filename, text)
		}
	}

	return 1
}

loadSettings(filename[])
{
	if (!file_exists(filename))
		return 0

	new text[16]
	new a, pos = 0

	arrayset(RestrictedBotEquipAmmos, '0', charsmax(RestrictedBotEquipAmmos));
	arrayset(RestrictedBotWeapons, '0', charsmax(RestrictedBotWeapons));
    
	while (read_file(filename, pos++, text, charsmax(text), a))
	{
		if (text[0] == ';' || !a)
			continue	// line is a comment
		
		parse(text, text, charsmax(text))
		
		if ((a = cs_get_item_id(text)) != CSI_NONE)
		{
			setWeapon(a, 1)
			if (a < 24) RestrictedBotWeapons[a + 1] = '1' // primary and secondary weapons
			else if ((a >= 24) && (a < 31)) RestrictedBotEquipAmmos[a - 24] = '1'  // equipments
			else if (a == 31) RestrictedBotWeapons[25] = '1'  // shield
			else if ((a > 31) && (a < 34)) RestrictedBotEquipAmmos[a - 25] = '1'  // primary and secondary ammo
		}
	}
	set_pcvar_string(CvarPointerRestrictedWeapons, RestrictedBotWeapons);
	set_pcvar_string(CvarPointerRestrictedEquipAmmos, RestrictedBotEquipAmmos);

	return 1
}

public plugin_init()
{
	register_plugin("Restrict Weapons", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("restmenu.txt")
	register_dictionary("common.txt")

	register_clcmd("amx_restmenu", "@ClientCommand_MainMenu", ADMIN_CFG, "- displays weapons restriction menu");
	register_concmd("amx_restrict", "cmdRest", ADMIN_CFG, "- displays help for weapons restriction");

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
