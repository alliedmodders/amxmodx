// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TFCX Constants
//

#if defined _tfcconst_included
  #endinput
#endif
#define _tfcconst_included


#define TFCMAX_WEAPONS	37

enum {
	TFC_AMMO_SHELLS = 0, 
	TFC_AMMO_BULLETS, 
	TFC_AMMO_CELLS, 
	TFC_AMMO_ROCKETS, 
	TFC_AMMO_NADE1, 
	TFC_AMMO_NADE2, 
};

enum {
	TFC_WPN_NONE = 0,
	TFC_WPN_TIMER,//TFC_WPN_UNK1,
	TFC_WPN_SENTRYGUN,//TFC_WPN_UNK2,
	TFC_WPN_MEDIKIT,
	TFC_WPN_SPANNER,
	TFC_WPN_AXE,
	TFC_WPN_SNIPERRIFLE,
	TFC_WPN_AUTORIFLE,
	TFC_WPN_SHOTGUN,
	TFC_WPN_SUPERSHOTGUN,
	TFC_WPN_NG,
	TFC_WPN_SUPERNG,
	TFC_WPN_GL,
	TFC_WPN_FLAMETHROWER,
	TFC_WPN_RPG,
	TFC_WPN_IC,
	TFC_WPN_FLAMES,//TFC_WPN_UNK16,
	TFC_WPN_AC,
	TFC_WPN_UNK18,
	TFC_WPN_UNK19,
	TFC_WPN_TRANQ,
	TFC_WPN_RAILGUN,
	TFC_WPN_PL,
	TFC_WPN_KNIFE,
	TFC_WPN_CALTROP, // 24
	TFC_WPN_CONCUSSIONGRENADE,
	TFC_WPN_NORMALGRENADE,
	TFC_WPN_NAILGRENADE,
	TFC_WPN_MIRVGRENADE,
	TFC_WPN_NAPALMGRENADE,
	TFC_WPN_GASGRENADE,
	TFC_WPN_EMPGRENADE,
};

enum {
	TFC_PC_SCOUT = 1,
	TFC_PC_SNIPER,
	TFC_PC_SOLDIER,
	TFC_PC_DEMOMAN,
	TFC_PC_MEDIC,
	TFC_PC_HWGUY,
	TFC_PC_PYRO,
	TFC_PC_SPY,
	TFC_PC_ENGENEER,	// Typo; preserved for backward compatibility
	TFC_PC_ENGINEER = 9,
	TFC_PC_CIVILIAN = 11,
};

// Goal items
#define TFC_GOALITEM_BLUE   (1 << 17)
#define TFC_GOALITEM_RED    (1 << 18)
#define TFC_GOALITEM_YELLOW (1 << 24)
#define TFC_GOALITEM_GREEN  (1 << 25)
