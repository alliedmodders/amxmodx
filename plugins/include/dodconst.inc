// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _dodconst_included
  #endinput
#endif
#define _dodconst_included

/* DoD teams */
#define ALLIES			1
#define AXIS 			2

#define FT_NEW			1<<0
#define FT_OLD			1<<1

#define STAMINA_SET		0
#define STAMINA_RESET		1

#define FUSE_SET		0
#define FUSE_RESET		1

#define DODMAX_WEAPONS		46 // 5 slots for custom weapons

// DoD Weapon Types
enum
{
	DODWT_PRIMARY = 0,
	DODWT_SECONDARY,
	DODWT_MELEE,
	DODWT_GRENADE, 
	DODWT_OTHER
};

// Ammo Channels
#define AMMO_SMG 1 		// thompson, greasegun, sten, mp40
#define AMMO_ALTRIFLE 2 	// carbine, k43, mg34
#define AMMO_RIFLE 3 		// garand, enfield, scoped enfield, k98, scoped k98
#define AMMO_PISTOL 4 		// colt, webley, luger
#define AMMO_SPRING 5 		// springfield
#define AMMO_HEAVY 6 		// bar, bren, stg44, fg42, scoped fg42
#define AMMO_MG42 7    		// mg42
#define AMMO_30CAL 8 		// 30cal
#define AMMO_GREN 9 		// grenades (should be all 3 types)
#define AMMO_ROCKET 13 		// bazooka, piat, panzerschreck
enum { 
	PS_NOPRONE =0,
	PS_PRONE,
	PS_PRONEDEPLOY,
	PS_DEPLOY,
};

/* info types for dod_get_map_info native */
enum {
	MI_ALLIES_TEAM = 0,
	MI_ALLIES_PARAS,
	MI_AXIS_PARAS,
};

/* DoD weapons */
enum {
	DODW_AMERKNIFE = 1,
	DODW_GERKNIFE,
	DODW_COLT,
	DODW_LUGER,
	DODW_GARAND,
	DODW_SCOPED_KAR,
	DODW_THOMPSON,
	DODW_STG44,
	DODW_SPRINGFIELD,
	DODW_KAR,
	DODW_BAR,
	DODW_MP40,
	DODW_HANDGRENADE,
	DODW_STICKGRENADE,
	DODW_STICKGRENADE_EX,
	DODW_HANDGRENADE_EX,
	DODW_MG42,
	DODW_30_CAL,
	DODW_SPADE,
	DODW_M1_CARBINE,
	DODW_MG34,
	DODW_GREASEGUN,
	DODW_FG42,
	DODW_K43,
	DODW_ENFIELD,
	DODW_STEN,
	DODW_BREN,
	DODW_WEBLEY,
	DODW_BAZOOKA,
	DODW_PANZERSCHRECK,
	DODW_PIAT,
	DODW_SCOPED_FG42,
	DODW_FOLDING_CARBINE,
	DODW_KAR_BAYONET,
	DODW_SCOPED_ENFIELD,
	DODW_MILLS_BOMB,
	DODW_BRITKNIFE,
	DODW_GARAND_BUTT,
	DODW_ENFIELD_BAYONET,
	DODW_MORTAR,
	DODW_K43_BUTT,
};

/* DoD Classes */
enum {
	DODC_GARAND = 1,
	DODC_CARBINE,
	DODC_THOMPSON, 
	DODC_GREASE, 
	DODC_SNIPER, 
	DODC_BAR, 
	DODC_30CAL, 
	DODC_BAZOOKA, 
	//DODC_ALLIES_MORTAR,
	DODC_KAR = 10, 
	DODC_K43, 
	DODC_MP40, 
	DODC_MP44, 
	DODC_SCHARFSCHUTZE, 
	DODC_FG42, 
	DODC_SCOPED_FG42, 
	DODC_MG34, 
	DODC_MG42, 
	DODC_PANZERJAGER, 
	//DODC_AXIS_MORTAR,
	DODC_ENFIELD = 21, 
	DODC_STEN, 
	DODC_MARKSMAN, 
	DODC_BREN, 
	DODC_PIAT, 
	//DODC_BRIT_MORTAR,
};

/* DoD stats constants */
enum {
	DODX_KILLS = 0,
	DODX_DEATHS,
	DODX_HEADSHOTS,
	DODX_TEAMKILLS,
	DODX_SHOTS,
	DODX_HITS,
	DODX_DAMAGE,
	DODX_POINTS,
	DODX_RANK,
	DODX_MAX_STATS
}
