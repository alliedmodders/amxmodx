/* Sven Co-op functions
 *
 * (c) 2007, XxAvalanchexX
 * This file is provided as is (no warranties).
 */

#if defined _svencoop_const_included
  #endinput
#endif
#define _svencoop_const_included

/* SvenCoop weapons */
enum
{
	SCW_CROWBAR = 1,
	SCW_9MMHANDGUN = 2, // ammo_9mmAR, ammo_9mmbox, ammo_9mmclip, ammo_glockclip, ammo_mp5clip
	SCW_357 = 3, // ammo_357
	SCW_9MMAR = 4, // ammo_9mmAR, ammo_9mmbox, ammo_9mmclip, ammo_glockclip, ammo_mp5clip, 
	SCW_CROSSBOW = 6, // ammo_crossbow
	SCW_SHOTGUN = 7, // ammo_buckshot
	SCW_RPG = 8, // ammo_rpgclip
	SCW_GAUSS = 9, // ammo_egonclip, ammo_gaussclip
	SCW_EGON = 10, // ammo_egonclip, ammo_gaussclip
	SCW_HORNETGUN = 11,
	SCW_HANDGRENADE = 12,
	SCW_TRIPMINE = 13,
	SCW_SATCHEL = 14,
	SCW_SNARK = 15,
	SCW_UZIAKIMBO = 16, // ammo_9mmAR, ammo_9mmbox, ammo_9mmclip, ammo_glockclip, ammo_mp5clip
	SCW_UZI = 17, // ammo_9mmAR, ammo_9mmbox, ammo_9mmclip, ammo_glockclip, ammo_mp5clip
	SCW_MEDKIT = 18,
	SCW_CROWBAR_ELECTRIC = 19, // item_battery
	SCW_PIPEWRENCH = 20,
	SCW_MINIGUN = 21, // ammo_556
	SCW_GRAPPLE = 22,
	SCW_SNIPERRIFLE = 23, // ammo_762

	// USE ONLY FOR sc_*et_user_bpammo NATIVES!
	SCW_ARGRENADE = 24 // ammo_ARgrenades, ammo_mp5grenades
};

stock const SCW_MAX_CLIP[25] =
{
	-1,	// NONE = 0
	0,	// SCW_CROWBAR = 1
	17,	// SCW_9MMHANDGUN = 2
	6,	// SCW_357 = 3
	50,	// SCW_9MMAR = 4
	-1,	// NONE = 5
	5,	// SCW_CROSSBOW = 6
	8,	// SCW_SHOTGUN = 7
	1,	// SCW_RPG = 8
	0,	// SCW_GAUSS = 9
	0,	// SCW_EGON = 10
	0,	// SCW_HORNETGUN = 11
	0,	// SCW_HANDGRENADE = 12
	0,	// SCW_TRIPMINE = 13
	0,	// SCW_SATCHEL = 14
	0,	// SCW_SNARK = 15
	32,	// SCW_UZIAKIMBO = 16
	32,	// SCW_UZI = 17
	0,	// SCW_MEDKIT = 18
	0,	// SCW_CROWBAR_ELECTRIC = 19
	0,	// SCW_PIPEWRENCH = 20
	0,	// SCW_MINIGUN = 21
	0,	// SCW_GRAPPLE = 22
	5,	// SCW_SNIPERRIFLE = 23
	0	// SCW_ARGRENADE = 24 
}

stock const SCW_MAX_AMMO[25] =
{
	-1,	// NONE = 0
	0,	// SCW_CROWBAR = 1
	250,	// SCW_9MMHANDGUN = 2
	36,	// SCW_357 = 3
	250,	// SCW_9MMAR = 4
	-1,	// NONE = 5
	50,	// SCW_CROSSBOW = 6
	125,	// SCW_SHOTGUN = 7
	5,	// SCW_RPG = 8
	100,	// SCW_GAUSS = 9
	100,	// SCW_EGON = 10
	8,	// SCW_HORNETGUN = 11
	10,	// SCW_HANDGRENADE = 12
	5,	// SCW_TRIPMINE = 13
	5,	// SCW_SATCHEL = 14
	15,	// SCW_SNARK = 15
	250,	// SCW_UZIAKIMBO = 16
	250,	// SCW_UZI = 17
	100,	// SCW_MEDKIT = 18
	0,	// SCW_CROWBAR_ELECTRIC = 19
	0,	// SCW_PIPEWRENCH = 20
	999,	// SCW_MINIGUN = 21
	0,	// SCW_GRAPPLE = 22
	15,	// SCW_SNIPERRIFLE = 23
	10	// SCW_ARGRENADE = 24 
}
