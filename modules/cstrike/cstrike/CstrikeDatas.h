// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#ifndef CSTRIKE_DATA_H
#define CSTRIKE_DATA_H

/**
 * Weapon Ids for use with CS_OnBuyAttempt(), CS_OnBuy().
 */
#define CSI_NONE                CSW_NONE
#define CSI_P228                CSW_P228
#define CSI_SCOUT               CSW_SCOUT
#define CSI_HEGRENADE           CSW_HEGRENADE
#define CSI_XM1014              CSW_XM1014
#define CSI_C4                  CSW_C4
#define CSI_MAC10               CSW_MAC10
#define CSI_AUG                 CSW_AUG
#define CSI_SMOKEGRENADE        CSW_SMOKEGRENADE
#define CSI_ELITE               CSW_ELITE
#define CSI_FIVESEVEN           CSW_FIVESEVEN
#define CSI_UMP45               CSW_UMP45
#define CSI_SG550               CSW_SG550
#define CSI_GALIL               CSW_GALIL
#define CSI_FAMAS               CSW_FAMAS
#define CSI_USP                 CSW_USP
#define CSI_GLOCK18             CSW_GLOCK18
#define CSI_AWP                 CSW_AWP
#define CSI_MP5NAVY             CSW_MP5NAVY
#define CSI_M249                CSW_M249
#define CSI_M3                  CSW_M3
#define CSI_M4A1                CSW_M4A1
#define CSI_TMP                 CSW_TMP
#define CSI_G3SG1               CSW_G3SG1
#define CSI_FLASHBANG           CSW_FLASHBANG
#define CSI_DEAGLE              CSW_DEAGLE
#define CSI_SG552               CSW_SG552
#define CSI_AK47                CSW_AK47
#define CSI_P90                 CSW_P90
#define CSI_SHIELDGUN           CSW_SHIELDGUN   // The real CS value, use CSI_SHIELD instead.
#define CSI_VEST                CSW_VEST        // Custom
#define CSI_VESTHELM            CSW_VESTHELM    // Custom
#define CSI_DEFUSER             33              // Custom
#define CSI_NVGS                34              // Custom
#define CSI_SHIELD              35              // Custom - The value passed by the forward, more convenient for plugins.
#define CSI_PRIAMMO             36              // Custom
#define CSI_SECAMMO             37              // Custom
#define CSI_MAX_COUNT           38
#define CSI_LAST_WEAPON         CSW_LAST_WEAPON

#define CSI_ALL_WEAPONS         CSW_ALL_WEAPONS
#define CSI_ALL_PISTOLS         CSW_ALL_PISTOLS
#define CSI_ALL_SHOTGUNS        CSW_ALL_SHOTGUNS
#define CSI_ALL_SMGS            CSW_ALL_SMGS
#define CSI_ALL_RIFLES          CSW_ALL_RIFLES
#define CSI_ALL_SNIPERRIFLES    CSW_ALL_SNIPERRIFLES
#define CSI_ALL_MACHINEGUNS     CSW_ALL_MACHINEGUNS
#define CSI_ALL_GRENADES        CSW_ALL_GRENADES
#define CSI_ALL_ARMORS          CSW_ALL_ARMORS
#define CSI_ALL_GUNS            CSW_ALL_GUNS

/**
 * Weapons Ids.
 */
#define CSW_NONE                        0
#define CSW_P228						1
#define CSW_SCOUT						3
#define CSW_HEGRENADE					4
#define CSW_XM1014						5
#define CSW_C4							6
#define CSW_MAC10						7
#define CSW_AUG							8
#define CSW_SMOKEGRENADE				9
#define CSW_ELITE						10
#define CSW_FIVESEVEN					11
#define CSW_UMP45						12
#define CSW_SG550						13
#define CSW_GALIL						14
#define CSW_FAMAS						15
#define CSW_USP							16
#define CSW_GLOCK18						17
#define CSW_AWP							18
#define CSW_MP5NAVY						19
#define CSW_M249						20
#define CSW_M3							21
#define CSW_M4A1						22
#define CSW_TMP							23
#define CSW_G3SG1						24
#define CSW_FLASHBANG					25
#define CSW_DEAGLE						26
#define CSW_SG552						27
#define CSW_AK47						28
#define CSW_KNIFE						29
#define CSW_P90							30
#define CSW_VEST						31 // Brand new invention!
#define CSW_VESTHELM					32 // Brand new invention!
#define CSW_SHIELDGUN					99
#define CSW_LAST_WEAPON                 CSW_P90

#define CSW_ALL_WEAPONS      (~(1 << CSW_VEST))
#define CSW_ALL_PISTOLS      (1 << CSW_P228 | 1 << CSW_ELITE | 1 << CSW_FIVESEVEN | 1 << CSW_USP | 1 << CSW_GLOCK18 | 1 << CSW_DEAGLE)
#define CSW_ALL_SHOTGUNS     (1 << CSW_M3 | 1 << CSW_XM1014)
#define CSW_ALL_SMGS         (1 << CSW_MAC10 | 1 << CSW_UMP45 | 1 << CSW_MP5NAVY | 1 << CSW_TMP | 1 << CSW_P90)
#define CSW_ALL_RIFLES       (1 << CSW_AUG | 1 << CSW_GALIL | 1 << CSW_FAMAS | 1 << CSW_M4A1 | 1 << CSW_AK47 | 1 << CSW_SG552)
#define CSW_ALL_SNIPERRIFLES (1 << CSW_SCOUT | 1 << CSW_AWP | 1 << CSW_G3SG1 | 1 << CSW_SG550)
#define CSW_ALL_MACHINEGUNS  (1 << CSW_M249)
#define CSW_ALL_GRENADES     (1 << CSW_HEGRENADE | 1 << CSW_SMOKEGRENADE | 1 << CSW_FLASHBANG)
#define CSW_ALL_ARMORS       (1 << CSW_VEST | 1 << CSW_VESTHELM)
#define CSW_ALL_GUNS         (CSW_ALL_PISTOLS | CSW_ALL_SHOTGUNS | CSW_ALL_SMGS | CSW_ALL_RIFLES | CSW_ALL_SNIPERRIFLES | CSW_ALL_MACHINEGUNS)

/**
 * Armoury entity ids for use with cs_get/set_armoury_type().
 */
#define CSA_MP5NAVY						0
#define CSA_TMP							1
#define CSA_P90							2
#define CSA_MAC10						3
#define CSA_AK47						4
#define CSA_SG552						5
#define CSA_M4A1						6
#define CSA_AUG							7
#define CSA_SCOUT						8
#define CSA_G3SG1						9
#define CSA_AWP							10
#define CSA_M3							11
#define CSA_XM1014						12
#define CSA_M249						13
#define CSA_FLASHBANG					14
#define CSA_HEGRENADE					15
#define CSA_VEST						16
#define CSA_VESTHELM					17
#define CSA_SMOKEGRENADE				18

/**
 * Weapon states of the following weapons: usp, elite, glock18, famas and m4a1.
 */
enum WeaponState
{
	WPNSTATE_USP_SILENCED       = (1 << 0),
	WPNSTATE_GLOCK18_BURST_MODE = (1 << 1),
	WPNSTATE_M4A1_SILENCED      = (1 << 2),
	WPNSTATE_ELITE_LEFT         = (1 << 3),
	WPNSTATE_FAMAS_BURST_MODE   = (1 << 4),
	WPNSTATE_SHIELD_DRAWN       = (1 << 5),
};

/**
 * M4A1 animations
 */
enum m4a1_e
{
	M4A1_IDLE,
	M4A1_SHOOT1,
	M4A1_SHOOT2,
	M4A1_SHOOT3,
	M4A1_RELOAD,
	M4A1_DRAW,
	M4A1_ATTACH_SILENCER,
	M4A1_UNSIL_IDLE,
	M4A1_UNSIL_SHOOT1,
	M4A1_UNSIL_SHOOT2,
	M4A1_UNSIL_SHOOT3,
	M4A1_UNSIL_RELOAD,
	M4A1_UNSIL_DRAW,
	M4A1_DETACH_SILENCER
};

/**
 * USP animations
 */
enum usp_e
{
	USP_IDLE,
	USP_SHOOT1,
	USP_SHOOT2,
	USP_SHOOT3,
	USP_SHOOT_EMPTY,
	USP_RELOAD,
	USP_DRAW,
	USP_ATTACH_SILENCER,
	USP_UNSIL_IDLE,
	USP_UNSIL_SHOOT1,
	USP_UNSIL_SHOOT2,
	USP_UNSIL_SHOOT3,
	USP_UNSIL_SHOOT_EMPTY,
	USP_UNSIL_RELOAD,
	USP_UNSIL_DRAW,
	USP_DETACH_SILENCER
};

/**
 * States of gameplay zones.
 */
enum SignalState
{
	SIGNAL_BUY       = (1 << 0),
	SIGNAL_BOMB      = (1 << 1),
	SIGNAL_RESCUE    = (1 << 2),
	SIGNAL_ESCAPE    = (1 << 3),
	SIGNAL_VIPSAFETY = (1 << 4),
};

/**
 * Default team indexes.
 */
#define TEAM_UNASSIGNED					0
#define TEAM_T							1
#define TEAM_CT							2
#define TEAM_SPECTATOR					3

/**
 * Default color of defuser HUD icon.
 */
#define DEFUSER_COLOUR_R				0
#define DEFUSER_COLOUR_G				160
#define DEFUSER_COLOUR_B				0

/**
 * Values for use with ScoreAttrib message.
 */
#define SCOREATTRIB_NOTHING				0
#define SCOREATTRIB_DEAD				1
#define SCOREATTRIB_BOMB				2 // t only
#define SCOREATTRIB_VIP					4 // ct only

/**
 * Armor types for use with cs_get/Set_user_armor().
 */
#define CS_ARMOR_NONE					0
#define CS_ARMOR_KEVLAR					1
#define CS_ARMOR_ASSAULTSUIT			2

/**
 * Default FOV values.
 */
#define CS_FIRST_ZOOM					40
#define CS_SECOND_AWP_ZOOM				10
#define CS_SECOND_NONAWP_ZOOM			15
#define CS_AUGSG552_ZOOM				55
#define CS_NO_ZOOM						90

/**
 * Zoom value for use with cs_get/set_user_zoom().
 */
enum
{
	CS_RESET_ZOOM = 0,
	CS_SET_NO_ZOOM,
	CS_SET_FIRST_ZOOM,
	CS_SET_SECOND_ZOOM,
	CS_SET_AUGSG552_ZOOM,

};

/**
 * Player's model types.
 */
enum CS_Internal_Models 
{
	CS_DONTCHANGE  = 0,
	CS_CT_URBAN    = 1,
	CS_T_TERROR    = 2,
	CS_T_LEET      = 3,
	CS_T_ARCTIC    = 4,
	CS_CT_GSG9     = 5,
	CS_CT_GIGN     = 6,
	CS_CT_SAS      = 7,
	CS_T_GUERILLA  = 8,
	CS_CT_VIP      = 9,
	CZ_T_MILITIA   = 10,
	CZ_CT_SPETSNAZ = 11
};

/**
 * Menu states.
 */
typedef enum
{
	Menu_OFF,
	Menu_ChooseTeam,
	Menu_IGChooseTeam,
	Menu_ChooseAppearance,
	Menu_Buy,
	Menu_BuyPistol,
	Menu_BuyRifle,
	Menu_BuyMachineGun,
	Menu_BuyShotgun,
	Menu_BuySubMachineGun,
	Menu_BuyItem,
	Menu_Radio1,
	Menu_Radio2,
	Menu_Radio3,
	Menu_ClientBuy

} Menu;


/**
 * Weapon class types.
 */
enum CsWeaponClassType
{
	CS_WEAPONCLASS_NONE          = 0,
	CS_WEAPONCLASS_KNIFE         = 1,
	CS_WEAPONCLASS_PISTOL        = 2,
	CS_WEAPONCLASS_GRENADE       = 3,
	CS_WEAPONCLASS_SUBMACHINEGUN = 4,
	CS_WEAPONCLASS_SHOTGUN       = 5,
	CS_WEAPONCLASS_MACHINEGUN    = 6,
	CS_WEAPONCLASS_RIFLE         = 7,
	CS_WEAPONCLASS_SNIPERRIFLE   = 8,
	CS_WEAPONCLASS_MAX_COUNT     = 9,
};

/**
 * Weapon infos.
 */
typedef struct
{
	int id;
	int cost;
	int clipCost;
	int buyClipSize;
	int gunClipSize;
	int maxRounds;
	int ammoType;
	char *entityName;
}
WeaponInfoStruct;

/**
 * Weapon infos for use with cs_get_weapon_info().
 */
enum CsWeaponInfo
{
	CS_WEAPONINFO_COST          = 0,
	CS_WEAPONINFO_CLIP_COST     = 1,
	CS_WEAPONINFO_BUY_CLIP_SIZE = 2,
	CS_WEAPONINFO_GUN_CLIP_SIZE = 3,
	CS_WEAPONINFO_MAX_ROUNDS    = 4,
	CS_WEAPONINFO_AMMO_TYPE     = 5,
};

/**
 * Weapon default cost.
 */
enum WeaponCostType
{
	AK47_PRICE      = 2500,
	AWP_PRICE       = 4750,
	DEAGLE_PRICE    = 650,
	G3SG1_PRICE     = 5000,
	SG550_PRICE     = 4200,
	GLOCK18_PRICE   = 400,
	M249_PRICE      = 5750,
	M3_PRICE        = 1700,
	M4A1_PRICE      = 3100,
	AUG_PRICE       = 3500,
	MP5NAVY_PRICE   = 1500,
	P228_PRICE      = 600,
	P90_PRICE       = 2350,
	UMP45_PRICE     = 1700,
	MAC10_PRICE     = 1400,
	SCOUT_PRICE     = 2750,
	SG552_PRICE     = 3500,
	TMP_PRICE       = 1250,
	USP_PRICE       = 500,
	ELITE_PRICE     = 800,
	FIVESEVEN_PRICE = 750,
	XM1014_PRICE    = 3000,
	GALIL_PRICE     = 2000,
	FAMAS_PRICE     = 2250,
	SHIELDGUN_PRICE = 2200
};

/**
 * Equipment default cost.
 */
enum ItemCostType
{
	ASSAULTSUIT_PRICE  = 1000,
	FLASHBANG_PRICE    = 200,
	HEGRENADE_PRICE    = 300,
	SMOKEGRENADE_PRICE = 300,
	KEVLAR_PRICE       = 650,
	HELMET_PRICE       = 350,
	NVG_PRICE          = 1250,
	DEFUSEKIT_PRICE    = 200
};

#endif // CSTRIKE_DATA_H
