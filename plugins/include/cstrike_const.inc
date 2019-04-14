// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Functions
//

#if defined _cstrike_const_included
	#endinput
#endif
#define _cstrike_const_included

/**
 * IDs of weapons in CS
 */
#define CSW_NONE            0
#define CSW_P228            1
#define CSW_GLOCK           2  // Unused by game, See CSW_GLOCK18.
#define CSW_SCOUT           3
#define CSW_HEGRENADE       4
#define CSW_XM1014          5
#define CSW_C4              6
#define CSW_MAC10           7
#define CSW_AUG             8
#define CSW_SMOKEGRENADE    9
#define CSW_ELITE           10
#define CSW_FIVESEVEN       11
#define CSW_UMP45           12
#define CSW_SG550           13
#define CSW_GALI            14
#define CSW_GALIL           14
#define CSW_FAMAS           15
#define CSW_USP             16
#define CSW_GLOCK18         17
#define CSW_AWP             18
#define CSW_MP5NAVY         19
#define CSW_M249            20
#define CSW_M3              21
#define CSW_M4A1            22
#define CSW_TMP             23
#define CSW_G3SG1           24
#define CSW_FLASHBANG       25
#define CSW_DEAGLE          26
#define CSW_SG552           27
#define CSW_AK47            28
#define CSW_KNIFE           29
#define CSW_P90             30
#define CSW_VEST            31  // Custom
#define CSW_VESTHELM        32  // Custom
#define CSW_SHIELDGUN       99
#define CSW_LAST_WEAPON     CSW_P90

const CSW_ALL_WEAPONS      = (~(1<<CSW_VEST));
const CSW_ALL_PISTOLS      = (1<<CSW_P228  | 1<<CSW_ELITE | 1<<CSW_FIVESEVEN | 1<<CSW_USP | 1<<CSW_GLOCK18 | 1<<CSW_DEAGLE);
const CSW_ALL_SHOTGUNS     = (1<<CSW_M3    | 1<<CSW_XM1014);
const CSW_ALL_SMGS         = (1<<CSW_MAC10 | 1<<CSW_UMP45 | 1<<CSW_MP5NAVY | 1<<CSW_TMP  | 1<<CSW_P90);
const CSW_ALL_RIFLES       = (1<<CSW_AUG   | 1<<CSW_GALIL | 1<<CSW_FAMAS   | 1<<CSW_M4A1 | 1<<CSW_AK47 | 1<<CSW_SG552);
const CSW_ALL_SNIPERRIFLES = (1<<CSW_SCOUT | 1<<CSW_AWP   | 1<<CSW_G3SG1   | 1<<CSW_SG550);
const CSW_ALL_MACHINEGUNS  = (1<<CSW_M249);
const CSW_ALL_GRENADES     = (1<<CSW_HEGRENADE | 1<<CSW_SMOKEGRENADE | 1<<CSW_FLASHBANG);
const CSW_ALL_ARMORS       = (1<<CSW_VEST | 1<<CSW_VESTHELM);
const CSW_ALL_GUNS         = (CSW_ALL_PISTOLS | CSW_ALL_SHOTGUNS | CSW_ALL_SMGS | CSW_ALL_RIFLES | CSW_ALL_SNIPERRIFLES | CSW_ALL_MACHINEGUNS);

/**
 * @section Team and team model constants, used by cs_[get|set]_user_team().
 */

/**
 * Internal Counter-Strike model id constants.
 *
 * @note Model ids starting with CZ_ are only valid in Condition Zero.
 */
enum CsInternalModel
{
	CS_NORESET     = -1,
	CS_DONTCHANGE  = 0,
	CS_UNASSIGNED  = 0,
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
	CZ_CT_SPETSNAZ = 11,
};

/**
 * Counter-Strike team id constants.
 */
enum CsTeams
{
	CS_TEAM_UNASSIGNED = 0,
	CS_TEAM_T          = 1,
	CS_TEAM_CT         = 2,
	CS_TEAM_SPECTATOR  = 3,
};

/**
 * @endsection
 */

/**
 * Counter-Strike armor types for use with cs_[get|set]_user_armor().
 */
enum CsArmorType
{
	CS_ARMOR_NONE     = 0,  // no armor
	CS_ARMOR_KEVLAR   = 1,  // body vest only
	CS_ARMOR_VESTHELM = 2,  // vest and helmet
};

/**
 * Map zone flags returned by cs_get_user_mapzones().
 */
#define CS_MAPZONE_BUY              (1<<0) // Buyzone
#define CS_MAPZONE_BOMBTARGET       (1<<1) // Bomb target zone
#define CS_MAPZONE_HOSTAGE_RESCUE   (1<<2) // Hostage rescue zone
#define CS_MAPZONE_ESCAPE           (1<<3) // Terrorist escape zone
#define CS_MAPZONE_VIP_SAFETY       (1<<4) // VIP escape zone

/**
 * Constants used for cs_[get|set]_user_zoom().
 */
enum
{
	CS_RESET_ZOOM = 0,      // Reset any zoom blocking (mode has no effect)
	CS_SET_NO_ZOOM,         // Disable any sort of zoom
	CS_SET_FIRST_ZOOM,      // Set first zoom level (AWP style)
	CS_SET_SECOND_ZOOM,     // Set second zoom level (AWP style)
	CS_SET_AUGSG552_ZOOM,   // Set AUG/SG552 zoom style
};

/**
 * Constants used for the CS_OnBuy() and CS_OnBuyAttempt() forwards.
 *
 * @note While these mostly overlap with the CSW_* constants the CSI_* constants
 *       contain custom AMXX values that do not correspond to any real value in
 *       the game. The CSI_* constants should therefore be used for consistency.
 */
#define CSI_NONE                CSW_NONE
#define CSI_P228                CSW_P228
#define CSI_GLOCK               CSW_GLOCK  // Unused by game, See CSI_GLOCK18.
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
#define CSI_KNIFE               CSW_KNIFE
#define CSI_P90                 CSW_P90
#define CSI_SHIELDGUN           CSW_SHIELDGUN   // The real CS value, use CSI_SHELD instead.
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
 * Player's movements constants.
 */
#define CS_PLAYER_FATAL_FALL_SPEED       1100.0
#define CS_PLAYER_MAX_SAFE_FALL_SPEED    500.0
#define CS_DAMAGE_FOR_FALL_SPEED         100.0 / (CS_PLAYER_FATAL_FALL_SPEED - CS_PLAYER_MAX_SAFE_FALL_SPEED)
#define CS_PLAYER_MIN_BOUNCE_SPEED       350
#define CS_PLAYER_FALL_PUNCH_THRESHHOLD  250.0

/**
 * Hint message flags for use with m_flDisplayHistory.
 */
#define CS_HINT_ROUND_STARTED     (1<<1)
#define CS_HINT_HOSTAGE_SEEN_FAR  (1<<2)
#define CS_HINT_HOSTAGE_SEEN_NEAR (1<<3)
#define CS_HINT_HOSTAGE_USED      (1<<4)
#define CS_HINT_HOSTAGE_INJURED   (1<<5)
#define CS_HINT_HOSTAGE_KILLED    (1<<6)
#define CS_HINT_FRIEND_SEEN       (1<<7)
#define CS_HINT_ENEMY_SEEN        (1<<8)
#define CS_HINT_FRIEND_INJURED    (1<<9)
#define CS_HINT_FRIEND_KILLED     (1<<10)
#define CS_HINT_ENEMY_KILLED      (1<<11)
#define CS_HINT_BOMB_RETRIEVED    (1<<12)
#define CS_HINT_AMMO_EXHAUSTED    (1<<15)
#define CS_HINT_IN_TARGET_ZONE    (1<<16)
#define CS_HINT_IN_RESCUE_ZONE    (1<<17)
#define CS_HINT_IN_ESCAPE_ZONE    (1<<18)
#define CS_HINT_IN_VIPSAFETY_ZONE (1<<19)
#define CS_HINT_NIGHTVISION       (1<<20)
#define CS_HINT_HOSTAGE_CTMOVE    (1<<21)
#define	CS_HINT_SPEC_DUCK         (1<<22)

/**
 * Constants for use with m_iIgnoreGlobalChat (ignoremsg client's command)
 */
#define CS_IGNOREMSG_NONE  0
#define CS_IGNOREMSG_ENEMY 1
#define CS_IGNOREMSG_TEAM  2

/**
 * Player's gait animation constants.
 */
enum CsPlayerAnim
{
	CS_PLAYER_IDLE          = 0,
	CS_PLAYER_WALK          = 1,
	CS_PLAYER_JUMP          = 2,
	CS_PLAYER_SUPERJUMP     = 3,
	CS_PLAYER_DIE           = 4,
	CS_PLAYER_ATTACK1       = 5,
	CS_PLAYER_ATTACK2       = 6,
	CS_PLAYER_FLINCH        = 7,
	CS_PLAYER_LARGE_FLINCH  = 8,
	CS_PLAYER_RELOAD        = 9,
	CS_PLAYER_HOLDBOMB      = 10,
};

/**
 * Player's menu states for use with m_iMenu.
 */
enum CsMenu
{
	CS_Menu_OFF              = 0,
	CS_Menu_ChooseTeam       = 1,
	CS_Menu_IGChooseTeam     = 2,
	CS_Menu_ChooseAppearance = 3,
	CS_Menu_Buy              = 4,
	CS_Menu_BuyPistol        = 5,
	CS_Menu_BuyRifle         = 6,
	CS_Menu_BuyMachineGun    = 7,
	CS_Menu_BuyShotgun       = 8,
	CS_Menu_BuySubMachineGun = 9,
	CS_Menu_BuyItem          = 10,
	CS_Menu_Radio1           = 11,
	CS_Menu_Radio2           = 12,
	CS_Menu_Radio3           = 13,
	CS_Menu_ClientBuy        = 14,
};

/**
 * Player's join states for use with m_iJoiningState.
 */
enum CsJoinState
{
	CS_STATE_JOINED           = 0,
	CS_STATE_SHOW_LTEXT       = 1,
	CS_STATE_READING_LTEXT    = 2,
	CS_STATE_SHOW_TEAM_SELECT = 3,
	CS_STATE_PICKING_TEAM     = 4,
	CS_STATE_GET_INTO_GAME    = 5,
};

/**
 * Player's throw direction on killed for use with m_iThrowDirection.
 */
enum CsThrowDirection
{
	CS_THROW_NONE                = 0,
	CS_THROW_FORWARD             = 1,
	CS_THROW_BACKWARD            = 2,
	CS_THROW_HITVEL              = 3,
	CS_THROW_BOMB                = 4,
	CS_THROW_GRENADE             = 5,
	CS_THROW_HITVEL_MINUS_AIRVEL = 6,
};

/**
 * @section Player's status bar constants.
 */

/**
 * Constants associated with mp_playerid server cvar.
 */
#define CS_PLAYERID_EVERYONE 0
#define CS_PLAYERID_TEAMONLY 1
#define CS_PLAYERID_OFF      2

/**
 * Maximum range a status bar can be displayed.
 */
#define CS_MAX_ID_RANGE             2048
#define CS_MAX_SPECTATOR_ID_RANGE   8192

/**
 * Maximum buffer length of a status bar message.
 */
#define CS_SBAR_STRING_SIZE         128

/**
 * Constants associated to CS_SBAR_ID_TARGETTYPE.
 */
#define CS_SBAR_TARGETTYPE_TEAMMATE 1
#define CS_SBAR_TARGETTYPE_ENEMY    2
#define CS_SBAR_TARGETTYPE_HOSTAGE  3

/**
 * Status bar states.
 */
enum CsStatusBar
{
	CS_SBAR_ID_TARGETTYPE   = 1,
	CS_SBAR_ID_TARGETNAME   = 2,
	CS_SBAR_ID_TARGETHEALTH = 3,
};

/**
 * @endsection
 */

/**
 * Weapon slot types.
 */
#define CS_WEAPONSLOT_PRIMARY    1
#define CS_WEAPONSLOT_SECONDARY  2
#define CS_WEAPONSLOT_KNIFE      3
#define CS_WEAPONSLOT_GRENADE    4
#define CS_WEAPONSLOT_C4         5

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
};

/**
 * Weapon autobuy class types.
 */
enum CsAutoBuyClassType
{
	CS_AUTOBUYCLASS_PRIMARY     = (1<<0),
	CS_AUTOBUYCLASS_SECONDARY   = (1<<1),
	CS_AUTOBUYCLASS_AMMO        = (1<<2),
	CS_AUTOBUYCLASS_ARMOR       = (1<<3),
	CS_AUTOBUYCLASS_DEFUSER     = (1<<4),
	CS_AUTOBUYCLASS_PISTOL      = (1<<5),
	CS_AUTOBUYCLASS_SMG         = (1<<6),
	CS_AUTOBUYCLASS_RIFLE       = (1<<7),
	CS_AUTOBUYCLASS_SNIPERRIFLE = (1<<8),
	CS_AUTOBUYCLASS_SHOTGUN     = (1<<9),
	CS_AUTOBUYCLASS_MACHINEGUN  = (1<<10),
	CS_AUTOBUYCLASS_GRENADE     = (1<<11),
	CS_AUTOBUYCLASS_NIGHTVISION = (1<<12),
	CS_AUTOBUYCLASS_SHIELD      = (1<<13),
};

/**
 * Ammo types for use with cs_get_weapon_info().
 */
enum CsAmmoType
{
	CS_AMMO_BUCKSHOT   = 0,
	CS_AMMO_9MM        = 1,
	CS_AMMO_556NATO    = 2,
	CS_AMMO_556NATOBOX = 3,
	CS_AMMO_762NATO    = 4,
	CS_AMMO_45ACP      = 5,
	CS_AMMO_50AE       = 6,
	CS_AMMO_338MAGNUM  = 7,
	CS_AMMO_57MM       = 8,
	CS_AMMO_357SIG     = 9,
};

/**
 * Weapon info types for use with cs_get_weapon_info().
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
enum CsWeaponCostType
{
	CS_AK47_PRICE      = 2500,
	CS_AWP_PRICE       = 4750,
	CS_DEAGLE_PRICE    = 650,
	CS_G3SG1_PRICE     = 5000,
	CS_SG550_PRICE     = 4200,
	CS_GLOCK18_PRICE   = 400,
	CS_M249_PRICE      = 5750,
	CS_M3_PRICE        = 1700,
	CS_M4A1_PRICE      = 3100,
	CS_AUG_PRICE       = 3500,
	CS_MP5NAVY_PRICE   = 1500,
	CS_P228_PRICE      = 600,
	CS_P90_PRICE       = 2350,
	CS_UMP45_PRICE     = 1700,
	CS_MAC10_PRICE     = 1400,
	CS_SCOUT_PRICE     = 2750,
	CS_SG552_PRICE     = 3500,
	CS_TMP_PRICE       = 1250,
	CS_USP_PRICE       = 500,
	CS_ELITE_PRICE     = 800,
	CS_FIVESEVEN_PRICE = 750,
	CS_XM1014_PRICE    = 3000,
	CS_GALIL_PRICE     = 2000,
	CS_FAMAS_PRICE     = 2250,
	CS_SHIELDGUN_PRICE = 2200
};

/**
 * Equipment default cost.
 */
enum CsItemCostType
{
	CS_ASSAULTSUIT_PRICE  = 1000,
	CS_FLASHBANG_PRICE    = 200,
	CS_HEGRENADE_PRICE    = 300,
	CS_SMOKEGRENADE_PRICE = 300,
	CS_KEVLAR_PRICE       = 650,
	CS_HELMET_PRICE       = 350,
	CS_NVG_PRICE          = 1250,
	CS_DEFUSEKIT_PRICE    = 200
};

/**
 * Ammo default cost.
 */
enum CsAmmoCostType
{
	CS_AMMO_338MAG_PRICE   = 125,
	CS_AMMO_357SIG_PRICE   = 50,
	CS_AMMO_45ACP_PRICE    = 25,
	CS_AMMO_50AE_PRICE     = 40,
	CS_AMMO_556NATO_PRICE  = 60,
	CS_AMMO_57MM_PRICE     = 50,
	CS_AMMO_762NATO_PRICE  = 80,
	CS_AMMO_9MM_PRICE      = 20,
	CS_AMMO_BUCKSHOT_PRICE = 65
};

/**
 * Weapon state for use with m_iWeaponState.
 */
#define CS_WPNSTATE_USP_SILENCED       (1<<0)
#define CS_WPNSTATE_GLOCK18_BURST_MODE (1<<1)
#define CS_WPNSTATE_M4A1_SILENCED      (1<<2)
#define CS_WPNSTATE_ELITE_LEFT         (1<<3)
#define CS_WPNSTATE_FAMAS_BURST_MODE   (1<<4)
#define CS_WPNSTATE_SHIELD_DRAWN       (1<<5)

/**
 * Internal commands, for use with m_flLastCommandTime.
 */
#define CS_CMD_SAY           0
#define CS_CMD_SAYTEAM       1
#define CS_CMD_FULLUPDATE    2
#define CS_CMD_VOTE          3
#define CS_CMD_VOTEMAP       4
#define CS_CMD_LISTMAPS      5
#define CS_CMD_LISTPLAYERS   6
#define CS_CMD_NIGHTVISION   7

/**
 * Signal state for use with m_signals.
 */
#define CS_SIGNAL_BUY        (1<<0)
#define CS_SIGNAL_BOMB       (1<<1)
#define CS_SIGNAL_RESCUE     (1<<2)
#define CS_SIGNAL_ESCAPE     (1<<3)
#define CS_SIGNAL_VIPSAFETY  (1<<4)
