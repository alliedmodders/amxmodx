// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#ifndef NS_CONST_H
#define NS_CONST_H



// Offsets (used in NPData.cpp)

#define NEED_TO_FIND 0


#define OFFSET_WIN_RESOURCES		1816
#define OFFSET_LIN_RESOURCES		1836

#define OFFSET_WIN_WEAPDMG			408
#define OFFSET_LIN_WEAPDMG			424

#define OFFSET_WIN_WEAPRANGE		404
#define OFFSET_LIN_WEAPRANGE		420

#define OFFSET_WIN_WEAPCLIP			364
#define OFFSET_LIN_WEAPCLIP			380

#define OFFSET_WIN_WEAPID			324
#define OFFSET_LIN_WEAPID			340

// Next weapon in the item linked list
#define	OFFSET_WIN_WEAP_NEXT		320
#define	OFFSET_LIN_WEAP_NEXT		336

#define OFFSET_WIN_HIVE_TRAIT		488
#define OFFSET_LIN_HIVE_TRAIT		504

#define OFFSET_WIN_SCORE			6588
#define OFFSET_LIN_SCORE			6608

#define OFFSET_WIN_EXP				6512
#define OFFSET_LIN_EXP				6532

#define OFFSET_WIN_POINTS			6520
#define OFFSET_LIN_POINTS			6540

#define OFFSET_WIN_AMMO_LMG			1116
#define OFFSET_LIN_AMMO_LMG			1136

#define OFFSET_WIN_AMMO_PISTOL		1120
#define OFFSET_LIN_AMMO_PISTOL		1140

#define OFFSET_WIN_AMMO_SHOTGUN		1124
#define OFFSET_LIN_AMMO_SHOTGUN		1144

#define OFFSET_WIN_AMMO_HMG			1128
#define OFFSET_LIN_AMMO_HMG			1148

#define OFFSET_WIN_AMMO_GL			1132
#define OFFSET_LIN_AMMO_GL			1152

#define OFFSET_WIN_AMMO_HG			1136
#define OFFSET_LIN_AMMO_HG			1156

#define OFFSET_WIN_DEATHS			1380
#define OFFSET_LIN_DEATHS			1400

#define OFFSET_WIN_STRUCTOWNER		324
#define OFFSET_LIN_STRUCTOWNER		340

#define OFFSET_WIN_HIVEABILITY		6248
#define OFFSET_LIN_HIVEABILITY		6268

#define OFFSET_WIN_PLAYER_ITEMS		1068
#define OFFSET_LIN_PLAYER_ITEMS		1088

#define OFFSET_WIN_NEXT_WEAPON		1280 
#define OFFSET_LIN_NEXT_WEAPON		1300

#define OFFSET_WIN_CURRENT_WEAPON	1092
#define OFFSET_LIN_CURRENT_WEAPON	1112 // +4 is the client known weapon if it's ever needed

#define OFFSET_WIN_LAST_WEAPON		1100
#define OFFSET_LIN_LAST_WEAPON		1120



#define OFFSET_WIN_ENTVAR			4
#define OFFSET_LIN_ENTVAR			4 // first entry in the virtual class (vtable is 0)

#define OFFSET_WIN_OBS_ENERGY		476
#define OFFSET_LIN_OBS_ENERGY		492

#define OFFSET_WIN_WELD_DONE		128
#define OFFSET_LIN_WELD_DONE		144

#define OFFSET_WIN_WELD_TIME		132
#define OFFSET_LIN_WELD_TIME		148

#define OFFSET_WIN_GHOST_STRUCTURE	472
#define OFFSET_LIN_GHOST_STRUCTURE	488

// The teamnumber for the specified team
#define OFFSET_WIN_GAMEPLAY_TEAMA	0x54
#define OFFSET_LIN_GAMEPLAY_TEAMA	0x64

#define OFFSET_WIN_GAMEPLAY_TEAMB	0x58
#define OFFSET_LIN_GAMEPLAY_TEAMB	0x68


#define OFFSET_WIN_UPGRADES_BOUGHT	0x1980
#define OFFSET_LIN_UPGRADES_BOUGHT	0x1994

#define OFFSET_WIN_UPGRADES_ACTIVE	0x198C
#define OFFSET_LIN_UPGRADES_ACTIVE	0x19A0

#define OFFSET_WIN_COMBAT_TECHTREE	0x1998
#define OFFSET_LIN_COMBAT_TECHTREE	0x19AC



// TODO: Symbols / signatures instead
// Not actually offsets, but easier to use MAKE_OFFSET stuff for this :o
// First functions listed in the disassembler
#define OFFSET_WIN_BASE				"?SUB_CallUseToggle@CBaseEntity@@QAEXXZ"
#define OFFSET_LIN_BASE				"_init"

#define OFFSET_LIN_MEMBERFUNCSTART	0x000EDBE8  // The location of _init in the binary
#define OFFSET_WIN_MEMBERFUNCSTART	0x00001000  // The location of CBaseEntity::SUB_CallUseToggle

// NOTE: These addresses are raw offsets, not including the
//       base addresses!  These are the exact offsets that 
//       the disassembler provides.
//       MAKE_MEMBER_OFFSET will compensate for the base address


// Recycle:   void AvHBaseBuildable::StartRecycle(void)
#define OFFSET_WIN_MEMBER_RECYCLE				MAKE_MEMBER_OFFSET(0x00053950)
#define OFFSET_LIN_MEMBER_RECYCLE				MAKE_MEMBER_OFFSET(0x00180800)


// Weldable:  void AvHWeldable::AddWeldTime(float Time)
#define OFFSET_WIN_MEMBER_TRIGGER_WELDABLE		MAKE_MEMBER_OFFSET(0x000A0E20)
#define OFFSET_LIN_MEMBER_TRIGGER_WELDABLE		MAKE_MEMBER_OFFSET(0x00232840)

// Research:  bool AvHTechTree::SetResearchDone(AvHMessageID inMessageID, bool inState)
#define OFFSET_WIN_MEMBER_SET_RESEARCH_DONE		MAKE_MEMBER_OFFSET(0x0009EC40)
#define OFFSET_LIN_MEMBER_SET_RESEARCH_DONE		MAKE_MEMBER_OFFSET(0x0022E740)

// Game Rules: AvHGameRules *GetGameRules(void)
#define OFFSET_WIN_GETGAMERULES					MAKE_MEMBER_OFFSET(0x00068000)
#define OFFSET_LIN_GETGAMERULES					MAKE_MEMBER_OFFSET(0x0019F930)

// Offset into the gamerules pointer to the TeamA / TeamB class
#define GAMERULES_TEAMA_OFFSET					0xCC
#define GAMERULES_TEAMB_OFFSET					0x2A8

#define AVHTEAM_RESOURCES_OFFSET				0x94

#define GAMERULES_TEAMA_RESOURCES				GAMERULES_TEAMA_OFFSET + AVHTEAM_RESOURCES_OFFSET
#define GAMERULES_TEAMB_RESOURCES				GAMERULES_TEAMB_OFFSET + AVHTEAM_RESOURCES_OFFSET


enum
{
	NSGame_CantTell,		/**< It is too soon to tell (can't find avhgameplay
								 entity or it doesn't have private data) */

	NSGame_MarineVAlien,	/**< Marine vs Aliens (standard) gameplay */
	NSGame_MarineVMarine,	/**< Marine vs Marine */
	NSGame_AlienVAlien,		/**< Alien  vs Alien  */

	NSGame_Unknown,			/**< Can find the gameplay entity, but can't 
								 determine gameplay type. */
};

enum
{
   MASK_NONE = 0,
   MASK_SIGHTED = 1,
   MASK_DETECTED = 2,
   MASK_BUILDABLE = 4,
   MASK_BASEBUILD0 = 8, // Base build slot #0
   MASK_WEAPONS1 = 8, // Marine weapons 1
   MASK_CARAPACE = 8, // Alien carapace
   MASK_WEAPONS2 = 16, // Marines weapons 2
   MASK_REGENERATION = 16, // Alien regeneration
   MASK_BASEBUILD1 = 16, // Base build slot #1
   MASK_WEAPONS3 = 32, // Marine weapons 3
   MASK_REDEMPTION = 32, // Alien redemption
   MASK_BASEBUILD2 = 32, // Base build slot #2
   MASK_ARMOR1 = 64, // Marine armor 1
   MASK_CELERITY = 64, // Alien celerity
   MASK_BASEBUILD3 = 64, // Base build slot #3
   MASK_ARMOR2 = 128, // Marine armor 2
   MASK_ADRENALINE = 128, // Alien adrenaline
   MASK_BASEBUILD4 = 128, // Base build slot #4
   MASK_ARMOR3 = 256, // Marine armor 3
   MASK_SILENCE = 256, // Alien silence
   MASK_BASEBUILD5 = 256, // Base build slot #5
   MASK_JETPACK = 512, // Marine jetpacks
   MASK_CLOAKING = 512, // Alien cloaking
   MASK_BASEBUILD6 = 512, // Base build slot #6
   MASK_FOCUS = 1024, // Alien focus
   MASK_MOTION = 1024, // Marine motion tracking
   MASK_BASEBUILD7 = 1024, // Base build slot #7
   MASK_SCENTOFFEAR = 2048,   // Alien scent of fear
   MASK_DEFENSE2 = 4096, // Defense level 2
   MASK_DEFENSE3 = 8192, // Defense level 3
   MASK_ELECTRICITY = 8192,   // Electricy
   MASK_MOVEMENT2 = 16384, // Movement level 2,
   MASK_MOVEMENT3 = 32768, // Movement level 3
   MASK_HEAVYARMOR = 32768,   // Marine heavy armor
   MASK_SENSORY2 = 65536, // Sensory level 2
   MASK_SENSORY3 = 131072, // Sensory level 3
   MASK_ALIEN_MOVEMENT = 262144,   // Onos is charging
   MASK_WALLSTICKING = 524288,   // Flag for wall-sticking
   MASK_PRIMALSCREAM = 1048576,   // Alien is in range of active primal scream
   MASK_UMBRA = 2097152, // In umbra
   MASK_DIGESTING = 4194304,   // When set on a visible player, player is digesting.  When set on invisible player, player is being digested
   MASK_RECYCLING = 8388608,   // Building is recycling
   MASK_TOPDOWN = 16777216,   // Commander view
   MASK_PLAYER_STUNNED = 33554432,   // Player has been stunned by stomp
   MASK_ENSNARED = 67108864,   // Webbed
   MASK_ALIEN_EMBRYO = 134217728, //268435456,   // Gestating
   MASK_SELECTABLE = 268435456, //536870912,   // ???
   MASK_PARASITED = 536870912,    //1073741824,   // Parasite flag
   MASK_SENSORY_NEARBY = 1073741824   //2147483648   // Sensory chamber in range
};

typedef enum
{
	AVH_USER3_NONE = 0,
	AVH_USER3_MARINE_PLAYER,
	AVH_USER3_COMMANDER_PLAYER,
	AVH_USER3_ALIEN_PLAYER1,
	AVH_USER3_ALIEN_PLAYER2,
	AVH_USER3_ALIEN_PLAYER3,
	AVH_USER3_ALIEN_PLAYER4,
	AVH_USER3_ALIEN_PLAYER5,
	AVH_USER3_ALIEN_EMBRYO,
	AVH_USER3_SPAWN_TEAMONE,
	AVH_USER3_SPAWN_TEAMTWO,
	AVH_USER3_PARTICLE_ON,				// only valid for AvHParticleEntity: entindex as int in fuser1, template index stored in fuser2
	AVH_USER3_PARTICLE_OFF,				// only valid for AvHParticleEntity: particle system handle in fuser1
	AVH_USER3_WELD,						// float progress (0 - 100) stored in fuser1
	AVH_USER3_ALPHA,					// fuser1 indicates how much alpha this entity toggles to in commander mode, fuser2 for players
	AVH_USER3_MARINEITEM,				// Something a friendly marine can pick up
	AVH_USER3_WAYPOINT,
	AVH_USER3_HIVE,
	AVH_USER3_NOBUILD,
	AVH_USER3_USEABLE,
	AVH_USER3_AUDIO_ON,
	AVH_USER3_AUDIO_OFF,
	AVH_USER3_FUNC_RESOURCE,
	AVH_USER3_COMMANDER_STATION,
	AVH_USER3_TURRET_FACTORY, 
	AVH_USER3_ARMORY, 
	AVH_USER3_ADVANCED_ARMORY,
	AVH_USER3_ARMSLAB,
	AVH_USER3_PROTOTYPE_LAB, 
	AVH_USER3_OBSERVATORY,
	AVH_USER3_CHEMLAB,
	AVH_USER3_MEDLAB,
	AVH_USER3_NUKEPLANT,
	AVH_USER3_TURRET,
	AVH_USER3_SIEGETURRET,
	AVH_USER3_RESTOWER,
	AVH_USER3_PLACEHOLDER,
	AVH_USER3_INFANTRYPORTAL,
	AVH_USER3_NUKE,
	AVH_USER3_BREAKABLE,
	AVH_USER3_UMBRA,
	AVH_USER3_PHASEGATE,
	AVH_USER3_DEFENSE_CHAMBER,
	AVH_USER3_MOVEMENT_CHAMBER,
	AVH_USER3_OFFENSE_CHAMBER,
	AVH_USER3_SENSORY_CHAMBER,
	AVH_USER3_ALIENRESTOWER,
	AVH_USER3_HEAVY,
	AVH_USER3_JETPACK,
	AVH_USER3_ADVANCED_TURRET_FACTORY,
	AVH_USER3_SPAWN_READYROOM,
	AVH_USER3_CLIENT_COMMAND,
	AVH_USER3_FUNC_ILLUSIONARY,
	AVH_USER3_MENU_BUILD,
	AVH_USER3_MENU_BUILD_ADVANCED,
	AVH_USER3_MENU_ASSIST,
	AVH_USER3_MENU_EQUIP,
	AVH_USER3_MINE,
	AVH_USER3_MAX
} AvHUser3;

enum
{
	WEAPON_NONE = 0,
	WEAPON_CLAWS,
	WEAPON_SPIT,
	WEAPON_SPORES,
	WEAPON_SPIKE,
	WEAPON_BITE,
	WEAPON_BITE2,
	WEAPON_SWIPE,
	WEAPON_WEBSPINNER,
	WEAPON_METABOLIZE,
	WEAPON_PARASITE,
	WEAPON_BLINK,
	WEAPON_DIVINEWIND,
	WEAPON_KNIFE,
	WEAPON_PISTOL,
	WEAPON_LMG,
	WEAPON_SHOTGUN,
	WEAPON_HMG,
	WEAPON_WELDER,
	WEAPON_MINE,
	WEAPON_GRENADE_GUN,
	WEAPON_LEAP,
	WEAPON_CHARGE,
	WEAPON_UMBRA,
	WEAPON_PRIMALSCREAM,
	WEAPON_BILEBOMB,
	WEAPON_ACIDROCKET,
	WEAPON_HEALINGSPRAY,
	WEAPON_GRENADE,
	WEAPON_STOMP,
	WEAPON_DEVOUR,
	WEAPON_MAX
};
enum
{
	PLAYERCLASS_NONE = 0,
	PLAYERCLASS_ALIVE_MARINE,
	PLAYERCLASS_ALIVE_JETPACK,
	PLAYERCLASS_ALIVE_HEAVY_MARINE,
	PLAYERCLASS_ALIVE_LEVEL1,
	PLAYERCLASS_ALIVE_LEVEL2,
	PLAYERCLASS_ALIVE_LEVEL3,
	PLAYERCLASS_ALIVE_LEVEL4,
	PLAYERCLASS_ALIVE_LEVEL5,
	PLAYERCLASS_ALIVE_DIGESTING,
	PLAYERCLASS_ALIVE_GESTATING,
	PLAYERCLASS_DEAD_MARINE,
	PLAYERCLASS_DEAD_ALIEN,
	PLAYERCLASS_COMMANDER,
	PLAYERCLASS_REINFORCING,
	PLAYERCLASS_SPECTATOR,
	PLAYERCLASS_READY,
};
enum classes
{
	CLASS_UNKNOWN = 0,
	CLASS_SKULK,
	CLASS_GORGE,
	CLASS_LERK,
	CLASS_FADE,
	CLASS_ONOS,
	CLASS_MARINE,
	CLASS_JETPACK,
	CLASS_HEAVY,
	CLASS_COMMANDER,
	CLASS_GESTATE,
	CLASS_DEAD,
	CLASS_NOTEAM
};


#endif

