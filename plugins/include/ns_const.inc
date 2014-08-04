// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module Constants
//

#if defined NS_CONST_INC
	#endinput
#endif
#define NS_CONST_INC


enum NSGameplay
{
	NSGame_CantTell,		/**< It is too soon to tell (can't find avhgameplay
								 entity or it doesn't have private data) */

	NSGame_MarineVAlien,	/**< Marine vs Aliens (standard) gameplay */
	NSGame_MarineVMarine,	/**< Marine vs Marine */
	NSGame_AlienVAlien,		/**< Alien  vs Alien  */

	NSGame_Unknown,			/**< Can find the gameplay entity, but can't 
								 determine gameplay type. */
};

// entity pev->iuser4 fields
enum {
	MASK_NONE = 0,
	MASK_SIGHTED = 1,
	MASK_DETECTED = 2,
	MASK_BUILDABLE = 4,
	MASK_BASEBUILD0 = 8,		// Base build slot #0
	MASK_WEAPONS1 = 8,		// Marine weapons 1
	MASK_CARAPACE = 8,		// Alien carapace
	MASK_WEAPONS2 = 16,		// Marines weapons 2
	MASK_REGENERATION = 16,		// Alien regeneration
	MASK_BASEBUILD1 = 16,		// Base build slot #1
	MASK_WEAPONS3 = 32,		// Marine weapons 3
	MASK_REDEMPTION = 32,		// Alien redemption
	MASK_BASEBUILD2 = 32,		// Base build slot #2
	MASK_ARMOR1 = 64,		// Marine armor 1
	MASK_CELERITY = 64,		// Alien celerity
	MASK_BASEBUILD3 = 64,		// Base build slot #3
	MASK_ARMOR2 = 128,		// Marine armor 2
	MASK_ADRENALINE = 128,		// Alien adrenaline
	MASK_BASEBUILD4 = 128,		// Base build slot #4
	MASK_ARMOR3 = 256,		// Marine armor 3
	MASK_SILENCE = 256,		// Alien silence
	MASK_BASEBUILD5 = 256,		// Base build slot #5
	MASK_JETPACK = 512,		// Marine jetpacks
	MASK_CLOAKING = 512,		// Alien cloaking
	MASK_BASEBUILD6 = 512,		// Base build slot #6
	MASK_FOCUS = 1024,		// Alien focus
	MASK_MOTION = 1024,		// Marine motion tracking
	MASK_BASEBUILD7 = 1024,		// Base build slot #7
	MASK_SCENTOFFEAR = 2048,	// Alien scent of fear
	MASK_DEFENSE2 = 4096,		// Defense level 2
	MASK_DEFENSE3 = 8192,		// Defense level 3
	MASK_ELECTRICITY = 8192,	// Electricy
	MASK_MOVEMENT2 = 16384,		// Movement level 2,
	MASK_MOVEMENT3 = 32768,		// Movement level 3
	MASK_HEAVYARMOR = 32768,	// Marine heavy armor
	MASK_SENSORY2 = 65536,		// Sensory level 2
	MASK_SENSORY3 = 131072,		// Sensory level 3
	MASK_ALIEN_MOVEMENT = 262144,	// Onos is charging
	MASK_WALLSTICKING = 524288,	// Flag for wall-sticking
	MASK_PRIMALSCREAM = 1048576,	// Alien is in range of active primal scream
	MASK_UMBRA = 2097152,		// In umbra
	MASK_DIGESTING = 4194304,	// When set on a visible player, player is digesting.  When set on invisible player, player is being digested
	MASK_RECYCLING = 8388608,	// Building is recycling
	MASK_TOPDOWN = 16777216,	// Commander view
	MASK_PLAYER_STUNNED = 33554432,	// Player has been stunned by stomp
	MASK_ENSNARED = 67108864,   	// Webbed
	MASK_ALIEN_EMBRYO = 134217728,  // Gestating
	MASK_SELECTABLE = 268435456,	// ???
	MASK_PARASITED = 536870912,	// Parasite flag
	MASK_SENSORY_NEARBY = 1073741824 // Sensory chamber in range
};


enum {
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

enum {
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

enum {
	HIVETRAIT_NONE	= 0,
	HIVETRAIT_DC	= 92,
	HIVETRAIT_SC	= 93,
	HIVETRAIT_MC	= 94
};

enum NSPS_VelShape
{
	NSPS_VS_POINT = 1,
	NSPS_VS_BOX,
	NSPS_VS_SPHERE,
	NSPS_VS_BLOB
};

/* Genshape used in ns_set_ps_genshape
 * NOTE: The following are in the ns.ps file but
 *       are not listed in the .fgd file.  Use
 *       at your own risk!
 *       Line, Triangle, Plane, Cylinder, 
 *       Cone, Disc, Rectangle and None
 */
enum NSPS_GenShape
{
	NSPS_GS_POINT = 0,
	NSPS_GS_LINE,
	NSPS_GS_TRIANGLE,
	NSPS_GS_PLANE,
	NSPS_GS_BOX,
	NSPS_GS_CYLINDER,
	NSPS_GS_CONE,
	NSPS_GS_BLOB,
	NSPS_GS_DISC,
	NSPS_GS_RECTANGLE,
	NSPS_GS_NONE
};
enum NSPS_RenderMode
{
	NSPS_R_NORMAL = 0,
	NSPS_R_TRANSCOLOR,
	NSPS_R_TRANSTEXTURE,
	NSPS_R_GLOW,
	NSPS_R_TRANSALPHA,
	NSPS_R_ADDITIVE
};
enum NSPS_Flags
{
	NSPS_FL_START_ON = 1,
	NSPS_FL_PARTICLE_DENSITY = 2,
	NSPS_FL_FADE_IN = 4,
	NSPS_FL_FADE_OUT = 8,
	NSPS_FL_USE_GRAVITY = 16,
	NSPS_FL_USE_TRI = 32,
	NSPS_FL_CONSTRAIN_PITCH = 128,
	NSPS_FL_COLLIDE = 256,
	NSPS_FL_HI_DETAIL = 512,
	NSPS_FL_FACE_UP = 1024
};
