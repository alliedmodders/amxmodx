/* NS2AMXDEFINES.INC
 * Various enums and definitions required for the ns2amx module.
 * Version 1.0
 */

#if defined __ns2amxxdefines__
 	#endinput
#endif
#define __ns2amxxdefines__ 02

// Set this to either 3 or 2.  If set to two, it will do NS 2.01 offsets.
// If set to 3, it will do the most recent NS 3.0 offsets.
#define NS_VERSION	3


// These values are customizable (for the custom menus)
// If your menu uses the default menu scheme, then it will use this.
#define NSMENU_X	0.2
#define NSMENU_Y	0.1
#define NSMENU_R	255
#define NSMENU_G	200
#define NSMENU_B	100




#if NS_VERSION == 3
#define PRIVATE_ALIEN_RES		0x190
#define PRIVATE_AMMO_LMG		0xE6
#define PRIVATE_AMMO_PISTOL		0xE7
#define PRIVATE_AMMO_SHOTGUN	0xE8
#define PRIVATE_AMMO_HMG		0xE9
#define PRIVATE_AMMO_GL			0xEA

//Thanks, voogru
#define PRIVATE_LEVELS_SPENT_WIN	0x62D
#define PRIVATE_EXPERIENCE_WIN		0x615
#define PRIVATE_LEVELS_SPENT_LINUX	0x62D
#define PRIVATE_EXPERIENCE_LINUX	0x612

#else

#define PRIVATE_ALIEN_RES	0x18E
#define PRIVATE_AMMO_LMG	0xE6
#define PRIVATE_AMMO_PISTOL	0xE7
#define PRIVATE_AMMO_SHOTGUN	0xE8
#define PRIVATE_AMMO_HMG	0xE9
#define PRIVATE_AMMO_GL		0xEA
#endif

// Used for anglevector()
#define ANGLEVECTOR_FORWARD		1
#define ANGLEVECTOR_RIGHT		2
#define ANGLEVECTOR_UP			3

// used for gpglobals_return()
#define GPGLOBALS_FORWARD 1
#define GPGLOBALS_RIGHT 2
#define GPGLOBALS_UP 3

// Used for get_class
#define CLASS_UNKNOWN	0
#define CLASS_SKULK		1
#define CLASS_GORGE		2
#define CLASS_LERK		3
#define CLASS_FADE		4
#define CLASS_ONOS		5
#define CLASS_MARINE	6
#define CLASS_JETPACK	7
#define CLASS_HEAVY		8
#define CLASS_COMMANDER	9
#define CLASS_GESTATE	10
#define CLASS_DEAD		11
#define CLASS_NOTEAM	12
// used for trace_hull
#define HULL_POINT 0
#define HULL_HUMAN 1
#define HULL_LARGE 2
#define HULL_HEAD 3

 // Taken from the NS mini SDK
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
	WEAPON_MG,
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
	WEAPON_HANDGRENADE,
	WEAPON_STOMP,
	WEAPON_DEVOUR,
	WEAPON_MAX
}
#define WEAPON_BABBLER WEAPON_HANDGRENADE
// A few of the AvHSpecials.h iuser4 flags, modified for ease-of-use...
enum
{
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
	MASK_ENSNARED = 67108864,	// Webbed
	MASK_ALIEN_EMBRYO = 268435456,	// Gestating
	MASK_SELECTABLE = 536870912,	// ???
	MASK_PARASITED = 1073741824,	// Parasite flag
	MASK_SENSORY_NEARBY = 2147483648	// Sensory chamber in range

}

#if !defined NULL
#define NULL 0
#endif
#if !defined FALSE
#define FALSE 0
#endif
#if !defined TRUE
#define TRUE 1
#endif
