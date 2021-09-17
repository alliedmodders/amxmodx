// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined _amxconst_included
	#endinput
#endif
#define _amxconst_included

#include <amxmodx_version>

/**
 * Internal AMXX buffer size for string retrieval.
 *
 * @note This is the buffer size AMX Mod X uses internally to retrieve strings
 *       from plugins. Most natives that take strings as arguments will
 *       implicitly truncate them to this maximum length. This has been raised
 *       to the current value with AMXX 1.8.3. Previously the limit was 3072.
 * @note This is here mainly for documentation purposes. By default plugins
 *       don't have enough memory available to allocate an array of this size.
 *       You probably should not use this to actually declare a buffer unless
 *       you *absolutely* have to. Look at #pragma dynamic to increase a plugins
 *       available memory.
 */
#define MAX_STRING_LENGTH 16384

/**
 * Defines and constants related to the maximum number of clients.
 *
 * @note  MAX_PLAYERS is not the same as MaxClients. MAX_PLAYERS is a hardcoded
 *        value as an upper limit, used mainly to declare arrays big enough for
 *        all possible server situations. MaxClients changes based on the
 *        server the plugin is deployed on..
 */
#define MAX_PLAYERS 32 /* Maximum number of players AMX Mod X supports */

/**
 * Maximum number of players the server supports 
 */
public stock const MaxClients; 

/**
 * Pass this into certain functions to act as a C++ NULL 
 */
public stock const NULL_STRING[1];

/**
 * Pass this into certain functions to act as a C++ NULL
 */
public stock const Float:NULL_VECTOR[3];

/**
 * The maximum buffer size required to store a client's name.
 */
#define MAX_NAME_LENGTH 32

/**
 * The maximum buffer size required to store a client's IP address without a port.
 */
#define MAX_IP_LENGTH 16

/**
 * The maximum buffer size required to store a client's IP address with a port.
 */
#define MAX_IP_WITH_PORT_LENGTH 22

/**
 * The maximum buffer size required to store a client's AuthID.
 */
#define MAX_AUTHID_LENGTH 64

/**
 * The maximum buffer size required to store a resource path.
 */
#define MAX_RESOURCE_PATH_LENGTH 64

/**
 * The maximum buffer size that can be displayed in a MOTD.
 */
#define MAX_MOTD_LENGTH 1536

/**
 * The maximum size accepted by the user info buffer.
 */
#define MAX_USER_INFO_LENGTH 256

/**
 * The maximum buffer size that can be displayed in a menu.
 */
#define MAX_MENU_LENGTH 512

/**
 * π
 */
#define M_PI 3.1415926535

/**
 * @section Admin privilege and authentication constants
 */

/**
 * Admin level constants
 */
#define ADMIN_ALL           0       /* everyone */
#define ADMIN_IMMUNITY      (1<<0)  /* flag "a" */
#define ADMIN_RESERVATION   (1<<1)  /* flag "b" */
#define ADMIN_KICK          (1<<2)  /* flag "c" */
#define ADMIN_BAN           (1<<3)  /* flag "d" */
#define ADMIN_SLAY          (1<<4)  /* flag "e" */
#define ADMIN_MAP           (1<<5)  /* flag "f" */
#define ADMIN_CVAR          (1<<6)  /* flag "g" */
#define ADMIN_CFG           (1<<7)  /* flag "h" */
#define ADMIN_CHAT          (1<<8)  /* flag "i" */
#define ADMIN_VOTE          (1<<9)  /* flag "j" */
#define ADMIN_PASSWORD      (1<<10) /* flag "k" */
#define ADMIN_RCON          (1<<11) /* flag "l" */
#define ADMIN_LEVEL_A       (1<<12) /* flag "m" */
#define ADMIN_LEVEL_B       (1<<13) /* flag "n" */
#define ADMIN_LEVEL_C       (1<<14) /* flag "o" */
#define ADMIN_LEVEL_D       (1<<15) /* flag "p" */
#define ADMIN_LEVEL_E       (1<<16) /* flag "q" */
#define ADMIN_LEVEL_F       (1<<17) /* flag "r" */
#define ADMIN_LEVEL_G       (1<<18) /* flag "s" */
#define ADMIN_LEVEL_H       (1<<19) /* flag "t" */
#define ADMIN_MENU          (1<<20) /* flag "u" */
#define ADMIN_BAN_TEMP      (1<<21) /* flag "v" */
#define ADMIN_ADMIN         (1<<24) /* flag "y" */
#define ADMIN_USER          (1<<25) /* flag "z" */

/**
 * Admin authentication behavior flags
 */
#define FLAG_KICK           (1<<0)  /* flag "a" */
#define FLAG_TAG            (1<<1)  /* flag "b" */
#define FLAG_AUTHID         (1<<2)  /* flag "c" */
#define FLAG_IP             (1<<3)  /* flag "d" */
#define FLAG_NOPASS         (1<<4)  /* flag "e" */
#define FLAG_CASE_SENSITIVE (1<<10) /* flag "k" */

/**
 * @endsection
 */

/**
 * Return codes
 */
#define PLUGIN_CONTINUE     0   /* Results returned by public functions */
#define PLUGIN_HANDLED      1   /* stop other plugins */
#define PLUGIN_HANDLED_MAIN 2   /* to use in client_command(), continue all plugins but stop the command */

/**
 * HI weapon constants
 */
#define HIW_BERETTA     1
#define HIW_SPAS12      2
#define HIW_M4A1        3
#define HIW_MP5A4       4
#define HIW_MP5SD5      5
#define HIW_AK47        6
#define HIW_AKS74U      7
#define HIW_GLOCK       8
#define HIW_M11         9
#define HIW_M11SD       10
#define HIW_PSG1        11
#define HIW_ZASTAVA     12
#define HIW_M16A2       13
#define HIW_REMINGTON   14
#define HIW_NATOGREN    15
#define HIW_TANGOGREN   16
#define HIW_FLASHBANG   17

/**
 * Parts of body for hits
 */
#define HIT_GENERIC     0   /* none */
#define HIT_HEAD        1
#define HIT_CHEST       2
#define HIT_STOMACH     3
#define HIT_LEFTARM     4
#define HIT_RIGHTARM    5
#define HIT_LEFTLEG     6
#define HIT_RIGHTLEG    7
#define HIT_SHIELD      8 // CS only
#define MAX_BODYHITS    8

/**
 * @section emit_sound() constants
 */

/**
 * Channels
 */
#define CHAN_AUTO       0
#define CHAN_WEAPON     1
#define CHAN_VOICE      2
#define CHAN_ITEM       3
#define CHAN_BODY       4
#define CHAN_STREAM     5   /* allocate stream channel from the static or dynamic area */
#define CHAN_STATIC     6   /* allocate channel from the static area  */
#define CHAN_NETWORKVOICE_BASE  7   /* voice data coming across the network */
#define CHAN_NETWORKVOICE_END   500 /* network voice data reserves slots (CHAN_NETWORKVOICE_BASE through CHAN_NETWORKVOICE_END). */

/**
 *Attenuation values
 */
#define ATTN_NONE       0.00
#define ATTN_NORM       0.80
#define ATTN_IDLE       2.00
#define ATTN_STATIC     1.25

/**
 * Pitch values
 */
#define PITCH_NORM      100 /* non-pitch shifted */
#define PITCH_LOW       95  /* other values are possible - 0-255, where 255 is very high */
#define PITCH_HIGH      120

/**
 * Volume values
 */
#define VOL_NORM        1.0

/**
 * Sound behavior constants
 */
#define SND_SPAWNING        (1<<8)      // we're spawing, used in some cases for ambients
#define SND_STOP            (1<<5)      // stop sound
#define SND_CHANGE_VOL      (1<<6)      // change sound vol
#define SND_CHANGE_PITCH    (1<<7)      // change sound pitch

/**
 * @endsection
 */

/**
 * Menu keys
 */
#define MENU_KEY_1      (1<<0)
#define MENU_KEY_2      (1<<1)
#define MENU_KEY_3      (1<<2)
#define MENU_KEY_4      (1<<3)
#define MENU_KEY_5      (1<<4)
#define MENU_KEY_6      (1<<5)
#define MENU_KEY_7      (1<<6)
#define MENU_KEY_8      (1<<7)
#define MENU_KEY_9      (1<<8)
#define MENU_KEY_0      (1<<9)

/**
 * Language constants
 */
#define LANG_SERVER     0
#define LANG_PLAYER     -1

/**
 * @section Client print native constants
 */

/**
 * Destination types for client_print()
 */
enum
{
	print_notify = 1,
	print_console,
	print_chat,
	print_center,
	print_radio     /* Counter-Strike only */
};

/**
 * Color types for client_print_color()
 */
enum
{
	print_team_default = 0,
	print_team_grey = -1,
	print_team_red = -2,
	print_team_blue = -3,
};

/**
 * Destination types for engclient_print()
 */
enum
{
	engprint_console = 0,
	engprint_center,
	engprint_chat,
};

/**
 * @endsection
 */

/**
 * @section Entity rendering constants
 */

/**
 * Rendering modes (i.e. for set_user_rendering())
 */
enum
{
	kRenderNormal = 0,      /* src */
	kRenderTransColor,      /* c*a+dest*(1-a) */
	kRenderTransTexture,    /* src*a+dest*(1-a) */
	kRenderGlow,            /* src*a+dest -- No Z buffer checks */
	kRenderTransAlpha,      /* src*srca+dest*(1-srca) */
	kRenderTransAdd,        /* src*a+dest */
};

/**
 * Rendering fx (i.e. for set_user_rendering())
 */
enum
{
	kRenderFxNone = 0,
	kRenderFxPulseSlow,
	kRenderFxPulseFast,
	kRenderFxPulseSlowWide,
	kRenderFxPulseFastWide,
	kRenderFxFadeSlow,
	kRenderFxFadeFast,
	kRenderFxSolidSlow,
	kRenderFxSolidFast,
	kRenderFxStrobeSlow,
	kRenderFxStrobeFast,
	kRenderFxStrobeFaster,
	kRenderFxFlickerSlow,
	kRenderFxFlickerFast,
	kRenderFxNoDissipation,
	kRenderFxDistort,           /* Distort/scale/translate flicker */
	kRenderFxHologram,          /* kRenderFxDistort + distance fade */
	kRenderFxDeadPlayer,        /* kRenderAmt is the player index */
	kRenderFxExplode,           /* Scale up really big! */
	kRenderFxGlowShell,         /* Glowing Shell */
	kRenderFxClampMinScale,     /* Keep this sprite from getting very small (SPRITES only!) */
	kRenderFxLightMultiplier,   /* CTM !!!CZERO added to tell the studiorender that the value in iuser2 is a lightmultiplier */
};

/**
 * @endsection
 */

/**
 * Type for force_unmodified()
 */
enum
{
	force_exactfile = 0,        /* File on client must exactly match server's file */
	force_model_samebounds,     /* For model files only, the geometry must fit in the same bbox */
	force_model_specifybounds,  /* For model files only, the geometry must fit in the specified bbox */
};

/**
 * Status for get_module()
 */
enum
{
	module_none = 0,
	module_query,
	module_badload,
	module_loaded,
	module_noinfo,
	module_noquery,
	module_noattach,
	module_old,
};

/**
 * AMX flag constants
 */
#define AMX_FLAG_DEBUG      0x02    /* symbolic info. available */
#define AMX_FLAG_COMPACT    0x04    /* compact encoding */
#define AMX_FLAG_BYTEOPC    0x08    /* opcode is a byte (not a cell) */
#define AMX_FLAG_NOCHECKS   0x10    /* no array bounds checking; no STMT opcode */
#define AMX_FLAG_NTVREG     0x1000  /* all native functions are registered */
#define AMX_FLAG_JITC       0x2000  /* abstract machine is JIT compiled */
#define AMX_FLAG_BROWSE     0x4000  /* busy browsing */
#define AMX_FLAG_RELOC      0x8000  /* jump/call addresses relocated */

/**
 * Invalid plugin id
 */
#define INVALID_PLUGIN_ID   -1

/**
 * Menu and menu item status codes
 */
#define MENU_TIMEOUT    -4
#define MENU_EXIT       -3
#define MENU_BACK       -2
#define MENU_MORE       -1
#define ITEM_IGNORE     0
#define ITEM_ENABLED    1
#define ITEM_DISABLED   2

/**
 * AMX error codes
 */
#define AMX_ERR_NATIVE      10
#define AMX_ERR_MEMACCESS   5
#define AMX_ERR_NONE        0
#define AMX_ERR_BOUNDS      4
#define AMX_ERR_STACKERR    3
#define AMX_ERR_STACKLOW    7
#define AMX_ERR_HEAPLOW     8
#define AMX_ERR_DIVIDE      11
#define AMX_ERR_NOTFOUND    19
#define AMX_ERR_PARAMS      25
#define AMX_ERR_GENERAL     27

/**
 * Generic invalid handle value
 */
#define INVALID_HANDLE      -1

/**
 * @section Plugin forward related constants
 */

/**
 * Stop types for plugin forwards
 */
#define ET_IGNORE       0   //ignore return val
#define ET_STOP         1   //stop on PLUGIN_HANDLED
#define ET_STOP2        2   //same, except return biggest
#define ET_CONTINUE     3   //no stop, return biggest

/**
 * Parameter types for plugin forwards
 */
#define FP_CELL         0
#define FP_FLOAT        1
#define FP_STRING       2
#define FP_ARRAY        4
#define FP_VAL_BYREF    5   //cell & float are handled in the same way

/**
 * @endsection
 */

/**
 * LibType constants
 */
enum LibType
{
	LibType_Library,
	LibType_Class
};

/**
 * AdminProp constants
 */
enum AdminProp
{
	AdminProp_Auth = 0,
	AdminProp_Password,
	AdminProp_Access,
	AdminProp_Flags
};

/**
 * HashType constants
 * To be used on hash_file() and hash_string()
 */
enum HashType
{
	Hash_Crc32 = 0,  // Provides CRC32 hashing
	Hash_Md5,        // Provides MD5 hashing
	Hash_Sha1,       // Provides SHA1 hashing
	Hash_Sha256,     // Provides SHA256 hashing

	Hash_Sha3_224,   // Provides SHA3 224 bit hashing
	Hash_Sha3_256,   // Provides SHA3 256 bit hashing
	Hash_Sha3_384,   // Provides SHA3 384 bit hashing
	Hash_Sha3_512,   // Provides SHA3 512 bit hashing

	Hash_Keccak_224, // Provides Keccak 224 bit hashing
	Hash_Keccak_256, // Provides Keccak 256 bit hashing
	Hash_Keccak_384, // Provides Keccak 384 bit hashing
	Hash_Keccak_512  // Provides Keccak 512 bit hashing
};

/**
 * SetTaskFlags constants for set_task_ex()
 */
enum SetTaskFlags (<<= 1)
{
	SetTask_Once = 0,          // None; execute callback after the specified amount of time (Default)
	SetTask_RepeatTimes = 1,   // Repeat timer a set amount of times
	SetTask_Repeat,            // Loop indefinitely until timer is stopped
	SetTask_AfterMapStart,     // Time interval is treated as absolute time after map start
	SetTask_BeforeMapChange    // Time interval is treated as absolute time before map change
};

/**
 * RegisterEventFlags constants for register_event_ex()
 */
enum RegisterEventFlags (<<= 1)
{
	RegisterEvent_None = 0,        // None
	RegisterEvent_Global = 1,      // Global event (sent to every client)
	RegisterEvent_Single,          // Event sent to single client
	RegisterEvent_OnceForMultiple, // Call only once when repeated to multiple clients
	RegisterEvent_OnlyDead,        // Call only if sent to dead client
	RegisterEvent_OnlyAlive,       // Call only if sent to alive client
	RegisterEvent_OnlyHuman,       // Call only if sent to human client (RegisterEvent_Single required)
	RegisterEvent_OnlyBots         // Call only if sent to bot (RegisterEvent_Single required)
};

/**
 * GetPlayerFlags constants for get_players_ex()
 */
enum GetPlayersFlags (<<= 1)
{
	GetPlayers_None = 0,           // No filter (Default)
	GetPlayers_ExcludeDead = 1,    // Do not include dead clients
	GetPlayers_ExcludeAlive,       // Do not include alive clients
	GetPlayers_ExcludeBots,        // Do not include bots
	GetPlayers_ExcludeHuman,       // Do not include human clients
	GetPlayers_MatchTeam,          // Match with team
	GetPlayers_MatchNameSubstring, // Match with part of name
	GetPlayers_CaseInsensitive,    // Match case insensitive
	GetPlayers_ExcludeHLTV,        // Do not include HLTV proxies
	GetPlayers_IncludeConnecting   // Include connecting clients
};

/**
 * FindPlayerFlags constants for find_player_ex()
 */
enum FindPlayerFlags (<<= 1)
{
	FindPlayer_None = 0,           // None
	FindPlayer_MatchName = 1,      // Match with name
	FindPlayer_MatchNameSubstring, // Match with name substring
	FindPlayer_MatchAuthId,        // Match with authid
	FindPlayer_MatchIP,            // Match with ip
	FindPlayer_MatchTeam,          // Match with team name
	FindPlayer_ExcludeDead,        // Do not include dead clients
	FindPlayer_ExcludeAlive,       // Do not include alive clients
	FindPlayer_ExcludeBots,        // Do not include bots
	FindPlayer_ExcludeHuman,       // Do not include human clients
	FindPlayer_LastMatched,        // Return last matched client instead of the first
	FindPlayer_MatchUserId,        // Match with userid
	FindPlayer_CaseInsensitive,    // Match case insensitively
	FindPlayer_IncludeConnecting   // Include connecting clients
}

/**
 * Constants for client statistics
 */
enum
{
	STATSX_KILLS = 0,
	STATSX_DEATHS,
	STATSX_HEADSHOTS,
	STATSX_TEAMKILLS,
	STATSX_SHOTS,
	STATSX_HITS,
	STATSX_DAMAGE,
	STATSX_RANK,
	STATSX_MAX_STATS
}

/**
 *  Constants for get_user_origin()
 */
enum
{
	Origin_Client = 0,      // Client's Origin
	Origin_Eyes,            // Eyes (and Weapon) Origin
	Origin_AimEndClient,    // Aim End Origin from Client's Position
	Origin_AimEndEyes,      // Aim End Origin from Eyes Position
	Origin_CS_LastBullet    // Last Bullet's Origin (Counter-Strike)
}

#include <cstrike_const> // To keep backward compatibility
