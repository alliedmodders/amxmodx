/* AMX Mod X constants
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

#if defined _amxconst_included
  #endinput
#endif
#define _amxconst_included

#include <svn_version>

#define M_PI 3.1415926535

#define ADMIN_ALL		0	/* everyone */
#define ADMIN_IMMUNITY		(1<<0)	/* flag "a" */
#define ADMIN_RESERVATION	(1<<1)	/* flag "b" */
#define ADMIN_KICK		(1<<2)	/* flag "c" */
#define ADMIN_BAN		(1<<3)	/* flag "d" */
#define ADMIN_SLAY		(1<<4)	/* flag "e" */
#define ADMIN_MAP		(1<<5)	/* flag "f" */
#define ADMIN_CVAR		(1<<6)	/* flag "g" */
#define ADMIN_CFG		(1<<7)	/* flag "h" */
#define ADMIN_CHAT		(1<<8)	/* flag "i" */
#define ADMIN_VOTE		(1<<9)	/* flag "j" */
#define ADMIN_PASSWORD		(1<<10)	/* flag "k" */
#define ADMIN_RCON		(1<<11)	/* flag "l" */
#define ADMIN_LEVEL_A		(1<<12)	/* flag "m" */
#define ADMIN_LEVEL_B		(1<<13)	/* flag "n" */
#define ADMIN_LEVEL_C		(1<<14)	/* flag "o" */
#define ADMIN_LEVEL_D		(1<<15)	/* flag "p" */
#define ADMIN_LEVEL_E		(1<<16)	/* flag "q" */
#define ADMIN_LEVEL_F		(1<<17)	/* flag "r" */
#define ADMIN_LEVEL_G		(1<<18)	/* flag "s" */
#define ADMIN_LEVEL_H		(1<<19)	/* flag "t" */
#define ADMIN_MENU		(1<<20)	/* flag "u" */
#define ADMIN_ADMIN		(1<<24)	/* flag "y" */
#define ADMIN_USER		(1<<25)	/* flag "z" */

#define FLAG_KICK		(1<<0)	/* flag "a" */
#define FLAG_TAG		(1<<1)	/* flag "b" */
#define FLAG_AUTHID		(1<<2)	/* flag "c" */
#define FLAG_IP			(1<<3)	/* flag "d" */
#define FLAG_NOPASS		(1<<4)	/* flag "e" */
#define FLAG_CASE_SENSITIVE	(1<<10) /* flag "k" */

#define PLUGIN_CONTINUE		0	/* Results returned by public functions */
#define PLUGIN_HANDLED		1	/* stop other plugins */
#define PLUGIN_HANDLED_MAIN	2	/* to use in client_command(), continue all plugins but stop the command */

/* Flags for register_cvar() */
#define	FCVAR_ARCHIVE		1	/* set to cause it to be saved to vars.rc */
#define	FCVAR_USERINFO		2	/* changes the client's info string */
#define	FCVAR_SERVER		4	/* notifies players when changed */
#define FCVAR_EXTDLL		8	/* defined by external DLL */
#define FCVAR_CLIENTDLL		16	/* defined by the client dll */
#define FCVAR_PROTECTED		32	/* It's a server cvar, but we don't send the data since it's a password, etc.  Sends 1 if it's not bland/zero, 0 otherwise as value */
#define FCVAR_SPONLY		64	/* This cvar cannot be changed by clients connected to a multiplayer server. */
#define FCVAR_PRINTABLEONLY	128	/* This cvar's string cannot contain unprintable characters ( e.g., used for player name etc ). */
#define FCVAR_UNLOGGED		256	/* If this is a FCVAR_SERVER, don't log changes to the log file / console if we are creating a log */


/* Id of weapons in CS */
#define CSW_P228		1
#define CSW_SCOUT		3
#define CSW_HEGRENADE		4
#define CSW_XM1014		5
#define CSW_C4			6
#define CSW_MAC10		7
#define CSW_AUG			8
#define CSW_SMOKEGRENADE	9
#define CSW_ELITE		10
#define CSW_FIVESEVEN		11
#define CSW_UMP45		12
#define CSW_SG550		13
#define CSW_GALI		14
#define CSW_GALIL		14
#define CSW_FAMAS		15
#define CSW_USP			16
#define CSW_GLOCK18		17
#define CSW_AWP			18
#define CSW_MP5NAVY		19
#define CSW_M249		20
#define CSW_M3			21
#define CSW_M4A1		22
#define CSW_TMP			23
#define CSW_G3SG1		24
#define CSW_FLASHBANG		25
#define CSW_DEAGLE		26
#define CSW_SG552		27
#define CSW_AK47		28
#define CSW_KNIFE		29
#define CSW_P90			30
#define CSW_VEST		31
#define CSW_VESTHELM	32

#define HIW_BERETTA		1
#define HIW_SPAS12		2
#define HIW_M4A1		3
#define HIW_MP5A4		4
#define HIW_MP5SD5		5
#define HIW_AK47		6
#define HIW_AKS74U		7
#define HIW_GLOCK		8
#define HIW_M11			9
#define HIW_M11SD		10
#define HIW_PSG1		11
#define HIW_ZASTAVA		12
#define HIW_M16A2		13
#define HIW_REMINGTON		14
#define HIW_NATOGREN		15
#define HIW_TANGOGREN		16
#define HIW_FLASHBANG		17

/* Parts of body for hits */
#define HIT_GENERIC		0	/* none */
#define HIT_HEAD		1
#define HIT_CHEST		2
#define HIT_STOMACH		3
#define HIT_LEFTARM		4
#define HIT_RIGHTARM		5
#define HIT_LEFTLEG		6
#define HIT_RIGHTLEG		7

/* Constants for emit_sound() */
/* Channels */
#define CHAN_AUTO		0
#define CHAN_WEAPON		1
#define	CHAN_VOICE		2
#define CHAN_ITEM		3
#define	CHAN_BODY		4
#define CHAN_STREAM		5	/* allocate stream channel from the static or dynamic area */
#define CHAN_STATIC		6	/* allocate channel from the static area  */
#define CHAN_NETWORKVOICE_BASE	7	/* voice data coming across the network */
#define CHAN_NETWORKVOICE_END	500	/* network voice data reserves slots (CHAN_NETWORKVOICE_BASE through CHAN_NETWORKVOICE_END). */

/* Attenuation values */
#define ATTN_NONE		0.00
#define	ATTN_NORM		0.80
#define ATTN_IDLE		2.00
#define ATTN_STATIC		1.25

/* Pitch values */
#define	PITCH_NORM		100	/* non-pitch shifted */
#define PITCH_LOW		95	/* other values are possible - 0-255, where 255 is very high */
#define PITCH_HIGH		120

/* Volume values */
#define VOL_NORM		1.0

/* Menu keys */
#define MENU_KEY_1		(1<<0)
#define MENU_KEY_2		(1<<1)
#define MENU_KEY_3		(1<<2)
#define MENU_KEY_4		(1<<3)
#define MENU_KEY_5		(1<<4)
#define MENU_KEY_6		(1<<5)
#define MENU_KEY_7		(1<<6)
#define MENU_KEY_8		(1<<7)
#define MENU_KEY_9		(1<<8)
#define MENU_KEY_0		(1<<9)

#define LANG_SERVER		0
#define LANG_PLAYER		-1

/* Destination types for client_print() */
enum {
	print_notify = 1,
	print_console,
	print_chat,
	print_center,
};

/* Destination types for engclient_print() */
enum {
	engprint_console = 0,
	engprint_center,
	engprint_chat,
};

/* Render for set_user_rendering() */
enum {
	kRenderNormal = 0,		/* src */
	kRenderTransColor,		/* c*a+dest*(1-a) */
	kRenderTransTexture,		/* src*a+dest*(1-a) */
	kRenderGlow,			/* src*a+dest -- No Z buffer checks */
	kRenderTransAlpha,		/* src*srca+dest*(1-srca) */
	kRenderTransAdd,		/* src*a+dest */
};

/* Fx for set_user_rendering() */
enum {
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
	kRenderFxDistort,		/* Distort/scale/translate flicker */
	kRenderFxHologram,		/* kRenderFxDistort + distance fade */
	kRenderFxDeadPlayer,		/* kRenderAmt is the player index */
	kRenderFxExplode,		/* Scale up really big! */
	kRenderFxGlowShell,		/* Glowing Shell */
	kRenderFxClampMinScale,		/* Keep this sprite from getting very small (SPRITES only!) */
};

/* Type for force_unmodified() */
enum {
	force_exactfile = 0,		/* File on client must exactly match server's file */
	force_model_samebounds,		/* For model files only, the geometry must fit in the same bbox */
	force_model_specifybounds,	/* For model files only, the geometry must fit in the specified bbox */
};

/* Status for get_module() */
enum {
	module_none = 0,
	module_query,
	module_badload,
	module_loaded,
	module_noinfo,
	module_noquery,
	module_noattach,
	module_old,
};

#define AMX_FLAG_DEBUG    0x02  /* symbolic info. available */
#define AMX_FLAG_COMPACT  0x04  /* compact encoding */
#define AMX_FLAG_BYTEOPC  0x08  /* opcode is a byte (not a cell) */
#define AMX_FLAG_NOCHECKS 0x10  /* no array bounds checking; no STMT opcode */
#define AMX_FLAG_OLDFILE  0x20	/* Old AMX Mod plugin */
#define AMX_FLAG_NTVREG 0x1000  /* all native functions are registered */
#define AMX_FLAG_JITC   0x2000  /* abstract machine is JIT compiled */
#define AMX_FLAG_BROWSE 0x4000  /* busy browsing */
#define AMX_FLAG_RELOC  0x8000  /* jump/call addresses relocated */

#define INVALID_PLUGIN_ID	-1

#define MENU_EXIT	-3
#define	MENU_BACK	-2
#define MENU_MORE	-1
#define	ITEM_IGNORE		0
#define	ITEM_ENABLED	1
#define ITEM_DISABLED	2

#define	AMX_ERR_NATIVE		10
#define	AMX_ERR_MEMACCESS	5
#define	AMX_ERR_NONE		0
#define AMX_ERR_BOUNDS		4
#define AMX_ERR_STACKERR	3
#define AMX_ERR_STACKLOW	7
#define AMX_ERR_HEAPLOW		8
#define AMX_ERR_DIVIDE		11
#define AMX_ERR_NOTFOUND	19
#define AMX_ERR_PARAMS		25
#define AMX_ERR_GENERAL		27

#define	INVALID_HANDLE		-1

#define ET_IGNORE		0	//ignore return val
#define ET_STOP			1	//stop on PLUGIN_HANDLED
#define ET_STOP2		2	//same, except return biggest
#define ET_CONTINUE		3	//no stop, return biggest

#define FP_CELL			0
#define FP_FLOAT		1
#define FP_STRING		2
#define FP_ARRAY		4

#define FORWARD_ONLY_OLD	1
#define FORWARD_ONLY_NEW	2
#define FORWARD_ALL		3

#define SND_SPAWNING		(1<<8)		// we're spawing, used in some cases for ambients 
#define SND_STOP		(1<<5)		// stop sound
#define SND_CHANGE_VOL		(1<<6)		// change sound vol
#define SND_CHANGE_PITCH	(1<<7)		// change sound pitch

enum LibType
{
	LibType_Library,
	LibType_Class
};

enum AdminProp
{
	AdminProp_Auth = 0,
	AdminProp_Password,
	AdminProp_Access,
	AdminProp_Flags
};
