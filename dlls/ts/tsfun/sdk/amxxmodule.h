/*
 * AMX Mod X Module Interface Functions
 * This file may be freely used
*/

// prevent double include
#ifndef __AMXXMODULE_H__
#define __AMXXMODULE_H__

// config
#include "moduleconfig.h"

#include <stddef.h> // size_t
// metamod include files
#ifdef USE_METAMOD
#include <extdll.h>
#include <meta_api.h>
#include "osdep.h"
#endif // #ifdef USE_METAMOD

// DLL Export
#undef DLLEXPORT
#if defined(_WIN32)
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT __attribute__((visibility("default")))
#endif

#if defined(__linux__) && !defined(LINUX)
#define LINUX
#elif defined(__APPLE__) && !defined(OSX)
#define OSX
#endif

#undef C_DLLEXPORT
#define C_DLLEXPORT extern "C" DLLEXPORT

// ***** AMXX stuff *****

// module interface version was 1
// 2 - added logtag to struct (amxx1.1-rc1)
// 3 - added new tagAMX structure (amxx1.5)
// 4 - added new 'library' setting for direct loading
#define AMXX_INTERFACE_VERSION 4

// amxx module info
struct amxx_module_info_s
{
	const char *name;
	const char *author;
	const char *version;
	int reload;				// reload on mapchange when nonzero
	const char *logtag;		// added in version 2
	const char *library;	// added in version 4
	const char *libclass;	// added in version 4
};

// return values from functions called by amxx
#define AMXX_OK					0			/* no error */
#define AMXX_IFVERS				1			/* interface version */
#define AMXX_PARAM				2			/* Invalid parameter */
#define AMXX_FUNC_NOT_PRESENT	3			/* Function not present */

#define AMXX_GAME_OK			0			/* This module can load on the current game mod. */
#define AMXX_GAME_BAD			1			/* This module can not load on the current game mod. */

// *** Small stuff ***
// The next section is copied from the amx.h file
// Copyright (c) ITB CompuPhase, 1997-2005

#if defined HAVE_STDINT_H
  #include <stdint.h>
#else
  #if defined __LCC__ || defined __DMC__ || defined LINUX || defined __APPLE__
    #if defined HAVE_INTTYPES_H
      #include <inttypes.h>
    #else
      #include <stdint.h>
    #endif
  #elif !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L
    /* The ISO C99 defines the int16_t and int_32t types. If the compiler got
     * here, these types are probably undefined.
     */
    #if defined __MACH__
      #include <ppc/types.h>
      typedef unsigned short int  uint16_t;
      typedef unsigned long int   uint32_t;
    #elif defined __FreeBSD__
      #include <inttypes.h>
    #else
      typedef short int           int16_t;
      typedef unsigned short int  uint16_t;
      #if defined SN_TARGET_PS2
        typedef int               int32_t;
        typedef unsigned int      uint32_t;
      #else
        typedef long int          int32_t;
        typedef unsigned long int uint32_t;
      #endif
      #if defined __WIN32__ || defined _WIN32 || defined WIN32
        typedef __int64	          int64_t;
        typedef unsigned __int64  uint64_t;
        #define HAVE_I64
      #elif defined __GNUC__
        typedef long long         int64_t;
        typedef unsigned long long uint64_t;
        #define HAVE_I64
      #endif
    #endif
  #endif
  #define HAVE_STDINT_H
#endif
#if defined _LP64 || defined WIN64 || defined _WIN64
  #if !defined __64BIT__
    #define __64BIT__
  #endif
#endif

/* calling convention for native functions */
#if !defined AMX_NATIVE_CALL
  #define AMX_NATIVE_CALL
#endif
/* calling convention for all interface functions and callback functions */
#if !defined AMXAPI
  #if defined STDECL
    #define AMXAPI      __stdcall
  #elif defined CDECL
    #define AMXAPI      __cdecl
  #else
    #define AMXAPI
  #endif
#endif
#if !defined AMXEXPORT
  #define AMXEXPORT
#endif

#if !defined PAWN_CELL_SIZE
  #define PAWN_CELL_SIZE 32     /* by default, use 32-bit cells */
#endif
#if PAWN_CELL_SIZE==16
  typedef uint16_t  ucell;
  typedef int16_t   cell;
#elif PAWN_CELL_SIZE==32
  typedef uint32_t  ucell;
  typedef int32_t   cell;
#define REAL	float
#elif PAWN_CELL_SIZE==64
  typedef uint64_t  ucell;
  typedef int64_t   cell;
#define REAL	double
#else
  #error Unsupported cell size (PAWN_CELL_SIZE)
#endif

#define UNPACKEDMAX   ((1 << (sizeof(cell)-1)*8) - 1)
#define UNLIMITED     (~1u >> 1)

struct tagAMX;
typedef cell (AMX_NATIVE_CALL *AMX_NATIVE)(struct tagAMX *amx, cell *params);
typedef int (AMXAPI *AMX_CALLBACK)(struct tagAMX *amx, cell index,
                                   cell *result, cell *params);
typedef int (AMXAPI *AMX_DEBUG)(struct tagAMX *amx);
#if !defined _FAR
  #define _FAR
#endif

#if defined _MSC_VER
	#pragma warning(disable:4103)  /* disable warning message 4103 that complains
	                                * about pragma pack in a header file */
	#pragma warning(disable:4100)  /* "'%$S' : unreferenced formal parameter" */

	#if _MSC_VER >= 1400
		#if !defined NO_MSVC8_AUTO_COMPAT

			/* Disable deprecation warnings concerning unsafe CRT functions */
			#if !defined _CRT_SECURE_NO_DEPRECATE
				#define _CRT_SECURE_NO_DEPRECATE
			#endif

			/* Replace the POSIX function with ISO C++ conformant ones as they are now deprecated */
			#define access _access
			#define cabs _cabs
			#define cgets _cgets
			#define chdir _chdir
			#define chmod _chmod
			#define chsize _chsize
			#define close _close
			#define cprintf _cprintf
			#define cputs _cputts
			#define creat _creat
			#define cscanf _cscanf
			#define cwait _cwait
			#define dup _dup
			#define dup2 _dup2
			#define ecvt _ecvt
			#define eof _eof
			#define execl _execl
			#define execle _execle
			#define execlp _execlp
			#define execlpe _execlpe
			#define execv _execv
			#define execve _execv
			#define execvp _execvp
			#define execvpe _execvpe
			#define fcloseall _fcloseall
			#define fcvt _fcvt
			#define fdopen _fdopen
			#define fgetchar _fgetchar
			#define filelength _filelength
			#define fileno _fileno
			#define flushall _flushall
			#define fputchar _fputchar
			#define gcvt _gcvt
			#define getch _getch
			#define getche _getche
			#define getcwd _getcwd
			#define getpid _getpid
			#define getw _getw
			#define hypot _hypot
			#define inp _inp
			#define inpw _inpw
			#define isascii __isascii
			#define isatty _isatty
			#define iscsym __iscsym
			#define iscsymf __iscsymf
			#define itoa _itoa
			#define j0 _j0
			#define j1 _j1
			#define jn _jn
			#define kbhit _kbhit
			#define lfind _lfind
			#define locking _locking
			#define lsearch _lsearch
			#define lseek _lseek
			#define ltoa _ltoa
			#define memccpy _memccpy
			#define memicmp _memicmp
			#define mkdir _mkdir
			#define mktemp _mktemp
			#define open _open
			#define outp _outp
			#define outpw _outpw
			#define putch _putch
			#define putenv _putenv
			#define putw _putw
			#define read _read
			#define rmdir _rmdir
			#define rmtmp _rmtmp
			#define setmode _setmode
			#define sopen _sopen
			#define spawnl _spawnl
			#define spawnle _spawnle
			#define spawnlp _spawnlp
			#define spawnlpe _spawnlpe
			#define spawnv _spawnv
			#define spawnve _spawnve
			#define spawnvp _spawnvp
			#define spawnvpe _spawnvpe
			#define strcmpi _strcmpi
			#define strdup _strdup
			#define stricmp _stricmp
			#define strlwr _strlwr
			#define strnicmp _strnicmp
			#define strnset _strnset
			#define strrev _strrev
			#define strset _strset
			#define strupr _strupr
			#define swab _swab
			#define tell _tell
			#define tempnam _tempnam
			#define toascii __toascii
			#define tzset _tzset
			#define ultoa _ultoa
			#define umask _umask
			#define ungetch _ungetch
			#define unlink _unlink
			#define wcsdup _wcsdup
			#define wcsicmp _wcsicmp
			#define wcsicoll _wcsicoll
			#define wcslwr _wcslwr
			#define wcsnicmp _wcsnicmp
			#define wcsnset _wcsnset
			#define wcsrev _wcsrev
			#define wcsset _wcsset
			#define wcsupr _wcsupr
			#define write _write
			#define y0 _y0
			#define y1 _y1
			#define yn _yn

			/* Disable deprecation warnings because MSVC8 seemingly thinks the ISO C++ conformant 
			 * functions above are deprecated. */
			#pragma warning (disable:4996)
				
		#endif
	#else
		#define vsnprintf _vsnprintf
	#endif
#endif


/* Some compilers do not support the #pragma align, which should be fine. Some
 * compilers give a warning on unknown #pragmas, which is not so fine...
 */
#if (defined SN_TARGET_PS2 || defined __GNUC__) && !defined AMX_NO_ALIGN
  #define AMX_NO_ALIGN
#endif

#if defined __GNUC__
  #define PACKED        __attribute__((packed))
#else
  #define PACKED
#endif

#if !defined AMX_NO_ALIGN
  #if defined LINUX || defined __FreeBSD__ || defined __APPLE__
    #pragma pack(1)         /* structures must be packed (byte-aligned) */
  #elif defined MACOS && defined __MWERKS__
	#pragma options align=mac68k
  #else
    #pragma pack(push)
    #pragma pack(1)         /* structures must be packed (byte-aligned) */
    #if defined __TURBOC__
      #pragma option -a-    /* "pack" pragma for older Borland compilers */
    #endif
  #endif
#endif

typedef struct {
  const char _FAR *name PACKED;
  AMX_NATIVE func       PACKED;
} AMX_NATIVE_INFO;

#define AMX_USERNUM     4

/* The AMX structure is the internal structure for many functions. Not all
 * fields are valid at all times; many fields are cached in local variables.
 */
typedef struct tagAMX {
  unsigned char _FAR *base PACKED; /* points to the AMX header plus the code, optionally also the data */
  unsigned char _FAR *data PACKED; /* points to separate data+stack+heap, may be NULL */
  AMX_CALLBACK callback PACKED;
  AMX_DEBUG debug       PACKED; /* debug callback */
  /* for external functions a few registers must be accessible from the outside */
  cell cip              PACKED; /* instruction pointer: relative to base + amxhdr->cod */
  cell frm              PACKED; /* stack frame base: relative to base + amxhdr->dat */
  cell hea              PACKED; /* top of the heap: relative to base + amxhdr->dat */
  cell hlw              PACKED; /* bottom of the heap: relative to base + amxhdr->dat */
  cell stk              PACKED; /* stack pointer: relative to base + amxhdr->dat */
  cell stp              PACKED; /* top of the stack: relative to base + amxhdr->dat */
  int flags             PACKED; /* current status, see amx_Flags() */
  /* user data */
  long usertags[AMX_USERNUM] PACKED;
  //okay userdata[3] in AMX Mod X is for the CPlugin * pointer
  //we're also gonna set userdata[2] to a special debug structure
  void _FAR *userdata[AMX_USERNUM] PACKED;
  /* native functions can raise an error */
  int error             PACKED;
  /* passing parameters requires a "count" field */
  int paramcount;
  /* the sleep opcode needs to store the full AMX status */
  cell pri              PACKED;
  cell alt              PACKED;
  cell reset_stk        PACKED;
  cell reset_hea        PACKED;
  cell sysreq_d         PACKED; /* relocated address/value for the SYSREQ.D opcode */
  /* support variables for the JIT */
  int reloc_size      PACKED; /* required temporary buffer for relocations */
  long code_size      PACKED; /* estimated memory footprint of the native code */
} PACKED AMX;

enum {
  AMX_ERR_NONE,
  /* reserve the first 15 error codes for exit codes of the abstract machine */
  AMX_ERR_EXIT,         /* forced exit */
  AMX_ERR_ASSERT,       /* assertion failed */
  AMX_ERR_STACKERR,     /* stack/heap collision */
  AMX_ERR_BOUNDS,       /* index out of bounds */
  AMX_ERR_MEMACCESS,    /* invalid memory access */
  AMX_ERR_INVINSTR,     /* invalid instruction */
  AMX_ERR_STACKLOW,     /* stack underflow */
  AMX_ERR_HEAPLOW,      /* heap underflow */
  AMX_ERR_CALLBACK,     /* no callback, or invalid callback */
  AMX_ERR_NATIVE,       /* native function failed */
  AMX_ERR_DIVIDE,       /* divide by zero */
  AMX_ERR_SLEEP,        /* go into sleepmode - code can be restarted */
  AMX_ERR_INVSTATE,     /* invalid state for this access */

  AMX_ERR_MEMORY = 16,  /* out of memory */
  AMX_ERR_FORMAT,       /* invalid file format */
  AMX_ERR_VERSION,      /* file is for a newer version of the AMX */
  AMX_ERR_NOTFOUND,     /* function not found */
  AMX_ERR_INDEX,        /* invalid index parameter (bad entry point) */
  AMX_ERR_DEBUG,        /* debugger cannot run */
  AMX_ERR_INIT,         /* AMX not initialized (or doubly initialized) */
  AMX_ERR_USERDATA,     /* unable to set user data field (table full) */
  AMX_ERR_INIT_JIT,     /* cannot initialize the JIT */
  AMX_ERR_PARAMS,       /* parameter error */
  AMX_ERR_DOMAIN,       /* domain error, expression result does not fit in range */
};

#if !defined AMX_NO_ALIGN
  #if defined(__linux__) || defined(__APPLE__)
    #pragma pack()    /* reset default packing */
  #else
    #pragma pack(pop) /* reset previous packing */
  #endif
#endif


// ***** declare functions *****

#ifdef USE_METAMOD
void UTIL_LogPrintf( const char *fmt, ... );
void UTIL_HudMessage(CBaseEntity *pEntity, const hudtextparms_t &textparms, const char *pMessage);
short FixedSigned16( float value, float scale );
unsigned short FixedUnsigned16( float value, float scale );

#ifdef FN_META_QUERY
void FN_META_QUERY(void);
#endif // FN_META_QUERY

#ifdef FN_META_ATTACH
void FN_META_ATTACH(void);
#endif // FN_META_ATTACH

#ifdef FN_META_DETACH
void FN_META_DETACH(void);
#endif // FN_META_DETACH





#ifdef FN_GameDLLInit
void FN_GameDLLInit(void);
#endif // FN_GameDLLInit

#ifdef FN_DispatchSpawn
int FN_DispatchSpawn(edict_t *pent);
#endif // FN_DispatchSpawn

#ifdef FN_DispatchThink
void FN_DispatchThink(edict_t *pent);
#endif // FN_DispatchThink

#ifdef FN_DispatchUse
void FN_DispatchUse(edict_t *pentUser, edict_t *pentOther);
#endif // FN_DispatchUse

#ifdef FN_DispatchTouch
void FN_DispatchTouch(edict_t *pentTouched, edict_t *pentOther);
#endif // FN_DispatchTouch

#ifdef FN_DispatchBlocked
void FN_DispatchBlocked(edict_t *pentBlocked, edict_t *pentOther);
#endif // FN_DispatchBlocked

#ifdef FN_DispatchKeyValue
void FN_DispatchKeyValue(edict_t *pentKeyvalue, KeyValueData *pkvd);
#endif // FN_DispatchKeyValue

#ifdef FN_DispatchSave
void FN_DispatchSave(edict_t *pent, SAVERESTOREDATA *pSaveData);
#endif // FN_DispatchSave

#ifdef FN_DispatchRestore
int FN_DispatchRestore(edict_t *pent, SAVERESTOREDATA *pSaveData, int globalEntity);
#endif // FN_DispatchRestore

#ifdef FN_DispatchObjectCollsionBox
void FN_DispatchObjectCollsionBox(edict_t *pent);
#endif // FN_DispatchObjectCollsionBox

#ifdef FN_SaveWriteFields
void FN_SaveWriteFields(SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount);
#endif // FN_SaveWriteFields

#ifdef FN_SaveReadFields
void FN_SaveReadFields(SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount);
#endif // FN_SaveReadFields

#ifdef FN_SaveGlobalState
void FN_SaveGlobalState(SAVERESTOREDATA *pSaveData);
#endif // FN_SaveGlobalState

#ifdef FN_RestoreGlobalState
void FN_RestoreGlobalState(SAVERESTOREDATA *pSaveData);
#endif // FN_RestoreGlobalState

#ifdef FN_ResetGlobalState
void FN_ResetGlobalState(void);
#endif // FN_ResetGlobalState

#ifdef FN_ClientConnect
BOOL FN_ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]);
#endif // FN_ClientConnect

#ifdef FN_ClientDisconnect
void FN_ClientDisconnect(edict_t *pEntity);
#endif // FN_ClientDisconnect

#ifdef FN_ClientKill
void FN_ClientKill(edict_t *pEntity);
#endif // FN_ClientKill

#ifdef FN_ClientPutInServer
void FN_ClientPutInServer(edict_t *pEntity);
#endif // FN_ClientPutInServer

#ifdef FN_ClientCommand
void FN_ClientCommand(edict_t *pEntity);
#endif // FN_ClientCommand

#ifdef FN_ClientUserInfoChanged
void FN_ClientUserInfoChanged(edict_t *pEntity, char *infobuffer);
#endif // FN_ClientUserInfoChanged

#ifdef FN_ServerActivate
void FN_ServerActivate(edict_t *pEdictList, int edictCount, int clientMax);
#endif // FN_ServerActivate

#ifdef FN_ServerDeactivate
void FN_ServerDeactivate(void);
#endif // FN_ServerDeactivate

#ifdef FN_PlayerPreThink
void FN_PlayerPreThink(edict_t *pEntity);
#endif // FN_PlayerPreThink

#ifdef FN_PlayerPostThink
void FN_PlayerPostThink(edict_t *pEntity);
#endif // FN_PlayerPostThink

#ifdef FN_StartFrame
void FN_StartFrame(void);
#endif // FN_StartFrame

#ifdef FN_ParmsNewLevel
void FN_ParmsNewLevel(void);
#endif // FN_ParmsNewLevel

#ifdef FN_ParmsChangeLevel
void FN_ParmsChangeLevel(void);
#endif // FN_ParmsChangeLevel

#ifdef FN_GetGameDescription
const char *FN_GetGameDescription(void);
#endif // FN_GetGameDescription

#ifdef FN_PlayerCustomization
void FN_PlayerCustomization(edict_t *pEntity, customization_t *pCust);
#endif // FN_PlayerCustomization

#ifdef FN_SpectatorConnect
void FN_SpectatorConnect(edict_t *pEntity);
#endif // FN_SpectatorConnect

#ifdef FN_SpectatorDisconnect
void FN_SpectatorDisconnect(edict_t *pEntity);
#endif // FN_SpectatorDisconnect

#ifdef FN_SpectatorThink
void FN_SpectatorThink(edict_t *pEntity);
#endif // FN_SpectatorThink

#ifdef FN_Sys_Error
void FN_Sys_Error(const char *error_string);
#endif // FN_Sys_Error

#ifdef FN_PM_Move
void FN_PM_Move(struct playermove_s *ppmove, int server);
#endif // FN_PM_Move

#ifdef FN_PM_Init
void FN_PM_Init(struct playermove_s *ppmove);
#endif // FN_PM_Init

#ifdef FN_PM_FindTextureType
char FN_PM_FindTextureType(char *name);
#endif // FN_PM_FindTextureType

#ifdef FN_SetupVisibility
void FN_SetupVisibility(edict_t *pViewEntity, edict_t *pClient, unsigned char **pvs, unsigned char **pas);
#endif // FN_SetupVisibility

#ifdef FN_UpdateClientData
void FN_UpdateClientData(const struct edict_s *ent, int sendweapons, struct clientdata_s *cd);
#endif // FN_UpdateClientData

#ifdef FN_AddToFullPack
int FN_AddToFullPack(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet);
#endif // FN_AddToFullPack

#ifdef FN_CreateBaseline
void FN_CreateBaseline(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs);
#endif // FN_CreateBaseline

#ifdef FN_RegisterEncoders
void FN_RegisterEncoders(void);
#endif // FN_RegisterEncoders

#ifdef FN_GetWeaponData
int FN_GetWeaponData(struct edict_s *player, struct weapon_data_s *info);
#endif // FN_GetWeaponData

#ifdef FN_CmdStart
void FN_CmdStart(const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed);
#endif // FN_CmdStart

#ifdef FN_CmdEnd
void FN_CmdEnd(const edict_t *player);
#endif // FN_CmdEnd

#ifdef FN_ConnectionlessPacket
int FN_ConnectionlessPacket(const struct netadr_s *net_from, const char *args, char *response_buffer, int *response_buffer_size);
#endif // FN_ConnectionlessPacket

#ifdef FN_GetHullBounds
int FN_GetHullBounds(int hullnumber, float *mins, float *maxs);
#endif // FN_GetHullBounds

#ifdef FN_CreateInstancedBaselines
void FN_CreateInstancedBaselines(void);
#endif // FN_CreateInstancedBaselines

#ifdef FN_InconsistentFile
int FN_InconsistentFile(const edict_t *player, const char *filename, char *disconnect_message);
#endif // FN_InconsistentFile

#ifdef FN_AllowLagCompensation
int FN_AllowLagCompensation(void);
#endif // FN_AllowLagCompensation




#ifdef FN_GameDLLInit_Post
void FN_GameDLLInit_Post(void);
#endif // FN_GameDLLInit_Post

#ifdef FN_DispatchSpawn_Post
int FN_DispatchSpawn_Post(edict_t *pent);
#endif // FN_DispatchSpawn_Post

#ifdef FN_DispatchThink_Post
void FN_DispatchThink_Post(edict_t *pent);
#endif // FN_DispatchThink_Post

#ifdef FN_DispatchUse_Post
void FN_DispatchUse_Post(edict_t *pentUser, edict_t *pentOther);
#endif // FN_DispatchUse_Post

#ifdef FN_DispatchTouch_Post
void FN_DispatchTouch_Post(edict_t *pentTouched, edict_t *pentOther);
#endif // FN_DispatchTouch_Post

#ifdef FN_DispatchBlocked_Post
void FN_DispatchBlocked_Post(edict_t *pentBlocked, edict_t *pentOther);
#endif // FN_DispatchBlocked_Post

#ifdef FN_DispatchKeyValue_Post
void FN_DispatchKeyValue_Post(edict_t *pentKeyvalue, KeyValueData *pkvd);
#endif // FN_DispatchKeyValue_Post

#ifdef FN_DispatchSave_Post
void FN_DispatchSave_Post(edict_t *pent, SAVERESTOREDATA *pSaveData);
#endif // FN_DispatchSave_Post

#ifdef FN_DispatchRestore_Post
int FN_DispatchRestore_Post(edict_t *pent, SAVERESTOREDATA *pSaveData, int globalEntity);
#endif // FN_DispatchRestore_Post

#ifdef FN_DispatchObjectCollsionBox_Post
void FN_DispatchObjectCollsionBox_Post(edict_t *pent);
#endif // FN_DispatchObjectCollsionBox_Post

#ifdef FN_SaveWriteFields_Post
void FN_SaveWriteFields_Post(SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount);
#endif // FN_SaveWriteFields_Post

#ifdef FN_SaveReadFields_Post
void FN_SaveReadFields_Post(SAVERESTOREDATA *pSaveData, const char *pname, void *pBaseData, TYPEDESCRIPTION *pFields, int fieldCount);
#endif // FN_SaveReadFields_Post

#ifdef FN_SaveGlobalState_Post
void FN_SaveGlobalState_Post(SAVERESTOREDATA *pSaveData);
#endif // FN_SaveGlobalState_Post

#ifdef FN_RestoreGlobalState_Post
void FN_RestoreGlobalState_Post(SAVERESTOREDATA *pSaveData);
#endif // FN_RestoreGlobalState_Post

#ifdef FN_ResetGlobalState_Post
void FN_ResetGlobalState_Post(void);
#endif // FN_ResetGlobalState_Post

#ifdef FN_ClientConnect_Post
BOOL FN_ClientConnect_Post(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ]);
#endif // FN_ClientConnect_Post

#ifdef FN_ClientDisconnect_Post
void FN_ClientDisconnect_Post(edict_t *pEntity);
#endif // FN_ClientDisconnect_Post

#ifdef FN_ClientKill_Post
void FN_ClientKill_Post(edict_t *pEntity);
#endif // FN_ClientKill_Post

#ifdef FN_ClientPutInServer_Post
void FN_ClientPutInServer_Post(edict_t *pEntity);
#endif // FN_ClientPutInServer_Post

#ifdef FN_ClientCommand_Post
void FN_ClientCommand_Post(edict_t *pEntity);
#endif // FN_ClientCommand_Post

#ifdef FN_ClientUserInfoChanged_Post
void FN_ClientUserInfoChanged_Post(edict_t *pEntity, char *infobuffer);
#endif // FN_ClientUserInfoChanged_Post

#ifdef FN_ServerActivate_Post
void FN_ServerActivate_Post(edict_t *pEdictList, int edictCount, int clientMax);
#endif // FN_ServerActivate_Post

#ifdef FN_ServerDeactivate_Post
void FN_ServerDeactivate_Post(void);
#endif // FN_ServerDeactivate_Post

#ifdef FN_PlayerPreThink_Post
void FN_PlayerPreThink_Post(edict_t *pEntity);
#endif // FN_PlayerPreThink_Post

#ifdef FN_PlayerPostThink_Post
void FN_PlayerPostThink_Post(edict_t *pEntity);
#endif // FN_PlayerPostThink_Post

#ifdef FN_StartFrame_Post
void FN_StartFrame_Post(void);
#endif // FN_StartFrame_Post

#ifdef FN_ParmsNewLevel_Post
void FN_ParmsNewLevel_Post(void);
#endif // FN_ParmsNewLevel_Post

#ifdef FN_ParmsChangeLevel_Post
void FN_ParmsChangeLevel_Post(void);
#endif // FN_ParmsChangeLevel_Post

#ifdef FN_GetGameDescription_Post
const char *FN_GetGameDescription_Post(void);
#endif // FN_GetGameDescription_Post

#ifdef FN_PlayerCustomization_Post
void FN_PlayerCustomization_Post(edict_t *pEntity, customization_t *pCust);
#endif // FN_PlayerCustomization_Post

#ifdef FN_SpectatorConnect_Post
void FN_SpectatorConnect_Post(edict_t *pEntity);
#endif // FN_SpectatorConnect_Post

#ifdef FN_SpectatorDisconnect_Post
void FN_SpectatorDisconnect_Post(edict_t *pEntity);
#endif // FN_SpectatorDisconnect_Post

#ifdef FN_SpectatorThink_Post
void FN_SpectatorThink_Post(edict_t *pEntity);
#endif // FN_SpectatorThink_Post

#ifdef FN_Sys_Error_Post
void FN_Sys_Error_Post(const char *error_string);
#endif // FN_Sys_Error_Post

#ifdef FN_PM_Move_Post
void FN_PM_Move_Post(struct playermove_s *ppmove, int server);
#endif // FN_PM_Move_Post

#ifdef FN_PM_Init_Post
void FN_PM_Init_Post(struct playermove_s *ppmove);
#endif // FN_PM_Init_Post

#ifdef FN_PM_FindTextureType_Post
char FN_PM_FindTextureType_Post(char *name);
#endif // FN_PM_FindTextureType_Post

#ifdef FN_SetupVisibility_Post
void FN_SetupVisibility_Post(edict_t *pViewEntity, edict_t *pClient, unsigned char **pvs, unsigned char **pas);
#endif // FN_SetupVisibility_Post

#ifdef FN_UpdateClientData_Post
void FN_UpdateClientData_Post(const struct edict_s *ent, int sendweapons, struct clientdata_s *cd);
#endif // FN_UpdateClientData_Post

#ifdef FN_AddToFullPack_Post
int FN_AddToFullPack_Post(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet);
#endif // FN_AddToFullPack_Post

#ifdef FN_CreateBaseline_Post
void FN_CreateBaseline_Post(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs);
#endif // FN_CreateBaseline_Post

#ifdef FN_RegisterEncoders_Post
void FN_RegisterEncoders_Post(void);
#endif // FN_RegisterEncoders_Post

#ifdef FN_GetWeaponData_Post
int FN_GetWeaponData_Post(struct edict_s *player, struct weapon_data_s *info);
#endif // FN_GetWeaponData_Post

#ifdef FN_CmdStart_Post
void FN_CmdStart_Post(const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed);
#endif // FN_CmdStart_Post

#ifdef FN_CmdEnd_Post
void FN_CmdEnd_Post(const edict_t *player);
#endif // FN_CmdEnd_Post

#ifdef FN_ConnectionlessPacket_Post
int FN_ConnectionlessPacket_Post(const struct netadr_s *net_from, const char *args, char *response_buffer, int *response_buffer_size);
#endif // FN_ConnectionlessPacket_Post

#ifdef FN_GetHullBounds_Post
int FN_GetHullBounds_Post(int hullnumber, float *mins, float *maxs);
#endif // FN_GetHullBounds_Post

#ifdef FN_CreateInstancedBaselines_Post
void FN_CreateInstancedBaselines_Post(void);
#endif // FN_CreateInstancedBaselines_Post

#ifdef FN_InconsistentFile_Post
int FN_InconsistentFile_Post(const edict_t *player, const char *filename, char *disconnect_message);
#endif // FN_InconsistentFile_Post

#ifdef FN_AllowLagCompensation_Post
int FN_AllowLagCompensation_Post(void);
#endif // FN_AllowLagCompensation_Post



#ifdef FN_PrecacheModel
int FN_PrecacheModel(const char *s);
#endif // FN_PrecacheModel

#ifdef FN_PrecacheSound
int FN_PrecacheSound(const char *s);
#endif // FN_PrecacheSound

#ifdef FN_SetModel
void FN_SetModel(edict_t *e, const char *m);
#endif // FN_SetModel

#ifdef FN_ModelIndex
int FN_ModelIndex(const char *m);
#endif // FN_ModelIndex

#ifdef FN_ModelFrames
int FN_ModelFrames(int modelIndex);
#endif // FN_ModelFrames

#ifdef FN_SetSize
void FN_SetSize(edict_t *e, const float *rgflMin, const float *rgflMax);
#endif // FN_SetSize

#ifdef FN_ChangeLevel
void FN_ChangeLevel(const char *s1, const char *s2);
#endif // FN_ChangeLevel

#ifdef FN_GetSpawnParms
void FN_GetSpawnParms(edict_t *ent);
#endif // FN_GetSpawnParms

#ifdef FN_SaveSpawnParms
void FN_SaveSpawnParms(edict_t *ent);
#endif // FN_SaveSpawnParms

#ifdef FN_VecToYaw
float FN_VecToYaw(const float *rgflVector);
#endif // FN_VecToYaw

#ifdef FN_VecToAngles
void FN_VecToAngles(const float *rgflVectorIn, float *rgflVectorOut);
#endif // FN_VecToAngles

#ifdef FN_MoveToOrigin
void FN_MoveToOrigin(edict_t *ent, const float *pflGoal, float dist, int iMoveType);
#endif // FN_MoveToOrigin

#ifdef FN_ChangeYaw
void FN_ChangeYaw(edict_t *ent);
#endif // FN_ChangeYaw

#ifdef FN_ChangePitch
void FN_ChangePitch(edict_t *ent);
#endif // FN_ChangePitch

#ifdef FN_FindEntityByString
edict_t *FN_FindEntityByString(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue);
#endif // FN_FindEntityByString

#ifdef FN_GetEntityIllum
int FN_GetEntityIllum(edict_t *pEnt);
#endif // FN_GetEntityIllum

#ifdef FN_FindEntityInSphere
edict_t *FN_FindEntityInSphere(edict_t *pEdictStartSearchAfter, const float *org, float rad);
#endif // FN_FindEntityInSphere

#ifdef FN_FindClientInPVS
edict_t *FN_FindClientInPVS(edict_t *pEdict);
#endif // FN_FindClientInPVS

#ifdef FN_EntitiesInPVS
edict_t *FN_EntitiesInPVS(edict_t *pplayer);
#endif // FN_EntitiesInPVS

#ifdef FN_MakeVectors
void FN_MakeVectors(const float *rgflVector);
#endif // FN_MakeVectors

#ifdef FN_AngleVectors
void FN_AngleVectors(const float *rgflVector, float *forward, float *right, float *up);
#endif // FN_AngleVectors

#ifdef FN_CreateEntity
edict_t *FN_CreateEntity(void);
#endif // FN_CreateEntity

#ifdef FN_RemoveEntity
void FN_RemoveEntity(edict_t *e);
#endif // FN_RemoveEntity

#ifdef FN_CreateNamedEntity
edict_t *FN_CreateNamedEntity(int className);
#endif // FN_CreateNamedEntity

#ifdef FN_MakeStatic
void FN_MakeStatic(edict_t *ent);
#endif // FN_MakeStatic

#ifdef FN_EntIsOnFloor
int FN_EntIsOnFloor(edict_t *ent);
#endif // FN_EntIsOnFloor

#ifdef FN_DropToFloor
int FN_DropToFloor(edict_t *ent);
#endif // FN_DropToFloor

#ifdef FN_WalkMove
int FN_WalkMove(edict_t *ent, float yaw, float dist, int iMode);
#endif // FN_WalkMove

#ifdef FN_SetOrigin
void FN_SetOrigin(edict_t *e, const float *rgflOrigin);
#endif // FN_SetOrigin

#ifdef FN_EmitSound
void FN_EmitSound(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch);
#endif // FN_EmitSound

#ifdef FN_EmitAmbientSound
void FN_EmitAmbientSound(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch);
#endif // FN_EmitAmbientSound

#ifdef FN_TraceLine
void FN_TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceLine

#ifdef FN_TraceToss
void FN_TraceToss(edict_t *pent, edict_t *pentToIgnore, TraceResult *ptr);
#endif // FN_TraceToss

#ifdef FN_TraceMonsterHull
int FN_TraceMonsterHull(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceMonsterHull

#ifdef FN_TraceHull
void FN_TraceHull(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceHull

#ifdef FN_TraceModel
void FN_TraceModel(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
#endif // FN_TraceModel

#ifdef FN_TraceTexture
const char *FN_TraceTexture(edict_t *pTextureEntity, const float *v1, const float *v2 );
#endif // FN_TraceTexture

#ifdef FN_TraceSphere
void FN_TraceSphere(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceSphere

#ifdef FN_GetAimVector
void FN_GetAimVector(edict_t *ent, float speed, float *rgflReturn);
#endif // FN_GetAimVector

#ifdef FN_ServerCommand
void FN_ServerCommand(char *str);
#endif // FN_ServerCommand

#ifdef FN_ServerExecute
void FN_ServerExecute(void);
#endif // FN_ServerExecute

#ifdef FN_engClientCommand
void FN_engClientCommand(edict_t *pEdict, char *szFmt, ...);
#endif // FN_engClientCommand

#ifdef FN_ParticleEffect
void FN_ParticleEffect(const float *org, const float *dir, float color, float count);
#endif // FN_ParticleEffect

#ifdef FN_LightStyle
void FN_LightStyle(int style, char *val);
#endif // FN_LightStyle

#ifdef FN_DecalIndex
int FN_DecalIndex(const char *name);
#endif // FN_DecalIndex

#ifdef FN_PointContents
int FN_PointContents(const float *rgflVector);
#endif // FN_PointContents

#ifdef FN_MessageBegin
void FN_MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
#endif // FN_MessageBegin

#ifdef FN_MessageEnd
void FN_MessageEnd(void);
#endif // FN_MessageEnd

#ifdef FN_WriteByte
void FN_WriteByte(int iValue);
#endif // FN_WriteByte

#ifdef FN_WriteChar
void FN_WriteChar(int iValue);
#endif // FN_WriteChar

#ifdef FN_WriteShort
void FN_WriteShort(int iValue);
#endif // FN_WriteShort

#ifdef FN_WriteLong
void FN_WriteLong(int iValue);
#endif // FN_WriteLong

#ifdef FN_WriteAngle
void FN_WriteAngle(float flValue);
#endif // FN_WriteAngle

#ifdef FN_WriteCoord
void FN_WriteCoord(float flValue);
#endif // FN_WriteCoord

#ifdef FN_WriteString
void FN_WriteString(const char *sz);
#endif // FN_WriteString

#ifdef FN_WriteEntity
void FN_WriteEntity(int iValue);
#endif // FN_WriteEntity

#ifdef FN_CVarRegister
void FN_CVarRegister(cvar_t *pCvar);
#endif // FN_CVarRegister

#ifdef FN_CVarGetFloat
float FN_CVarGetFloat(const char *szVarName);
#endif // FN_CVarGetFloat

#ifdef FN_CVarGetString
const char *FN_CVarGetString(const char *szVarName);
#endif // FN_CVarGetString

#ifdef FN_CVarSetFloat
void FN_CVarSetFloat(const char *szVarName, float flValue);
#endif // FN_CVarSetFloat

#ifdef FN_CVarSetString
void FN_CVarSetString(const char *szVarName, const char *szValue);
#endif // FN_CVarSetString

#ifdef FN_AlertMessage
void FN_AlertMessage(ALERT_TYPE atype, char *szFmt, ...);
#endif // FN_AlertMessage

#ifdef FN_EngineFprintf
void FN_EngineFprintf(void *pfile, char *szFmt, ...);
#endif // FN_EngineFprintf

#ifdef FN_PvAllocEntPrivateData
void *FN_PvAllocEntPrivateData(edict_t *pEdict, int32 cb);
#endif // FN_PvAllocEntPrivateData

#ifdef FN_PvEntPrivateData
void *FN_PvEntPrivateData(edict_t *pEdict);
#endif // FN_PvEntPrivateData

#ifdef FN_FreeEntPrivateData
void FN_FreeEntPrivateData(edict_t *pEdict);
#endif // FN_FreeEntPrivateData

#ifdef FN_SzFromIndex
const char *FN_SzFromIndex(int iString);
#endif // FN_SzFromIndex

#ifdef FN_AllocString
int FN_AllocString(const char *szValue);
#endif // FN_AllocString

#ifdef FN_GetVarsOfEnt
struct entvars_s *FN_GetVarsOfEnt(edict_t *pEdict);
#endif // FN_GetVarsOfEnt

#ifdef FN_PEntityOfEntOffset
edict_t *FN_PEntityOfEntOffset(int iEntOffset);
#endif // FN_PEntityOfEntOffset

#ifdef FN_EntOffsetOfPEntity
int FN_EntOffsetOfPEntity(const edict_t *pEdict);
#endif // FN_EntOffsetOfPEntity

#ifdef FN_IndexOfEdict
int FN_IndexOfEdict(const edict_t *pEdict);
#endif // FN_IndexOfEdict

#ifdef FN_PEntityOfEntIndex
edict_t *FN_PEntityOfEntIndex(int iEntIndex);
#endif // FN_PEntityOfEntIndex

#ifdef FN_FindEntityByVars
edict_t *FN_FindEntityByVars(struct entvars_s *pvars);
#endif // FN_FindEntityByVars

#ifdef FN_GetModelPtr
void *FN_GetModelPtr(edict_t *pEdict);
#endif // FN_GetModelPtr

#ifdef FN_RegUserMsg
int FN_RegUserMsg(const char *pszName, int iSize);
#endif // FN_RegUserMsg

#ifdef FN_AnimationAutomove
void FN_AnimationAutomove(const edict_t *pEdict, float flTime);
#endif // FN_AnimationAutomove

#ifdef FN_GetBonePosition
void FN_GetBonePosition(const edict_t *pEdict, int iBone, float *rgflOrigin, float *rgflAngles);
#endif // FN_GetBonePosition

#ifdef FN_FunctionFromName
uint32 FN_FunctionFromName(const char *pName);
#endif // FN_FunctionFromName

#ifdef FN_NameForFunction
const char *FN_NameForFunction(uint32);
#endif // FN_NameForFunction

#ifdef FN_ClientPrintf
void FN_ClientPrintf(edict_t *pEdict, PRINT_TYPE ptype, const char *szMsg);
#endif // FN_ClientPrintf

#ifdef FN_ServerPrint
void FN_ServerPrint(const char *szMsg);
#endif // FN_ServerPrint

#ifdef FN_Cmd_Args
const char *FN_Cmd_Args(void);
#endif // FN_Cmd_Args

#ifdef FN_Cmd_Argv
const char *FN_Cmd_Argv(int argc);
#endif // FN_Cmd_Argv

#ifdef FN_Cmd_Argc
int FN_Cmd_Argc(void);
#endif // FN_Cmd_Argc

#ifdef FN_GetAttachment
void FN_GetAttachment(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );
#endif // FN_GetAttachment

#ifdef FN_CRC32_Init
void FN_CRC32_Init(CRC32_t *pulCRC);
#endif // FN_CRC32_Init

#ifdef FN_CRC32_ProcessBuffer
void FN_CRC32_ProcessBuffer(CRC32_t *pulCRC, void *p, int len);
#endif // FN_CRC32_ProcessBuffer

#ifdef FN_CRC32_ProcessByte
void FN_CRC32_ProcessByte(CRC32_t *pulCRC, unsigned char ch);
#endif // FN_CRC32_ProcessByte

#ifdef FN_CRC32_Final
CRC32_t FN_CRC32_Final(CRC32_t pulCRC);
#endif // FN_CRC32_Final

#ifdef FN_RandomLong
int32 FN_RandomLong(int32 lLow, int32 lHigh);
#endif // FN_RandomLong

#ifdef FN_RandomFloat
float FN_RandomFloat(float flLow, float flHigh);
#endif // FN_RandomFloat

#ifdef FN_SetView
void FN_SetView(const edict_t *pClient, const edict_t *pViewent);
#endif // FN_SetView

#ifdef FN_Time
float FN_Time(void);
#endif // FN_Time

#ifdef FN_CrosshairAngle
void FN_CrosshairAngle(const edict_t *pClient, float pitch, float yaw);
#endif // FN_CrosshairAngle

#ifdef FN_LoadFileForMe
byte *FN_LoadFileForMe(char *filename, int *pLength);
#endif // FN_LoadFileForMe

#ifdef FN_FreeFile
void FN_FreeFile(void *buffer);
#endif // FN_FreeFile

#ifdef FN_EndSection
void FN_EndSection(const char *pszSectionName);
#endif // FN_EndSection

#ifdef FN_CompareFileTime
int FN_CompareFileTime(char *filename1, char *filename2, int *iCompare);
#endif // FN_CompareFileTime

#ifdef FN_GetGameDir
void FN_GetGameDir(char *szGetGameDir);
#endif // FN_GetGameDir

#ifdef FN_Cvar_RegisterVariable
void FN_Cvar_RegisterVariable(cvar_t *variable);
#endif // FN_Cvar_RegisterVariable

#ifdef FN_FadeClientVolume
void FN_FadeClientVolume(const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
#endif // FN_FadeClientVolume

#ifdef FN_SetClientMaxspeed
void FN_SetClientMaxspeed(const edict_t *pEdict, float fNewMaxspeed);
#endif // FN_SetClientMaxspeed

#ifdef FN_CreateFakeClient
edict_t *FN_CreateFakeClient(const char *netname);
#endif // FN_CreateFakeClient

#ifdef FN_RunPlayerMove
void FN_RunPlayerMove(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec);
#endif // FN_RunPlayerMove

#ifdef FN_NumberOfEntities
int FN_NumberOfEntities(void);
#endif // FN_NumberOfEntities

#ifdef FN_GetInfoKeyBuffer
char *FN_GetInfoKeyBuffer(edict_t *e);
#endif // FN_GetInfoKeyBuffer

#ifdef FN_InfoKeyValue
char *FN_InfoKeyValue(char *infobuffer, const char *key);
#endif // FN_InfoKeyValue

#ifdef FN_SetKeyValue
void FN_SetKeyValue(char *infobuffer, const char *key, const char *value);
#endif // FN_SetKeyValue

#ifdef FN_SetClientKeyValue
void FN_SetClientKeyValue(int clientIndex, char *infobuffer, const char *key, const char *value);
#endif // FN_SetClientKeyValue

#ifdef FN_IsMapValid
int FN_IsMapValid(const char *filename);
#endif // FN_IsMapValid

#ifdef FN_StaticDecal
void FN_StaticDecal(const float *origin, int decalIndex, int entityIndex, int modelIndex);
#endif // FN_StaticDecal

#ifdef FN_PrecacheGeneric
int FN_PrecacheGeneric(const char *s);
#endif // FN_PrecacheGeneric

#ifdef FN_GetPlayerUserId
int FN_GetPlayerUserId(edict_t *e );
#endif // FN_GetPlayerUserId

#ifdef FN_BuildSoundMsg
void FN_BuildSoundMsg(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
#endif // FN_BuildSoundMsg

#ifdef FN_IsDedicatedServer
int FN_IsDedicatedServer(void);
#endif // FN_IsDedicatedServer

#ifdef FN_CVarGetPointer
cvar_t *FN_CVarGetPointer(const char *szVarName);
#endif // FN_CVarGetPointer

#ifdef FN_GetPlayerWONId
unsigned int FN_GetPlayerWONId(edict_t *e);
#endif // FN_GetPlayerWONId

#ifdef FN_Info_RemoveKey
void FN_Info_RemoveKey( char *s, const char *key);
#endif // FN_Info_RemoveKey

#ifdef FN_GetPhysicsKeyValue
const char *FN_GetPhysicsKeyValue(const edict_t *pClient, const char *key);
#endif // FN_GetPhysicsKeyValue

#ifdef FN_SetPhysicsKeyValue
void FN_SetPhysicsKeyValue(const edict_t *pClient, const char *key, const char *value);
#endif // FN_SetPhysicsKeyValue

#ifdef FN_GetPhysicsInfoString
const char *FN_GetPhysicsInfoString( const edict_t *pClient);
#endif // FN_GetPhysicsInfoString

#ifdef FN_PrecacheEvent
unsigned short FN_PrecacheEvent(int type, const char *psz);
#endif // FN_PrecacheEvent

#ifdef FN_PlaybackEvent
void FN_PlaybackEvent(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2);
#endif // FN_PlaybackEvent

#ifdef FN_SetFatPVS
unsigned char *FN_SetFatPVS(float *org);
#endif // FN_SetFatPVS

#ifdef FN_SetFatPAS
unsigned char *FN_SetFatPAS(float *org);
#endif // FN_SetFatPAS

#ifdef FN_CheckVisibility
int FN_CheckVisibility(const edict_t *entity, unsigned char *pset);
#endif // FN_CheckVisibility

#ifdef FN_DeltaSetField
void FN_DeltaSetField(struct delta_s *pFields, const char *fieldname);
#endif // FN_DeltaSetField

#ifdef FN_DeltaUnsetField
void FN_DeltaUnsetField(struct delta_s *pFields, const char *fieldname);
#endif // FN_DeltaUnsetField

#ifdef FN_DeltaAddEncoder
void FN_DeltaAddEncoder(char *name, void (*conditionalencode)( struct delta_s *pFields, const unsigned char *from, const unsigned char *to ) );
#endif // FN_DeltaAddEncoder

#ifdef FN_GetCurrentPlayer
int FN_GetCurrentPlayer(void);
#endif // FN_GetCurrentPlayer

#ifdef FN_CanSkipPlayer
int FN_CanSkipPlayer(const edict_t *player);
#endif // FN_CanSkipPlayer

#ifdef FN_DeltaFindField
int FN_DeltaFindField(struct delta_s *pFields, const char *fieldname);
#endif // FN_DeltaFindField

#ifdef FN_DeltaSetFieldByIndex
void FN_DeltaSetFieldByIndex(struct delta_s *pFields, int fieldNumber);
#endif // FN_DeltaSetFieldByIndex

#ifdef FN_DeltaUnsetFieldByIndex
void FN_DeltaUnsetFieldByIndex(struct delta_s *pFields, int fieldNumber);
#endif // FN_DeltaUnsetFieldByIndex

#ifdef FN_SetGroupMask
void FN_SetGroupMask(int mask, int op);
#endif // FN_SetGroupMask

#ifdef FN_engCreateInstancedBaseline
int FN_engCreateInstancedBaseline(int classname, struct entity_state_s *baseline);
#endif // FN_engCreateInstancedBaseline

#ifdef FN_Cvar_DirectSet
void FN_Cvar_DirectSet(struct cvar_s *var, char *value);
#endif // FN_Cvar_DirectSet

#ifdef FN_ForceUnmodified
void FN_ForceUnmodified(FORCE_TYPE type, float *mins, float *maxs, const char *filename);
#endif // FN_ForceUnmodified

#ifdef FN_GetPlayerStats
void FN_GetPlayerStats(const edict_t *pClient, int *ping, int *packet_loss);
#endif // FN_GetPlayerStats

#ifdef FN_AddServerCommand
void FN_AddServerCommand(char *cmd_name, void (*function) (void));
#endif // FN_AddServerCommand

#ifdef FN_Voice_GetClientListening
qboolean FN_Voice_GetClientListening(int iReceiver, int iSender);
#endif // FN_Voice_GetClientListening

#ifdef FN_Voice_SetClientListening
qboolean FN_Voice_SetClientListening(int iReceiver, int iSender, qboolean bListen);
#endif // FN_Voice_SetClientListening

#ifdef FN_GetPlayerAuthId
const char *FN_GetPlayerAuthId(edict_t *e);
#endif // FN_GetPlayerAuthId






#ifdef FN_PrecacheModel_Post
int FN_PrecacheModel_Post(const char *s);
#endif // FN_PrecacheModel_Post

#ifdef FN_PrecacheSound_Post
int FN_PrecacheSound_Post(const char *s);
#endif // FN_PrecacheSound_Post

#ifdef FN_SetModel_Post
void FN_SetModel_Post(edict_t *e, const char *m);
#endif // FN_SetModel_Post

#ifdef FN_ModelIndex_Post
int FN_ModelIndex_Post(const char *m);
#endif // FN_ModelIndex_Post

#ifdef FN_ModelFrames_Post
int FN_ModelFrames_Post(int modelIndex);
#endif // FN_ModelFrames_Post

#ifdef FN_SetSize_Post
void FN_SetSize_Post(edict_t *e, const float *rgflMin, const float *rgflMax);
#endif // FN_SetSize_Post

#ifdef FN_ChangeLevel_Post
void FN_ChangeLevel_Post(const char *s1, const char *s2);
#endif // FN_ChangeLevel_Post

#ifdef FN_GetSpawnParms_Post
void FN_GetSpawnParms_Post(edict_t *ent);
#endif // FN_GetSpawnParms_Post

#ifdef FN_SaveSpawnParms_Post
void FN_SaveSpawnParms_Post(edict_t *ent);
#endif // FN_SaveSpawnParms_Post

#ifdef FN_VecToYaw_Post
float FN_VecToYaw_Post(const float *rgflVector);
#endif // FN_VecToYaw_Post

#ifdef FN_VecToAngles_Post
void FN_VecToAngles_Post(const float *rgflVectorIn, float *rgflVectorOut);
#endif // FN_VecToAngles_Post

#ifdef FN_MoveToOrigin_Post
void FN_MoveToOrigin_Post(edict_t *ent, const float *pflGoal, float dist, int iMoveType);
#endif // FN_MoveToOrigin_Post

#ifdef FN_ChangeYaw_Post
void FN_ChangeYaw_Post(edict_t *ent);
#endif // FN_ChangeYaw_Post

#ifdef FN_ChangePitch_Post
void FN_ChangePitch_Post(edict_t *ent);
#endif // FN_ChangePitch_Post

#ifdef FN_FindEntityByString_Post
edict_t *FN_FindEntityByString_Post(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue);
#endif // FN_FindEntityByString_Post

#ifdef FN_GetEntityIllum_Post
int FN_GetEntityIllum_Post(edict_t *pEnt);
#endif // FN_GetEntityIllum_Post

#ifdef FN_FindEntityInSphere_Post
edict_t *FN_FindEntityInSphere_Post(edict_t *pEdictStartSearchAfter, const float *org, float rad);
#endif // FN_FindEntityInSphere_Post

#ifdef FN_FindClientInPVS_Post
edict_t *FN_FindClientInPVS_Post(edict_t *pEdict);
#endif // FN_FindClientInPVS_Post

#ifdef FN_EntitiesInPVS_Post
edict_t *FN_EntitiesInPVS_Post(edict_t *pplayer);
#endif // FN_EntitiesInPVS_Post

#ifdef FN_MakeVectors_Post
void FN_MakeVectors_Post(const float *rgflVector);
#endif // FN_MakeVectors_Post

#ifdef FN_AngleVectors_Post
void FN_AngleVectors_Post(const float *rgflVector, float *forward, float *right, float *up);
#endif // FN_AngleVectors_Post

#ifdef FN_CreateEntity_Post
edict_t *FN_CreateEntity_Post(void);
#endif // FN_CreateEntity_Post

#ifdef FN_RemoveEntity_Post
void FN_RemoveEntity_Post(edict_t *e);
#endif // FN_RemoveEntity_Post

#ifdef FN_CreateNamedEntity_Post
edict_t *FN_CreateNamedEntity_Post(int className);
#endif // FN_CreateNamedEntity_Post

#ifdef FN_MakeStatic_Post
void FN_MakeStatic_Post(edict_t *ent);
#endif // FN_MakeStatic_Post

#ifdef FN_EntIsOnFloor_Post
int FN_EntIsOnFloor_Post(edict_t *ent);
#endif // FN_EntIsOnFloor_Post

#ifdef FN_DropToFloor_Post
int FN_DropToFloor_Post(edict_t *ent);
#endif // FN_DropToFloor_Post

#ifdef FN_WalkMove_Post
int FN_WalkMove_Post(edict_t *ent, float yaw, float dist, int iMode);
#endif // FN_WalkMove_Post

#ifdef FN_SetOrigin_Post
void FN_SetOrigin_Post(edict_t *e, const float *rgflOrigin);
#endif // FN_SetOrigin_Post

#ifdef FN_EmitSound_Post
void FN_EmitSound_Post(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch);
#endif // FN_EmitSound_Post

#ifdef FN_EmitAmbientSound_Post
void FN_EmitAmbientSound_Post(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch);
#endif // FN_EmitAmbientSound_Post

#ifdef FN_TraceLine_Post
void FN_TraceLine_Post(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceLine_Post

#ifdef FN_TraceToss_Post
void FN_TraceToss_Post(edict_t *pent, edict_t *pentToIgnore, TraceResult *ptr);
#endif // FN_TraceToss_Post

#ifdef FN_TraceMonsterHull_Post
int FN_TraceMonsterHull_Post(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceMonsterHull_Post

#ifdef FN_TraceHull_Post
void FN_TraceHull_Post(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceHull_Post

#ifdef FN_TraceModel_Post
void FN_TraceModel_Post(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
#endif // FN_TraceModel_Post

#ifdef FN_TraceTexture_Post
const char *FN_TraceTexture_Post(edict_t *pTextureEntity, const float *v1, const float *v2 );
#endif // FN_TraceTexture_Post

#ifdef FN_TraceSphere_Post
void FN_TraceSphere_Post(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
#endif // FN_TraceSphere_Post

#ifdef FN_GetAimVector_Post
void FN_GetAimVector_Post(edict_t *ent, float speed, float *rgflReturn);
#endif // FN_GetAimVector_Post

#ifdef FN_ServerCommand_Post
void FN_ServerCommand_Post(char *str);
#endif // FN_ServerCommand_Post

#ifdef FN_ServerExecute_Post
void FN_ServerExecute_Post(void);
#endif // FN_ServerExecute_Post

#ifdef FN_engClientCommand_Post
void FN_engClientCommand_Post(edict_t *pEdict, char *szFmt, ...);
#endif // FN_engClientCommand_Post

#ifdef FN_ParticleEffect_Post
void FN_ParticleEffect_Post(const float *org, const float *dir, float color, float count);
#endif // FN_ParticleEffect_Post

#ifdef FN_LightStyle_Post
void FN_LightStyle_Post(int style, char *val);
#endif // FN_LightStyle_Post

#ifdef FN_DecalIndex_Post
int FN_DecalIndex_Post(const char *name);
#endif // FN_DecalIndex_Post

#ifdef FN_PointContents_Post
int FN_PointContents_Post(const float *rgflVector);
#endif // FN_PointContents_Post

#ifdef FN_MessageBegin_Post
void FN_MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
#endif // FN_MessageBegin_Post

#ifdef FN_MessageEnd_Post
void FN_MessageEnd_Post(void);
#endif // FN_MessageEnd_Post

#ifdef FN_WriteByte_Post
void FN_WriteByte_Post(int iValue);
#endif // FN_WriteByte_Post

#ifdef FN_WriteChar_Post
void FN_WriteChar_Post(int iValue);
#endif // FN_WriteChar_Post

#ifdef FN_WriteShort_Post
void FN_WriteShort_Post(int iValue);
#endif // FN_WriteShort_Post

#ifdef FN_WriteLong_Post
void FN_WriteLong_Post(int iValue);
#endif // FN_WriteLong_Post

#ifdef FN_WriteAngle_Post
void FN_WriteAngle_Post(float flValue);
#endif // FN_WriteAngle_Post

#ifdef FN_WriteCoord_Post
void FN_WriteCoord_Post(float flValue);
#endif // FN_WriteCoord_Post

#ifdef FN_WriteString_Post
void FN_WriteString_Post(const char *sz);
#endif // FN_WriteString_Post

#ifdef FN_WriteEntity_Post
void FN_WriteEntity_Post(int iValue);
#endif // FN_WriteEntity_Post

#ifdef FN_CVarRegister_Post
void FN_CVarRegister_Post(cvar_t *pCvar);
#endif // FN_CVarRegister_Post

#ifdef FN_CVarGetFloat_Post
float FN_CVarGetFloat_Post(const char *szVarName);
#endif // FN_CVarGetFloat_Post

#ifdef FN_CVarGetString_Post
const char *FN_CVarGetString_Post(const char *szVarName);
#endif // FN_CVarGetString_Post

#ifdef FN_CVarSetFloat_Post
void FN_CVarSetFloat_Post(const char *szVarName, float flValue);
#endif // FN_CVarSetFloat_Post

#ifdef FN_CVarSetString_Post
void FN_CVarSetString_Post(const char *szVarName, const char *szValue);
#endif // FN_CVarSetString_Post

#ifdef FN_AlertMessage_Post
void FN_AlertMessage_Post(ALERT_TYPE atype, char *szFmt, ...);
#endif // FN_AlertMessage_Post

#ifdef FN_EngineFprintf_Post
void FN_EngineFprintf_Post(void *pfile, char *szFmt, ...);
#endif // FN_EngineFprintf_Post

#ifdef FN_PvAllocEntPrivateData_Post
void *FN_PvAllocEntPrivateData_Post(edict_t *pEdict, int32 cb);
#endif // FN_PvAllocEntPrivateData_Post

#ifdef FN_PvEntPrivateData_Post
void *FN_PvEntPrivateData_Post(edict_t *pEdict);
#endif // FN_PvEntPrivateData_Post

#ifdef FN_FreeEntPrivateData_Post
void FN_FreeEntPrivateData_Post(edict_t *pEdict);
#endif // FN_FreeEntPrivateData_Post

#ifdef FN_SzFromIndex_Post
const char *FN_SzFromIndex_Post(int iString);
#endif // FN_SzFromIndex_Post

#ifdef FN_AllocString_Post
int FN_AllocString_Post(const char *szValue);
#endif // FN_AllocString_Post

#ifdef FN_GetVarsOfEnt_Post
struct entvars_s *FN_GetVarsOfEnt_Post(edict_t *pEdict);
#endif // FN_GetVarsOfEnt_Post

#ifdef FN_PEntityOfEntOffset_Post
edict_t *FN_PEntityOfEntOffset_Post(int iEntOffset);
#endif // FN_PEntityOfEntOffset_Post

#ifdef FN_EntOffsetOfPEntity_Post
int FN_EntOffsetOfPEntity_Post(const edict_t *pEdict);
#endif // FN_EntOffsetOfPEntity_Post

#ifdef FN_IndexOfEdict_Post
int FN_IndexOfEdict_Post(const edict_t *pEdict);
#endif // FN_IndexOfEdict_Post

#ifdef FN_PEntityOfEntIndex_Post
edict_t *FN_PEntityOfEntIndex_Post(int iEntIndex);
#endif // FN_PEntityOfEntIndex_Post

#ifdef FN_FindEntityByVars_Post
edict_t *FN_FindEntityByVars_Post(struct entvars_s *pvars);
#endif // FN_FindEntityByVars_Post

#ifdef FN_GetModelPtr_Post
void *FN_GetModelPtr_Post(edict_t *pEdict);
#endif // FN_GetModelPtr_Post

#ifdef FN_RegUserMsg_Post
int FN_RegUserMsg_Post(const char *pszName, int iSize);
#endif // FN_RegUserMsg_Post

#ifdef FN_AnimationAutomove_Post
void FN_AnimationAutomove_Post(const edict_t *pEdict, float flTime);
#endif // FN_AnimationAutomove_Post

#ifdef FN_GetBonePosition_Post
void FN_GetBonePosition_Post(const edict_t *pEdict, int iBone, float *rgflOrigin, float *rgflAngles);
#endif // FN_GetBonePosition_Post

#ifdef FN_FunctionFromName_Post
uint32 FN_FunctionFromName_Post(const char *pName);
#endif // FN_FunctionFromName_Post

#ifdef FN_NameForFunction_Post
const char *FN_NameForFunction_Post(uint32);
#endif // FN_NameForFunction_Post

#ifdef FN_ClientPrintf_Post
void FN_ClientPrintf_Post(edict_t *pEdict, PRINT_TYPE ptype, const char *szMsg);
#endif // FN_ClientPrintf_Post

#ifdef FN_ServerPrint_Post
void FN_ServerPrint_Post(const char *szMsg);
#endif // FN_ServerPrint_Post

#ifdef FN_Cmd_Args_Post
const char *FN_Cmd_Args_Post(void);
#endif // FN_Cmd_Args_Post

#ifdef FN_Cmd_Argv_Post
const char *FN_Cmd_Argv_Post(int argc);
#endif // FN_Cmd_Argv_Post

#ifdef FN_Cmd_Argc_Post
int FN_Cmd_Argc_Post(void);
#endif // FN_Cmd_Argc_Post

#ifdef FN_GetAttachment_Post
void FN_GetAttachment_Post(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );
#endif // FN_GetAttachment_Post

#ifdef FN_CRC32_Init_Post
void FN_CRC32_Init_Post(CRC32_t *pulCRC);
#endif // FN_CRC32_Init_Post

#ifdef FN_CRC32_ProcessBuffer_Post
void FN_CRC32_ProcessBuffer_Post(CRC32_t *pulCRC, void *p, int len);
#endif // FN_CRC32_ProcessBuffer_Post

#ifdef FN_CRC32_ProcessByte_Post
void FN_CRC32_ProcessByte_Post(CRC32_t *pulCRC, unsigned char ch);
#endif // FN_CRC32_ProcessByte_Post

#ifdef FN_CRC32_Final_Post
CRC32_t FN_CRC32_Final_Post(CRC32_t pulCRC);
#endif // FN_CRC32_Final_Post

#ifdef FN_RandomLong_Post
int32 FN_RandomLong_Post(int32 lLow, int32 lHigh);
#endif // FN_RandomLong_Post

#ifdef FN_RandomFloat_Post
float FN_RandomFloat_Post(float flLow, float flHigh);
#endif // FN_RandomFloat_Post

#ifdef FN_SetView_Post
void FN_SetView_Post(const edict_t *pClient, const edict_t *pViewent);
#endif // FN_SetView_Post

#ifdef FN_Time_Post
float FN_Time_Post(void);
#endif // FN_Time_Post

#ifdef FN_CrosshairAngle_Post
void FN_CrosshairAngle_Post(const edict_t *pClient, float pitch, float yaw);
#endif // FN_CrosshairAngle_Post

#ifdef FN_LoadFileForMe_Post
byte *FN_LoadFileForMe_Post(char *filename, int *pLength);
#endif // FN_LoadFileForMe_Post

#ifdef FN_FreeFile_Post
void FN_FreeFile_Post(void *buffer);
#endif // FN_FreeFile_Post

#ifdef FN_EndSection_Post
void FN_EndSection_Post(const char *pszSectionName);
#endif // FN_EndSection_Post

#ifdef FN_CompareFileTime_Post
int FN_CompareFileTime_Post(char *filename1, char *filename2, int *iCompare);
#endif // FN_CompareFileTime_Post

#ifdef FN_GetGameDir_Post
void FN_GetGameDir_Post(char *szGetGameDir);
#endif // FN_GetGameDir_Post

#ifdef FN_Cvar_RegisterVariable_Post
void FN_Cvar_RegisterVariable_Post(cvar_t *variable);
#endif // FN_Cvar_RegisterVariable_Post

#ifdef FN_FadeClientVolume_Post
void FN_FadeClientVolume_Post(const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
#endif // FN_FadeClientVolume_Post

#ifdef FN_SetClientMaxspeed_Post
void FN_SetClientMaxspeed_Post(const edict_t *pEdict, float fNewMaxspeed);
#endif // FN_SetClientMaxspeed_Post

#ifdef FN_CreateFakeClient_Post
edict_t *FN_CreateFakeClient_Post(const char *netname);
#endif // FN_CreateFakeClient_Post

#ifdef FN_RunPlayerMove_Post
void FN_RunPlayerMove_Post(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec);
#endif // FN_RunPlayerMove_Post

#ifdef FN_NumberOfEntities_Post
int FN_NumberOfEntities_Post(void);
#endif // FN_NumberOfEntities_Post

#ifdef FN_GetInfoKeyBuffer_Post
char *FN_GetInfoKeyBuffer_Post(edict_t *e);
#endif // FN_GetInfoKeyBuffer_Post

#ifdef FN_InfoKeyValue_Post
char *FN_InfoKeyValue_Post(char *infobuffer, const char *key);
#endif // FN_InfoKeyValue_Post

#ifdef FN_SetKeyValue_Post
void FN_SetKeyValue_Post(char *infobuffer, const char *key, const char *value);
#endif // FN_SetKeyValue_Post

#ifdef FN_SetClientKeyValue_Post
void FN_SetClientKeyValue_Post(int clientIndex, char *infobuffer, const char *key, const char *value);
#endif // FN_SetClientKeyValue_Post

#ifdef FN_IsMapValid_Post
int FN_IsMapValid_Post(const char *filename);
#endif // FN_IsMapValid_Post

#ifdef FN_StaticDecal_Post
void FN_StaticDecal_Post(const float *origin, int decalIndex, int entityIndex, int modelIndex);
#endif // FN_StaticDecal_Post

#ifdef FN_PrecacheGeneric_Post
int FN_PrecacheGeneric_Post(const char *s);
#endif // FN_PrecacheGeneric_Post

#ifdef FN_GetPlayerUserId_Post
int FN_GetPlayerUserId_Post(edict_t *e );
#endif // FN_GetPlayerUserId_Post

#ifdef FN_BuildSoundMsg_Post
void FN_BuildSoundMsg_Post(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
#endif // FN_BuildSoundMsg_Post

#ifdef FN_IsDedicatedServer_Post
int FN_IsDedicatedServer_Post(void);
#endif // FN_IsDedicatedServer_Post

#ifdef FN_CVarGetPointer_Post
cvar_t *FN_CVarGetPointer_Post(const char *szVarName);
#endif // FN_CVarGetPointer_Post

#ifdef FN_GetPlayerWONId_Post
unsigned int FN_GetPlayerWONId_Post(edict_t *e);
#endif // FN_GetPlayerWONId_Post

#ifdef FN_Info_RemoveKey_Post
void FN_Info_RemoveKey_Post( char *s, const char *key);
#endif // FN_Info_RemoveKey_Post

#ifdef FN_GetPhysicsKeyValue_Post
const char *FN_GetPhysicsKeyValue_Post(const edict_t *pClient, const char *key);
#endif // FN_GetPhysicsKeyValue_Post

#ifdef FN_SetPhysicsKeyValue_Post
void FN_SetPhysicsKeyValue_Post(const edict_t *pClient, const char *key, const char *value);
#endif // FN_SetPhysicsKeyValue_Post

#ifdef FN_GetPhysicsInfoString_Post
const char *FN_GetPhysicsInfoString_Post( const edict_t *pClient);
#endif // FN_GetPhysicsInfoString_Post

#ifdef FN_PrecacheEvent_Post
unsigned short FN_PrecacheEvent_Post(int type, const char *psz);
#endif // FN_PrecacheEvent_Post

#ifdef FN_PlaybackEvent_Post
void FN_PlaybackEvent_Post(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2);
#endif // FN_PlaybackEvent_Post

#ifdef FN_SetFatPVS_Post
unsigned char *FN_SetFatPVS_Post(float *org);
#endif // FN_SetFatPVS_Post

#ifdef FN_SetFatPAS_Post
unsigned char *FN_SetFatPAS_Post(float *org);
#endif // FN_SetFatPAS_Post

#ifdef FN_CheckVisibility_Post
int FN_CheckVisibility_Post(const edict_t *entity, unsigned char *pset);
#endif // FN_CheckVisibility_Post

#ifdef FN_DeltaSetField_Post
void FN_DeltaSetField_Post(struct delta_s *pFields, const char *fieldname);
#endif // FN_DeltaSetField_Post

#ifdef FN_DeltaUnsetField_Post
void FN_DeltaUnsetField_Post(struct delta_s *pFields, const char *fieldname);
#endif // FN_DeltaUnsetField_Post

#ifdef FN_DeltaAddEncoder_Post
void FN_DeltaAddEncoder_Post(char *name, void (*conditionalencode)( struct delta_s *pFields, const unsigned char *from, const unsigned char *to ) );
#endif // FN_DeltaAddEncoder_Post

#ifdef FN_GetCurrentPlayer_Post
int FN_GetCurrentPlayer_Post(void);
#endif // FN_GetCurrentPlayer_Post

#ifdef FN_CanSkipPlayer_Post
int FN_CanSkipPlayer_Post(const edict_t *player);
#endif // FN_CanSkipPlayer_Post

#ifdef FN_DeltaFindField_Post
int FN_DeltaFindField_Post(struct delta_s *pFields, const char *fieldname);
#endif // FN_DeltaFindField_Post

#ifdef FN_DeltaSetFieldByIndex_Post
void FN_DeltaSetFieldByIndex_Post(struct delta_s *pFields, int fieldNumber);
#endif // FN_DeltaSetFieldByIndex_Post

#ifdef FN_DeltaUnsetFieldByIndex_Post
void FN_DeltaUnsetFieldByIndex_Post(struct delta_s *pFields, int fieldNumber);
#endif // FN_DeltaUnsetFieldByIndex_Post

#ifdef FN_SetGroupMask_Post
void FN_SetGroupMask_Post(int mask, int op);
#endif // FN_SetGroupMask_Post

#ifdef FN_engCreateInstancedBaseline_Post
int FN_engCreateInstancedBaseline_Post(int classname, struct entity_state_s *baseline);
#endif // FN_engCreateInstancedBaseline_Post

#ifdef FN_Cvar_DirectSet_Post
void FN_Cvar_DirectSet_Post(struct cvar_s *var, char *value);
#endif // FN_Cvar_DirectSet_Post

#ifdef FN_ForceUnmodified_Post
void FN_ForceUnmodified_Post(FORCE_TYPE type, float *mins, float *maxs, const char *filename);
#endif // FN_ForceUnmodified_Post

#ifdef FN_GetPlayerStats_Post
void FN_GetPlayerStats_Post(const edict_t *pClient, int *ping, int *packet_loss);
#endif // FN_GetPlayerStats_Post

#ifdef FN_AddServerCommand_Post
void FN_AddServerCommand_Post(char *cmd_name, void (*function)(void));
#endif // FN_AddServerCommand_Post

#ifdef FN_Voice_GetClientListening_Post
qboolean FN_Voice_GetClientListening_Post(int iReceiver, int iSender);
#endif // FN_Voice_GetClientListening_Post

#ifdef FN_Voice_SetClientListening_Post
qboolean FN_Voice_SetClientListening_Post(int iReceiver, int iSender, qboolean bListen);
#endif // FN_Voice_SetClientListening_Post

#ifdef FN_GetPlayerAuthId_Post
const char *FN_GetPlayerAuthId_Post(edict_t *e);
#endif // FN_GetPlayerAuthId




#ifdef FN_OnFreeEntPrivateData
void FN_OnFreeEntPrivateData(edict_t *pEnt);
#endif // FN_OnFreeEntPrivateData

#ifdef FN_GameShutdown
void FN_GameShutdown(void);
#endif // FN_GameShutdown

#ifdef FN_ShouldCollide
int FN_ShouldCollide(edict_t *pentTouched, edict_t *pentOther);
#endif // FN_ShouldCollide





#ifdef FN_OnFreeEntPrivateData_Post
void FN_OnFreeEntPrivateData_Post(edict_t *pEnt);
#endif // FN_OnFreeEntPrivateData_Post

#ifdef FN_GameShutdown_Post
void FN_GameShutdown_Post(void);
#endif // FN_GameShutdown_Post

#ifdef FN_ShouldCollide_Post
int FN_ShouldCollide_Post(edict_t *pentTouched, edict_t *pentOther);
#endif // FN_ShouldCollide_Post

#endif // USE_METAMOD


#ifdef FN_AMXX_QUERY
void FN_AMXX_QUERY(void);
#endif // FN_AMXX_QUERY

#ifdef FN_AMXX_CHECKGAME
int FN_AMXX_CHECKGAME(const char *);
#endif // FN_AMXX_CHECKGAME

#ifdef FN_AMXX_ATTACH
void FN_AMXX_ATTACH(void);
#endif // FN_AMXX_ATTACH

#ifdef FN_AMXX_DETACH
void FN_AMXX_DETACH(void);
#endif // FN_AMXX_DETACH

#ifdef FN_AMXX_PLUGINSLOADED
void FN_AMXX_PLUGINSLOADED(void);
#endif // FN_AMXX_PLUGINSLOADED

#ifdef FN_AMXX_PLUGINSUNLOADING
void FN_AMXX_PLUGINSUNLOADING(void);
#endif // FN_AMXX_PLUGINSUNLOADING

#ifdef FN_AMXX_PLUGINSUNLOADED
void FN_AMXX_PLUGINSUNLOADED(void);
#endif // FN_AMXX_PLUGINSUNLOADED

// *** Types ***
typedef void* (*PFN_REQ_FNPTR)(const char * /*name*/);

// ***** Module funcs stuff *****
enum ForwardExecType
{
	ET_IGNORE = 0,					// Ignore return vaue
	ET_STOP,						// Stop on PLUGIN_HANDLED
	ET_STOP2,						// Stop on PLUGIN_HANDLED, continue on other values, return biggest return value
	ET_CONTINUE,					// Continue; return biggest return value
};

enum ForwardParam
{
	FP_DONE = -1,					// specify this as the last argument
									// only tells the function that there are no more arguments
	FP_CELL,						// normal cell
	FP_FLOAT,						// float; used as normal cell though
	FP_STRING,						// string
	FP_STRINGEX,					// string; will be updated to the last function's value
	FP_ARRAY,						// array; use the return value of prepareArray.
};

enum PlayerProp
{
	Player_Name,		//String
	Player_Ip,			//String
	Player_Team,		//String
	Player_Ingame,		//bool
	Player_Authorized,	//bool
	Player_Vgui,		//bool
	Player_Time,		//float
	Player_Playtime,	//float
	Player_MenuExpire,	//float
	Player_Weapons,		//struct{int,int}[32]
	Player_CurrentWeapon,	//int
	Player_TeamID,			//int
	Player_Deaths,			//int
	Player_Aiming,			//int
	Player_Menu,			//int
	Player_Keys,			//int
	Player_Flags,			//int[32]
	Player_Newmenu,			//int
	Player_NewmenuPage,		//int
};

enum LibType
{
	LibType_Library,
	LibType_Class
};

#define MSGBLOCK_SET	0
#define MSGBLOCK_GET	1
#define BLOCK_NOT 0
#define BLOCK_ONCE 1
#define BLOCK_SET 2

typedef void (*AUTHORIZEFUNC)(int player, const char *authstring);

typedef int				(*PFN_ADD_NATIVES)				(const AMX_NATIVE_INFO * /*list*/);
typedef int				(*PFN_ADD_NEW_NATIVES)			(const AMX_NATIVE_INFO * /*list*/);
typedef char *			(*PFN_BUILD_PATHNAME)			(const char * /*format*/, ...);
typedef char *			(*PFN_BUILD_PATHNAME_R)			(char * /*buffer*/, size_t /* maxlen */, const char * /* format */, ...);
typedef cell *			(*PFN_GET_AMXADDR)				(AMX * /*amx*/, cell /*offset*/);
typedef void			(*PFN_PRINT_SRVCONSOLE)			(const char * /*format*/, ...);
typedef const char *	(*PFN_GET_MODNAME)				(void);
typedef const char *	(*PFN_GET_AMXSCRIPTNAME)		(int /*id*/);
typedef AMX *			(*PFN_GET_AMXSCRIPT)			(int /*id*/);
typedef int				(*PFN_FIND_AMXSCRIPT_BYAMX)		(const AMX * /*amx*/);
typedef int				(*PFN_FIND_AMXSCRIPT_BYNAME)	(const char * /*name*/);
typedef int				(*PFN_SET_AMXSTRING)			(AMX * /*amx*/, cell /*amx_addr*/, const char * /* source */, int /* max */);
typedef char *			(*PFN_GET_AMXSTRING)			(AMX * /*amx*/, cell /*amx_addr*/, int /*bufferId*/, int * /*pLen*/);
typedef int				(*PFN_GET_AMXSTRINGLEN)			(const cell *ptr);
typedef char *			(*PFN_FORMAT_AMXSTRING)			(AMX * /*amx*/, cell * /*params*/, int /*startParam*/, int * /*pLen*/);
typedef void			(*PFN_COPY_AMXMEMORY)			(cell * /*dest*/, const cell * /*src*/, int /*len*/);
typedef void			(*PFN_LOG)						(const char * /*fmt*/, ...);
typedef void			(*PFN_LOG_ERROR)				(AMX * /*amx*/, int /*err*/, const char * /*fmt*/, ...);
typedef int				(*PFN_RAISE_AMXERROR)			(AMX * /*amx*/, int /*error*/);
typedef int				(*PFN_REGISTER_FORWARD)			(const char * /*funcname*/, ForwardExecType /*exectype*/, ... /*paramtypes terminated by PF_DONE*/);
typedef int				(*PFN_EXECUTE_FORWARD)			(int /*id*/, ... /*params*/);
typedef cell			(*PFN_PREPARE_CELLARRAY)		(cell * /*ptr*/, unsigned int /*size*/);
typedef cell			(*PFN_PREPARE_CHARARRAY)		(char * /*ptr*/, unsigned int /*size*/);
typedef cell			(*PFN_PREPARE_CELLARRAY_A)		(cell * /*ptr*/, unsigned int /*size*/, bool /*copyBack*/);
typedef cell			(*PFN_PREPARE_CHARARRAY_A)		(char * /*ptr*/, unsigned int /*size*/, bool /*copyBack*/);
typedef int				(*PFN_IS_PLAYER_VALID)			(int /*id*/);
typedef const char *	(*PFN_GET_PLAYER_NAME)			(int /*id*/);
typedef const char *	(*PFN_GET_PLAYER_IP)			(int /*id*/);
typedef int				(*PFN_IS_PLAYER_INGAME)			(int /*id*/);
typedef int				(*PFN_IS_PLAYER_BOT)			(int /*id*/);
typedef int				(*PFN_IS_PLAYER_AUTHORIZED)		(int /*id*/);
typedef float			(*PFN_GET_PLAYER_TIME)			(int /*id*/);
typedef float			(*PFN_GET_PLAYER_PLAYTIME)		(int /*id*/);
typedef int				(*PFN_GETPLAYERFLAGS) 			(int /* id*/);
typedef int				(*PFN_GET_PLAYER_CURWEAPON)		(int /*id*/);
typedef const char *    (*PFN_GET_PLAYER_TEAM)			(int /*id*/);
typedef int				(*PFN_GET_PLAYER_TEAMID)		(int /*id*/);
typedef int				(*PFN_GET_PLAYER_DEATHS)		(int /*id*/);
typedef int				(*PFN_GET_PLAYER_MENU)			(int /*id*/);
typedef int				(*PFN_GET_PLAYER_KEYS)			(int /*id*/);
typedef int				(*PFN_IS_PLAYER_ALIVE)			(int /*id*/);
typedef int				(*PFN_GET_PLAYER_FRAGS)			(int /*id*/);
typedef int				(*PFN_IS_PLAYER_CONNECTING)		(int /*id*/);
typedef int				(*PFN_IS_PLAYER_HLTV)			(int /*id*/);
typedef int				(*PFN_GET_PLAYER_ARMOR)			(int /*id*/);
typedef int				(*PFN_GET_PLAYER_HEALTH)		(int /*id*/);
#ifdef USE_METAMOD
typedef edict_t *		(*PFN_GET_PLAYER_EDICT)			(int /*id*/);
#else
typedef void *			(*PFN_GET_PLAYER_EDICT)			(int /*id*/);
#endif
typedef void *			(*PFN_PLAYER_PROP_ADDR)			(int /*id*/, int /*prop*/);

#ifdef MEMORY_TEST
typedef void *			(*PFN_ALLOCATOR)				(const char* /*filename*/, const unsigned int /*line*/, const char* /*func*/,
														 const unsigned int /*type*/, const size_t /*size*/);
typedef void *			(*PFN_REALLOCATOR)				(const char* /*filename*/, const unsigned int /*line*/, const char* /*func*/,
														 const unsigned int /*type*/, const size_t /*size*/, void* /*addr*/ );
typedef void			(*PFN_DEALLOCATOR)				(const char* /*filename*/, const unsigned int /*line*/, const char* /*func*/,
														 const unsigned int /*type*/, const void* /*addr*/ );
#endif
typedef int				(*PFN_AMX_EXEC)					(AMX* /*amx*/, cell* /*return val*/, int /*index*/);
typedef int				(*PFN_AMX_EXECV)				(AMX* /*amx*/, cell* /*return val*/, int /*index*/, int /*numparams*/, cell[] /*params*/);
typedef int				(*PFN_AMX_ALLOT)				(AMX* /*amx*/, int /*length*/, cell* /*amx_addr*/, cell** /*phys_addr*/);
typedef int				(*PFN_AMX_FINDPUBLIC)			(AMX* /*amx*/, const char* /*func name*/, int* /*index*/);
typedef int				(*PFN_AMX_FINDNATIVE)			(AMX* /*amx*/, const char* /*func name*/, int* /*index*/);
typedef int				(*PFN_LOAD_AMXSCRIPT)			(AMX* /*amx*/, void** /*code*/, const char* /*path*/, char[64] /*error info*/, int /* debug */);
typedef int				(*PFN_UNLOAD_AMXSCRIPT)			(AMX* /*amx*/,void** /*code*/);
typedef cell			(*PFN_REAL_TO_CELL)				(REAL /*x*/);
typedef REAL			(*PFN_CELL_TO_REAL)				(cell /*x*/);
typedef int				(*PFN_REGISTER_SPFORWARD)		(AMX * /*amx*/, int /*func*/, ... /*params*/);
typedef int				(*PFN_REGISTER_SPFORWARD_BYNAME)	(AMX * /*amx*/, const char * /*funcName*/, ... /*params*/);
typedef void			(*PFN_UNREGISTER_SPFORWARD)		(int /*id*/);
typedef	void			(*PFN_MERGEDEFINITION_FILE)		(const char * /*filename*/);
typedef const char *	(*PFN_FORMAT)					(const char * /*fmt*/, ... /*params*/);
typedef void			(*PFN_REGISTERFUNCTION)			(void * /*pfn*/, const char * /*desc*/);
typedef	int				(*PFN_AMX_PUSH)					(AMX * /*amx*/, cell /*value*/);
typedef	int				(*PFN_SET_TEAM_INFO)			(int /*player */, int /*teamid */, const char * /*name */);
typedef void			(*PFN_REG_AUTH_FUNC)			(AUTHORIZEFUNC);
typedef void			(*PFN_UNREG_AUTH_FUNC)			(AUTHORIZEFUNC);
typedef int				(*PFN_FINDLIBRARY)				(const char * /*name*/, LibType /*type*/);
typedef size_t			(*PFN_ADDLIBRARIES)				(const char * /*name*/, LibType /*type*/, void * /*parent*/);
typedef size_t			(*PFN_REMOVELIBRARIES)			(void * /*parent*/);
typedef void			(*PFN_OVERRIDENATIVES)			(AMX_NATIVE_INFO * /*natives*/, const char * /*myname*/);
typedef const char *	(*PFN_GETLOCALINFO)				(const char * /*name*/, const char * /*def*/);
typedef int				(*PFN_AMX_REREGISTER)			(AMX * /*amx*/, AMX_NATIVE_INFO * /*list*/, int /*list*/);
typedef void *			(*PFN_REGISTERFUNCTIONEX)		(void * /*pfn*/, const char * /*desc*/);
typedef void			(*PFN_MESSAGE_BLOCK)			(int /* mode */, int /* message */, int * /* opt */);

extern PFN_ADD_NATIVES				g_fn_AddNatives;
extern PFN_ADD_NEW_NATIVES			g_fn_AddNewNatives;
extern PFN_BUILD_PATHNAME			g_fn_BuildPathname;
extern PFN_BUILD_PATHNAME_R			g_fn_BuildPathnameR;
extern PFN_GET_AMXADDR				g_fn_GetAmxAddr;
extern PFN_PRINT_SRVCONSOLE			g_fn_PrintSrvConsole;
extern PFN_GET_MODNAME				g_fn_GetModname;
extern PFN_GET_AMXSCRIPTNAME		g_fn_GetAmxScriptName;
extern PFN_GET_AMXSCRIPT			g_fn_GetAmxScript;
extern PFN_FIND_AMXSCRIPT_BYAMX		g_fn_FindAmxScriptByAmx;
extern PFN_FIND_AMXSCRIPT_BYNAME	g_fn_FindAmxScriptByName;
extern PFN_SET_AMXSTRING			g_fn_SetAmxString;
extern PFN_GET_AMXSTRING			g_fn_GetAmxString;
extern PFN_GET_AMXSTRINGLEN			g_fn_GetAmxStringLen;
extern PFN_FORMAT_AMXSTRING			g_fn_FormatAmxString;
extern PFN_COPY_AMXMEMORY			g_fn_CopyAmxMemory;
extern PFN_LOG						g_fn_Log;
extern PFN_LOG_ERROR				g_fn_LogErrorFunc;
extern PFN_RAISE_AMXERROR			g_fn_RaiseAmxError;
extern PFN_REGISTER_FORWARD			g_fn_RegisterForward;
extern PFN_EXECUTE_FORWARD			g_fn_ExecuteForward;
extern PFN_PREPARE_CELLARRAY		g_fn_PrepareCellArray;
extern PFN_PREPARE_CHARARRAY		g_fn_PrepareCharArray;
extern PFN_PREPARE_CELLARRAY_A		g_fn_PrepareCellArrayA;
extern PFN_PREPARE_CHARARRAY_A		g_fn_PrepareCharArrayA;
extern PFN_IS_PLAYER_VALID			g_fn_IsPlayerValid;
extern PFN_GET_PLAYER_NAME			g_fn_GetPlayerName;
extern PFN_GET_PLAYER_IP			g_fn_GetPlayerIP;
extern PFN_IS_PLAYER_INGAME			g_fn_IsPlayerIngame;
extern PFN_IS_PLAYER_BOT			g_fn_IsPlayerBot;
extern PFN_IS_PLAYER_AUTHORIZED		g_fn_IsPlayerAuthorized;
extern PFN_GET_PLAYER_TIME			g_fn_GetPlayerTime;
extern PFN_GET_PLAYER_PLAYTIME		g_fn_GetPlayerPlayTime;
extern PFN_GET_PLAYER_CURWEAPON		g_fn_GetPlayerCurweapon;
extern PFN_GET_PLAYER_TEAMID		g_fn_GetPlayerTeamID;
extern PFN_GET_PLAYER_DEATHS		g_fn_GetPlayerDeaths;
extern PFN_GET_PLAYER_MENU			g_fn_GetPlayerMenu;
extern PFN_GET_PLAYER_KEYS			g_fn_GetPlayerKeys;
extern PFN_IS_PLAYER_ALIVE			g_fn_IsPlayerAlive;
extern PFN_GET_PLAYER_FRAGS			g_fn_GetPlayerFrags;
extern PFN_IS_PLAYER_CONNECTING		g_fn_IsPlayerConnecting;
extern PFN_IS_PLAYER_HLTV			g_fn_IsPlayerHLTV;
extern PFN_GET_PLAYER_ARMOR			g_fn_GetPlayerArmor;
extern PFN_GET_PLAYER_HEALTH		g_fn_GetPlayerHealth;
extern PFN_AMX_EXEC					g_fn_AmxExec;
extern PFN_AMX_ALLOT				g_fn_AmxAllot;
extern PFN_AMX_FINDPUBLIC			g_fn_AmxFindPublic;
extern PFN_LOAD_AMXSCRIPT			g_fn_LoadAmxScript;
extern PFN_UNLOAD_AMXSCRIPT			g_fn_UnloadAmxScript;
extern PFN_REAL_TO_CELL				g_fn_RealToCell;
extern PFN_CELL_TO_REAL				g_fn_CellToReal;
extern PFN_REGISTER_SPFORWARD		g_fn_RegisterSPForward;
extern PFN_REGISTER_SPFORWARD_BYNAME	g_fn_RegisterSPForwardByName;
extern PFN_UNREGISTER_SPFORWARD		g_fn_UnregisterSPForward;
extern PFN_MERGEDEFINITION_FILE		g_fn_MergeDefinition_File;
extern PFN_AMX_FINDNATIVE			g_fn_AmxFindNative;
extern PFN_GETPLAYERFLAGS		g_fn_GetPlayerFlags;
extern PFN_GET_PLAYER_EDICT			g_fn_GetPlayerEdict;
extern PFN_FORMAT					g_fn_Format;
extern PFN_GET_PLAYER_TEAM			g_fn_GetPlayerTeam;
extern PFN_REGISTERFUNCTION			g_fn_RegisterFunction;
extern PFN_REQ_FNPTR				g_fn_RequestFunction;
extern PFN_AMX_PUSH					g_fn_AmxPush;
extern PFN_SET_TEAM_INFO			g_fn_SetTeamInfo;
extern PFN_PLAYER_PROP_ADDR			g_fn_PlayerPropAddr;
extern PFN_REG_AUTH_FUNC			g_fn_RegAuthFunc;
extern PFN_UNREG_AUTH_FUNC			g_fn_UnregAuthFunc;
extern PFN_FINDLIBRARY				g_fn_FindLibrary;
extern PFN_ADDLIBRARIES				g_fn_AddLibraries;
extern PFN_REMOVELIBRARIES			g_fn_RemoveLibraries;
extern PFN_OVERRIDENATIVES			g_fn_OverrideNatives;
extern PFN_GETLOCALINFO				g_fn_GetLocalInfo;
extern PFN_AMX_REREGISTER			g_fn_AmxReRegister;
extern PFN_REGISTERFUNCTIONEX		g_fn_RegisterFunctionEx;
extern PFN_MESSAGE_BLOCK			g_fn_MessageBlock;

#ifdef MAY_NEVER_BE_DEFINED
// Function prototypes for intellisense and similar systems
// They understand #if 0 so we use #ifdef MAY_NEVER_BE_DEFINED
int				MF_AddNatives				(const AMX_NATIVE_INFO *list) { }
int				MF_AddNewNatives			(const AMX_NATIVE_INFO *list) { }
char *			MF_BuildPathname			(const char * format, ...) { }
char *			MF_BuildPathnameR			(char *buffer, size_t maxlen, const char *fmt, ...) { }
cell *			MF_GetAmxAddr				(AMX * amx, cell offset) { }
void			MF_PrintSrvConsole			(char * format, ...) { }
const char *	MF_GetModname				(void) { }
const char *	MF_GetScriptName			(int id) { }
AMX *			MF_GetScriptAmx				(int id) { }
int				MF_FindScriptByAmx			(const AMX * amx) { }
int				MF_FindScriptByAmx			(const char * name) { }
int				MF_SetAmxString				(AMX * amx, cell amx_addr, const char *  source , int  max ) { }
char *			MF_GetAmxString				(AMX * amx, cell amx_addr, int bufferId, int * pLen) { }
int				MF_GetAmxStringLen			(const cell *ptr) { }
char *			MF_FormatAmxString			(AMX * amx, cell * params, int startParam, int * pLen) { }
void			MF_CopyAmxMemory			(cell * dest, const cell * src, int len) { }
void			MF_Log						(const char * fmt, ...) { }
void			MF_LogError					(AMX * amx, int err, const char *fmt, ...) { }
int				MF_RaiseAmxError			(AMX * amx, int error) { }
int				MF_RegisterForward			(const char * funcname, ForwardExecType exectype, ...) { }
int				MF_ExecuteForward			(int id, ...) { }
cell			MF_PrepareCellArray			(cell * ptr, unsigned int size) { }
cell			MF_PrepareCharArray			(char * ptr, unsigned int size) { }
cell			MF_PrepareCellArrayA			(cell * ptr, unsigned int size, bool copyBack) { }
cell			MF_PrepareCharArrayA			(char * ptr, unsigned int size, bool copyBack) { }
int				MF_IsPlayerValid			(int id) { }
const char *	MF_GetPlayerName			(int id) { }
const char *	MF_GetPlayerIP				(int id) { }
int				MF_IsPlayerIngame			(int id) { }
int				MF_IsPlayerBot				(int id) { }
int				MF_IsPlayerAuthorized		(int id) { }
float			MF_GetPlayerTime			(int id) { }
float			MF_GetPlayerPlayTime		(int id) { }
int				MF_GetPlayerCurweapon		(int id) { }
const char *	MF_GetPlayerTeam			(int id) { }
int				MF_GetPlayerTeamID			(int id) { }
int				MF_GetPlayerDeaths			(int id) { }
int				MF_GetPlayerMenu			(int id) { }
int				MF_GetPlayerKeys			(int id) { }
int				MF_IsPlayerAlive			(int id) { }
int				MF_GetPlayerFrags			(int id) { }
int				MF_IsPlayerConnecting		(int id) { }
int				MF_IsPlayerHLTV				(int id) { }
int				MF_GetPlayerArmor			(int id) { }
int				MF_GetPlayerHealth			(int id) { }
REAL			amx_ctof					(cell x) { }
cell			amx_ftoc					(float x) { }
int				MF_RegisterSPForwardByName	(AMX * amx, const char *str, ...) { }
int				MF_RegisterSPForward		(AMX * amx, int func, ...) { }
void			MF_UnregisterSPForward		(int id) { }
int				MF_GetPlayerFlags			(int id) { }
edict_t*		MF_GetPlayerEdict			(int id) { }
const char *	MF_Format					(const char *fmt, ...) { }
void			MF_RegisterFunction			(void *pfn, const char *description) { }
void *			MF_RequestFunction			(const char *description) { }
int				MF_AmxPush					(AMX *amx, cell *params) { }
int				MF_AmxExec					(AMX *amx, cell *retval, int idx) { }
int				MF_SetPlayerTeamInfo		(int id, int teamid, const char *teamname) { }
void *			MF_PlayerPropAddr			(int id, int prop) { }
void			MF_RegAuthFunc				(AUTHORIZEFUNC fn) { }
void			MF_UnregAuthFunc			(AUTHORIZEFUNC fn) { }
int				MF_FindLibrary				(const char *name, LibType type) { }
size_t			MF_AddLibraries				(const char *name, LibType type, void *parent) { }
size_t			MF_RemoveLibraries			(void *parent) { }
void			MF_OverrideNatives			(AMX_NATIVE_INFO *natives, const char *myname) { }
const char *	MF_GetLocalInfo				(const char *name, const char *def) { }
int				MF_AmxReRegister			(AMX *amx, AMX_NATIVE_INFO *list, int number) { return 0; }
void *			MF_RegisterFunctionEx		(void *pfn, const char *description) { }
void *			MF_MessageBlock				(int mode, int msg, int *opt) { }
#endif	// MAY_NEVER_BE_DEFINED

#define MF_AddNatives g_fn_AddNatives
#define MF_AddNewNatives g_fn_AddNewNatives
#define MF_BuildPathname g_fn_BuildPathname
#define MF_BuildPathnameR g_fn_BuildPathnameR
#define MF_FormatAmxString g_fn_FormatAmxString
#define MF_GetAmxAddr g_fn_GetAmxAddr
#define MF_PrintSrvConsole g_fn_PrintSrvConsole
#define MF_GetModname g_fn_GetModname
#define MF_GetScriptName g_fn_GetAmxScriptName
#define MF_GetScriptAmx g_fn_GetAmxScript
#define MF_FindScriptByAmx g_fn_FindAmxScriptByAmx
#define MF_FindScriptByName g_fn_FindAmxScriptByName
#define MF_SetAmxString g_fn_SetAmxString
#define MF_GetAmxString g_fn_GetAmxString
#define MF_GetAmxStringLen g_fn_GetAmxStringLen
#define MF_CopyAmxMemory g_fn_CopyAmxMemory
void MF_Log(const char *fmt, ...);
void MF_LogError(AMX *amx, int err, const char *fmt, ...);
#define MF_RaiseAmxError g_fn_RaiseAmxError
#define MF_RegisterForward g_fn_RegisterForward
#define MF_ExecuteForward g_fn_ExecuteForward
#define MF_PrepareCellArray g_fn_PrepareCellArray
#define MF_PrepareCharArray g_fn_PrepareCharArray
#define MF_PrepareCellArrayA g_fn_PrepareCellArrayA
#define MF_PrepareCharArrayA g_fn_PrepareCharArrayA
#define MF_IsPlayerValid g_fn_IsPlayerValid
#define MF_GetPlayerName g_fn_GetPlayerName
#define MF_GetPlayerIP g_fn_GetPlayerIP
#define MF_IsPlayerIngame g_fn_IsPlayerIngame
#define MF_IsPlayerBot g_fn_IsPlayerBot
#define MF_IsPlayerAuthorized g_fn_IsPlayerAuthorized
#define MF_GetPlayerTime g_fn_GetPlayerTime
#define MF_GetPlayerPlayTime g_fn_GetPlayerPlayTime
#define MF_GetPlayerCurweapon g_fn_GetPlayerCurweapon
#define MF_GetPlayerTeam g_fn_GetPlayerTeam
#define MF_GetPlayerTeamID g_fn_GetPlayerTeamID
#define MF_GetPlayerDeaths g_fn_GetPlayerDeaths
#define MF_GetPlayerMenu g_fn_GetPlayerMenu
#define MF_GetPlayerKeys g_fn_GetPlayerKeys
#define MF_IsPlayerAlive g_fn_IsPlayerAlive
#define MF_GetPlayerFrags g_fn_GetPlayerFrags
#define MF_IsPlayerConnecting g_fn_IsPlayerConnecting
#define MF_IsPlayerHLTV g_fn_IsPlayerHLTV
#define MF_GetPlayerArmor g_fn_GetPlayerArmor
#define MF_GetPlayerHealth g_fn_GetPlayerHealth
#define MF_AmxExec g_fn_AmxExec
#define MF_AmxExecv g_fn_AmxExecv
#define MF_AmxFindPublic g_fn_AmxFindPublic
#define MF_AmxAllot g_fn_AmxAllot
#define MF_AmxFindNative g_fn_AmxFindNative
#define MF_LoadAmxScript g_fn_LoadAmxScript
#define MF_UnloadAmxScript g_fn_UnloadAmxScript
#define MF_MergeDefinitionFile g_fn_MergeDefinition_File
#define amx_ctof g_fn_CellToReal
#define amx_ftoc g_fn_RealToCell
#define MF_RegisterSPForwardByName g_fn_RegisterSPForwardByName
#define MF_RegisterSPForward g_fn_RegisterSPForward
#define MF_UnregisterSPForward g_fn_UnregisterSPForward
#define MF_GetPlayerFlags g_fn_GetPlayerFlags
#define MF_GetPlayerEdict g_fn_GetPlayerEdict
#define MF_Format g_fn_Format
#define MF_RegisterFunction g_fn_RegisterFunction
#define MF_RequestFunction g_fn_RequestFunction
#define MF_AmxPush g_fn_AmxPush
#define	MF_SetPlayerTeamInfo g_fn_SetTeamInfo
#define MF_PlayerPropAddr g_fn_PlayerPropAddr
#define MF_RegAuthFunc g_fn_RegAuthFunc
#define MF_UnregAuthFunc g_fn_UnregAuthFunc
#define MF_FindLibrary g_fn_FindLibrary
#define MF_AddLibraries g_fn_AddLibraries
#define MF_RemoveLibraries g_fn_RemoveLibraries
#define MF_OverrideNatives g_fn_OverrideNatives
#define MF_GetLocalInfo g_fn_GetLocalInfo
#define MF_AmxReRegister g_fn_AmxReRegister
#define MF_RegisterFunctionEx g_fn_RegisterFunctionEx
#define MF_MessageBlock g_fn_MessageBlock

#ifdef MEMORY_TEST
/*** Memory ***/
void	*operator new(size_t reportedSize);
void	*operator new[](size_t reportedSize);
void	*operator new(size_t reportedSize, const char *sourceFile, int sourceLine);
void	*operator new[](size_t reportedSize, const char *sourceFile, int sourceLine);
void	operator delete(void *reportedAddress);
void	operator delete[](void *reportedAddress);

// Allocation types
extern	const	unsigned int	m_alloc_unknown;
extern	const	unsigned int	m_alloc_new;
extern	const	unsigned int	m_alloc_new_array;
extern	const	unsigned int	m_alloc_malloc;
extern	const	unsigned int	m_alloc_calloc;
extern	const	unsigned int	m_alloc_realloc;
extern	const	unsigned int	m_alloc_delete;
extern	const	unsigned int	m_alloc_delete_array;
extern	const	unsigned int	m_alloc_free;

// To be called before new / delete
void	Mem_SetOwner(const char *filename, int line, const char *function);
// Actual allocator
void *	Mem_Allocator(const char *sourceFile, const unsigned int sourceLine, const char *sourceFunc,
					const unsigned int allocationType, const size_t reportedSize);
void *	Mem_Reallocator(const char *sourceFile, const unsigned int sourceLine, const char *sourceFunc,
					const unsigned int reallocationType, const size_t reportedSize, void *reportedAddress);
void	Mem_Deallocator(const char *sourceFile, const unsigned int sourceLine, const char *sourceFunc,
					const unsigned int deallocationType, void *reportedAddress);

// memory macros
#ifndef __FUNCTION__
#define __FUNCTION__ "??"
#endif

// call Mem_SetOwner, followed by the actual new operator
#define	new			(Mem_SetOwner(__FILE__,__LINE__,__FUNCTION__),false) ? NULL : new
// call Mem_SetOwner, followed by the actual delete operator
#define	delete		(Mem_SetOwner(__FILE__,__LINE__,__FUNCTION__),false) ? Mem_SetOwner("",0,"") : delete
#define	malloc(sz)	Mem_Allocator  (__FILE__,__LINE__,__FUNCTION__,m_alloc_malloc,sz)
#define	calloc(sz)	Mem_Allocator  (__FILE__,__LINE__,__FUNCTION__,m_alloc_calloc,sz)
#define	realloc(ptr,sz)	Mem_Reallocator(__FILE__,__LINE__,__FUNCTION__,m_alloc_realloc,sz,ptr)
#define	free(ptr)	Mem_Deallocator(__FILE__,__LINE__,__FUNCTION__,m_alloc_free,ptr)

#endif //MEMORY_TEST

#endif // #ifndef __AMXXMODULE_H__
