#ifndef NS_H
#define NS_H


#include <extdll.h>
#include <string.h>
#include "amxxmodule.h"
#include "ns_const.h"
#include "CPlayer.h"
#include "CSpawn.h"
#include "utilfunctions.h"

extern DLL_FUNCTIONS *g_pFunctionTable;
extern DLL_FUNCTIONS *g_pFunctionTable_Post;
extern enginefuncs_t *g_pengfuncsTable;
extern enginefuncs_t *g_pengfuncsTable_Post;
extern NEW_DLL_FUNCTIONS *g_pNewFunctionsTable;
extern NEW_DLL_FUNCTIONS *g_pNewFunctionsTable_Post;


extern CSpawn ns_spawnpoints;


extern BOOL iscombat;
extern CPlayer g_player[33];
extern edict_t *player_edicts[33]; // Stupid INDEXENT() bug.

extern int gmsgHudText2;


extern AMX_NATIVE_INFO ns_misc_natives[];
extern AMX_NATIVE_INFO ns_pdata_natives[];

extern int SpawnForward;
extern int TeamForward;

extern int ChangeclassForward;
extern int BuiltForward;


////////////////////////////////



#endif // NS_H

