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

#ifndef NS_H
#define NS_H

#include "ns_const.h"

#if defined CRAZY_OPTS
#define EXTERN_VIS __attribute__((externally_visible))
#else
#define EXTERN_VIS
#endif

extern DLL_FUNCTIONS *g_pFunctionTable;
extern DLL_FUNCTIONS *g_pFunctionTable_Post;
extern enginefuncs_t *g_pengfuncsTable;
extern enginefuncs_t *g_pengfuncsTable_Post;
extern NEW_DLL_FUNCTIONS *g_pNewFunctionsTable;
extern NEW_DLL_FUNCTIONS *g_pNewFunctionsTable_Post;




extern class CPlayer g_player[33];
extern edict_t *player_edicts[33]; // Stupid INDEXENT() bug.



void PlayerPreThink(edict_t *pEntity);
void PlayerPreThink_Post(edict_t *pEntity);
void PlayerPostThink_Post(edict_t *pEntity);
void UpdateClientData( const struct edict_s *ent, int sendweapons, struct clientdata_s *cd );
void StartFrame(void);
edict_t* CreateNamedEntity_Post(int className);
void AlertMessage_Post(ALERT_TYPE atype, const char *szFmt, ...);


typedef struct tagAMX_HEADER {
  int32_t size          PACKED; /* size of the "file" */
  uint16_t magic        PACKED; /* signature */
  char    file_version;         /* file format version */
  char    amx_version;          /* required version of the AMX */
  int16_t flags         PACKED;
  int16_t defsize       PACKED; /* size of a definition record */
  int32_t cod           PACKED; /* initial value of COD - code block */
  int32_t dat           PACKED; /* initial value of DAT - data block */
  int32_t hea           PACKED; /* initial value of HEA - start of the heap */
  int32_t stp           PACKED; /* initial value of STP - stack top */
  int32_t cip           PACKED; /* initial value of CIP - the instruction pointer */
  int32_t publics       PACKED; /* offset to the "public functions" table */
  int32_t natives       PACKED; /* offset to the "native functions" table */
  int32_t libraries     PACKED; /* offset to the table of libraries */
  int32_t pubvars       PACKED; /* the "public variables" table */
  int32_t tags          PACKED; /* the "public tagnames" table */
  int32_t nametable     PACKED; /* name table */
} PACKED AMX_HEADER;

// Don't enable this, it just adds some offset finding natives
#define DEVELOPER_BUILD


#endif // NS_H

