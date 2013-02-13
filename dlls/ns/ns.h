/* AMX Mod X 
 *   Natural Selection Module 
 * 
 * by the AMX Mod X Development Team 
 *
 * This file is part of AMX Mod X. 
 * 
 * 
 *  This program is free software; you can redistribute it and/or modify it 
 *  under the terms of the GNU General Public License as published by the 
 *  Free Software Foundation; either version 2 of the License, or (at 
 *  your option) any later version. 
 * 
 *  This program is distributed in the hope that it will be useful, but 
 *  WITHOUT ANY WARRANTY; without even the implied warranty of 
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 *  General Public License for more details. 
 * 
 *  You should have received a copy of the GNU General Public License 
 *  along with this program; if not, write to the Free Software Foundation, 
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
 * 
 *  In addition, as a special exception, the author gives permission to 
 *  link the code of this program with the Half-Life Game Engine ("HL 
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve, 
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all 
 *  respects for all of the code used other than the HL Engine and MODs 
 *  from Valve. If you modify this file, you may extend this exception 
 *  to your version of the file, but you are not obligated to do so. If 
 *  you do not wish to do so, delete this exception statement from your 
 *  version. 
 */ 

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

