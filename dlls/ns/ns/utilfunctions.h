//======================================================================
// Forward declarations for functions that deal directly with the engine
//======================================================================
#ifndef UTILFUNCTIONS_H
#define UTILFUNCTIONS_H

#include "CPlayer.h"
extern CPlayer g_player[33];

#define GET_PLAYER_E(x) (&g_player[ENTINDEX(x)]);
#define GET_PLAYER_I(x) (&g_player[x]);

/*
#define FLOAT_TO_CELL(x) *(cell*)&x
#define CELL_TO_FLOAT(x) *(float*)&x
*/
#define FLOAT_TO_CELL	amx_ftoc
#define CELL_TO_FLOAT	amx_ctof



#define ABSOLUTE_VALUE_EASY(x) (((x) < 0) ? (-(x)) : (x)) //very useful for gpGlobals->time comparisons
#define GetEdictModel(edict) ( (g_engfuncs.pfnInfoKeyValue((*g_engfuncs.pfnGetInfoKeyBuffer)(edict), "model")) )
//#define INFO_KEY_VALUE(entity,keyname) (*g_engfuncs.pfnGetInfoKeyBuffer)(entity),keyname)
#define GetKeyValue(edict,key) ( (g_engfuncs.pfnInfoKeyValue((*g_engfuncs.pfnGetInfoKeyBuffer)(edict), key)) )
#define INFO_KEY_BUFFER	(*g_engfuncs.pfnGetInfoKeyBuffer)
#define INFO_KEY_VALUE	(*g_engfuncs.pfnInfoKeyValue)


//just declare extra helper functions you need here
edict_t *UTIL_FindEntityByString(edict_t *pentStart, const char *szKeyword, const char *szValue);
edict_t	*UTIL_PlayerByIndexE( int playerIndex );
int LogToIndex(char logline[128]);
int Find_Building_Hive(void);

void GiveItem(edict_t *pEntity,char *szname);

void HudMessage(int index, const hudtextparms_t &textparms, const char *pMessage);
void ClearHudMessage(edict_t *pEntity, const hudtextparms_t &textparms, const char *pMessage);
void UTIL_EmptyMenu(edict_t *pEntity, int keys, int time);
void UTIL_FakeClientCmd(edict_t *pEntity, char *cmd);

inline edict_t* INDEXENT2( int iEdictNum )
{ 
	if (iEdictNum >= 1 && iEdictNum <= gpGlobals->maxClients)
	{
		CPlayer *player = GET_PLAYER_I(iEdictNum);
		return player->edict;
	}
	else
	{
		return (*g_engfuncs.pfnPEntityOfEntIndex)(iEdictNum); 
	}
}
inline BOOL isValidEntity(int x)
{
	if (x < 0)
		return FALSE;
	if (x >= 0 || x <= gpGlobals->maxClients)
		return TRUE;
	if (x > gpGlobals->maxEntities)
		return FALSE;
	if (FNullEnt(x))
		return FALSE;
	return TRUE;
}
#define CHECK_ENTITY(x) if (x != 0 && (FNullEnt(INDEXENT2(x)) || x < 0 || x > gpGlobals->maxEntities)) return 0;
#define CHECK_PARAMS(x) if (*params/sizeof(cell) < x) return 0;


#endif // UTILFUNCTIONS_H