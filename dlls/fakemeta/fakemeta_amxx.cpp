#include "fakemeta_amxx.h"

edict_t *g_player_edicts[33]; // Used for INDEXENT() forward.

void OnAmxxAttach()
{
	MF_AddNatives(engfunc_natives);
	MF_AddNatives(dllfunc_natives);
	MF_AddNatives(pev_natives);
}

// sawce:  Do not null out the forward for ServerActivate.  It's required for the INDEXENT() fix. (I don't think ServerActivate is planned on being forwarded anyway)
void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	for(int i = 1; i <= gpGlobals->maxClients;i++) 
		g_player_edicts[i]=pEdictList + i;
	RETURN_META(MRES_IGNORED);
}
