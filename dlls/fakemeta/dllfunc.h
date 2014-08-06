// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#ifndef _INCLUDE_DLLFUNC_H
#define _INCLUDE_DLLFUNC_H

enum
{
	DLLFunc_GameInit,					// void )			( void );				
	DLLFunc_Spawn,						// int  )			(edict_t *pent);
	DLLFunc_Think,						// void )			(edict_t *pent);
	DLLFunc_Use,						// void )			(edict_t *pentUsed, edict_t *pentOther);
	DLLFunc_Touch,						// void )			(edict_t *pentTouched, edict_t *pentOther);
	DLLFunc_Blocked,					// void )			(edict_t *pentBlocked, edict_t *pentOther);

	//You can pass in 0 for glb kvd handle or a kvd handle here
	DLLFunc_KeyValue,					// void )			(edict_t *pentKeyvalue, KeyValueData *pkvd);
	DLLFunc_SetAbsBox,					// void )			(edict_t *pent);
	DLLFunc_ClientConnect,				// bool )			(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128]);

	DLLFunc_ClientDisconnect,			// void )			(edict_t *pEntity);
	DLLFunc_ClientKill,					// void )			(edict_t *pEntity);
	DLLFunc_ClientPutInServer,			// void )			(edict_t *pEntity);
	DLLFunc_ClientCommand,				// void )			(edict_t *pEntity);

	DLLFunc_ServerDeactivate,			// void )			( void );

	DLLFunc_PlayerPreThink,				// void )			(edict_t *pEntity);
	DLLFunc_PlayerPostThink,			// void )			(edict_t *pEntity);

	DLLFunc_StartFrame,					// void )			( void );
	DLLFunc_ParmsNewLevel,				// void )			( void );
	DLLFunc_ParmsChangeLevel,			// void )			( void );

	// Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	// This also gets called when the server is queried for information (for example, by a server browser tool)
	DLLFunc_GetGameDescription,	 		// const char *)	( void );     

	// Spectator funcs
	DLLFunc_SpectatorConnect,			// void )			(edict_t *pEntity);
	DLLFunc_SpectatorDisconnect,		// void )			(edict_t *pEntity);
	DLLFunc_SpectatorThink,				// void )			(edict_t *pEntity);

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	DLLFunc_Sys_Error,					// void )			(const char *error_string);

	DLLFunc_PM_FindTextureType,			// char )			(char *name);
	DLLFunc_RegisterEncoders,			// void )			( void );

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	DLLFunc_GetHullBounds,				// int  )			(int hullnumber, float *mins, float *maxs);

	// Create baselines for certain "unplaced" items.
	DLLFunc_CreateInstancedBaselines,	// void )			( void );
	DLLFunc_pfnAllowLagCompensation,	// int  )			( void );
	// I know this does not fit with DLLFUNC(), but I don't want another native just for it.
	MetaFunc_CallGameEntity,			// bool	)			(plid_t plid, const char *entStr,entvars_t *pev);
	DLLFunc_ClientUserInfoChanged,		// void	)			(edict *pEntity, char *infobuffer);
	// You can pass in 0 for global client data handle or another client data handle here
	DLLFunc_UpdateClientData,			// void )			(const struct edict_s *ent, int sendweapons, struct clientdata_s *cd);
	// You can pass in 0 for global entity state handle or another entity state handle here
	DLLFunc_AddToFullPack,				// int  )			(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet);
	// You can pass in 0 for global usercmd handle or another usercmd handle here
	DLLFunc_CmdStart,					// void )			(const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed);
	DLLFunc_CmdEnd,						// void )			(const edict_t *player);
	DLLFunc_CreateBaseline				// void )			(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs);
};

#endif //_INCLUDE_DLLFUNC_H

