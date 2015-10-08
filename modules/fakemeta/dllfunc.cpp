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

#include "fakemeta_amxx.h"

//by mahnsawce from his NS module
static cell AMX_NATIVE_CALL dllfunc(AMX *amx,cell *params)
{
	int type;
	int index;
	int indexb;
	unsigned char *pset;
	const char *temp = "";
	const char *temp2 = "";
	const char *temp3 = "";
	vec3_t Vec1;
	vec3_t Vec2;
	int iparam1;
	int iparam2;
	int iparam3;
	entity_state_t *es;
	int len;
	cell *cRet;
	type = params[1];
	switch(type)
	{

		// pfnGameInit
	case	DLLFunc_GameInit:	// void)			( void );
		gpGamedllFuncs->dllapi_table->pfnGameInit();
		return 1;

		// pfnSpawn
	case	DLLFunc_Spawn:	// int )				( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		return gpGamedllFuncs->dllapi_table->pfnSpawn(TypeConversion.id_to_edict(index));

		// pfnThink
	case	DLLFunc_Think:	// void )				( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnThink(TypeConversion.id_to_edict(index));
		return 1;

		// pfnUse
	case	DLLFunc_Use:	// void )				( edict_t *pentUsed, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnUse(TypeConversion.id_to_edict(index), TypeConversion.id_to_edict(indexb));
		return 1;

	case DLLFunc_KeyValue:
		{
			cRet = MF_GetAmxAddr(amx, params[2]);
			index=cRet[0];
			CHECK_ENTITY(index);
			cRet = MF_GetAmxAddr(amx, params[3]);

			KeyValueData *kvd;
			if (*cRet == 0)
				kvd = &(g_kvd_glb.kvd);
			else
				kvd = reinterpret_cast<KeyValueData *>(*cRet);

			gpGamedllFuncs->dllapi_table->pfnKeyValue(TypeConversion.id_to_edict(index), kvd);
			return 1;
		}

		// pfnTouch
	case	DLLFunc_Touch:	// void )				( edict_t *pentTouched, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnTouch(TypeConversion.id_to_edict(index), TypeConversion.id_to_edict(indexb));
		return 1;

	case	DLLFunc_Blocked:	// void )			( edict_t *pentBlocked, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnBlocked(TypeConversion.id_to_edict(index), TypeConversion.id_to_edict(indexb));
		return 1;


	case	DLLFunc_SetAbsBox:			// void )			( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSetAbsBox(TypeConversion.id_to_edict(index));
		return 1;

	case	DLLFunc_ClientConnect:		// bool)		( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] );
		// index,szName,szAddress,szRetRejectReason,size
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],1,&len);
		//temp3 = GET_AMXSTRING(amx,params[5],2,len);
		iparam1 = MDLL_ClientConnect(TypeConversion.id_to_edict(index),STRING(ALLOC_STRING(temp)),STRING(ALLOC_STRING(temp2)),(char *)temp3);
		cRet = MF_GetAmxAddr(amx,params[6]);
		MF_SetAmxString(amx,params[5],temp3,cRet[0]);
		return iparam1;

	case	DLLFunc_ClientDisconnect:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientDisconnect(TypeConversion.id_to_edict(index));
		return 1;

	case	DLLFunc_ClientKill:		// void )		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientKill(TypeConversion.id_to_edict(index));
		return 1;

	case	DLLFunc_ClientPutInServer:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientPutInServer(TypeConversion.id_to_edict(index));
		return 1;

	case	DLLFunc_ServerDeactivate:	// void)	( void );
		gpGamedllFuncs->dllapi_table->pfnServerDeactivate();
		return 1;

	case	DLLFunc_PlayerPreThink:		// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnPlayerPreThink(TypeConversion.id_to_edict(index));
		return 1;

	case	DLLFunc_PlayerPostThink:		// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnPlayerPostThink(TypeConversion.id_to_edict(index));
		return 1;

	case	DLLFunc_StartFrame:		// void )		( void );
		gpGamedllFuncs->dllapi_table->pfnStartFrame();
		return 1;

	case	DLLFunc_ParmsNewLevel:		// void )		( void );
		gpGamedllFuncs->dllapi_table->pfnParmsNewLevel();
		return 1;

	case	DLLFunc_ParmsChangeLevel:	// void )	( void );
		gpGamedllFuncs->dllapi_table->pfnParmsChangeLevel();
		return 1;

	 // Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	case	DLLFunc_GetGameDescription:	 // const char * )( void );
		temp = (char*)gpGamedllFuncs->dllapi_table->pfnGetGameDescription();
		cRet = MF_GetAmxAddr(amx,params[3]);
		MF_SetAmxString(amx,params[2],temp,cRet[0]);
		return 1;

		// Spectator funcs
	case	DLLFunc_SpectatorConnect:	// void)		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorConnect(TypeConversion.id_to_edict(index));
		return 1;
	case	DLLFunc_SpectatorDisconnect:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorDisconnect(TypeConversion.id_to_edict(index));
		return 1;
	case	DLLFunc_SpectatorThink:		// void )		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorThink(TypeConversion.id_to_edict(index));
		return 1;

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	case	DLLFunc_Sys_Error:		// void )			( const char *error_string );
		temp = MF_GetAmxString(amx,params[2],0,&len);
		gpGamedllFuncs->dllapi_table->pfnSys_Error(STRING(ALLOC_STRING(temp)));
		return 1;

	case	DLLFunc_PM_FindTextureType:	// char )( char *name );
		temp = MF_GetAmxString(amx,params[2],0,&len);
		return gpGamedllFuncs->dllapi_table->pfnPM_FindTextureType(temp);

	case	DLLFunc_RegisterEncoders:	// void )	( void );
		gpGamedllFuncs->dllapi_table->pfnRegisterEncoders();
		return 1;

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	case	DLLFunc_GetHullBounds:	// int)	( int hullnumber, float *mins, float *maxs );
		cRet = MF_GetAmxAddr(amx,params[2]);
		iparam1 = gpGamedllFuncs->dllapi_table->pfnGetHullBounds(cRet[0],Vec1,Vec2);
		cRet = MF_GetAmxAddr(amx,params[3]);
		cRet[0]=amx_ftoc(Vec1[0]);
		cRet[1]=amx_ftoc(Vec1[1]);
		cRet[2]=amx_ftoc(Vec1[2]);
		cRet = MF_GetAmxAddr(amx,params[4]);
		cRet[0]=amx_ftoc(Vec2[0]);
		cRet[1]=amx_ftoc(Vec2[1]);
		cRet[2]=amx_ftoc(Vec2[2]);
		return iparam1;

	// Create baselines for certain "unplaced" items.
	case	DLLFunc_CreateInstancedBaselines:		// void ) ( void );
		gpGamedllFuncs->dllapi_table->pfnCreateInstancedBaselines();
		return 1;

	case	DLLFunc_pfnAllowLagCompensation:	// int )( void );
		return gpGamedllFuncs->dllapi_table->pfnAllowLagCompensation();
		// I know this doesn't fit with dllfunc, but I don't want to create another native JUST for this.
	case	MetaFunc_CallGameEntity:	// bool	(plid_t plid, const char *entStr,entvars_t *pev);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		cRet = MF_GetAmxAddr(amx,params[3]);
		index = cRet[0];
		CHECK_ENTITY(index);
		iparam1 = gpMetaUtilFuncs->pfnCallGameEntity(PLID,STRING(ALLOC_STRING(temp)),VARS(TypeConversion.id_to_edict(index)));
		return iparam1;
	case	DLLFunc_ClientUserInfoChanged: // void ) (edict_t *pEntity, char *infobuffer)
		cRet = MF_GetAmxAddr(amx,params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientUserInfoChanged(TypeConversion.id_to_edict(index),(*g_engfuncs.pfnGetInfoKeyBuffer)(TypeConversion.id_to_edict(index)));
		return 1;
	case	DLLFunc_UpdateClientData:		// void ) (const struct edict_s *ent, int sendweapons, struct clientdata_s *cd)
		cRet = MF_GetAmxAddr(amx, params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx, params[3]);
		iparam1 = cRet[0];

		clientdata_t *cd;

		if ((params[0] / sizeof(cell)) == 4)
		{
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			if (*ptr == 0)
				cd = &g_cd_glb;
			else
				cd = reinterpret_cast<clientdata_s *>(*ptr);
		}
		else
			cd = &g_cd_glb;


		gpGamedllFuncs->dllapi_table->pfnUpdateClientData(TypeConversion.id_to_edict(index), iparam1, cd);

		return 1;
	case	DLLFunc_AddToFullPack:		// int ) (struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet)
		cRet = MF_GetAmxAddr(amx, params[2]);

		if (*cRet == 0)
			es = &g_es_glb;
		else
			es = reinterpret_cast<entity_state_t *>(*cRet);

		// int e
		cRet = MF_GetAmxAddr(amx, params[3]);
		iparam1 = cRet[0];

		// edict_t *ent
		cRet = MF_GetAmxAddr(amx, params[4]);
		index = cRet[0];
		CHECK_ENTITY(index);

		// edict_t *host
		cRet = MF_GetAmxAddr(amx, params[5]);
		indexb = cRet[0];
		CHECK_ENTITY(indexb);

		// int hostflags
		cRet = MF_GetAmxAddr(amx, params[6]);
		iparam2 = cRet[0];

		// int player
		cRet = MF_GetAmxAddr(amx, params[7]);
		iparam3 = cRet[0];

		// unsigned char *pSet
		cRet = MF_GetAmxAddr(amx, params[8]);
		pset = reinterpret_cast<unsigned char *>(*cRet);

		return gpGamedllFuncs->dllapi_table->pfnAddToFullPack(es, iparam1, TypeConversion.id_to_edict(index), TypeConversion.id_to_edict(indexb), iparam2, iparam3, pset);
	case DLLFunc_CmdStart:			// void ) (const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed)
		cRet = MF_GetAmxAddr(amx, params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);

		usercmd_t *uc;
		cRet = MF_GetAmxAddr(amx, params[3]);

		if (*cRet == 0)
			uc = &g_uc_glb;
		else
			uc = reinterpret_cast<usercmd_t *>(*cRet);


		cRet = MF_GetAmxAddr(amx, params[4]);
		iparam1 = cRet[0];

		gpGamedllFuncs->dllapi_table->pfnCmdStart(TypeConversion.id_to_edict(index), uc, iparam1);

		return 1;
	case DLLFunc_CmdEnd:			// void ) (const edict_t *player)
		cRet = MF_GetAmxAddr(amx, params[2]);
		index = cRet[0];
		CHECK_ENTITY(index);

		gpGamedllFuncs->dllapi_table->pfnCmdEnd(TypeConversion.id_to_edict(index));

		return 1;

	case DLLFunc_CreateBaseline:	// void )	(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs);
		cRet = MF_GetAmxAddr(amx, params[2]);
		iparam1 = cRet[0];
		cRet = MF_GetAmxAddr(amx, params[3]);
		iparam2 = cRet[0];	

		cRet = MF_GetAmxAddr(amx, params[4]);

		if (*cRet == 0)
			es = &g_es_glb;
		else
			es = reinterpret_cast<entity_state_t *>(*cRet);

		cRet = MF_GetAmxAddr(amx, params[5]);
		index = cRet[0];

		cRet = MF_GetAmxAddr(amx, params[6]);
		iparam3 = cRet[0];

		CHECK_ENTITY(index);

		cRet = MF_GetAmxAddr(amx, params[7]);
		Vec1.x = amx_ctof(cRet[0]);
		Vec1.y = amx_ctof(cRet[1]);
		Vec1.z = amx_ctof(cRet[2]);

		cRet = MF_GetAmxAddr(amx, params[8]);
		Vec2.x = amx_ctof(cRet[0]);
		Vec2.y = amx_ctof(cRet[1]);
		Vec2.z = amx_ctof(cRet[2]);

		gpGamedllFuncs->dllapi_table->pfnCreateBaseline(iparam1, iparam2, es, TypeConversion.id_to_edict(index), iparam3, Vec1, Vec2);

		return 1;
	default:
		MF_LogError(amx, AMX_ERR_NATIVE, "Unknown dllfunc entry %d", type);
		return 0;
	}
}

AMX_NATIVE_INFO dllfunc_natives[] = {
	{"dllfunc",			dllfunc},
	{NULL,				NULL},
};

