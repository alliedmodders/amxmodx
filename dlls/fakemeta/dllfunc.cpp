#include "fakemeta_amxx.h"

//by mahnsawce from his NS module
static cell AMX_NATIVE_CALL dllfunc(AMX *amx,cell *params)
{
	int type;
	int index;
	int indexb;
	char *temp = "";
	char *temp2 = "";
	char *temp3 = "";
	vec3_t Vec1;
	vec3_t Vec2;
	int iparam1;
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
		return gpGamedllFuncs->dllapi_table->pfnSpawn(INDEXENT2(index));

		// pfnThink
	case	DLLFunc_Think:	// void )				( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnThink(INDEXENT2(index));
		return 1;

		// pfnUse
	case	DLLFunc_Use:	// void )				( edict_t *pentUsed, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnUse(INDEXENT2(index),INDEXENT2(indexb));
		return 1;

		// pfnTouch
	case	DLLFunc_Touch:	// void )				( edict_t *pentTouched, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnTouch(INDEXENT2(index),INDEXENT2(indexb));
		return 1;

	case	DLLFunc_Blocked:	// void )			( edict_t *pentBlocked, edict_t *pentOther );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		gpGamedllFuncs->dllapi_table->pfnBlocked(INDEXENT2(index),INDEXENT2(indexb));
		return 1;

		
	case	DLLFunc_SetAbsBox:			// void )			( edict_t *pent );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSetAbsBox(INDEXENT2(index));
		return 1;

	case	DLLFunc_ClientConnect:		// bool)		( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] );
		// index,szName,szAddress,szRetRejectReason,size
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		temp = MF_GetAmxString(amx,params[3],0,&len);
		temp2 = MF_GetAmxString(amx,params[4],1,&len);
		//temp3 = GET_AMXSTRING(amx,params[5],2,len);
		iparam1 = MDLL_ClientConnect(INDEXENT2(index),STRING(ALLOC_STRING(temp)),STRING(ALLOC_STRING(temp2)),temp3);
		cRet = MF_GetAmxAddr(amx,params[6]);
		MF_SetAmxString(amx,params[5],temp3,cRet[0]);
		return 1;
	
	case	DLLFunc_ClientDisconnect:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientDisconnect(INDEXENT2(index));
		return 1;

	case	DLLFunc_ClientKill:		// void )		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientKill(INDEXENT2(index));
		return 1;

	case	DLLFunc_ClientPutInServer:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientPutInServer(INDEXENT2(index));
		return 1;

	case	DLLFunc_ServerDeactivate:	// void)	( void );
		gpGamedllFuncs->dllapi_table->pfnServerDeactivate();
		return 1;

	case	DLLFunc_PlayerPreThink:		// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnPlayerPreThink(INDEXENT2(index));
		return 1;

	case	DLLFunc_PlayerPostThink:		// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnPlayerPostThink(INDEXENT2(index));
		return 1;

	case	DLLFunc_StartFrame:		// void )		( void );
		gpGamedllFuncs->dllapi_table->pfnStartFrame();
		return 1;

	case	DLLFunc_ParmsNewLevel:		// void )		( void );
		gpGamedllFuncs->dllapi_table->pfnParmsNewLevel();


	case	DLLFunc_ParmsChangeLevel:	// void )	( void );
		gpGamedllFuncs->dllapi_table->pfnParmsChangeLevel();

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
		gpGamedllFuncs->dllapi_table->pfnSpectatorConnect(INDEXENT2(index));
		return 1;
	case	DLLFunc_SpectatorDisconnect:	// void )	( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorDisconnect(INDEXENT2(index));
		return 1;
	case	DLLFunc_SpectatorThink:		// void )		( edict_t *pEntity );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnSpectatorThink(INDEXENT2(index));
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
		gpGamedllFuncs->dllapi_table->pfnRegisterEncoders;
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
	case	DLLFunc_CreateInstancedBaselines:	// void ) ( void );
		gpGamedllFuncs->dllapi_table->pfnCreateInstancedBaselines();
		return 1;

	case	DLLFunc_pfnAllowLagCompensation:	// int )( void );
		return gpGamedllFuncs->dllapi_table->pfnAllowLagCompensation();
		// I know this doesn't fit with dllfunc, but I dont want to create another native JUST for this.
	case	MetaFunc_CallGameEntity:	// bool	(plid_t plid, const char *entStr,entvars_t *pev);
		temp = MF_GetAmxString(amx,params[2],0,&len);
		cRet = MF_GetAmxAddr(amx,params[3]);
		index = cRet[0];
		CHECK_ENTITY(index);
		iparam1 = gpMetaUtilFuncs->pfnCallGameEntity(PLID,STRING(ALLOC_STRING(temp)),VARS(INDEXENT2(index)));
		return iparam1;
	case	DLLFunc_ClientUserInfoChanged: // void ) (edict_t *pEntity, char *infobuffer)
		index = cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->dllapi_table->pfnClientUserInfoChanged(INDEXENT2(index),(*g_engfuncs.pfnGetInfoKeyBuffer)(INDEXENT2(index)));
		return 1;


	default:
		MF_Log("Unknown dllfunc entry.");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}
}

AMX_NATIVE_INFO dllfunc_natives[] = {
	{"dllfunc",			dllfunc},
	{NULL,				NULL},
};