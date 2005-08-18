#include "newdllfunc.h"

static cell AMX_NATIVE_CALL newdllfunc(AMX *amx,cell *params)
{
	int type;
	int index;
	int indexb;
	//char *temp = "";
	//char *temp2 = "";
	//char *temp3 = "";
	//vec3_t Vec1;
	//vec3_t Vec2;
	int iparam1;
	//int len;
	cell *cRet;
	type = params[1];
	switch(type)
	{

		/*// pfnGameInit
	case	DLLFunc_GameInit:	// void)			( void );				
		gpGamedllFuncs->dllapi_table->pfnGameInit();
		return 1;
		*/
	case NEWDLLFunc_OnFreeEntPrivateData:	// void )				( edict_t *pent );
		cRet = MF_GetAmxAddr(amx, params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		gpGamedllFuncs->newapi_table->pfnOnFreeEntPrivateData(INDEXENT2(index));
		return 1;
	case NEWDLLFunc_GameShutdown:	// void ) ( void );
		gpGamedllFuncs->newapi_table->pfnGameShutdown();
		return 1;
	case NEWDLLFunc_ShouldCollide:	// int ) ( ent1, ent2 );
		cRet = MF_GetAmxAddr(amx,params[2]);
		index=cRet[0];
		CHECK_ENTITY(index);
		cRet = MF_GetAmxAddr(amx,params[3]);
		indexb=cRet[0];
		CHECK_ENTITY(indexb);
		iparam1 = gpGamedllFuncs->newapi_table->pfnShouldCollide(INDEXENT2(index),INDEXENT2(indexb));
		return iparam1;
	default:
		MF_LogError(amx, AMX_ERR_NATIVE, "Unknown newdllfunc entry %d", type);
		return 0;
	}
}

AMX_NATIVE_INFO newdllfunc_natives[] = {
	{"newdllfunc",		newdllfunc},
	{NULL,				NULL},
};