#include "fakemeta_amxx.h"

TraceResult *gfm_tr;

/*enum
{
	TR_AllSolid,
    TR_StartSolid,
	TR_InOpen,
	TR_InWater,
	TR_flFraction,
	TR_vecEndPos,
	TR_flPlaneDist,
	TR_vecPlaneNormal,
	TR_pHit,
	TR_iHitgroup,
};*/

static cell AMX_NATIVE_CALL set_tr(AMX *amx, cell *params)
{
	int type = params[1];
	//TraceResult *tr = (TraceResult*)((void *)cPtr);

	if (*params / sizeof(cell) < 2)
		return 0;	//TODO: Error
	
	cell *ptr = MF_GetAmxAddr(amx, params[2]);
	edict_t *e = 0;

	switch (type)
	{
	case TR_AllSolid:
		{
			gfm_tr->fAllSolid = *ptr;
			return 1;
			break;
		}
	case TR_StartSolid:
		{
			return gfm_tr->fStartSolid;
			break;
		}
	case TR_InWater:
		{
			gfm_tr->fInWater = *ptr;
			return 1;
			break;
		}
	case TR_flFraction:
		{
			gfm_tr->flFraction = amx_ctof(*ptr);
			return 1;
			break;
		}
	case TR_vecEndPos:
		{
			gfm_tr->vecEndPos.x = amx_ctof(ptr[0]);
			gfm_tr->vecEndPos.y = amx_ctof(ptr[1]);
			gfm_tr->vecEndPos.z = amx_ctof(ptr[2]);
			return 1;
			break;
		}
	case TR_flPlaneDist:
		{
			gfm_tr->flPlaneDist = amx_ctof(*ptr);
			return 1;
			break;
		}
	case TR_vecPlaneNormal:
		{
			gfm_tr->vecPlaneNormal.x = amx_ctof(ptr[0]);
			gfm_tr->vecPlaneNormal.y = amx_ctof(ptr[1]);
			gfm_tr->vecPlaneNormal.z = amx_ctof(ptr[2]);
			return 1;
			break;
		}
	case TR_pHit:
		{
			e = INDEXENT(*ptr);
			if (!e || FNullEnt(e))
				return 0; //TODO: return error
			gfm_tr->pHit = e;
			return 1;
			break;
		}
	case TR_iHitgroup:
		{
			gfm_tr->iHitgroup = *ptr;
			return 1;
			break;
		}
	default:
		{
			//TODO: error
			return 0;
		}
	}
}

static cell AMX_NATIVE_CALL get_tr(AMX *amx, cell *params)
{
	int type = params[1];
	cell *ptr = 0;

	switch (type)
	{
	case TR_AllSolid:
		{
			return gfm_tr->fAllSolid;
			break;
		}
	case TR_StartSolid:
		{
			return gfm_tr->fStartSolid;
			break;
		}
	case TR_InWater:
		{
			return gfm_tr->fInWater;
			break;
		}
	case TR_flFraction:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			*ptr = amx_ftoc(gfm_tr->flFraction);
			return 1;
			break;
		}
	case TR_vecEndPos:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			ptr[0] = amx_ftoc(gfm_tr->vecEndPos.x);
			ptr[1] = amx_ftoc(gfm_tr->vecEndPos.y);
			ptr[2] = amx_ftoc(gfm_tr->vecEndPos.z);
			return 1;
			break;
		}
	case TR_flPlaneDist:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			*ptr = amx_ftoc(gfm_tr->flPlaneDist);
			return 1;
			break;
		}
	case TR_vecPlaneNormal:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			ptr[0] = amx_ftoc(gfm_tr->vecPlaneNormal.x);
			ptr[1] = amx_ftoc(gfm_tr->vecPlaneNormal.y);
			ptr[2] = amx_ftoc(gfm_tr->vecPlaneNormal.z);
			return 1;
			break;
		}
	case TR_pHit:
		{
			return ENTINDEX(gfm_tr->pHit);
			break;
		}
	case TR_iHitgroup:
		{
			return gfm_tr->iHitgroup;
			break;
		}
	default:
		{
			//TODO: error
			return 0;
		}
	}
}

AMX_NATIVE_INFO tr_Natives[] = {
	{"get_tr",			get_tr},
	{"set_tr",			set_tr},
	{NULL,				NULL},
};