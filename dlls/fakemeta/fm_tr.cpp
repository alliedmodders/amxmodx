#include "fakemeta_amxx.h"

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
	cell *cPtr = &params[1];
	int type = params[2];
	TraceResult *tr = (TraceResult*)((void *)cPtr);

	if (*params / sizeof(cell) < 3)
		return 0;	//TODO: Error
	
	cell *ptr = MF_GetAmxAddr(amx, params[3]);
	edict_t *e = 0;

	switch (type)
	{
	case TR_AllSolid:
		{
			tr->fAllSolid = *ptr;
			return 1;
			break;
		}
	case TR_StartSolid:
		{
			return tr->fStartSolid;
			break;
		}
	case TR_InWater:
		{
			tr->fInWater = *ptr;
			return 1;
			break;
		}
	case TR_flFraction:
		{
			tr->flFraction = amx_ctof(*ptr);
			return 1;
			break;
		}
	case TR_vecEndPos:
		{
			tr->vecEndPos.x = amx_ctof(ptr[0]);
			tr->vecEndPos.y = amx_ctof(ptr[1]);
			tr->vecEndPos.z = amx_ctof(ptr[2]);
			return 1;
			break;
		}
	case TR_flPlaneDist:
		{
			tr->flPlaneDist = amx_ctof(*ptr);
			return 1;
			break;
		}
	case TR_vecPlaneNormal:
		{
			tr->vecPlaneNormal.x = amx_ctof(ptr[0]);
			tr->vecPlaneNormal.y = amx_ctof(ptr[1]);
			tr->vecPlaneNormal.z = amx_ctof(ptr[2]);
			return 1;
			break;
		}
	case TR_pHit:
		{
			e = INDEXENT(*ptr);
			if (!e || FNullEnt(e))
				return 0; //TODO: return error
			tr->pHit = e;
			return 1;
			break;
		}
	case TR_iHitgroup:
		{
			tr->iHitgroup = *ptr;
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
	cell *cPtr = &params[1];
	int type = params[2];
	TraceResult *tr = (TraceResult*)((void *)cPtr);
	cell *ptr = 0;

	switch (type)
	{
	case TR_AllSolid:
		{
			return tr->fAllSolid;
			break;
		}
	case TR_StartSolid:
		{
			return tr->fStartSolid;
			break;
		}
	case TR_InWater:
		{
			return tr->fInWater;
			break;
		}
	case TR_flFraction:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			*ptr = amx_ftoc(tr->flFraction);
			return 1;
			break;
		}
	case TR_vecEndPos:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			ptr[0] = amx_ftoc(tr->vecEndPos.x);
			ptr[1] = amx_ftoc(tr->vecEndPos.y);
			ptr[2] = amx_ftoc(tr->vecEndPos.z);
			return 1;
			break;
		}
	case TR_flPlaneDist:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			*ptr = amx_ftoc(tr->flPlaneDist);
			return 1;
			break;
		}
	case TR_vecPlaneNormal:
		{
			ptr = MF_GetAmxAddr(amx, params[3]);
			ptr[0] = amx_ftoc(tr->vecPlaneNormal.x);
			ptr[1] = amx_ftoc(tr->vecPlaneNormal.y);
			ptr[2] = amx_ftoc(tr->vecPlaneNormal.z);
			return 1;
			break;
		}
	case TR_pHit:
		{
			return ENTINDEX(tr->pHit);
			break;
		}
	case TR_iHitgroup:
		{
			return tr->iHitgroup;
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