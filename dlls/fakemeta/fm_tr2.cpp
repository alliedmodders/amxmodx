#include "fakemeta_amxx.h"

TraceResult g_tr_2;
KeyValueData g_kvd_2;

KVD_Wrapper g_kvd_glb;

static cell AMX_NATIVE_CALL set_tr2(AMX *amx, cell *params)
{
	TraceResult *tr;
	if (params[1] == 0)
		tr = &g_tr_2;
	else
		tr = reinterpret_cast<TraceResult *>(params[1]);

	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	cell *ptr = MF_GetAmxAddr(amx, params[3]);

	switch (params[2])
	{
	case TR_AllSolid:
		{
			tr->fAllSolid = *ptr;
			return 1;
			break;
		}
	case TR_InOpen:
		{
			tr->fInOpen = *ptr;
			return 1;
			break;
		}
	case TR_StartSolid:
		{
			tr->fStartSolid = *ptr;
			return 1;
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
			edict_t *e = INDEXENT(*ptr);
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
			MF_LogError(amx, AMX_ERR_NATIVE, "Unknown traceresult member %d", params[2]);
			return 0;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_tr2(AMX *amx, cell *params)
{
	TraceResult *tr;
	if (params[1] == 0)
		tr = &g_tr_2;
	else
		tr = reinterpret_cast<TraceResult *>(params[1]);

	cell *ptr;

	switch (params[2])
	{
	case TR_AllSolid:
		{
			return tr->fAllSolid;
			break;
		}
	case TR_InOpen:
		{
			return tr->fInOpen;
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
			if (gfm_tr->pHit == NULL || FNullEnt(gfm_tr->pHit))
				return -1;
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
			MF_LogError(amx, AMX_ERR_NATIVE, "Unknown traceresult member %d", params[2]);
			return 0;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_kvd(AMX *amx, cell *params)
{
	KVD_Wrapper *kvdw;
	KeyValueData *kvd;
	if (params[1] == 0)
		kvdw = &g_kvd_glb;
	else
		kvdw = reinterpret_cast<KVD_Wrapper *>(params[1]);
	kvd = kvdw->kvd;

	switch (params[2])
	{
	case KV_fHandled:
		{
			return kvd->fHandled;
			break;
		}
	case KV_ClassName:
		{
			if (params[0] / sizeof(cell) != 4)
			{
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
				return 0;
			}
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			return MF_SetAmxString(amx, params[3], kvd->szClassName, (int)*ptr);
			break;
		}
	case KV_KeyName:
		{
			if (params[0] / sizeof(cell) != 4)
			{
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
				return 0;
			}
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			return MF_SetAmxString(amx, params[3], kvd->szKeyName, (int)*ptr);
			break;
		}
	case KV_Value:
		{
			if (params[0] / sizeof(cell) != 4)
			{
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid number of parameters passed");
				return 0;
			}
			cell *ptr = MF_GetAmxAddr(amx, params[4]);
			return MF_SetAmxString(amx, params[3], kvd->szValue, (int)*ptr);
			break;
		}
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid KeyValueData member: %d", params[2]);

	return 0;
}

static cell AMX_NATIVE_CALL set_kvd(AMX *amx, cell *params)
{
	KVD_Wrapper *kvdw;
	KeyValueData *kvd;
	if (params[1] == 0)
		kvdw = &g_kvd_glb;
	else
		kvdw = reinterpret_cast<KVD_Wrapper *>(params[1]);
	kvd = kvdw->kvd;

	if (*params / sizeof(cell) < 3)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	cell *ptr = MF_GetAmxAddr(amx, params[3]);
	int len;

	switch (params[2])
	{
	case KV_fHandled:
		{
			kvd->fHandled = (int)*ptr;
			return 1;
			break;
		}
	case KV_ClassName:
		{
			kvdw->cls.assign(MF_GetAmxString(amx, params[1], 0, &len));
			kvd->szClassName = const_cast<char *>(kvdw->cls.c_str());
			return 1;
			break;
		}
	case KV_KeyName:
		{
			kvdw->key.assign(MF_GetAmxString(amx, params[1], 0, &len));
			kvd->szKeyName = const_cast<char *>(kvdw->key.c_str());
			return 1;
			break;
		}
	case KV_Value:
		{
			kvdw->val.assign(MF_GetAmxString(amx, params[1], 0, &len));
			kvd->szValue = const_cast<char *>(kvdw->val.c_str());
			return 1;
			break;
		}
	}

	MF_LogError(amx, AMX_ERR_NATIVE, "Invalid KeyValueData member: %d", params[2]);

	return 0;
}

AMX_NATIVE_INFO ext2_natives[] = 
{
	{"get_tr2",			get_tr2},
	{"set_tr2",			set_tr2},
	{"get_kvd",			get_kvd},
	{"set_kvd",			set_kvd},
	{NULL,				NULL},
};

