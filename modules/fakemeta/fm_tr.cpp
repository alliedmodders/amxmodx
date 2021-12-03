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

TraceResult *gfm_tr;
KeyValueData *g_fm_keyValueData;

static cell AMX_NATIVE_CALL set_tr(AMX *amx, cell *params)
{
	int type = params[1];

	if (*params / sizeof(cell) < 2)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "No data passed");
		return 0;
	}

	cell *ptr = MF_GetAmxAddr(amx, params[2]);

	switch (type)
	{
	case TR_AllSolid:
		{
			gfm_tr->fAllSolid = *ptr;
			return 1;
		}
	case TR_StartSolid:
		{
			gfm_tr->fStartSolid = *ptr;
			return 1;
		}
	case TR_InOpen:
		{
			gfm_tr->fInOpen = *ptr;
			return 1;
		}
	case TR_InWater:
		{
			gfm_tr->fInWater = *ptr;
			return 1;
		}
	case TR_flFraction:
		{
			gfm_tr->flFraction = amx_ctof(*ptr);
			return 1;
		}
	case TR_vecEndPos:
		{
			gfm_tr->vecEndPos.x = amx_ctof(ptr[0]);
			gfm_tr->vecEndPos.y = amx_ctof(ptr[1]);
			gfm_tr->vecEndPos.z = amx_ctof(ptr[2]);
			return 1;
		}
	case TR_flPlaneDist:
		{
			gfm_tr->flPlaneDist = amx_ctof(*ptr);
			return 1;
		}
	case TR_vecPlaneNormal:
		{
			gfm_tr->vecPlaneNormal.x = amx_ctof(ptr[0]);
			gfm_tr->vecPlaneNormal.y = amx_ctof(ptr[1]);
			gfm_tr->vecPlaneNormal.z = amx_ctof(ptr[2]);
			return 1;
		}
	case TR_pHit:
		{
			const auto pEdict = TypeConversion.id_to_edict(*ptr);
			if (pEdict == nullptr)
			{
				return 0;
			}
			gfm_tr->pHit = pEdict;
			return 1;
		}
	case TR_iHitgroup:
		{
			gfm_tr->iHitgroup = *ptr;
			return 1;
		}
	}
	
	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown TraceResult member %d", params[2]);
	
	return 0;
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
		}
	case TR_StartSolid:
		{
			return gfm_tr->fStartSolid;
		}
	case TR_InOpen:
		{
			return gfm_tr->fInOpen;
		}
	case TR_InWater:
		{
			return gfm_tr->fInWater;
		}
	case TR_flFraction:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			*ptr = amx_ftoc(gfm_tr->flFraction);
			return 1;
		}
	case TR_vecEndPos:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			ptr[0] = amx_ftoc(gfm_tr->vecEndPos.x);
			ptr[1] = amx_ftoc(gfm_tr->vecEndPos.y);
			ptr[2] = amx_ftoc(gfm_tr->vecEndPos.z);
			return 1;
		}
	case TR_flPlaneDist:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			*ptr = amx_ftoc(gfm_tr->flPlaneDist);
			return 1;
		}
	case TR_vecPlaneNormal:
		{
			ptr = MF_GetAmxAddr(amx, params[2]);
			ptr[0] = amx_ftoc(gfm_tr->vecPlaneNormal.x);
			ptr[1] = amx_ftoc(gfm_tr->vecPlaneNormal.y);
			ptr[2] = amx_ftoc(gfm_tr->vecPlaneNormal.z);
			return 1;
		}
	case TR_pHit:
		{
			if (FNullEnt(gfm_tr->pHit))
				return -1;
			return ENTINDEX(gfm_tr->pHit);
		}
	case TR_iHitgroup:
		{
			return gfm_tr->iHitgroup;
		}
	}
	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown TraceResult member %d", params[2]);
	
	return 0;
}

AMX_NATIVE_INFO tr_Natives[] = 
{
	{"get_tr",			get_tr},
	{"set_tr",			set_tr},
	{NULL,				NULL},
};



