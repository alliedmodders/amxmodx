#ifndef _INCLUDE_TR_H
#define _INCLUDE_TR_H

extern TraceResult *gfm_tr;

enum
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
};

extern AMX_NATIVE_INFO tr_Natives[];

#endif //_INCLUDE_TR_H