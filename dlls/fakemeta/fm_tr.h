#ifndef _INCLUDE_TR_H
#define _INCLUDE_TR_H

extern TraceResult *gfm_tr;
extern KeyValueData *g_fm_keyValueData; // JGHG: Yeah yeah I know this doesn't fit in here. Then again, neither does gaben.
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

enum KeyValue
{
	KV_ClassName,
    KV_KeyName,
	KV_Value,
	KV_fHandled
};

extern AMX_NATIVE_INFO tr_Natives[];

#endif //_INCLUDE_TR_H

