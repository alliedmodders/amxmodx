#ifndef _INCLUDE_TR_H
#define _INCLUDE_TR_H

#include "CString.h"

extern TraceResult *gfm_tr;

//these also don't fit in here but gaben does not care.  GABEN DOES NOT CARE!!!
extern TraceResult g_tr_2;
extern KeyValueData g_kvd_2;

struct KVD_Wrapper
{
	KeyValueData *kvd;
	String cls;
	String key;
	String val;
};

extern KVD_Wrapper g_kvd_glb;
extern KVD_Wrapper g_kvd_hook;

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
extern AMX_NATIVE_INFO ext2_natives[];

#endif //_INCLUDE_TR_H

