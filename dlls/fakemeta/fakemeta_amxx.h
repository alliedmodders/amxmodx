#ifndef _FAKEMETA_INCLUDE_H
#define _FAKEMETA_INCLUDE_H

#include "sdk/amxxmodule.h"
#include "CVector.h"
#include "engfunc.h"
#include "dllfunc.h"
#include "pev.h"
#include "forward.h"

#define CHECK_ENTITY(x) if (x != 0 && (FNullEnt(INDEXENT(x)) || x < 0 || x > gpGlobals->maxEntities)) { MF_RaiseAmxError(amx,AMX_ERR_NATIVE); return 0; }

extern AMX_NATIVE_INFO engfunc_natives[];
extern AMX_NATIVE_INFO dllfunc_natives[];
extern AMX_NATIVE_INFO pev_natives[];
extern TraceResult g_tr;

extern enginefuncs_t g_EngineFuncs_Table;
extern enginefuncs_t g_EngineFuncs_Post_Table;

#endif //_FAKEMETA_INCLUDE_H