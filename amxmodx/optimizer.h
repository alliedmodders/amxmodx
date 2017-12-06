// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#ifndef _INCLUDE_AMXMODX_OPTIMIZER_H
#define _INCLUDE_AMXMODX_OPTIMIZER_H

#include "amx.h"
#include "cpuinfo.h"

#define OPT_FLOAT1_JIT		(1<<0)
#define OPT_FLOAT2_JIT		(1<<1)
#define OPT_VECTOR_JIT		(1<<2)
#define OPT_DISABLE			65536 // (1<<16)

enum
{
	N_Float_Mul = 0,
	N_Float_Div,
	N_Float_Add,
	N_Float_Sub,
	N_Float_To,
	N_Float_Round,
	N_Float_Cmp,
	// N_Vector, // optimizer xs_vec...
	/* ------------ */
	N_Total_FloatOps,
};

struct optimizer_s
{
	int natives[N_Total_FloatOps];
};

extern CPUInfo* g_cpuInfo;
extern int g_optimizerFlags;


void SetupOptimizer(AMX *amx);
void CheckOptimizerCPU();


#endif //_INCLUDE_AMXMODX_OPTIMIZER_H
