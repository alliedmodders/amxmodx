#ifndef _INCLUDE_AMXMODX_OPTIMIZER_H
#define _INCLUDE_AMXMODX_OPTIMIZER_H

#include "amx.h"

enum
{
	N_Float_Mul=0,
	N_Float_Div,
	N_Float_Add,
	N_Float_Sub,
	N_Float_To,
	N_Float_Round,
	N_Float_Cmp,
	/* ------------ */
	N_Total_FloatOps,
};

struct optimizer_s
{
	int natives[N_Total_FloatOps];
};

void SetupOptimizer(AMX *amx);

#endif //_INCLUDE_AMXMODX_OPTIMIZER_H
