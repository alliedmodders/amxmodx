#include "fakemeta_amxx.h"

void OnAmxxAttach()
{
	MF_AddNatives(engfunc_natives);
	MF_AddNatives(dllfunc_natives);
}