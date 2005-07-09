#include "esforces.h"

void OnAmxxAttach()
{
	MF_AddNatives(g_AnimationNatives);
	MF_AddNatives(g_PdataNatives);
	MF_AddNatives(g_EffectsNatives);
	MF_AddNatives(g_BaseNatives);
}
