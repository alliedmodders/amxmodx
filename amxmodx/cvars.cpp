// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "cvars.h"
#include "amxmodx.h"
#include <CDetour/detours.h>

CDetour *Cvar_DirectSetDetour;

DETOUR_DECL_STATIC2(Cvar_DirectSet, void, struct cvar_s*, var, const char*, value)
{
	printf("Cvar_DirectSet - %s -> %s\n", var->name, value);

	DETOUR_STATIC_CALL(Cvar_DirectSet)(var, value);
}

void CreateCvarHook(void)
{
	// void PF_Cvar_DirectSet(struct cvar_s *var, const char *value) // = pfnCvar_DirectSet
	// {
	//   	Cvar_DirectSet(var, value); // <- We want to hook this.
	// }

	byte *baseAddress = (byte *)g_engfuncs.pfnCvar_DirectSet;
	uintptr_t *functionAddress = nullptr;

#if defined(WIN32)
	// 55              push    ebp
	// 8B EC           mov     ebp, esp
	// 8B 45 0C        mov     eax, [ebp+arg_4]
	// 8B 4D 08        mov     ecx, [ebp+arg_0]
	// 50              push    eax
	// 51              push    ecx
	// E8 XX XX XX XX  call    Cvar_DirectSet
	const byte opcodeJump = 0xE8;
#else
	// E9 XX XX XX XX  jmp     Cvar_DirectSet
	const byte opcodeJump = 0xE9;
#endif

	const byte opcodeJumpSize     = 5;
	const byte opcodeJumpByteSize = 1;

	const int maxBytesLimit = 20;

	for (size_t i = 0; i < maxBytesLimit; ++i, ++baseAddress)
	{
		if (*baseAddress == opcodeJump)
		{
			functionAddress = (uintptr_t *)(&baseAddress[opcodeJumpSize] + *(uintptr_t *)&baseAddress[opcodeJumpByteSize]);
			break;
		}
	}

	if (functionAddress)
	{
		CDetour *Cvar_DirectSetDetour = DETOUR_CREATE_STATIC_FIXED(Cvar_DirectSet, (void *)functionAddress);

		if (Cvar_DirectSetDetour)
		{
			Cvar_DirectSetDetour->EnableDetour();
		}
	}
}