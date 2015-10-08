// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Module
//

#include "amxxmodule.h"
#include "offsets.h"
#include "ham_utils.h"

#ifdef DONT_TOUCH_THIS_AGAIN_BAIL
#define FM_CHECK_ENTITY(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if (x != 0 && FNullEnt(INDEXENT(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}
#endif

#define FM_CHECK_ENTITY(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else if (x != 0 && FNullEnt(TypeConversion.id_to_edict(x))) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
		return 0; \
	}

// Return -1 on null, -2 on invalid, and the the index of any other.
static cell AMX_NATIVE_CALL get_pdata_cbase_safe(AMX *amx, cell *params)
{
	int index=params[1];
	FM_CHECK_ENTITY(index);
	int iOffset=params[2];
#ifdef __linux__
	iOffset += params[3];
#elif defined __APPLE__
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		iOffset += params[3];
	else
		iOffset += params[4];
#endif
	if (iOffset <0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset provided. (got: %d)", iOffset);
		return 0;
	}
	void *ptr = get_pdata<void*>(TypeConversion.id_to_edict(index), iOffset * 4); // *4 because macro is char-based and native is int-based.

	if (!ptr)
	{
		return -1;
	}

	for (int i=0; i<gpGlobals->maxEntities; ++i)
	{
		if (ptr == TypeConversion.id_to_cbase(i))
		{
			return i;
		}
	}

	return -2;
}

static cell AMX_NATIVE_CALL get_pdata_cbase(AMX *amx, cell *params)
{
	int index=params[1];
	FM_CHECK_ENTITY(index);
	int iOffset=params[2];
#ifdef __linux__
	iOffset += params[3];
#elif defined __APPLE__
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 3)
		iOffset += params[3];
	else
		iOffset += params[4];
#endif

	if (iOffset <0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset provided. (got: %d)", iOffset);
		return 0;
	}
	void *ptr = get_pdata<void*>(TypeConversion.id_to_edict(index), iOffset * 4);

	return TypeConversion.cbase_to_id(ptr);
}

static cell AMX_NATIVE_CALL set_pdata_cbase(AMX *amx, cell *params)
{
	int index=params[1];
	FM_CHECK_ENTITY(index);
	int target=params[3];

	if (target != -1)
	{
		FM_CHECK_ENTITY(target);
	}
	int iOffset=params[2];
#ifdef __linux__
	iOffset += params[4];
#elif defined __APPLE__
	// Use Linux offset in older plugins
	if (params[0] / sizeof(cell) == 4)
		iOffset += params[4];
	else
		iOffset += params[5];
#endif
	if (iOffset <0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset provided. (got: %d)", iOffset);
		return 0;
	}

	if (target == -1)
	{
		set_pdata<void*>(TypeConversion.id_to_edict(index), iOffset * 4, nullptr);
	}
	else
	{
		set_pdata<void*>(TypeConversion.id_to_edict(index), iOffset * 4, TypeConversion.id_to_cbase(target));
	}

	return 1;
}

AMX_NATIVE_INFO pdata_natives_safe[] =
{
	{ "get_pdata_cbase_safe",	get_pdata_cbase_safe },

	{ NULL,						NULL }
};
AMX_NATIVE_INFO pdata_natives[] =
{
	{ "get_pdata_cbase",		get_pdata_cbase },
	{ "set_pdata_cbase",		set_pdata_cbase },

	{ NULL,						NULL }
};
