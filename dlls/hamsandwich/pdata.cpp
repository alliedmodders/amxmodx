#include "sdk/amxxmodule.h"
#include "offsets.h"
#include "NEW_Util.h"
#include "ham_utils.h"

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

// Return -1 on null, -2 on invalid, and the the index of any other.
static cell AMX_NATIVE_CALL get_pdata_cbase_safe(AMX *amx, cell *params)
{
	int index=params[1];
	FM_CHECK_ENTITY(index);
	int iOffset=params[2];
#ifdef __linux__
	iOffset += params[3];
#endif
	if (iOffset <0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset provided. (got: %d)", iOffset);
		return 0;
	}
	void *ptr=*((void **)((int *)INDEXENT_NEW(index)->pvPrivateData + iOffset));

	if (ptr == 0)
	{
		return -1;
	}

	for (int i=0; i<gpGlobals->maxEntities; ++i)
	{
		if (ptr == INDEXENT_NEW(i)->pvPrivateData)
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
#endif
	if (iOffset <0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset provided. (got: %d)", iOffset);
		return 0;
	}
	void *ptr=*((void **)((int *)INDEXENT_NEW(index)->pvPrivateData + iOffset));

	return PrivateToIndex(ptr);
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
#endif
	if (iOffset <0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid offset provided. (got: %d)", iOffset);
		return 0;
	}

	if (target == -1)
	{
		*((void **)((int *)INDEXENT_NEW(index)->pvPrivateData + iOffset)) = NULL;
	}
	else
	{
		*((void **)((int *)INDEXENT_NEW(index)->pvPrivateData + iOffset)) = INDEXENT_NEW(target)->pvPrivateData;
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
