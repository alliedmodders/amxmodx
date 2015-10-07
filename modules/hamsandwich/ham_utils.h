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

#ifndef HAM_UTILS_H
#define HAM_UTILS_H

#include "amxxmodule.h"
#include "offsets.h"
#include <HLTypeConversion.h>

extern HLTypeConversion TypeConversion;

#define CHECK_FUNCTION(x)																			\
	if (x < 0 || x >= HAM_LAST_ENTRY_DONT_USE_ME_LOL) {												\
		char msg[1024];																				\
		ke::SafeSprintf(msg, sizeof(msg), "Function out of bounds.  Got: %d  Max: %d", x, HAM_LAST_ENTRY_DONT_USE_ME_LOL - 1);	\
		FailPlugin(amx, x, HAM_INVALID_FUNC, msg);							\
		return 0;																					\
	} else if (hooklist[x].isset == 0) {															\
		char msg[1024];																				\
		ke::SafeSprintf(msg, sizeof(msg), "Function %s is not configured in hamdata.ini.", hooklist[x].name);	\
		FailPlugin(amx, x, HAM_FUNC_NOT_CONFIGURED, msg);											\
		return 0;																					\
	}


#define CHECK_ENTITY(x)																	\
	if (x < 0 || x > gpGlobals->maxEntities) {											\
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x);				\
		return 0;																		\
	} else {																			\
		if (TypeConversion.id_to_edict(x)->free) {										\
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity (%d)", x);					\
			return 0;																	\
		} else if (TypeConversion.id_to_edict(x)->pvPrivateData == NULL) {				\
			MF_LogError(amx, AMX_ERR_NATIVE, "Entity has null private data (%d)", x);	\
			return 0;																	\
		}																				\
	}

inline void **EdictToVTable(edict_t *ent)
{
	char *btbl = (char *)ent->pvPrivateData;
	btbl += Offsets.GetBase();
	return *((void ***)btbl);
};

inline void **GetVTable(void *pthis, int size)
{
	return *((void***)(((char*)pthis) + size));
}
inline void *GetVTableEntry(void *pthis, int ventry, int size)
{
	void **vtbl = GetVTable(pthis, size);

	return vtbl[ventry];
}

#endif
