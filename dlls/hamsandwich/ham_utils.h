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
#include "NEW_Util.h"

#define CHECK_FUNCTION(x)																			\
	if (x < 0 || x >= HAM_LAST_ENTRY_DONT_USE_ME_LOL) {												\
		char msg[1024];																				\
		UTIL_Format(msg, sizeof(msg)-1, "Function out of bounds.  Got: %d  Max: %d", x, HAM_LAST_ENTRY_DONT_USE_ME_LOL - 1);	\
		FailPlugin(amx, x, HAM_INVALID_FUNC, msg);							\
		return 0;																					\
	} else if (hooklist[x].isset == 0) {															\
		char msg[1024];																				\
		UTIL_Format(msg, sizeof(msg)-1, "Function %s is not configured in hamdata.ini.", hooklist[x].name);	\
		FailPlugin(amx, x, HAM_FUNC_NOT_CONFIGURED, msg);											\
		return 0;																					\
	}


#define CHECK_ENTITY(x)																	\
	if (x < 0 || x > gpGlobals->maxEntities) {											\
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x);				\
		return 0;																		\
	} else {																			\
		if (INDEXENT_NEW(x)->free) {													\
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity (%d)", x);					\
			return 0;																	\
		} else if (INDEXENT_NEW(x)->pvPrivateData == NULL) {							\
			MF_LogError(amx, AMX_ERR_NATIVE, "Entity has null private data (%d)", x);	\
			return 0;																	\
		}																				\
	}

inline edict_t *PrivateToEdict(const void *pdata)
{

	if (!pdata)
	{
		return NULL;
	}

	char *ptr=(char*)pdata + Offsets.GetPev();
	entvars_t *pev=(entvars_t *)ptr;

	if (!pev)
	{
		return NULL;
	}
	return pev->pContainingEntity;
};

inline int PrivateToIndex(const void *pdata)
{

	if (pdata==NULL)
	{
		return -1;
	}
	char *ptr=(char*)pdata;

	ptr+=Offsets.GetPev();

	entvars_t *pev=*(entvars_t **)ptr;


	if (pev==NULL)
	{
		return -1;
	}
	
	if (pev->pContainingEntity==NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(pev->pContainingEntity);
};
inline void *IndexToPrivate(int index)
{
	return INDEXENT_NEW(index)->pvPrivateData;
};
inline entvars_t *IndexToEntvar(int index)
{
	return &(INDEXENT_NEW(index)->v);
};

inline int EntvarToIndex(entvars_t *pev)
{
	if (pev==NULL)
	{
		return -1;
	}
	
	if (pev->pContainingEntity==NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(pev->pContainingEntity);
};

inline int EdictToIndex(edict_t *v)
{
	if (v==NULL)
	{
		return -1;
	}

	return ENTINDEX_NEW(v);
}

inline edict_t *IndexToEdict(int index)
{
	return INDEXENT_NEW(index);
};

inline edict_t *EntvarToEdict(entvars_t *pev)
{
	if (pev==NULL)
	{
		return NULL;
	}
	
	return pev->pContainingEntity;
};
inline void **EdictToVTable(edict_t *ent)
{
	char *btbl=(char *)ent->pvPrivateData;
	btbl+=Offsets.GetBase();
	return *((void ***)btbl);
};

inline void **GetVTable(void *pthis, int size)
{
	return *((void***)(((char*)pthis)+size));
}
inline void *GetVTableEntry(void *pthis, int ventry, int size)
{
	void **vtbl=GetVTable(pthis, size);

	return vtbl[ventry];
}

void print_srvconsole(const char *fmt, ...);
#endif
