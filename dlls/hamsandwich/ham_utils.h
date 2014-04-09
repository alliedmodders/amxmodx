/* Ham Sandwich
 *   Copyright 2007-2014
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

#ifndef HAM_UTILS_H
#define HAM_UTILS_H

#include "amxxmodule.h"
#include "offsets.h"
#include "NEW_Util.h"

#define CHECK_FUNCTION(x)																			\
	if (x < 0 || x >= HAM_LAST_ENTRY_DONT_USE_ME_LOL) {												\
		char msg[1024];																				\
		snprintf(msg, sizeof(msg)-1, "Function out of bounds.  Got: %d  Max: %d",x, HAM_LAST_ENTRY_DONT_USE_ME_LOL - 1);	\
		FailPlugin(amx, x, HAM_INVALID_FUNC, msg);							\
		return 0;																					\
	} else if (hooklist[x].isset == 0) {															\
		char msg[1024];																				\
		snprintf(msg, sizeof(msg)-1, "Function %s is not configured in hamdata.ini.",hooklist[x].name);	\
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
