#ifndef HAM_UTILS_H
#define HAM_UTILS_H

#include "sdk/amxxmodule.h"
#include "offsets.h"
#include "NEW_Util.h"

#define CHECK_FUNCTION(x)																			\
	if (x < 0 || x > HAM_LAST_ENTRY_DONT_USE_ME_LOL) {												\
		FailPlugin(amx, x, HAM_INVALID_FUNC, "Function out of bounds.");							\
		return 0;																					\
	} else if (hooklist[x].isset == 0) {															\
		FailPlugin(amx, x, HAM_FUNC_NOT_CONFIGURED, "Function not configured in hamdata.ini");		\
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

void print_srvconsole(char *fmt, ...);
#endif
