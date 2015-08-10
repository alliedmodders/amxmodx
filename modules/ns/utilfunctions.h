// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

#ifndef UTILFUNCTIONS_H
#define UTILFUNCTIONS_H

#include "NEW_Util.h" // include my experimental INDEXENT / ENTINDEX replacements



#define GET_PLAYER_E(x) (&g_player[ENTINDEX_NEW(x)])
#define GET_PLAYER_I(x) (&g_player[x])


/**
 * Like GET_PLAYER_I, but this handles the invalid player index
 * as well as setting the CPlayer pointer.
 * NOTE: This declares CPlayer *player for you
 */
#define CreatePlayerPointer(AMX__, INDEX__)					\
	if (INDEX__ < 1 || INDEX__ > gpGlobals->maxClients)		\
	{														\
		return 0;											\
	}														\
	CPlayer *player = &g_player[INDEX__]

/**
 * Declares an edict (Entity) and verify it's not a player
 */
#define CreateNonPlayerEdict(AMX__, INDEX__)									\
	if (INDEX__ <= gpGlobals->maxClients || INDEX__ > gpGlobals->maxEntities)	\
	{																			\
		return 0;																\
	}																			\
	edict_t *Entity=INDEXENT_NEW(INDEX__)

/**
 * Declares an edict (Entity) does not care if it is a player
 */
#define CreateEdict(AMX__, INDEX__, INVALIDRET__)								\
	if (INDEX__ < 1 || INDEX__ > gpGlobals->maxEntities)						\
	{																			\
		return INVALIDRET__;													\
	}																			\
	edict_t *Entity=INDEXENT_NEW(INDEX__)


/**
 * There is no need to use the AMXx provided calls as
 * they do an api call to recast, seems like overkill
 */
//#define amx_ftoc2(x) *((cell *)&x)
//#define amx_ctof2(x) *((REAL *)&x)
#define amx_ftoc3(x) *((cell *)x)
#define amx_ctof3(x) *((REAL *)x)
inline cell amx_ftoc2(REAL x)
{
	return *((cell *)&x);
};
inline REAL amx_ctof2(cell x)
{
	return *((REAL *)&x);
};



inline BOOL isValidEntity(int x)
{
	if (x < 0)
		return FALSE;
	if (x >= 0 || x <= gpGlobals->maxClients)
		return TRUE;
	if (x > gpGlobals->maxEntities)
		return FALSE;
	if (FNullEnt(x))
		return FALSE;
	return TRUE;
}
#define CHECK_ENTITY(x) if (x != 0 && (FNullEnt(INDEXENT_NEW(x)) || x < 0 || x > gpGlobals->maxEntities)) return 0;
#define CHECK_PARAMS(x) if (*params/sizeof(cell) < x) return 0;


// Stuff in utils.cpp

edict_t *UTIL_FindEntityByString(edict_t *Start, const char *Keyword, const char *Value);

int UTIL_FindBuildingHive(void);

BOOL UTIL_CheckForPublic(const char *publicname);
BOOL UTIL_CheckForNative(const char *NativeName);

char *UTIL_ToLowerCase(const char *str);

class CPlayer;

CPlayer *UTIL_PlayerByCID(int CID);

// Converts something such as RESOURCES into OFFSET_WIN_RESOURCES or OFFSET_LIN_RESOURCES
#if defined(__linux__) || defined(__APPLE__)
#define MAKE_OFFSET(Offset) OFFSET_LIN_##Offset
#define MAKE_MEMBER_OFFSET(Offs) (Offs - OFFSET_LIN_MEMBERFUNCSTART)
#else
#define MAKE_OFFSET(Offset) OFFSET_WIN_##Offset
#define MAKE_MEMBER_OFFSET(Offs) (Offs - OFFSET_WIN_MEMBERFUNCSTART)
#endif // __linux__

template <typename Output>
inline Output get_private_p(edict_t *pEntity, int offset)
{
	return (Output)(*(void**)((char*)(pEntity->pvPrivateData)+offset));
};

/**
 * Converts the private data pointer into an edict by
 * looking up the entvar pointer (first entry in cbaseentity)
 * and then getting pContainingEntity from that
 */
inline edict_t *private_to_edict(void *PrivateData)
{
	if (PrivateData==NULL)
	{
		return NULL;
	}
	return (*((entvars_t **)((char *)PrivateData + MAKE_OFFSET(ENTVAR))))->pContainingEntity;
};
inline int clamp(int value, int min, int max)
{
	if (value < min)
	{
		return min;
	}
	if (value > max)
	{
		return max;
	}
	
	return value;
}
inline REAL clamp(REAL value, REAL min, REAL max)
{
	if (value < min)
	{
		return min;
	}
	if (value > max)
	{
		return max;
	}
	
	return value;
}
inline int clamp(int value, int min)
{
	if (value < min)
	{
		return min;
	}
	
	return value;
}

inline REAL clamp(REAL value, REAL min)
{
	if (value < min)
	{
		return min;
	}
	
	return value;
}

inline int get_private(edict_t *pEntity, int offset)
{
	return *(int*)((char*)(pEntity->pvPrivateData)+offset);
}

inline REAL get_private_f(edict_t *pEntity, int offset)
{
	return *(REAL*)((char*)(pEntity->pvPrivateData)+offset);
}

inline void set_private(edict_t *pEntity, int offset, int value)
{
	*(int*)((char*)(pEntity->pvPrivateData)+offset) = value;
}
inline int inc_private(edict_t *pEntity, int offset, int value)
{
	return *(int*)((char*)(pEntity->pvPrivateData)+offset) = *(int*)((char*)(pEntity->pvPrivateData)+offset) + value;
}
inline int inc_private(edict_t *pEntity, int offset, int value, int min)
{
	return *(int*)((char*)(pEntity->pvPrivateData)+offset) = clamp(*(int*)((char*)(pEntity->pvPrivateData)+offset) + value, min);
}
inline int inc_private(edict_t *pEntity, int offset, int value, int min, int max)
{
	return *(int*)((char*)(pEntity->pvPrivateData)+offset) = clamp(*(int*)((char*)(pEntity->pvPrivateData)+offset) + value, min, max);
}

inline void set_private_f(edict_t *pEntity, int offset, REAL value)
{
	*(REAL*)((char*)(pEntity->pvPrivateData)+offset) = value;
}
inline REAL inc_private_f(edict_t *pEntity, int offset, REAL value)
{
	return *(REAL*)((char*)(pEntity->pvPrivateData)+offset) = *(REAL*)((char*)(pEntity->pvPrivateData)+offset) + value;
}
inline REAL inc_private_f(edict_t *pEntity, int offset, REAL value, REAL min)
{
	return *(REAL*)((char*)(pEntity->pvPrivateData)+offset) = clamp(*(REAL*)((char*)(pEntity->pvPrivateData)+offset) + value, min);
}
inline REAL inc_private_f(edict_t *pEntity, int offset, REAL value, REAL min, REAL max)
{
	return *(REAL*)((char*)(pEntity->pvPrivateData)+offset) = clamp(*(REAL*)((char*)(pEntity->pvPrivateData)+offset) + value, min, max);
}

inline unsigned char get_private_b(edict_t *pEntity, int offset)
{
	return *(((unsigned char*)pEntity->pvPrivateData)+offset);
};
inline unsigned char set_private_b(edict_t *pEntity, int offset, unsigned char value)
{
	return *(((unsigned char*)pEntity->pvPrivateData)+offset)=value;
};


/**
* sizeof(void (detail::GenericClass::*fptr)())
* is 8 in GCC.  Add an empty void * pointer at
* the end to compensate.
* Layout in GCC:
* union {
*   void *address;      // When this is an address it will always be positive
*   int   vtable_index; // When it is a vtable index it will always be odd = (vindex*2)+1
* };
* int delta;
* -
* Delta is the adjustment to the this pointer
* For my implementations I will only need it to 0
*/
#ifdef __GNUC__
template <typename OutType>
inline void set_mfp(OutType &out, void *in)
{
	union
	{
		void    *in[2];
		OutType out;
	} mfpu;

	mfpu.in[0] = in;
	mfpu.in[1] = NULL;
	out = mfpu.out;
};
#else
template <typename OutType>
inline void set_mfp(OutType &out, void *in)
{
	out = horrible_cast<OutType>(in);
};
#endif

#define MFP(Offs) (((void *)(((char *)base)+MAKE_OFFSET(Offs))))

#endif // UTILFUNCTIONS_H

