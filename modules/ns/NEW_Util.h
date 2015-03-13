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

/* Inlined replacements for INDEXENT/ENTINDEX
 * It only really removes the overhead of the push/jump
 * but since INDEXENT/ENTINDEX are used a lot with amxx
 * it might be beneficial to include.
 * NOTE: Bad stuff will happen if you call these before
 *       NEW_Initialize()
 * NOTE: No bounds checking is done because natives
 *       should use their own bounds checking!
 */

#ifndef NEW_UTIL_H
#define NEW_UTIL_H


extern edict_t *NEW_FirstEdict;
extern bool NEW_Initialized;

/**
 * This is called on the first Spawn() ever hooked.  This would be worldspawn (index 0)
 */
inline void NEW_Initialize(edict_t *Entity)
{
	// call the HL Engine ENTINDEX to make sure this is index 0

	int index=ENTINDEX(Entity);

	if (index==0)
	{
		NEW_FirstEdict=Entity;
		NEW_Initialized=true;
		return;
	}

	// This is not worldspawn?
	// compensate
	NEW_FirstEdict=Entity - index;
	NEW_Initialized=true;
}


/**
 * Converts an integer index into an edict pointer
 */
inline edict_t *INDEXENT_NEW(const int Index)
{
	return (edict_t *)(NEW_FirstEdict + Index);
};

/**
 * Converts an edict pointer into an integer index
 */
inline int ENTINDEX_NEW(const edict_t *Ent)
{
	return (int)(Ent - NEW_FirstEdict);
};

// Inlined replacement of MF_GetAmxAddr


// straight from amxmodx's string.cpp; no need for this to be an api call
inline cell *MF_GetAmxAddr_NEW(AMX *amx, cell amx_addr)
{
	return (cell *)(amx->base + (int)(((AMX_HEADER *)amx->base)->dat + amx_addr));
};


#endif // NEW_UTIL_H
