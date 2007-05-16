/* Ham Sandwich
 *   Copyright 2007
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
	NEW_FirstEdict=Entity;
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


inline REAL amx_ctof2(cell x)
{
	return *(REAL*)&x;
}
inline cell amx_ftoc2(REAL x)
{
	return *(cell*)&x;
}


#endif // NEW_UTIL_H

