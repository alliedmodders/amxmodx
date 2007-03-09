
/* Ham Sandwich
 *
 * by sawce
 *
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of this program with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

#include "sdk/amxxmodule.h"

#include "hamsandwich.h"
#include "NEW_Util.h"

#include "vfunc_msvc.h"
#include "vfunc_gcc295.h"

static cell AMX_NATIVE_CALL hs_takedamage(AMX *amx, cell *params)
{
	return VCall4<int>(
		INDEXENT_NEW(params[1])->pvPrivateData, /*this*/
		HAM_Takedamage,							/*vtable entry*/
		HAM_Classbase,							/*size of class*/
		&(INDEXENT_NEW(params[2])->v),			/*inflictor*/
		&(INDEXENT_NEW(params[3])->v),			/*attacker*/
		amx_ctof2(params[4]),					/*damage*/
		(int)params[5]);						/*dmgtype*/
};
static cell AMX_NATIVE_CALL hs_use(AMX *amx, cell *params)
{
	VoidVCall4(
		INDEXENT_NEW(params[1])->pvPrivateData,	/*this*/
		HAM_Use,								/*vtable entry*/
		HAM_Classbase,							/*size of class*/
		INDEXENT_NEW(params[2])->pvPrivateData,	/*activator*/
		INDEXENT_NEW(params[3])->pvPrivateData,	/*caller*/
		params[4],								/*use type*/
		amx_ctof2(params[5]));					/*value*/

	return 1;
}

/**
 * Very fast, but potentially unsafe (if you use the wrong index/offset)
 * method of converting a CBaseEntity pointer into a usable entity index
 */
static cell AMX_NATIVE_CALL hs_pdata_cbase(AMX *amx, cell *params)
{
	// Get the offset of the private data
	int Offset=params[2];

	// If this is a linux server increase the offset
#ifdef __linux__
	Offset+=params[3];
#endif

	// Get the CBase pointer
	int *ent=*((int **)INDEXENT_NEW(params[1])->pvPrivateData + Offset);

	// Null pointer; get out
	if (ent==NULL)
	{
		return 0;
	}

	// Now move up HAM_Pev bytes
	char *bent=*(char**)&ent;

	bent+=HAM_Pev;

	entvars_t *pev=*(entvars_t **)&bent;

	// Null pointer, get out
	if (pev==NULL)
	{
		return 0;
	}

	return ENTINDEX_NEW(pev->pContainingEntity);


}
/**
 * Slow, but very safe replacement for hs_pdata_cbase
 * -
 * This will scan through all entities to check their private 
 * data against the requested offset's data.
 * It will never reference the requested PData, so unless
 * the plugin author is way off with the offset it should
 * never crash.
 * -
 * This should only be used for offset searching; NEVER
 * in a release quality script.
 */
static cell AMX_NATIVE_CALL hs_pdata_cbase_safe(AMX *amx, cell *params)
{
	// Get the offset of the private data
	int Offset=params[2];

	// If this is a linux server increase the offset
#ifdef __linux__
	Offset+=params[3];
#endif

	// Get the CBase pointer
	int *data=*((int **)INDEXENT_NEW(params[1])->pvPrivateData + Offset);

	// Get the first entity
	edict_t *Entity=INDEXENT_NEW(0);

	// Get the last entity
	edict_t *Last=INDEXENT_NEW(gpGlobals->maxEntities);

	// Scan through all of the entities (excluding 0, because no other module allows for worldspawn)
	while (Entity++<Last)
	{
		// If this entity's private data matches the CBase pointer requested, return
		if (((int *)Entity->pvPrivateData)==data)
		{
			return ENTINDEX_NEW(Entity);
		}
	}

	// Not found
	return 0;
}

static AMX_NATIVE_INFO reg_takedamage[] = {
	{ "hs_takedamage",				hs_takedamage },
	{ NULL,							NULL }
};
static AMX_NATIVE_INFO reg_use[] = {
	{ "hs_use",						hs_use },

	{ NULL,							NULL }
};
static AMX_NATIVE_INFO reg_cbase_fast[] = {
	{ "hs_pdata_cbase",				hs_pdata_cbase },

	{ NULL,							NULL }
};
static AMX_NATIVE_INFO reg_cbase_safe[] = {
	{ "hs_pdata_cbase_safe",		hs_pdata_cbase_safe },

	{ NULL,							NULL }
};
void HAM_RegisterTakeDamage()
{
	MF_AddNatives(reg_takedamage);
}
void HAM_RegisterUse()
{
	MF_AddNatives(reg_use);
}
void HAM_RegisterCbaseFast()
{
	MF_AddNatives(reg_cbase_fast);
}
void HAM_RegisterCbaseSafe()
{
	MF_AddNatives(reg_cbase_safe);
}

