/* VexdUM backwards compatibility
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */


#if !defined _fakemeta_included
	#include <fakemeta>
#endif

#if !defined _engine_included
	#include <engine>
#endif

#if defined _vexd_bcompat_included
  #endinput
#endif
#define _vexd_bcompat_included

#include <VexdUM_const>

native radius_damage(inflictor, Float:dmg, Float:orig[3], Float:rad, bit = DMG_BULLET, wpnName[]="", hs = 0);
native take_damage(victim, attacker, Float:orig[3], Float:dmg, bit = DMG_BULLET, wpnName[]="", hs = 0);
native set_user_model(id, const Model[]="");
native entity_use(eUsed, eOther);
native get_num_ents();

native DispatchKeyValue(ent, szKey[], szValue[]);

// Trace a line from Start(X, Y, Z) to End(X, Y, Z), will return the point hit in vReturn[3]
// Will return an entindex if an entity is hit.
native trace_line(ent, Float:vStart[3], Float:vEnd[3], Float:vReturn[3]);

native traceline_get_int(iVar);
native traceline_set_int(iVar, iVal);
native Float:traceline_get_float(iVar);
native traceline_set_float(iVar, Float:fVal);
native traceline_get_vector(iVar, Float:vVal[3]);
native traceline_set_vector(iVar, Float:vVal[3]);
native traceline_get_edict(iVar);
native traceline_set_edict(iVar, iEnt);

/* Wrapper around pfn_touch */
forward entity_touch(entity1, entity2);

/* Wrapper around pfn_think */
forward entity_think(entity);

/* Wrapper around pfn_spawn */
forward entity_spawn(entity);

/* Wrapper around client_PreThink */
forward client_prethink(id);

/* Wrapper around client_PostThink */
forward client_postthink(id);

//From AMX Mod:
// Called when an Emitting Sound is played Server-Side
forward emitsound(entity, const sample[]);

//From AMX Mod:
// Called when an Emitting Ambient Sound is played Server-Side
forward emitambientsound(entity, const sample[]);

//From AMX Mod:
// Called when a model spawns
forward set_model(entity, const model[]);

//From AMX Mod:
// Called whatever an entity looks at
forward traceline(entity);

//:TODO: ?
// Called when a monster is hurt by VexdUM damage natives
// forward monster_hurt(monster, attacker, damage);

//From AMX Mod:
// Called when a keyvalue is set on a player
forward setclientkeyvalue(id, key[], value[]);

//From AMX Mod:
// Called when an entity gets a keyvalue set on it from the engine.
// Use copy_keyvalue to get the keyvalue information
forward keyvalue(entity);

#include <VexdUM_stock>
