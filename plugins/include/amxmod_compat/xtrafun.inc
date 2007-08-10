/* Xtrafun backwards compatibility
*
* by the AMX Mod X Development Team
* These natives were originally made by SpaceDude, EJ, and JustinHoMi.
*
* This file is provided as is (no warranties).
*/

#if !defined _xtrafun_included
	#define _xtrafun_included

#if !defined _engine_included
	#include <engine.inc>
#endif

/* Gets the velocity of an entity */
stock get_entity_velocity(index, velocity[3]) {
	new Float:vector[3];
	entity_get_vector(index, EV_VEC_velocity, vector);
	FVecIVec(vector, velocity);
}

/* Sets the velocity of an entity */
stock set_entity_velocity(index, velocity[3]) {
	new Float:vector[3];
	IVecFVec(velocity, vector);
	entity_set_vector(index, EV_VEC_velocity, vector);
}

/* Gets the origin of an entity */
stock get_entity_origin(index, origin[3]) {
	new Float:vector[3];
	entity_get_vector(index, EV_VEC_origin, vector);
	FVecIVec(vector, origin);
}

/* Sets the origin of an entity */
stock set_entity_origin(index, origin[3]) {
	new Float:vector[3];
	IVecFVec(origin, vector);
	entity_set_vector(index, EV_VEC_origin, vector);
}

/* Get the index of the grenade belonging to index.
 * Model of grenade is returned in model[].
 * Specify the grenadeindex to start searching from,
 * or leave it at 0 to search from the start.
 * Returns grenade index.
 * Paths + models of grenades in Counter-Strike:
 * HEGRENADE = "models/w_hegrenade.mdl"
 * FLASHBANG = "models/w_flashbang.mdl"
 * SMOKEGRENADE = "models/w_smokegrenade.mdl" */
stock get_grenade_index(index, model[], len, grenadeindex = 0) {
	new entfind = grenadeindex;
	new entowner = index;

	for (;;) {
		entfind = find_ent_by_class(entfind, "grenade");

		if (entfind && is_valid_ent(entfind)) {
			if (entity_get_edict(entFind, EV_ENT_owner) == entowner) {
				entity_get_string(entfind, EV_SZ_model, model);
				return entfind;
			}
		}
		else {
			// Eventually comes here if loop fails to find a grenade with specified owner.
			return 0;
		}
	}
}

/* Find the number of entities in the game */
stock current_num_ents() {
	return entity_count();
}

enum {
   classname = 0,
   target,
   targetname
};

#if !defined _vexd_bcompat_included
/* Find an entity ID from start_from_ent id (use 0 to start from
 * the beginning, category is either "classname", "target" or
 * "targetname", value is the name you are searching for */
stock find_entity(start_from_ent, category, value[]) {
	switch (category) {
		case target: return find_ent_by_target(start_from_ent, value);
		case targetname: return find_ent_by_tname(start_from_ent, value);
	}
	return find_ent_by_class(start_from_ent, value);
}
#endif

#endif // _xtrafun_included