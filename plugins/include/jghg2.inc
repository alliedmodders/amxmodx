// JGHG2 module
//This file is provided for backwards compatibility.
//It includes the engine and cstrike modules automatically.
//It is intended for AMX Mod X

#if !defined INCLUDED_JGHG
	#define INCLUDED_JGHG

#include <engine>
#include <cstrike>
#include <fakemeta>

#if !defined _jghg_enums
	#define _jghg_enums
// Global member variables
enum {
	// Edict
	GL_trace_ent = 0,

	// Float
	GL_coop,
	GL_deathmatch,
	GL_force_retouch,
	GL_found_secrets,
	GL_frametime,
	GL_serverflags,
	GL_teamplay,
	GL_time,
	GL_trace_allsolid,
	GL_trace_fraction,
	GL_trace_inopen,
	GL_trace_inwater,
	GL_trace_plane_dist,
	GL_trace_startsolid,

	// Int
	GL_cdAudioTrack,
	GL_maxClients,
	GL_maxEntities,
	GL_msg_entity,
	GL_trace_flags,
	GL_trace_hitgroup,

	// String
	GL_pStringBase,
	GL_mapname,
	GL_startspot,

	// Vector
	GL_trace_endpos,
	GL_trace_plane_normal,
	GL_v_forward,
	GL_v_right,
	GL_v_up,
	GL_vecLandmarkOffset,

	// Void (not supported)
	GL_pSaveData
}

// jghg_categories
enum {
   jghg2_classname = 0,
   jghg2_target = 1,
   jghg2_targetname = 2
}
#endif

stock jghg_find_ent_owner(start_from_ent, jghg_category, value[], owner_index)
{
	find_ent_by_owner(start_from_ent, value, owner_index, jghg_category)
}

stock find_ent_sphere(start_from_ent, Float:origin[3], Float:radius)
{
	return find_ent_in_sphere(start_from_ent, origin, radius)
}

stock get_hostage_id(hostage)
{
	return cs_get_hostage_id(hostage)
}


stock get_owner(id)
{
	return entity_get_edict(id, EV_ENT_owner)
}

stock get_pdata(entity, offset)
{
	return get_pdata_int(entity, offset)
}
stock get_pdata_char(entity, offset)
{
	return get_pdata_char(entity, offset)
}
stock get_pdata_short(entity, offset)
{
	return get_pdata_short(entity, offset)
}
stock set_pdata(entity, offset, value)
{
	return set_pdata_int(entity, offset, value)
}
stock set_pdata_char(entity, offset, value)
{
	return set_pdata_char(entity, offset, value)
}
stock set_pdata_short(entity, offset, value)
{
	return set_pdata_short(entity, offset, value)
}

stock is_ent_valid(id)
{
	return is_valid_ent(id)
}

stock number_of_entities()
{
	return entity_count()
}

stock use(used, user)
{
	return force_use(user, used)
}

stock Float:globals_get_float(variable)
{
	return get_global_float(variable)
}
stock globals_get_int(variable)
{
	return get_global_int(variable)
}
stock globals_get_string(variable, string[], maxlen)
{
	return get_global_string(variable, string, maxlen)
}
stock globals_get_vector(variable, Float:vector[3])
{
	return get_global_vector(variable, vector)
}
stock globals_get_edict(variable)
{
	return get_global_edict(variable)
}

stock get_max_entities() {
	return get_global_int(GL_maxEntities)
}

stock jghg2_set_size(index, Float:mins[3], Float:maxs[3])
{
	return set_size(index, mins, maxs)
}

stock jghg2_think(index)
{
	return call_think(index)
}

#endif // INCLUDED_JGHG
