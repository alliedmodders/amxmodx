// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Engine Module
//

#include "gpglobals.h"

static cell AMX_NATIVE_CALL get_global_float(AMX *amx, cell *params)
{
	REAL returnValue;

	switch (params[1]) {
		case GL_coop:
			returnValue = gpGlobals->coop;
			break;
		case GL_deathmatch:
			returnValue = gpGlobals->deathmatch;
			break;
		case GL_force_retouch:
			returnValue = gpGlobals->force_retouch;
			break;
		case GL_found_secrets:
			returnValue = gpGlobals->found_secrets;
			break;
		case GL_frametime:
			returnValue = gpGlobals->frametime;
			break;
		case GL_serverflags:
			returnValue = gpGlobals->serverflags;
			break;
		case GL_teamplay:
			returnValue = gpGlobals->teamplay;
			break;
		case GL_time:
			returnValue = gpGlobals->time;
			break;
		case GL_trace_allsolid:
			returnValue = gpGlobals->trace_allsolid;
			break;
		case GL_trace_fraction:
			returnValue = gpGlobals->trace_fraction;
			break;
		case GL_trace_inopen:
			returnValue = gpGlobals->trace_inopen;
			break;
		case GL_trace_inwater:
			returnValue = gpGlobals->trace_inwater;
			break;
		case GL_trace_plane_dist:
			returnValue = gpGlobals->trace_plane_dist;
			break;
		case GL_trace_startsolid:
			returnValue = gpGlobals->trace_startsolid;
			break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global_float index %d", params[1]);
			return 0;
	}

	return amx_ftoc(returnValue);
}

static cell AMX_NATIVE_CALL get_global_int(AMX *amx, cell *params)
{
	int returnValue = 0;

	switch (params[1]) {
		case GL_cdAudioTrack:
			returnValue = gpGlobals->cdAudioTrack;
			break;
		case GL_maxClients:
			returnValue = gpGlobals->maxClients;
			break;
		case GL_maxEntities:
			returnValue = gpGlobals->maxEntities;
			break;
		case GL_msg_entity:
			returnValue = gpGlobals->msg_entity;
			break;
		case GL_trace_flags:
			returnValue = gpGlobals->trace_flags;
			break;
		case GL_trace_hitgroup:
			returnValue = gpGlobals->trace_hitgroup;
			break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global_int index %d", params[1]);
			return 0;
	}

	return returnValue;
}

static cell AMX_NATIVE_CALL get_global_string(AMX *amx, cell *params)
{
	string_t* returnValue;

	switch(params[1]) {
		case GL_pStringBase: // const char *, so no string_t
			return MF_SetAmxString(amx, params[2], gpGlobals->pStringBase, params[3]);
		// The rest are string_t:s...
		case GL_mapname:
			returnValue = &(gpGlobals->mapname);
			break;
		case GL_startspot:
			returnValue = &(gpGlobals->startspot);
			break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global_string index %d", params[1]);
			return 0;
	}

	return MF_SetAmxString(amx, params[2], STRING(*returnValue), params[3]);
}

static cell AMX_NATIVE_CALL get_global_vector(AMX *amx, cell *params) // globals_get_vector(variable, Float:vector[3]); = 2 params
{
	cell *cRet = MF_GetAmxAddr(amx, params[2]);
	vec3_t fetchedVector;

	switch (params[1]) {
		case GL_trace_endpos:
			fetchedVector = gpGlobals->trace_endpos;
			break;
		case GL_trace_plane_normal:
			fetchedVector = gpGlobals->trace_plane_normal;
			break;
		case GL_v_forward:
			fetchedVector = gpGlobals->v_forward;
			break;
		case GL_v_right:
			fetchedVector = gpGlobals->v_right;
			break;
		case GL_v_up:
			fetchedVector = gpGlobals->v_up;
			break;
		case GL_vecLandmarkOffset:
			fetchedVector = gpGlobals->vecLandmarkOffset;
			break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global_vector index %d", params[1]);
			return 0;
	}

	cRet[0] = amx_ftoc(fetchedVector.x); 
	cRet[1] = amx_ftoc(fetchedVector.y); 
	cRet[2] = amx_ftoc(fetchedVector.z); 

	return 1;
}

static cell AMX_NATIVE_CALL get_global_edict2(AMX *amx, cell *params) 
{
	edict_t* pReturnEntity;

	switch (params[1]) {
		case GL_trace_ent:
			pReturnEntity = gpGlobals->trace_ent;
			break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global_edict index %d", params[1]);
			return -1;
	}

	// Will crash if ENTINDEX() is called on bad pointer?
	if(!FNullEnt(pReturnEntity))
		return ENTINDEX(pReturnEntity);

	return -1;
}

static cell AMX_NATIVE_CALL get_global_edict(AMX *amx, cell *params) // globals_get_edict(variable); = 1 param
{
	cell res = get_global_edict2(amx, params);

	if (res == -1)
		res = 0;

	return res;
}

AMX_NATIVE_INFO global_Natives[] = {
	{"get_global_float",	get_global_float},
	{"get_global_int",		get_global_int},
	{"get_global_string",	get_global_string},
	{"get_global_edict",	get_global_edict},
	{"get_global_edict2",	get_global_edict2},
	{"get_global_vector",	get_global_vector},
	{NULL,					NULL},
	  ///////////////////
};

