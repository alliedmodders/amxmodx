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

#ifndef _INCLUDE_ENGINE_GLOBAL
#define _INCLUDE_ENGINE_GLOBAL

#include "engine.h"

enum globals {
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
};

extern AMX_NATIVE_INFO global_Natives[];

#endif //_INCLUDE_ENGINE_GLOBAL

