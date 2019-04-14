// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Engine Constants
//

#if defined _engine_const_included
	#endinput
#endif
#define _engine_const_included

#include <hlsdk_const>

/**
 * Flags for the [set|get]_speak() natives.
 *
 * @note These do not correspond to any HLSDK constants and are only relevant to
 *       the custom functionality provided by the engine module.
 */
#define SPEAK_NORMAL        0 // Normal behavior, client follows alltalk rules
#define SPEAK_MUTED         1 // Client can not talk
#define SPEAK_ALL           2 // Client can talk to every other client
#define SPEAK_LISTENALL     4 // Client can listen to every other client

/**
 * View types for the set_view() native.
 *
 * @note These do not correspond to any HLSDK constants and are only relevant to
 * 		 the custom functionality provided by the engine module.
 */
#define CAMERA_NONE         0
#define CAMERA_3RDPERSON    1
#define CAMERA_UPLEFT       2
#define CAMERA_TOPDOWN      3

/**
 * @section Entvar constants used with entity_[get|set]_* functions.
 */

/**
 * Integers, use with entity_[get|set]_int()
 */
enum {
	EV_INT_gamestate = 0,
	EV_INT_oldbuttons,
	EV_INT_groupinfo,
	EV_INT_iuser1,
	EV_INT_iuser2,
	EV_INT_iuser3,
	EV_INT_iuser4,
	EV_INT_weaponanim,
	EV_INT_pushmsec,
	EV_INT_bInDuck,
	EV_INT_flTimeStepSound,
	EV_INT_flSwimTime,
	EV_INT_flDuckTime,
	EV_INT_iStepLeft,
	EV_INT_movetype,
	EV_INT_solid,
	EV_INT_skin,
	EV_INT_body,
	EV_INT_effects,
	EV_INT_light_level,
	EV_INT_sequence,
	EV_INT_gaitsequence,
	EV_INT_modelindex,
	EV_INT_playerclass,
	EV_INT_waterlevel,
	EV_INT_watertype,
	EV_INT_spawnflags,
	EV_INT_flags,
	EV_INT_colormap,
	EV_INT_team,
	EV_INT_fixangle,
	EV_INT_weapons,
	EV_INT_rendermode,
	EV_INT_renderfx,
	EV_INT_button,
	EV_INT_impulse,
	EV_INT_deadflag,
};

/**
 * Floats, use with entity_[get|set]_float()
 */
enum {
	EV_FL_impacttime = 0,
	EV_FL_starttime,
	EV_FL_idealpitch,
	EV_FL_pitch_speed,
	EV_FL_ideal_yaw,
	EV_FL_yaw_speed,
	EV_FL_ltime,
	EV_FL_nextthink,
	EV_FL_gravity,
	EV_FL_friction,
	EV_FL_frame,
	EV_FL_animtime,
	EV_FL_framerate,
	EV_FL_health,
	EV_FL_frags,
	EV_FL_takedamage,
	EV_FL_max_health,
	EV_FL_teleport_time,
	EV_FL_armortype,
	EV_FL_armorvalue,
	EV_FL_dmg_take,
	EV_FL_dmg_save,
	EV_FL_dmg,
	EV_FL_dmgtime,
	EV_FL_speed,
	EV_FL_air_finished,
	EV_FL_pain_finished,
	EV_FL_radsuit_finished,
	EV_FL_scale,
	EV_FL_renderamt,
	EV_FL_maxspeed,
	EV_FL_fov,
	EV_FL_flFallVelocity,
	EV_FL_fuser1,
	EV_FL_fuser2,
	EV_FL_fuser3,
	EV_FL_fuser4,
};

/**
 * Vectors, use with entity_[get|set]_vector()
 */
enum {
	EV_VEC_origin = 0,
	EV_VEC_oldorigin,
	EV_VEC_velocity,
	EV_VEC_basevelocity,
	EV_VEC_clbasevelocity,
	EV_VEC_movedir,
	EV_VEC_angles,
	EV_VEC_avelocity,
	EV_VEC_punchangle,
	EV_VEC_v_angle,
	EV_VEC_endpos,
	EV_VEC_startpos,
	EV_VEC_absmin,
	EV_VEC_absmax,
	EV_VEC_mins,
	EV_VEC_maxs,
	EV_VEC_size,
	EV_VEC_rendercolor,
	EV_VEC_view_ofs,
	EV_VEC_vuser1,
	EV_VEC_vuser2,
	EV_VEC_vuser3,
	EV_VEC_vuser4,
};

/**
 * Edicts, use with entity_[get|set]_edict()
 */
enum {
	EV_ENT_chain = 0,
	EV_ENT_dmg_inflictor,
	EV_ENT_enemy,
	EV_ENT_aiment,
	EV_ENT_owner,
	EV_ENT_groundentity,
	EV_ENT_pContainingEntity,
	EV_ENT_euser1,
	EV_ENT_euser2,
	EV_ENT_euser3,
	EV_ENT_euser4,
};

/**
 * Strings, use with entity_[get|set]_string()
 */
enum {
	EV_SZ_classname = 0,
	EV_SZ_globalname,
	EV_SZ_model,
	EV_SZ_target,
	EV_SZ_targetname,
	EV_SZ_netname,
	EV_SZ_message,
	EV_SZ_noise,
	EV_SZ_noise1,
	EV_SZ_noise2,
	EV_SZ_noise3,
	EV_SZ_viewmodel,
	EV_SZ_weaponmodel,
};

/**
 * Bytearrays, use with entity_[get|set]_byte()
 */
enum {
	EV_BYTE_controller1 = 0,
	EV_BYTE_controller2,
	EV_BYTE_controller3,
	EV_BYTE_controller4,
	EV_BYTE_blending1,
	EV_BYTE_blending2,
};

/**
 * @endsection
 */

#if defined _jghg_enums
	#endinput
#endif
#define _jghg_enums

/**
 * Used by get_global_[edict/float/int/string/vector]()
 */
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
};


/**
 * Used by [get/set]_usercmd()
 */
enum
{
	usercmd_float_start,
	usercmd_forwardmove,       // Float
	usercmd_sidemove,          // Float
	usercmd_upmove,            // Float
	usercmd_float_end,
	usercmd_int_start,
	usercmd_lerp_msec,         // short
	usercmd_msec,              // byte
	usercmd_lightlevel,        // byte
	usercmd_buttons,           // unsigned short
	usercmd_impulse,           // byte
	usercmd_weaponselect,      // byte
	usercmd_impact_index,      // int
	usercmd_int_end,
	usercmd_vec_start,
	usercmd_viewangles,        // Vector
	usercmd_impact_position,   // vec
	usercmd_vec_end
};

/**
 * Used by the traceresult()
 */
enum
{
	TR_AllSolid,       // (int) if true, plane is not valid
	TR_StartSolid,     // (int) if true, the initial point was in a solid area
	TR_InOpen,         // (int)
	TR_InWater,        // (int)
	TR_Fraction,       // (float) time completed, 1.0 = didn't hit anything
	TR_EndPos,         // (vector) final position
	TR_PlaneDist,      // (float)
	TR_PlaneNormal,    // (vector) surface normal at impact
	TR_Hit,            // (entity) entity the surface is on
	TR_Hitgroup        // (int) 0 == generic, non zero is specific body part
};

