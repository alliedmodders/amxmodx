/* Fakemeta pev to engine wrapper for backwards compatibility.
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _fakemeta_pev_included
  #endinput
#endif
#define _fakemeta_pev_included

#include <engine>

enum {
	pev_string_start = 0,
	pev_classname,
	pev_globalname,
	pev_model,
	pev_target,
	pev_targetname,
	pev_netname,
	pev_message,
	pev_noise,
	pev_noise1,
	pev_noise2,
	pev_noise3,
	pev_string_end,
	pev_edict_start,
	pev_chain,
	pev_dmg_inflictor,
	pev_enemy,
	pev_aiment,
	pev_owner,
	pev_groundentity,
	pev_euser1,
	pev_euser2,
	pev_euser3,
	pev_euser4,
	pev_edict_end,
	pev_float_start,
	pev_impacttime,
	pev_starttime,
	pev_idealpitch,
	pev_ideal_yaw,
	pev_pitch_speed,
	pev_yaw_speed,
	pev_ltime,
	pev_nextthink,
	pev_gravity,
	pev_friction,
	pev_frame,
	pev_animtime,
	pev_framerate,
	pev_scale,
	pev_renderamt,
	pev_health,
	pev_frags,
	pev_takedamage,
	pev_max_health,
	pev_teleport_time,
	pev_armortype,
	pev_armorvalue,
	pev_dmg_take,
	pev_dmg_save,
	pev_dmg,
	pev_dmgtime,
	pev_speed,
	pev_air_finished,
	pev_pain_finished,
	pev_radsuit_finished,
	pev_maxspeed,
	pev_fov,
	pev_flFallVelocity,
	pev_fuser1,
	pev_fuser2,
	pev_fuser3,
	pev_fuser4,
	pev_float_end,
	pev_int_start,
	pev_fixangle,
	pev_modelindex,
	pev_viewmodel,
	pev_weaponmodel,
	pev_movetype,
	pev_solid,
	pev_skin,
	pev_body,
	pev_effects,
	pev_light_level,
	pev_sequence,
	pev_gaitsequence,
	pev_rendermode,
	pev_renderfx,
	pev_weapons,
	pev_deadflag,
	pev_button,
	pev_impulse,
	pev_spawnflags,
	pev_flags,
	pev_colormap,
	pev_team,
	pev_waterlevel,
	pev_watertype,
	pev_playerclass,
	pev_weaponanim,
	pev_pushmsec,
	pev_bInDuck,
	pev_flTimeStepSound,
	pev_flSwimTime,
	pev_flDuckTime,
	pev_iStepLeft,
	pev_gamestate,
	pev_oldbuttons,
	pev_groupinfo,
	pev_iuser1,
	pev_iuser2,
	pev_iuser3,
	pev_iuser4,
	pev_int_end,
	pev_byte_start,
	pev_controller_0,
	pev_controller_1,
	pev_controller_2,
	pev_controller_3,
	pev_blending_0,
	pev_blending_1,
	pev_byte_end,
	pev_bytearray_start,
	pev_controller,
	pev_blending,
	pev_bytearray_end,
	pev_vecarray_start,
	pev_origin,
	pev_oldorigin,
	pev_velocity,
	pev_basevelocity,
	pev_clbasevelocity,
	pev_movedir,
	pev_angles,
	pev_avelocity,
	pev_v_angle,
	pev_endpos,
	pev_startpos,
	pev_absmin,
	pev_absmax,
	pev_mins,
	pev_maxs,
	pev_size,
	pev_rendercolor,
	pev_view_ofs,
	pev_vuser1,
	pev_vuser2,
	pev_vuser3,
	pev_vuser4,
	pev_punchangle,
	pev_vecarray_end
};

new const _ENUMWRAPPER[pev_vecarray_end + 1] = {
	0,
	EV_SZ_classname,
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
	0,
	0,
	EV_ENT_chain,
	EV_ENT_dmg_inflictor,
	EV_ENT_enemy,
	EV_ENT_aiment,
	EV_ENT_owner,
	EV_ENT_groundentity,
	EV_ENT_euser1,
	EV_ENT_euser2,
	EV_ENT_euser3,
	EV_ENT_euser4,
	0,
	0,
	EV_FL_impacttime,
	EV_FL_starttime,
	EV_FL_idealpitch,
	EV_FL_ideal_yaw,
	EV_FL_pitch_speed,
	EV_FL_yaw_speed,
	EV_FL_ltime,
	EV_FL_nextthink,
	EV_FL_gravity,
	EV_FL_friction,
	EV_FL_frame,
	EV_FL_animtime,
	EV_FL_framerate,
	EV_FL_scale,
	EV_FL_renderamt,
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
	EV_FL_maxspeed,
	EV_FL_fov,
	EV_FL_flFallVelocity,
	EV_FL_fuser1,
	EV_FL_fuser2,
	EV_FL_fuser3,
	EV_FL_fuser4,
	0,
	0,
	EV_INT_fixangle,
	EV_INT_modelindex,
	-1, // pev_viewmodel
	-1, // pev_weaponmodel
	EV_INT_movetype,
	EV_INT_solid,
	EV_INT_skin,
	EV_INT_body,
	EV_INT_effects,
	EV_INT_light_level,
	EV_INT_sequence,
	EV_INT_gaitsequence,
	EV_INT_rendermode,
	EV_INT_renderfx,
	EV_INT_weapons,
	EV_INT_deadflag,
	EV_INT_button,
	EV_INT_impulse,
	EV_INT_spawnflags,
	EV_INT_flags,
	EV_INT_colormap,
	EV_INT_team,
	EV_INT_waterlevel,
	EV_INT_watertype,
	EV_INT_playerclass,
	EV_INT_weaponanim,
	EV_INT_pushmsec,
	EV_INT_bInDuck,
	EV_INT_flTimeStepSound,
	EV_INT_flSwimTime,
	EV_INT_flDuckTime,
	EV_INT_iStepLeft,
	EV_INT_gamestate,
	EV_INT_oldbuttons,
	EV_INT_groupinfo,
	EV_INT_iuser1,
	EV_INT_iuser2,
	EV_INT_iuser3,
	EV_INT_iuser4,
	0,
	0,
	EV_BYTE_controller1,
	EV_BYTE_controller2,
	EV_BYTE_controller3,
	EV_BYTE_controller4,
	EV_BYTE_blending1,
	EV_BYTE_blending2,
	0,
	0,
	-1, // bytearray controller
	-1, // bytearray blending
	0,
	0,
	EV_VEC_origin,
	EV_VEC_oldorigin,
	EV_VEC_velocity,
	EV_VEC_basevelocity,
	EV_VEC_clbasevelocity,
	EV_VEC_movedir,
	EV_VEC_angles,
	EV_VEC_avelocity,
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
	EV_VEC_punchangle,
	0,
}
stock pev(_index, _value, {Float, Sql, Result, _}:...) {
	if (_value > pev_string_start && _value < pev_string_end) {
		const BUFFERSIZE = 511
		new buffer[BUFFERSIZE + 1]
		if (!entity_get_string(_index, _ENUMWRAPPER[_value], buffer, getarg(3) < BUFFERSIZE ? getarg(3) : BUFFERSIZE))
			return 0

		new const STRLEN = strlen(buffer)
		new const MAXLEN = getarg(3)
		new i
		for (i = 0; i < STRLEN && i < MAXLEN && setarg(2, i, buffer[i]); i++) {}
		return setarg(2, i, '^0')
	}
	else if (_value > pev_edict_start && _value < pev_edict_end) {
		return entity_get_edict(_index, _ENUMWRAPPER[_value])
	}
	else if (_value > pev_float_start && _value < pev_float_end) {
		return setarg(2, 0, _:entity_get_float(_index, _ENUMWRAPPER[_value]))
	}
	else if (_value > pev_int_start && _value < pev_int_end) {
		return entity_get_int(_index, _ENUMWRAPPER[_value])
	}
	else if (_value > pev_byte_start && _value < pev_byte_end) {
		return entity_get_byte(_index, _ENUMWRAPPER[_value])
	}
	else if (_value > pev_vecarray_start && _value < pev_vecarray_end) {
		new Float:vector[3]
		entity_get_vector(_index, _ENUMWRAPPER[_value], vector)

		if (!setarg(2, 0, _:vector[0]) || !setarg(2, 1, _:vector[1]) || !setarg(2, 2, _:vector[2]))
			return 0

		return 1
	}
	else if (_value == pev_controller) {
		if (!setarg(2, 0, entity_get_byte(_index, EV_BYTE_controller1)) || setarg(2, 1, entity_get_byte(_index, EV_BYTE_controller2)) || setarg(2, 2, entity_get_byte(_index, EV_BYTE_controller3)) || setarg(2, 3, entity_get_byte(_index, EV_BYTE_controller4)))
			return 0
		return 1
	}
	else if (_value == pev_blending) {
		if (!setarg(2, 0, entity_get_byte(_index, EV_BYTE_blending1)) || setarg(2, 1, entity_get_byte(_index, EV_BYTE_blending2)))
			return 0
		return 1
	}

	return 0
}

stock set_pev(_index, _value, {Float, Sql, Result, _}:...) {
	if (_value > pev_string_start && _value < pev_string_end) {
		new string[512], i = 0
		do string[i] = getarg(2, i); while (string[i++] != '^0' && i < 512)
		return entity_set_string(_index, _ENUMWRAPPER[_value], string)
	}
	else if (_value > pev_edict_start && _value < pev_edict_end) {
		return entity_set_edict(_index, _ENUMWRAPPER[_value], getarg(2))
	}
	else if (_value > pev_float_start && _value < pev_float_end) {
		return entity_set_float(_index, _ENUMWRAPPER[_value], Float:getarg(2))
	}
	else if (_value > pev_int_start && _value < pev_int_end) {
		return entity_set_int(_index, _ENUMWRAPPER[_value], getarg(2))
	}
	else if (_value > pev_byte_start && _value < pev_byte_end) {
		return entity_set_byte(_index, _ENUMWRAPPER[_value], getarg(2))
	}
	else if (_value > pev_vecarray_start && _value < pev_vecarray_end) {
		new Float:vector[3]
		vector[0] = Float:getarg(2, 0); vector[1] = Float:getarg(2, 1); vector[2] = Float:getarg(2, 2);
		return entity_set_vector(_index, _ENUMWRAPPER[_value], vector)
	}
	else if (_value == pev_controller) {
		return entity_set_byte(_index, EV_BYTE_controller1, getarg(2, 0)) && entity_set_byte(_index, EV_BYTE_controller2, getarg(2, 1)) && entity_set_byte(_index, EV_BYTE_controller3, getarg(2, 2)) && entity_set_byte(_index, EV_BYTE_controller4, getarg(2, 3))
	}
	else if (_value == pev_blending) {
		return entity_set_byte(_index, EV_BYTE_blending1, getarg(2, 0)) && entity_set_byte(_index, EV_BYTE_blending2, getarg(2, 1))
	}

	return 0
}
