// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <amxmodx.h>
#include <messages.h>

static cell AMX_NATIVE_CALL te_create_beam_between_points(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_sprite, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMPOINTS);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_from_entity(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startent, arg_endpos, arg_sprite, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMENTPOINT);
		WRITE_SHORT(params[arg_startent]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_gunshot(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_GUNSHOT);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_explosion(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_scale, arg_framerate, arg_flags, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_EXPLOSION);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_scale]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_flags]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_tar_explosion(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_TAREXPLOSION);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_smoke(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_scale, arg_framerate, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SMOKE);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_scale]);
		WRITE_BYTE(params[arg_framerate]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_tracer(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_TRACER);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_between_entities(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startent, arg_endent, arg_sprite, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMENTS);
		WRITE_SHORT(params[arg_startent]);
		WRITE_SHORT(params[arg_endent]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_sparks(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SPARKS);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_lava_splash(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_LAVASPLASH);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_teleport_splash(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_TELEPORT);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_colored_explosion(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_startcolor, arg_numcolors, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	int numcolors = params[arg_numcolors];

	// Prevent client crash if param is 0
	if(!numcolors)
		numcolors = 1;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_EXPLOSION2);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_BYTE(params[arg_startcolor]);
		WRITE_BYTE(numcolors);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_place_decal_from_bsp_file(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_texture, arg_entity, arg_entabove, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	int entity = params[arg_entity];
	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);

		WRITE_BYTE(TE_BSPDECAL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_texture]);
		WRITE_SHORT(entity);

		if(entity)
			WRITE_SHORT(params[arg_entabove]);

	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_implosion(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_radius, arg_count, arg_life, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_IMPLOSION);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_BYTE(params[arg_radius]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_life]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_model_trail(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_sprite, arg_count, arg_life, arg_scale, arg_velocity, arg_randomness, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SPRITETRAIL);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_scale]);
		WRITE_BYTE(params[arg_velocity]);
		WRITE_BYTE(params[arg_randomness]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_display_additive_sprite(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_scale, arg_brightness, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SPRITE);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_scale]);
		WRITE_BYTE(params[arg_brightness]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_sprite(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_beamid, arg_endid, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMSPRITE);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_SHORT(params[arg_beamid]);
		WRITE_SHORT(params[arg_endid]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_ring(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_axis, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };
	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_position]);
	cell *axis = get_amxaddr(amx, params[arg_axis]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMTORUS);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(axis[0]);
		WRITE_COORD(axis[1]);
		WRITE_COORD(axis[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_disk(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_axis, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_position]);
	cell *axis = get_amxaddr(amx, params[arg_axis]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMDISK);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(axis[0]);
		WRITE_COORD(axis[1]);
		WRITE_COORD(axis[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_cylinder(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_axis, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_position]);
	cell *axis = get_amxaddr(amx, params[arg_axis]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMCYLINDER);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(axis[0]);
		WRITE_COORD(axis[1]);
		WRITE_COORD(axis[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_following_beam(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_entity, arg_sprite, arg_life, arg_width, arg_r, arg_g, arg_b, arg_a, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMFOLLOW);
		WRITE_SHORT(params[arg_entity]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_display_glow_sprite(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_scale, arg_size, arg_brightness, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_GLOWSPRITE);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_scale]);
		WRITE_BYTE(params[arg_size]);
		WRITE_BYTE(params[arg_brightness]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_beam_ring_between_entities(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startent, arg_endent, arg_sprite, arg_startframe, arg_framerate, arg_life, arg_width, arg_noise, arg_r, arg_g, arg_b, arg_a, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BEAMRING);
		WRITE_SHORT(params[arg_startent]);
		WRITE_SHORT(params[arg_endent]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_startframe]);
		WRITE_BYTE(params[arg_framerate]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_width]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_a]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_tracer_shower(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_direction, arg_color, arg_count, arg_speed, arg_velocity, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *direction = get_amxaddr(amx, params[arg_direction]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_STREAK_SPLASH);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(direction[0]);
		WRITE_COORD(direction[1]);
		WRITE_COORD(direction[2]);
		WRITE_BYTE(params[arg_color]);
		WRITE_SHORT(params[arg_count]);
		WRITE_SHORT(params[arg_speed]);
		WRITE_SHORT(params[arg_velocity]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_dynamic_light(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_radius, arg_r, arg_g, arg_b, arg_life, arg_decay, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_DLIGHT);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_BYTE(params[arg_radius]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_decay]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_entity_light(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_entity, arg_position, arg_radius, arg_r, arg_g, arg_b, arg_life, arg_decay, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_ELIGHT);
		WRITE_SHORT(params[arg_entity]);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(params[arg_radius]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
		WRITE_BYTE(params[arg_life]);
		WRITE_COORD(params[arg_decay]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_draw_line(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_life, arg_r, arg_g, arg_b, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_LINE);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_SHORT(params[arg_life]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_box(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_life, arg_r, arg_g, arg_b, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BOX);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_SHORT(params[arg_life]);
		WRITE_BYTE(params[arg_r]);
		WRITE_BYTE(params[arg_g]);
		WRITE_BYTE(params[arg_b]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_remove_all_beams_from_entity(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_entity, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_KILLBEAM);
		WRITE_SHORT(params[arg_entity]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_large_funnel(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_flag, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_LARGEFUNNEL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_SHORT(params[arg_flag]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_bloodstream(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_direction, arg_color, arg_count, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *direction = get_amxaddr(amx, params[arg_direction]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BLOODSTREAM);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(direction[0]);
		WRITE_COORD(direction[1]);
		WRITE_COORD(direction[2]);
		WRITE_BYTE(params[arg_color]);
		WRITE_BYTE(params[arg_count]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_draw_blood_line(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SHOWLINE);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_spray_blood(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_direction, arg_color, arg_speed, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *direction = get_amxaddr(amx, params[arg_direction]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BLOOD);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(direction[0]);
		WRITE_COORD(direction[1]);
		WRITE_COORD(direction[2]);
		WRITE_BYTE(params[arg_color]);
		WRITE_BYTE(params[arg_speed]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_place_brush_decal(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_texture, arg_entity, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	int texture = params[arg_texture];
	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(texture > 256 ? TE_DECALHIGH : TE_DECAL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_BYTE(texture > 256 ? texture - 256 : texture);
		WRITE_SHORT(params[arg_entity]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_bouncing_model(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_model, arg_velocity, arg_yaw, arg_bouncesound, arg_life, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *velocity = get_amxaddr(amx, params[arg_velocity]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_MODEL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(velocity[0]);
		WRITE_COORD(velocity[1]);
		WRITE_COORD(velocity[2]);
		WRITE_ANGLE(params[arg_yaw]);
		WRITE_SHORT(params[arg_model]);
		WRITE_BYTE(params[arg_bouncesound]);
		WRITE_BYTE(params[arg_life]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_explode_model(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_model, arg_speed, arg_count, arg_life, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_EXPLODEMODEL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(params[arg_speed]);
		WRITE_SHORT(params[arg_model]);
		WRITE_SHORT(params[arg_count]);
		WRITE_BYTE(params[arg_life]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_break_model(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_model, arg_size, arg_velocity, arg_random, arg_count, arg_life, arg_flags, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *size = get_amxaddr(amx, params[arg_size]);
	cell *velocity = get_amxaddr(amx, params[arg_velocity]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BREAKMODEL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(size[0]);
		WRITE_COORD(size[1]);
		WRITE_COORD(size[2]);
		WRITE_COORD(velocity[0]);
		WRITE_COORD(velocity[1]);
		WRITE_COORD(velocity[2]);
		WRITE_BYTE(params[arg_random]);
		WRITE_SHORT(params[arg_model]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_flags]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_place_gunshot_decal(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_decal, arg_entity, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_GUNSHOTDECAL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_entity]);
		WRITE_BYTE(params[arg_decal]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_sprite_spray(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_velocity, arg_count, arg_speed, arg_noise, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *velocity = get_amxaddr(amx, params[arg_velocity]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SPRITE_SPRAY);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(velocity[0]);
		WRITE_COORD(velocity[1]);
		WRITE_COORD(velocity[2]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_speed]);
		WRITE_BYTE(params[arg_noise]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_armor_ricochet(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_scale, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_ARMOR_RICOCHET);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_BYTE(params[arg_scale]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_place_player_spray(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_player, arg_spray, arg_entity, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_PLAYERDECAL);
		WRITE_BYTE(params[arg_player]);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_entity]);
		WRITE_BYTE(params[arg_spray]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_bubble_box(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_sprite, arg_count, arg_randomness, arg_height, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BUBBLES);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_COORD(params[arg_height]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_count]);
		WRITE_COORD(params[arg_randomness]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_bubble_line(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_startpos, arg_endpos, arg_sprite, arg_count, arg_randomness, arg_height, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *startpos = get_amxaddr(amx, params[arg_startpos]);
	cell *endpos = get_amxaddr(amx, params[arg_endpos]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BUBBLETRAIL);
		WRITE_COORD(startpos[0]);
		WRITE_COORD(startpos[1]);
		WRITE_COORD(startpos[2]);
		WRITE_COORD(endpos[0]);
		WRITE_COORD(endpos[1]);
		WRITE_COORD(endpos[2]);
		WRITE_COORD(params[arg_height]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_count]);
		WRITE_COORD(params[arg_randomness]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_display_falling_sprite(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite1id, arg_sprite2id, arg_color, arg_scale, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_BLOODSPRITE);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_sprite1id]);
		WRITE_SHORT(params[arg_sprite2id]);
		WRITE_BYTE(params[arg_color]);
		WRITE_BYTE(params[arg_scale]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_place_world_decal(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_texture, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	int texture = params[arg_texture];
	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(texture > 256 ? TE_WORLDDECALHIGH : TE_WORLDDECAL);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_BYTE(texture > 256 ? texture - 256 : texture);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_projectile(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_velocity, arg_model, arg_life, arg_owner, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *velocity = get_amxaddr(amx, params[arg_velocity]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_PROJECTILE);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(velocity[0]);
		WRITE_COORD(velocity[1]);
		WRITE_COORD(velocity[2]);
		WRITE_SHORT(params[arg_model]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(params[arg_owner]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_sprite_shower(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_model, arg_direction, arg_count, arg_speed, arg_noise, arg_rendermode, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *direction = get_amxaddr(amx, params[arg_direction]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_SPRAY);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(direction[0]);
		WRITE_COORD(direction[1]);
		WRITE_COORD(direction[2]);
		WRITE_SHORT(params[arg_model]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_speed]);
		WRITE_BYTE(params[arg_noise]);
		WRITE_BYTE(params[arg_rendermode]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_emit_sprite_from_player(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_player, arg_sprite, arg_count, arg_variance, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_PLAYERSPRITES);
		WRITE_SHORT(params[arg_player]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_variance]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_particle_burst(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_radius, arg_color, arg_duration, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_PARTICLEBURST);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_radius]);
		WRITE_BYTE(params[arg_color]);
		WRITE_BYTE(params[arg_duration]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_fire_field(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_sprite, arg_radius, arg_count, arg_duration, arg_flags, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_FIREFIELD);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_SHORT(params[arg_radius]);
		WRITE_SHORT(params[arg_sprite]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_flags]);
		WRITE_BYTE(params[arg_duration]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_attach_model_to_player(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_player, arg_model, arg_offset, arg_life, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_PLAYERATTACHMENT);
		WRITE_BYTE(params[arg_player]);
		WRITE_COORD(params[arg_offset]);
		WRITE_SHORT(params[arg_model]);
		WRITE_SHORT(params[arg_life]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_remove_all_player_attachments(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_player, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_KILLPLAYERATTACHMENTS);
		WRITE_BYTE(params[arg_player]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_multi_gunshot(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_direction, arg_decal, arg_count, arg_noise_x, arg_noise_y, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *direction = get_amxaddr(amx, params[arg_direction]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_MULTIGUNSHOT);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(direction[0]);
		WRITE_COORD(direction[1]);
		WRITE_COORD(direction[2]);
		WRITE_COORD(params[arg_noise_x]);
		WRITE_COORD(params[arg_noise_y]);
		WRITE_BYTE(params[arg_count]);
		WRITE_BYTE(params[arg_decal]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL te_create_user_tracer(AMX *amx, cell *params)
{
	enum args { arg_numargs, arg_position, arg_velocity, arg_life, arg_color, arg_length, arg_receiver, arg_reliable };

	int index = params[arg_receiver];

	if(!check_msg_receiver(amx, index))
		return 0;

	// Values outside the range 0-12 will throw an error
	int color = params[arg_color];

	if(color < 0)
		color = 0;

	if(color > 12)
		color = 12;

	cell *position = get_amxaddr(amx, params[arg_position]);
	cell *velocity = get_amxaddr(amx, params[arg_velocity]);

	MESSAGE_BEGIN(get_msg_destination(index, params[arg_reliable] != 0), SVC_TEMPENTITY, NULL, index ? TypeConversion.id_to_edict(index) : NULL);
		WRITE_BYTE(TE_USERTRACER);
		WRITE_COORD(position[0]);
		WRITE_COORD(position[1]);
		WRITE_COORD(position[2]);
		WRITE_COORD(velocity[0]);
		WRITE_COORD(velocity[1]);
		WRITE_COORD(velocity[2]);
		WRITE_BYTE(params[arg_life]);
		WRITE_BYTE(color);
		WRITE_BYTE(params[arg_length]);
	MESSAGE_END();

	return 1;
}

AMX_NATIVE_INFO tempent_Natives[] =
{
	{"te_create_beam_between_points",			te_create_beam_between_points},
	{"te_create_beam_from_entity",				te_create_beam_from_entity},
	{"te_create_gunshot",						te_create_gunshot},
	{"te_create_explosion",						te_create_explosion},
	{"te_create_tar_explosion",					te_create_tar_explosion},
	{"te_create_smoke",							te_create_smoke},
	{"te_create_tracer",						te_create_tracer},
	{"te_create_beam_between_entities",			te_create_beam_between_entities},
	{"te_create_sparks",						te_create_sparks},
	{"te_create_lava_splash",					te_create_lava_splash},
	{"te_create_teleport_splash",				te_create_teleport_splash},
	{"te_create_colored_explosion",				te_create_colored_explosion},
	{"te_place_decal_from_bsp_file",			te_place_decal_from_bsp_file},
	{"te_create_implosion",						te_create_implosion},
	{"te_create_model_trail",					te_create_model_trail},
	{"te_display_additive_sprite",				te_display_additive_sprite},
	{"te_create_beam_sprite",					te_create_beam_sprite},
	{"te_create_beam_ring",						te_create_beam_ring},
	{"te_create_beam_disk",						te_create_beam_disk},
	{"te_create_beam_cylinder",					te_create_beam_cylinder},
	{"te_create_following_beam",				te_create_following_beam},
	{"te_display_glow_sprite",					te_display_glow_sprite},
	{"te_create_beam_ring_between_entities",	te_create_beam_ring_between_entities},
	{"te_create_tracer_shower",					te_create_tracer_shower},
	{"te_create_dynamic_light",					te_create_dynamic_light},
	{"te_create_entity_light",					te_create_entity_light},
	{"te_draw_line",							te_draw_line},
	{"te_create_box",							te_create_box},
	{"te_remove_all_beams_from_entity",			te_remove_all_beams_from_entity},
	{"te_create_large_funnel",					te_create_large_funnel},
	{"te_create_bloodstream",					te_create_bloodstream},
	{"te_draw_blood_line",						te_draw_blood_line},
	{"te_spray_blood",							te_spray_blood},
	{"te_place_brush_decal",					te_place_brush_decal},
	{"te_create_bouncing_model",				te_create_bouncing_model},
	{"te_create_explode_model",					te_create_explode_model},
	{"te_create_break_model",					te_create_break_model},
	{"te_place_gunshot_decal",					te_place_gunshot_decal},
	{"te_create_sprite_spray",					te_create_sprite_spray},
	{"te_create_armor_ricochet",				te_create_armor_ricochet},
	{"te_place_player_spray",					te_place_player_spray},
	{"te_create_bubble_box",					te_create_bubble_box},
	{"te_create_bubble_line",					te_create_bubble_line},
	{"te_display_falling_sprite",				te_display_falling_sprite},
	{"te_place_world_decal",					te_place_world_decal},
	{"te_create_projectile",					te_create_projectile},
	{"te_create_sprite_shower",					te_create_sprite_shower},
	{"te_emit_sprite_from_player",				te_emit_sprite_from_player},
	{"te_create_particle_burst",				te_create_particle_burst},
	{"te_create_fire_field",					te_create_fire_field},
	{"te_attach_model_to_player",				te_attach_model_to_player},
	{"te_remove_all_player_attachments",		te_remove_all_player_attachments},
	{"te_create_multi_gunshot",					te_create_multi_gunshot},
	{"te_create_user_tracer",					te_create_user_tracer},
	{NULL,										NULL},
};