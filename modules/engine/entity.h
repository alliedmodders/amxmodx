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

#ifndef _INCLUDE_ENGINE_ENTSTUFF
#define _INCLUDE_ENGINE_ENTSTUFF

#include "engine.h"

enum {
	gamestate,
	oldbuttons,
	groupinfo,
	iuser1,
	iuser2,
	iuser3,
	iuser4,
	weaponanim,
	pushmsec,
	bInDuck,
	flTimeStepSound,
	flSwimTime,
	flDuckTime,
	iStepLeft,
	movetype,
	solid,
	skin,			
	body,
	effects,
	light_level,
	sequence,
	gaitsequence,
	modelindex,
	playerclass,
	waterlevel,
	watertype,
	spawnflags,
	flags,
	colormap,
	team,
	fixangle,
	weapons,
	rendermode,
	renderfx,
	button,
	impulse,
	deadflag,
};

enum {
	impacttime,
	starttime,
	idealpitch,
	pitch_speed,
	ideal_yaw,
	yaw_speed,
	ltime,
	nextthink,
	gravity,
	friction,
	frame,
	animtime,
	framerate,
	health,
	frags,
	takedamage,
	max_health,
	teleport_time,
	armortype,
	armorvalue,
	dmg_take,
	dmg_save,
	dmg,
	dmgtime,
	speed,
	air_finished,
	pain_finished,
	radsuit_finished,
	scale,
	renderamt,
	maxspeed,
	fov,
	flFallVelocity,
	fuser1,
	fuser2,
	fuser3,
	fuser4,
};

enum {
	origin,
	oldorigin,
	velocity,
	basevelocity,
	clbasevelocity,
	movedir,
	angles,
	avelocity,
	punchangle,
	v_angle,
	endpos,
	startpos,
	absmin,
	absmax,
	mins,
	maxs,
	size,
	rendercolor,
	view_ofs,
	vuser1,
	vuser2,
	vuser3,
	vuser4,
};

enum {
	chain,
	dmg_inflictor,
	enemy,
	aiment,
	owner,
	groundentity,
	pContainingEntity,
	euser1,
	euser2,
	euser3,
	euser4,
};

enum {
	classname,
	globalname,
	model,
	target,
	targetname,
	netname,
	message,
	noise,
	noise1,
	noise2,
	noise3,
	viewmodel,
	weaponmodel,
};

enum {
	controller1,
	controller2,
	controller3,
	controller4,
	blending1,
	blending2,
};

void UTIL_SetSize(edict_t *pev, const Vector &vecMin, const Vector &vecMax);

extern AMX_NATIVE_INFO ent_Natives[];
extern AMX_NATIVE_INFO ent_NewNatives[];

#endif //_INCLUDE_ENGINE_ENTSTUFF

