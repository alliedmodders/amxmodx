/**
 * This file provides various utility functions that use the Fakemeta module.
 * This file is created and maintained by VEN.
 * For support and issues, see:
 * http://forums.alliedmods.net/showthread.php?t=28284
 */
 

/* Fakemeta Utilities
*
* by VEN
*
* This file is provided as is (no warranties).
*/

#if !defined _fakemeta_included
	#include <fakemeta>
#endif

#if defined _fakemeta_util_included
	#endinput
#endif
#define _fakemeta_util_included

#include <xs>


/* Engine functions */

#define fm_precache_generic(%1) engfunc(EngFunc_PrecacheGeneric, %1)
/* stock fm_precache_generic(const file[])
	return engfunc(EngFunc_PrecacheGeneric, file) */

#define fm_precache_event(%1,%2) engfunc(EngFunc_PrecacheEvent, %1, %2)
/* stock fm_precache_event(type, const name[])
	return engfunc(EngFunc_PrecacheEvent, type, name) */

// ported by v3x
#define fm_drop_to_floor(%1) engfunc(EngFunc_DropToFloor, %1)
/* stock fm_drop_to_floor(entity)
	return engfunc(EngFunc_DropToFloor, entity) */

#define fm_force_use(%1,%2) dllfunc(DLLFunc_Use, %2, %1)
/* stock fm_force_use(user, used)
	return dllfunc(DLLFunc_Use, used, user) */

#define fm_entity_set_size(%1,%2,%3) engfunc(EngFunc_SetSize, %1, %2, %3)
/* stock fm_entity_set_size(index, const Float:mins[3], const Float:maxs[3])
	return engfunc(EngFunc_SetSize, index, mins, maxs) */

#define fm_get_decal_index(%1) engfunc(EngFunc_DecalIndex, %1)
/* stock fm_get_decal_index(const decalname[])
	return engfunc(EngFunc_DecalIndex, decalname) */

stock Float:fm_entity_range(ent1, ent2) {
	new Float:origin1[3], Float:origin2[3];
	pev(ent1, pev_origin, origin1);
	pev(ent2, pev_origin, origin2);

	return get_distance_f(origin1, origin2);
}

// based on KoST's port, upgraded version fits into the macros
#define fm_create_entity(%1) engfunc(EngFunc_CreateNamedEntity, engfunc(EngFunc_AllocString, %1))
/* stock fm_create_entity(const classname[])
	return engfunc(EngFunc_CreateNamedEntity, engfunc(EngFunc_AllocString, classname)) */

#define fm_find_ent_by_class(%1,%2) engfunc(EngFunc_FindEntityByString, %1, "classname", %2)
/* stock fm_find_ent_by_class(index, const classname[])
	return engfunc(EngFunc_FindEntityByString, index, "classname", classname) */

stock fm_find_ent_by_owner(index, const classname[], owner, jghgtype = 0) {
	new strtype[11] = "classname", ent = index;
	switch (jghgtype) {
		case 1: strtype = "target";
		case 2: strtype = "targetname";
	}

	while ((ent = engfunc(EngFunc_FindEntityByString, ent, strtype, classname)) && pev(ent, pev_owner) != owner) {}

	return ent;
}

#define fm_find_ent_by_target(%1,%2) engfunc(EngFunc_FindEntityByString, %1, "target", %2)
/* stock fm_find_ent_by_target(index, const target[])
	return engfunc(EngFunc_FindEntityByString, index, "target", target) */

#define fm_find_ent_by_tname(%1,%2) engfunc(EngFunc_FindEntityByString, %1, "targetname", %2)
/* stock fm_find_ent_by_tname(index, const targetname[])
	return engfunc(EngFunc_FindEntityByString, index, "targetname", targetname) */

stock fm_find_ent_by_model(index, const classname[], const model[]) {
	new ent = index, mdl[72];
	while ((ent = fm_find_ent_by_class(ent, classname))) {
		pev(ent, pev_model, mdl, sizeof mdl - 1);
		if (equal(mdl, model))
			return ent;
	}

	return 0;
}

#define fm_find_ent_in_sphere(%1,%2,%3) engfunc(EngFunc_FindEntityInSphere, %1, %2, %3)
/* stock fm_find_ent_in_sphere(index, const Float:origin[3], Float:radius)
	return engfunc(EngFunc_FindEntityInSphere, index, origin, radius) */

#define fm_call_think(%1) dllfunc(DLLFunc_Think, %1)
/* stock fm_call_think(entity)
	return dllfunc(DLLFunc_Think, entity) */

#define fm_is_valid_ent(%1) pev_valid(%1)
/* stock fm_is_valid_ent(index)
	return pev_valid(index) */

stock fm_entity_set_origin(index, const Float:origin[3]) {
	new Float:mins[3], Float:maxs[3];
	pev(index, pev_mins, mins);
	pev(index, pev_maxs, maxs);
	engfunc(EngFunc_SetSize, index, mins, maxs);

	return engfunc(EngFunc_SetOrigin, index, origin);
}

#define fm_entity_set_model(%1,%2) engfunc(EngFunc_SetModel, %1, %2)
/* stock fm_entity_set_model(index, const model[])
	return engfunc(EngFunc_SetModel, index, model) */

// ported by v3x
#define fm_remove_entity(%1) engfunc(EngFunc_RemoveEntity, %1)
/* stock fm_remove_entity(index)
	return engfunc(EngFunc_RemoveEntity, index) */

#define fm_entity_count() engfunc(EngFunc_NumberOfEntities)
/* stock fm_entity_count()
	return engfunc(EngFunc_NumberOfEntities) */

#define fm_fake_touch(%1,%2) dllfunc(DLLFunc_Touch, %1, %2)
/* stock fm_fake_touch(toucher, touched)
	return dllfunc(DLLFunc_Touch, toucher, touched) */

#define fm_DispatchSpawn(%1) dllfunc(DLLFunc_Spawn, %1)
/* stock fm_DispatchSpawn(entity)
	return dllfunc(DLLFunc_Spawn, entity) */

// ported by v3x
#define fm_point_contents(%1) engfunc(EngFunc_PointContents, %1)
/* stock fm_point_contents(const Float:point[3])
	return engfunc(EngFunc_PointContents, point) */

stock fm_trace_line(ignoreent, const Float:start[3], const Float:end[3], Float:ret[3]) {
	engfunc(EngFunc_TraceLine, start, end, ignoreent == -1 ? 1 : 0, ignoreent, 0);

	new ent = get_tr2(0, TR_pHit);
	get_tr2(0, TR_vecEndPos, ret);

	return pev_valid(ent) ? ent : 0;
}

stock fm_trace_hull(const Float:origin[3], hull, ignoredent = 0, ignoremonsters = 0) {
	new result = 0;
	engfunc(EngFunc_TraceHull, origin, origin, ignoremonsters, hull, ignoredent > 0 ? ignoredent : 0, 0);

	if (get_tr2(0, TR_StartSolid))
		result += 1;
	if (get_tr2(0, TR_AllSolid))
		result += 2;
	if (!get_tr2(0, TR_InOpen))
		result += 4;

	return result;
}

stock fm_trace_normal(ignoreent, const Float:start[3], const Float:end[3], Float:ret[3]) {
	engfunc(EngFunc_TraceLine, start, end, 0, ignoreent, 0);
	get_tr2(0, TR_vecPlaneNormal, ret);

	new Float:fraction;
	get_tr2(0, TR_flFraction, fraction);
	if (fraction >= 1.0)
		return 0;

	return 1;
}

// note that for CS planted C4 has a "grenade" classname as well
stock fm_get_grenade_id(id, model[], len, grenadeid = 0) {
	new ent = fm_find_ent_by_owner(grenadeid, "grenade", id);
	if (ent && len > 0)
		pev(ent, pev_model, model, len);

	return ent;
}

#define fm_halflife_time() get_gametime()
/* stock Float:fm_halflife_time()
	return get_gametime() */

#define fm_attach_view(%1,%2) engfunc(EngFunc_SetView, %1, %2)
/* stock fm_attach_view(index, entity)
	return engfunc(EngFunc_SetView, index, entity) */

stock fm_playback_event(flags, invoker, eventindex, Float:delay, const Float:origin[3], const Float:angles[3], Float:fparam1, Float:fparam2, iparam1, iparam2, bparam1, bparam2) {
	return engfunc(EngFunc_PlaybackEvent, flags, invoker, eventindex, delay, origin, angles, fparam1, fparam2, iparam1, iparam2, bparam1, bparam2);
}

#define fm_eng_get_string(%1,%2,%3) engfunc(EngFunc_SzFromIndex, %1, %2, %3)
/* stock fm_eng_get_string(istring, string[], len)
	return engfunc(EngFunc_SzFromIndex, istring, string, len) */


/* HLSDK functions */

// the dot product is performed in 2d, making the view cone infinitely tall
stock bool:fm_is_in_viewcone(index, const Float:point[3]) {
	new Float:angles[3];
	pev(index, pev_angles, angles);
	engfunc(EngFunc_MakeVectors, angles);
	global_get(glb_v_forward, angles);
	angles[2] = 0.0;

	new Float:origin[3], Float:diff[3], Float:norm[3];
	pev(index, pev_origin, origin);
	xs_vec_sub(point, origin, diff);
	diff[2] = 0.0;
	xs_vec_normalize(diff, norm);

	new Float:dot, Float:fov;
	dot = xs_vec_dot(norm, angles);
	pev(index, pev_fov, fov);
	if (dot >= floatcos(fov * M_PI / 360))
		return true;

	return false;
}

stock bool:fm_is_visible(index, const Float:point[3], ignoremonsters = 0) {
	new Float:start[3], Float:view_ofs[3];
	pev(index, pev_origin, start);
	pev(index, pev_view_ofs, view_ofs);
	xs_vec_add(start, view_ofs, start);

	engfunc(EngFunc_TraceLine, start, point, ignoremonsters, index, 0);

	new Float:fraction;
	get_tr2(0, TR_flFraction, fraction);
	if (fraction == 1.0)
		return true;

	return false;
}


/* Engine_stocks functions */

stock fm_fakedamage(victim, const classname[], Float:takedmgdamage, damagetype) {
	new class[] = "trigger_hurt";
	new entity = fm_create_entity(class);
	if (!entity)
		return 0;

	new value[16];
	float_to_str(takedmgdamage * 2, value, sizeof value - 1);
	fm_set_kvd(entity, "dmg", value, class);

	num_to_str(damagetype, value, sizeof value - 1);
	fm_set_kvd(entity, "damagetype", value, class);

	fm_set_kvd(entity, "origin", "8192 8192 8192", class);
	fm_DispatchSpawn(entity);

	set_pev(entity, pev_classname, classname);
	fm_fake_touch(entity, victim);
	fm_remove_entity(entity);

	return 1;
}

#define fm_find_ent(%1,%2) engfunc(EngFunc_FindEntityByString, %1, "classname", %2)
/* stock fm_find_ent(index, const classname[])
	return engfunc(EngFunc_FindEntityByString, index, "classname", classname) */

#define fm_get_user_button(%1) pev(%1, pev_button)
/* stock fm_get_user_button(index)
	return pev(index, pev_button) */

#define fm_get_user_oldbutton(%1) pev(%1, pev_oldbuttons)
/* stock fm_get_user_oldbutton(index)
	return pev(index, pev_oldbuttons) */

#define fm_get_entity_flags(%1) pev(%1, pev_flags)
/* stock fm_get_entity_flags(index)
	return pev(index, pev_flags) */

#define fm_get_entity_distance(%1,%2) floatround(fm_entity_range(%1, %2))
/* stock fm_get_entity_distance(ent1, ent2)
	return floatround(fm_entity_range(ent1, ent2)) */

#define fm_get_grenade(%1) fm_get_grenade_id(%1, "", 0)
/* stock fm_get_grenade(id)
	return fm_get_grenade_id(id, "", 0) */

// optimization idea by Orangutanz
stock fm_get_brush_entity_origin(index, Float:origin[3]) {
	new Float:mins[3], Float:maxs[3];

	pev(index, pev_origin, origin);
	pev(index, pev_mins, mins);
	pev(index, pev_maxs, maxs);

	origin[0] += (mins[0] + maxs[0]) * 0.5;
	origin[1] += (mins[1] + maxs[1]) * 0.5;
	origin[2] += (mins[2] + maxs[2]) * 0.5;

	return 1;
}

// based on v3x's port, upgraded version returns number of removed entities
stock fm_remove_entity_name(const classname[]) {
	new ent = -1, num = 0;
	while ((ent = fm_find_ent_by_class(ent, classname)))
		num += fm_remove_entity(ent);

	return num;
}

stock fm_ViewContents(id) {
	new origin[3], Float:Orig[3];
	get_user_origin(id, origin, Origin_AimEndEyes);
	IVecFVec(origin, Orig);

	return fm_point_contents(Orig);
}

stock fm_get_speed(entity) {
	new Float:Vel[3];
	pev(entity, pev_velocity, Vel);

	return floatround(vector_length(Vel));
}

stock fm_set_rendering(entity, fx = kRenderFxNone, r = 255, g = 255, b = 255, render = kRenderNormal, amount = 16) {
	new Float:RenderColor[3];
	RenderColor[0] = float(r);
	RenderColor[1] = float(g);
	RenderColor[2] = float(b);

	set_pev(entity, pev_renderfx, fx);
	set_pev(entity, pev_rendercolor, RenderColor);
	set_pev(entity, pev_rendermode, render);
	set_pev(entity, pev_renderamt, float(amount));

	return 1;
}

stock fm_set_entity_flags(index, flag, onoff) {
	new flags = pev(index, pev_flags);
	if ((flags & flag) > 0)
		return onoff == 1 ? 2 : 1 + 0 * set_pev(index, pev_flags, flags - flag);
	else
		return onoff == 0 ? 2 : 1 + 0 * set_pev(index, pev_flags, flags + flag);

	return 0;
}

stock fm_set_entity_visibility(index, visible = 1) {
	set_pev(index, pev_effects, visible == 1 ? pev(index, pev_effects) & ~EF_NODRAW : pev(index, pev_effects) | EF_NODRAW);

	return 1;
}

#define fm_get_entity_visibility(%1) (!(pev(%1, pev_effects) & EF_NODRAW))
/* stock fm_get_entity_visibility(index)
	return !(pev(index, pev_effects) & EF_NODRAW) */

stock fm_set_user_velocity(entity, const Float:vector[3]) {
	set_pev(entity, pev_velocity, vector);

	return 1;
}

#define fm_get_user_velocity(%1,%2) pev(%1, pev_velocity, %2)
/* stock fm_get_user_velocity(entity, Float:vector[3])
	return pev(entity, pev_velocity, vector) */


/* Fun functions */

#define fm_get_client_listen(%1,%2) engfunc(EngFunc_GetClientListening, %1, %2)
/* stock fm_get_client_listen(receiver, sender)
	return engfunc(EngFunc_GetClientListening, receiver, sender) */

#define fm_set_client_listen(%1,%2,%3) engfunc(EngFunc_SetClientListening, %1, %2, %3)
/* stock fm_set_client_listen(receiver, sender, listen)
	return engfunc(EngFunc_SetClientListening, receiver, sender, listen) */

stock fm_get_user_godmode(index) {
	new Float:val;
	pev(index, pev_takedamage, val);

	return (val == DAMAGE_NO);
}

stock fm_set_user_godmode(index, godmode = 0) {
	set_pev(index, pev_takedamage, godmode == 1 ? DAMAGE_NO : DAMAGE_AIM);

	return 1;
}

stock fm_set_user_armor(index, armor) {
	set_pev(index, pev_armorvalue, float(armor));

	return 1;
}

stock fm_set_user_health(index, health) {
	health > 0 ? set_pev(index, pev_health, float(health)) : dllfunc(DLLFunc_ClientKill, index);

	return 1;
}

stock fm_set_user_origin(index, /* const */ origin[3]) {
	new Float:orig[3];
	IVecFVec(origin, orig);

	return fm_entity_set_origin(index, orig);
}

stock fm_set_user_rendering(index, fx = kRenderFxNone, r = 255, g = 255, b = 255, render = kRenderNormal, amount = 16) {
	return fm_set_rendering(index, fx, r, g, b, render, amount);
}

stock fm_give_item(index, const item[]) {
	if (!equal(item, "weapon_", 7) && !equal(item, "ammo_", 5) && !equal(item, "item_", 5) && !equal(item, "tf_weapon_", 10))
		return 0;

	new ent = fm_create_entity(item);
	if (!pev_valid(ent))
		return 0;

	new Float:origin[3];
	pev(index, pev_origin, origin);
	set_pev(ent, pev_origin, origin);
	set_pev(ent, pev_spawnflags, pev(ent, pev_spawnflags) | SF_NORESPAWN);
	dllfunc(DLLFunc_Spawn, ent);

	new save = pev(ent, pev_solid);
	dllfunc(DLLFunc_Touch, ent, index);
	if (pev(ent, pev_solid) != save)
		return ent;

	engfunc(EngFunc_RemoveEntity, ent);

	return -1;
}

stock fm_set_user_maxspeed(index, Float:speed = -1.0) {
	engfunc(EngFunc_SetClientMaxspeed, index, speed);
	set_pev(index, pev_maxspeed, speed);

	return 1;
}

stock Float:fm_get_user_maxspeed(index) {
	new Float:speed;
	pev(index, pev_maxspeed, speed);

	return speed;
}

stock fm_set_user_gravity(index, Float:gravity = 1.0) {
	set_pev(index, pev_gravity, gravity);

	return 1;
}

stock Float:fm_get_user_gravity(index) {
	new Float:gravity;
	pev(index, pev_gravity, gravity);

	return gravity;
}

/* interferes with FM_Spawn enum, just use fm_DispatchSpawn
stock fm_spawn(entity) {
	return dllfunc(DLLFunc_Spawn, entity)
}
*/

stock fm_set_user_noclip(index, noclip = 0) {
	set_pev(index, pev_movetype, noclip == 1 ? MOVETYPE_NOCLIP : MOVETYPE_WALK);

	return 1;
}

#define fm_get_user_noclip(%1) (pev(%1, pev_movetype) == MOVETYPE_NOCLIP)
/* stock fm_get_user_noclip(index)
	return (pev(index, pev_movetype) == MOVETYPE_NOCLIP) */

// note: get_user_weapon will still return former weapon index
stock fm_strip_user_weapons(index) {
	new ent = fm_create_entity("player_weaponstrip");
	if (!pev_valid(ent))
		return 0;

	dllfunc(DLLFunc_Spawn, ent);
	dllfunc(DLLFunc_Use, ent, index);
	engfunc(EngFunc_RemoveEntity, ent);

	return 1;
}

stock fm_set_user_frags(index, frags) {
	set_pev(index, pev_frags, float(frags));

	return 1;
}


/* Cstrike functions */

stock fm_cs_user_spawn(index) {
	set_pev(index, pev_deadflag, DEAD_RESPAWNABLE);
	dllfunc(DLLFunc_Spawn, index);
	set_pev(index, pev_iuser1, 0);

	return 1;
}


/* Custom functions */

// based on Basic-Master's set_keyvalue, upgraded version accepts an optional classname (a bit more efficient if it is passed)
stock fm_set_kvd(entity, const key[], const value[], const classname[] = "") {
	if (classname[0])
		set_kvd(0, KV_ClassName, classname);
	else {
		new class[32];
		pev(entity, pev_classname, class, sizeof class - 1);
		set_kvd(0, KV_ClassName, class);
	}

	set_kvd(0, KV_KeyName, key);
	set_kvd(0, KV_Value, value);
	set_kvd(0, KV_fHandled, 0);

	return dllfunc(DLLFunc_KeyValue, entity, 0);
}

stock fm_find_ent_by_integer(index, pev_field, value) {
	static maxents;
	if (!maxents)
		maxents = global_get(glb_maxEntities);

	for (new i = index + 1; i < maxents; ++i) {
		if (pev_valid(i) && pev(i, pev_field) == value)
			return i;
	}

	return 0;
}

stock fm_find_ent_by_flags(index, pev_field, flags) {
	static maxents;
	if (!maxents)
		maxents = global_get(glb_maxEntities);

	for (new i = index + 1; i < maxents; ++i) {
		if (pev_valid(i) && (pev(i, pev_field) & flags) == flags)
			return i;
	}

	return 0;
}

stock Float:fm_distance_to_box(const Float:point[3], const Float:mins[3], const Float:maxs[3]) {
	new Float:dist[3];
	for (new i = 0; i < 3; ++i) {
		if (point[i] > maxs[i])
			dist[i] = point[i] - maxs[i];
		else if (mins[i] > point[i])
			dist[i] = mins[i] - point[i];
	}

	return vector_length(dist);
}

stock Float:fm_boxes_distance(const Float:mins1[3], const Float:maxs1[3], const Float:mins2[3], const Float:maxs2[3]) {
	new Float:dist[3];
	for (new i = 0; i < 3; ++i) {
		if (mins1[i] > maxs2[i])
			dist[i] = mins1[i] - maxs2[i];
		else if (mins2[i] > maxs1[i])
			dist[i] = mins2[i] - maxs1[i];
	}

	return vector_length(dist);
}

stock Float:fm_distance_to_boxent(entity, boxent) {
	new Float:point[3];
	pev(entity, pev_origin, point);

	new Float:mins[3], Float:maxs[3];
	pev(boxent, pev_absmin, mins);
	pev(boxent, pev_absmax, maxs);

	return fm_distance_to_box(point, mins, maxs);
}

stock Float:fm_boxents_distance(boxent1, boxent2) {
	new Float:mins1[3], Float:maxs1[3];
	pev(boxent1, pev_absmin, mins1);
	pev(boxent1, pev_absmax, maxs1);

	new Float:mins2[3], Float:maxs2[3];
	pev(boxent2, pev_absmin, mins2);
	pev(boxent2, pev_absmax, maxs2);

	return fm_boxes_distance(mins1, maxs1, mins2, maxs2);
}

// projects a center of a player's feet base (originally by P34nut, improved)
stock Float:fm_distance_to_floor(index, ignoremonsters = 1) {
	new Float:start[3], Float:dest[3], Float:end[3];
	pev(index, pev_origin, start);
	dest[0] = start[0];
	dest[1] = start[1];
	dest[2] = -8191.0;

	engfunc(EngFunc_TraceLine, start, dest, ignoremonsters, index, 0);
	get_tr2(0, TR_vecEndPos, end);

	pev(index, pev_absmin, start);
	new Float:ret = start[2] - end[2];

	return ret > 0 ? ret : 0.0;
}

// potential to crash (?) if used on weaponbox+weapon_* entity pair (use fm_remove_weaponbox instead)
stock fm_kill_entity(index) {
	set_pev(index, pev_flags, pev(index, pev_flags) | FL_KILLME);

	return 1;
}

// if weapon index isn't passed then assuming that it's the current weapon
stock fm_get_user_weapon_entity(id, wid = 0) {
	new weap = wid, clip, ammo;
	if (!weap && !(weap = get_user_weapon(id, clip, ammo)))
		return 0;
	
	new class[32];
	get_weaponname(weap, class, sizeof class - 1);

	return fm_find_ent_by_owner(-1, class, id);
}

// only weapon index or its name can be passed, if neither is passed then the current gun will be stripped
stock bool:fm_strip_user_gun(index, wid = 0, const wname[] = "") {
	new ent_class[32];
	if (!wid && wname[0])
		copy(ent_class, sizeof ent_class - 1, wname);
	else {
		new weapon = wid, clip, ammo;
		if (!weapon && !(weapon = get_user_weapon(index, clip, ammo)))
			return false;
		
		get_weaponname(weapon, ent_class, sizeof ent_class - 1);
	}

	new ent_weap = fm_find_ent_by_owner(-1, ent_class, index);
	if (!ent_weap)
		return false;

	engclient_cmd(index, "drop", ent_class);

	new ent_box = pev(ent_weap, pev_owner);
	if (!ent_box || ent_box == index)
		return false;

	dllfunc(DLLFunc_Think, ent_box);

	return true;
}

// only weapon index or its name can be passed, if neither is passed then the current gun will be transferred
stock bool:fm_transfer_user_gun(index1, index2, wid = 0, const wname[] = "") {
	new ent_class[32];
	if (!wid && wname[0])
		copy(ent_class, sizeof ent_class - 1, wname);
	else {
		new weapon = wid, clip, ammo;
		if (!weapon && !(weapon = get_user_weapon(index1, clip, ammo)))
			return false;
		
		get_weaponname(weapon, ent_class, sizeof ent_class - 1);
	}

	new ent_weap = fm_find_ent_by_owner(-1, ent_class, index1);
	if (!ent_weap)
		return false;

	engclient_cmd(index1, "drop", ent_class);

	new ent_box = pev(ent_weap, pev_owner);
	if (!ent_box || ent_box == index1)
		return false;

	set_pev(ent_box, pev_flags, pev(ent_box, pev_flags) | FL_ONGROUND);
	dllfunc(DLLFunc_Touch, ent_box, index2);
	if (pev(ent_weap, pev_owner) != index2)
		return false;

	return true;
}

stock bool:fm_is_ent_visible(index, entity, ignoremonsters = 0) {
	new Float:start[3], Float:dest[3];
	pev(index, pev_origin, start);
	pev(index, pev_view_ofs, dest);
	xs_vec_add(start, dest, start);

	pev(entity, pev_origin, dest);
	engfunc(EngFunc_TraceLine, start, dest, ignoremonsters, index, 0);

	new Float:fraction;
	get_tr2(0, TR_flFraction, fraction);
	if (fraction == 1.0 || get_tr2(0, TR_pHit) == entity)
		return true;

	return false;
}

// ported from AMXX's core get_user_origin(..., 3) (suggested by Greenberet)
stock fm_get_aim_origin(index, Float:origin[3]) {
	new Float:start[3], Float:view_ofs[3];
	pev(index, pev_origin, start);
	pev(index, pev_view_ofs, view_ofs);
	xs_vec_add(start, view_ofs, start);

	new Float:dest[3];
	pev(index, pev_v_angle, dest);
	engfunc(EngFunc_MakeVectors, dest);
	global_get(glb_v_forward, dest);
	xs_vec_mul_scalar(dest, 9999.0, dest);
	xs_vec_add(start, dest, dest);

	engfunc(EngFunc_TraceLine, start, dest, 0, index, 0);
	get_tr2(0, TR_vecEndPos, origin);

	return 1;
}

stock bool:fm_get_user_longjump(index) {
	new value[2];
	engfunc(EngFunc_GetPhysicsKeyValue, index, "slj", value, 1);
	switch (value[0]) {
		case '1': return true;
	}

	return false;
}

stock fm_set_user_longjump(index, bool:longjump = true, bool:tempicon = true) {
	if (longjump == fm_get_user_longjump(index))
		return;

	if (longjump) {
		engfunc(EngFunc_SetPhysicsKeyValue, index, "slj", "1");
		if (tempicon) {
			static msgid_itempickup;
			if (!msgid_itempickup)
				msgid_itempickup = get_user_msgid("ItemPickup");

			message_begin(MSG_ONE, msgid_itempickup, _, index);
			write_string("item_longjump");
			message_end();
		}
	}
	else
		engfunc(EngFunc_SetPhysicsKeyValue, index, "slj", "0");
}

#define WEAPON_SUIT 31

stock bool:fm_get_user_suit(index) {
	return bool:(!(!(pev(index, pev_weapons) & (1<<WEAPON_SUIT)))); // i'm not insane, this is a trick!
}

stock fm_set_user_suit(index, bool:suit = true, bool:sound = true) {
	new weapons = pev(index, pev_weapons);
	if (!suit)
		set_pev(index, pev_weapons, weapons & ~(1<<WEAPON_SUIT));
	else if (!(weapons & (1<<WEAPON_SUIT))) {
		set_pev(index, pev_weapons, weapons | (1<<WEAPON_SUIT));
		if (sound)
			emit_sound(index, CHAN_VOICE, "items/tr_kevlar.wav", VOL_NORM, ATTN_NORM, 0, PITCH_NORM);
	}
}

#define FEV_RELIABLE (1<<1)
#define FEV_GLOBAL (1<<2)

// removes all created decals and players' corpses from the world
// set a specific index to remove decals only for the given client
stock fm_cs_remove_decals(index = 0) {
	static eventindex_decal_reset;
	if (!eventindex_decal_reset)
		eventindex_decal_reset = engfunc(EngFunc_PrecacheEvent, 1, "events/decal_reset.sc");

	new flags = FEV_RELIABLE;
	if (!index)
		flags |= FEV_GLOBAL;

	engfunc(EngFunc_PlaybackEvent, flags, index, eventindex_decal_reset, 0.0, Float:{0.0, 0.0, 0.0}, Float:{0.0, 0.0, 0.0}, 0.0, 0.0, 0, 0, 0, 0);
}

// checks whether the entity's classname is equal to the passed classname
stock bool:fm_is_ent_classname(index, const classname[]) {
	if (!pev_valid(index))
		return false;

	new class[32];
	pev(index, pev_classname, class, sizeof class - 1);
	if (equal(class, classname))
		return true;

	return false;
}

// the same as AMXX's core user_kill but fixes the issue when the scoreboard doesn't update immediately if flag is set to 1
stock fm_user_kill(index, flag = 0) {
	if (flag) {
		new Float:frags;
		pev(index, pev_frags, frags);
		set_pev(index, pev_frags, ++frags);
	}

	dllfunc(DLLFunc_ClientKill, index);

	return 1;
}

// returns a degree angle between player-to-point and player's view vectors
stock Float:fm_get_view_angle_diff(index, const Float:point[3]) {
	new Float:vec[3], Float:ofs[3], Float:aim[3];
	pev(index, pev_origin, vec);
	pev(index, pev_view_ofs, ofs);
	xs_vec_add(vec, ofs, vec);
	xs_vec_sub(point, vec, vec);
	xs_vec_normalize(vec, vec);

	pev(index, pev_v_angle, aim);
	engfunc(EngFunc_MakeVectors, aim);
	global_get(glb_v_forward, aim);

	return xs_vec_angle(vec, aim);
}

// gets a weapon type of the linked to weaponbox weapon_* entity
stock fm_get_weaponbox_type(entity) {
	static max_clients, max_entities;
	if (!max_clients)
		max_clients = global_get(glb_maxClients);
	if (!max_entities)
		max_entities = global_get(glb_maxEntities);

	for (new i = max_clients + 1; i < max_entities; ++i) {
		if (pev_valid(i) && entity == pev(i, pev_owner)) {
			new wname[32];
			pev(i, pev_classname, wname, sizeof wname - 1);
			return get_weaponid(wname);
		}
	}

	return 0;
}

// safe removal of weaponbox+weapon_* entity pair (delay =~= 0.03 second)
#define fm_remove_weaponbox(%1) dllfunc(DLLFunc_Think, %1)
/* stock fm_remove_weaponbox(entity)
	return dllfunc(DLLFunc_Think, entity) */
