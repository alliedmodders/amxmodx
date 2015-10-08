// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#include "fakemeta_amxx.h"

/** Optimizations for Fakemeta.  In the end we'll do this for other things too.
 */
static int g_offset_table[pev_absolute_end] = {-1};

#define DO_OFFSET(offs)	g_offset_table[offs] = offsetof(entvars_t, offs)
#define DO_OFFSET_R(named, real, offs) g_offset_table[named] = offsetof(entvars_t, real) + offs

void initialze_offsets()
{
	DO_OFFSET(fixangle);
	DO_OFFSET(modelindex);
	DO_OFFSET(viewmodel);
	DO_OFFSET(weaponmodel);
	DO_OFFSET(movetype);
	DO_OFFSET(solid);
	DO_OFFSET(skin);
	DO_OFFSET(body);
	DO_OFFSET(effects);
	DO_OFFSET(light_level);
	DO_OFFSET(sequence);
	DO_OFFSET(gaitsequence);
	DO_OFFSET(rendermode);
	DO_OFFSET(renderfx);
	DO_OFFSET(weapons);
	DO_OFFSET(deadflag);
	DO_OFFSET(button);
	DO_OFFSET(impulse);
	DO_OFFSET(spawnflags);
	DO_OFFSET(flags);
	DO_OFFSET(colormap);
	DO_OFFSET(team);
	DO_OFFSET(waterlevel);
	DO_OFFSET(watertype);
	DO_OFFSET(playerclass);
	DO_OFFSET(weaponanim);
	DO_OFFSET(pushmsec);
	DO_OFFSET(bInDuck);
	DO_OFFSET(flTimeStepSound);
	DO_OFFSET(flSwimTime);
	DO_OFFSET(flDuckTime);
	DO_OFFSET(iStepLeft);
	DO_OFFSET(gamestate);
	DO_OFFSET(oldbuttons);
	DO_OFFSET(groupinfo);
	DO_OFFSET(iuser1);
	DO_OFFSET(iuser2);
	DO_OFFSET(iuser3);
	DO_OFFSET(iuser4);
	DO_OFFSET(impacttime);
	DO_OFFSET(starttime);
	DO_OFFSET(idealpitch);
	DO_OFFSET(ideal_yaw);
	DO_OFFSET(pitch_speed);
	DO_OFFSET(yaw_speed);
	DO_OFFSET(ltime);
	DO_OFFSET(nextthink);
	DO_OFFSET(gravity);
	DO_OFFSET(friction);
	DO_OFFSET(frame);
	DO_OFFSET(animtime);
	DO_OFFSET(framerate);
	DO_OFFSET(scale);
	DO_OFFSET(renderamt);
	DO_OFFSET(health);
	DO_OFFSET(frags);
	DO_OFFSET(takedamage);
	DO_OFFSET(max_health);
	DO_OFFSET(teleport_time);
	DO_OFFSET(armortype);
	DO_OFFSET(armorvalue);
	DO_OFFSET(dmg_take);
	DO_OFFSET(dmg_save);
	DO_OFFSET(dmg);
	DO_OFFSET(dmgtime);
	DO_OFFSET(speed);
	DO_OFFSET(air_finished);
	DO_OFFSET(pain_finished);
	DO_OFFSET(radsuit_finished);
	DO_OFFSET(maxspeed);
	DO_OFFSET(fov);
	DO_OFFSET(flFallVelocity);
	DO_OFFSET(fuser1);
	DO_OFFSET(fuser2);
	DO_OFFSET(fuser3);
	DO_OFFSET(fuser4);
	DO_OFFSET(classname);
	DO_OFFSET(globalname);
	DO_OFFSET(model);
	DO_OFFSET(target);
	DO_OFFSET(targetname);
	DO_OFFSET(netname);
	DO_OFFSET(message);
	DO_OFFSET(noise);
	DO_OFFSET(noise1);
	DO_OFFSET(noise2);
	DO_OFFSET(noise3);
	DO_OFFSET(chain);
	DO_OFFSET(dmg_inflictor);
	DO_OFFSET(enemy);
	DO_OFFSET(aiment);
	DO_OFFSET(owner);
	DO_OFFSET(groundentity);
	DO_OFFSET(euser1);
	DO_OFFSET(euser2);
	DO_OFFSET(euser3);
	DO_OFFSET(euser4);
	DO_OFFSET(origin);
	DO_OFFSET(oldorigin);
	DO_OFFSET(velocity);
	DO_OFFSET(basevelocity);
	DO_OFFSET(clbasevelocity);
	DO_OFFSET(movedir);
	DO_OFFSET(angles);
	DO_OFFSET(avelocity);
	DO_OFFSET(v_angle);
	DO_OFFSET(endpos);
	DO_OFFSET(startpos);
	DO_OFFSET(absmin);
	DO_OFFSET(absmax);
	DO_OFFSET(mins);
	DO_OFFSET(maxs);
	DO_OFFSET(size);
	DO_OFFSET(rendercolor);
	DO_OFFSET(view_ofs);
	DO_OFFSET(vuser1);
	DO_OFFSET(vuser2);
	DO_OFFSET(vuser3);
	DO_OFFSET(vuser4);
	DO_OFFSET(punchangle);
	DO_OFFSET(controller);
	DO_OFFSET_R(controller_0, controller, 0);
	DO_OFFSET_R(controller_1, controller, 1);
	DO_OFFSET_R(controller_2, controller, 2);
	DO_OFFSET_R(controller_3, controller, 3);
	DO_OFFSET(blending);
	DO_OFFSET_R(blending_0, blending, 0);
	DO_OFFSET_R(blending_1, blending, 1);
	DO_OFFSET_R(pev_weaponmodel2, weaponmodel, 0);
	DO_OFFSET_R(pev_viewmodel2, viewmodel, 0);
	DO_OFFSET(pContainingEntity);
}

#define EDICT_OFFS(v,o) ((char *)v + o)

// originally by mahnsawce
static cell AMX_NATIVE_CALL amx_pev(AMX *amx,cell *params)
{
	int index = params[1];
	CHECK_ENTITY(index);
	edict_t *pEdict = TypeConversion.id_to_edict(index);
	int iSwitch = params[2];

	//onto normal cases - sanity check
	if (iSwitch <= pev_string_start || iSwitch >= pev_absolute_end)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined pev index: %d", iSwitch);
		return 0;
	}

	int offs = g_offset_table[iSwitch];

	//sanity check #2
	if (offs == -1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined pev index: %d", iSwitch);
		return 0;
	}

	enum
	{
		Ret_Int = (1<<0),
		Ret_Float = (1<<1),
		Ret_Vec = (1<<2),
		Ret_ByteArray = (1<<3),
		Ret_String = (1<<4),
		Ret_Edict = (1<<5),
		Ret_Bytes2 = (1<<6),
		Ret_Bytes4 = (1<<7)
	};

	union
	{
		int i;
		float f;
		byte b;
		string_t s;
		byte ba[4];
	} rets;
	Vector vr;

	int ValType = 0;

	entvars_t *v = &(pEdict->v);

	//get primitive data types
    if (iSwitch > pev_int_start && iSwitch < pev_int_end)
	{
		rets.i = *(int *)EDICT_OFFS(v, offs);
		ValType = Ret_Int;
	} else if (iSwitch > pev_float_start && iSwitch < pev_float_end) {
		rets.f = *(float *)EDICT_OFFS(v, offs);
		ValType = Ret_Float;
	} else if (iSwitch > pev_vecarray_start && iSwitch < pev_vecarray_end) {
		vr = *(vec3_t *)EDICT_OFFS(v, offs);
		ValType = Ret_Vec;
	} else if (iSwitch > pev_bytearray_start && iSwitch < pev_bytearray_end) {
		if (iSwitch == controller)
		{
			rets.ba[0] = v->controller[0];
			rets.ba[1] = v->controller[1];
			rets.ba[2] = v->controller[2];
			rets.ba[3] = v->controller[3];
			ValType = Ret_Bytes4;
		} else {
			rets.ba[0] = v->blending[0];
			rets.ba[1] = v->blending[1];
			ValType = Ret_Bytes2;
		}
	} else if (iSwitch > pev_byte_start && iSwitch < pev_byte_end) {
		rets.b = *(byte *)EDICT_OFFS(v, offs);
		ValType = Ret_Int;
	} else if ( (iSwitch > pev_string_start && iSwitch < pev_string_end)
				|| (iSwitch > pev_string2_begin && iSwitch < pev_string2_end) ) {
		rets.s = *(string_t *)EDICT_OFFS(v, offs);
		ValType = Ret_String;
	} else if ( (iSwitch > pev_edict_start && iSwitch < pev_edict_end)
				|| (iSwitch > pev_edict2_start && iSwitch < pev_absolute_end) ) {
		edict_t *e = *(edict_t **)EDICT_OFFS(v, offs);
		rets.i = ENTINDEX(e);
		ValType = Ret_Int;
		ValType |= Ret_Edict;
	}

	size_t count = params[0] / sizeof(cell) - 2;

	if (count == 0)
	{
		//return an int
		if (ValType & Ret_Int)
		{
			return rets.i;
		} else if (ValType == Ret_Float) {
			return (cell)rets.f;
		} else if (ValType == Ret_String) {
			return (cell)rets.s;
		} else {
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
			return 0;
		}
	} else if (count == 1) {
		//return a byref float - usually
		cell *addr = MF_GetAmxAddr(amx, params[3]);
		if (ValType == Ret_Float)
		{
			*addr = amx_ftoc(rets.f);
		} else if (ValType == Ret_Int) {
			REAL f = (REAL)rets.i;
			*addr = amx_ftoc(f);
		} else if (ValType == Ret_Vec) {
			addr[0] = amx_ftoc(vr.x);
			addr[1] = amx_ftoc(vr.y);
			addr[2] = amx_ftoc(vr.z);
		} else if (ValType == Ret_Bytes2) {
			addr[0] = rets.ba[0];
			addr[1] = rets.ba[1];
		} else if (ValType == Ret_Bytes4) {
			addr[0] = rets.ba[0];
			addr[1] = rets.ba[1];
			addr[2] = rets.ba[2];
			addr[3] = rets.ba[3];
		} else {
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
			return 0;
		}
		return 1;
	} else if (count == 2) {
		cell size = *(MF_GetAmxAddr(amx, params[4]));
		if (ValType == Ret_String)
		{
			const char *str = STRING(rets.s);
			if (!str)
				str = "";
			int num = MF_SetAmxString(amx, params[3], str, size);
			return num;
		} else if (ValType & Ret_Int) {
			char temp[32];
			ke::SafeSprintf(temp, sizeof(temp), "%d", rets.i);
			return MF_SetAmxString(amx, params[3], temp, size);
		} else if (ValType == Ret_Float) {
			char temp[32];
			ke::SafeSprintf(temp, sizeof(temp), "%f", rets.f);
			return MF_SetAmxString(amx, params[3], temp, size);
		} else if (ValType == Ret_Vec) {
			char temp[32];
			ke::SafeSprintf(temp, sizeof(temp), "%f %f %f", vr.x, vr.y, vr.z);
			return MF_SetAmxString(amx, params[3], temp, size);
		} else if (ValType == Ret_Bytes2) {
			char temp[32];
			ke::SafeSprintf(temp, sizeof(temp), "%d %d", rets.ba[0], rets.ba[1]);
			return MF_SetAmxString(amx, params[3], temp, size);
		} else if (ValType == Ret_Bytes4) {
			char temp[32];
			ke::SafeSprintf(temp, sizeof(temp), "%d %d %d %d", rets.ba[0], rets.ba[1], rets.ba[2], rets.ba[3]);
			return MF_SetAmxString(amx, params[3], temp, size);
		}

		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
	} else if (count == 3) {
		cell size = *(MF_GetAmxAddr(amx, params[5]));
		if (ValType == Ret_String)
		{
			const char *str = STRING(rets.s);
			cell *addr = MF_GetAmxAddr(amx, params[3]);
			*addr = (cell)rets.s;
			if (!str)
				str = "";
			int num = MF_SetAmxString(amx, params[4], str, size);
			return num;
		}

		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
	}

	//if we got here, something happened
	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown pev index or return combination %d", iSwitch);

	return 0;
}

static cell AMX_NATIVE_CALL amx_set_pev(AMX *amx, cell *params)
{
	// index, pevdata
	int index = params[1];
	CHECK_ENTITY(index);
	edict_t *pEdict = TypeConversion.id_to_edict(index);
	int iSwitch = params[2];

	//onto normal cases - sanity check
	if (iSwitch <= pev_string_start || iSwitch >= pev_absolute_end)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined pev index: %d", iSwitch);
		return 0;
	}

	int offs = g_offset_table[iSwitch];

	//sanity check #2
	if (offs == -1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined pev index: %d", iSwitch);
		return 0;
	}

	cell *blah = MF_GetAmxAddr(amx,params[3]);
	entvars_t *v = &(pEdict->v);

	if (iSwitch > pev_int_start && iSwitch < pev_int_end)
	{
		*(int *)EDICT_OFFS(v, offs) = (int)*blah;
	} else if (iSwitch > pev_float_start && iSwitch < pev_float_end) {
		*(float *)EDICT_OFFS(v, offs) = (float)amx_ctof(blah[0]);
	} else if ( (iSwitch > pev_string_start && iSwitch < pev_string_end)
				|| (iSwitch > pev_string2_begin && iSwitch < pev_string2_end) ) {
		int len;
		char *string = MF_GetAmxString(amx, params[3], 0, &len);
		string_t value = ALLOC_STRING(string);
		*(string_t *)EDICT_OFFS(v, offs) = value;
	} else if ( (iSwitch > pev_edict_start && iSwitch < pev_edict_end)
				|| (iSwitch > pev_edict2_start && iSwitch < pev_absolute_end) ) {
		edict_t *e = TypeConversion.id_to_edict((int)*blah);
		*(edict_t **)EDICT_OFFS(v, offs) = e;
	} else if (iSwitch > pev_vecarray_start && iSwitch < pev_vecarray_end) {
		vec3_t vec;
		vec[0] = amx_ctof(blah[0]);
		vec[1] = amx_ctof(blah[1]);
		vec[2] = amx_ctof(blah[2]);
		*(vec3_t *)EDICT_OFFS(v, offs) = vec;
	} else if (iSwitch > pev_byte_start && iSwitch < pev_byte_end) {
		byte b = static_cast<byte>(blah[0]);
		*(byte *)EDICT_OFFS(v, offs) = b;
	} else if (iSwitch > pev_bytearray_start && iSwitch < pev_bytearray_end) {
        switch(iSwitch)
		{
			case controller:
				pEdict->v.controller[0]=blah[0];
				pEdict->v.controller[1]=blah[1];
				pEdict->v.controller[2]=blah[2];
				pEdict->v.controller[3]=blah[3];
				return 1;
			case blending:
				pEdict->v.blending[0]=blah[0];
				pEdict->v.blending[1]=blah[1];
				return 1;
		}
	}
	
	return 0;
}
static cell AMX_NATIVE_CALL amx_set_pev_string(AMX *amx, cell *params)
{
	// index, pevdata
	int index = params[1];
	CHECK_ENTITY(index);
	edict_t *pEdict = TypeConversion.id_to_edict(index);
	int iSwitch = params[2];

	//onto normal cases - sanity check
	if (iSwitch <= pev_string_start || iSwitch >= pev_absolute_end)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined pev index: %d", iSwitch);
		return 0;
	}

	int offs = g_offset_table[iSwitch];

	//sanity check #2
	if (offs == -1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined pev index: %d", iSwitch);
		return 0;
	}

	entvars_t *v = &(pEdict->v);

	if ( (iSwitch > pev_string_start && iSwitch < pev_string_end)
				|| (iSwitch > pev_string2_begin && iSwitch < pev_string2_end) ) 
	{
		*(string_t *)EDICT_OFFS(v, offs) = params[3];
	}
	else 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Non-string field passed to set_pev_string!");
		return 0;
	}
	
	return 0;
}

static cell AMX_NATIVE_CALL amx_pev_valid(AMX *amx, cell *params)
{
	int idx = static_cast<int>(params[1]);

	edict_t *e = TypeConversion.id_to_edict(idx);

	if (FNullEnt(e))
		return 0;

	if (e->pvPrivateData)
		return 2;

	return 1;
}
static cell AMX_NATIVE_CALL amx_pev_serial(AMX* amx, cell* params)
{
	int id = static_cast<int>(params[1]);

	CHECK_ENTITY(id);
	edict_t* ent = TypeConversion.id_to_edict(id);

	return ent->serialnumber;
}
AMX_NATIVE_INFO pev_natives[] = {
	{ "pev",			amx_pev },
	{ "set_pev",		amx_set_pev },
	{ "set_pev_string",	amx_set_pev_string },
	{ "pev_valid",		amx_pev_valid },
	{ "pev_serial",		amx_pev_serial },
	{NULL,				NULL},
};
