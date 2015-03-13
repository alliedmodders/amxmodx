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

static int g_glob_offset_table[glb_end_pchar] = {-1};

#define DO_OFFSET_GLB(offset) g_glob_offset_table[offset] = offsetof(globalvars_t, offset)

void initialize_glb_offsets()
{
	DO_OFFSET_GLB(trace_hitgroup);
	DO_OFFSET_GLB(trace_flags);
	DO_OFFSET_GLB(msg_entity);
	DO_OFFSET_GLB(cdAudioTrack);
	DO_OFFSET_GLB(maxClients);
	DO_OFFSET_GLB(maxEntities);
	g_glob_offset_table[gl_time] = offsetof(globalvars_t, time);
	DO_OFFSET_GLB(frametime);
	DO_OFFSET_GLB(force_retouch);
	DO_OFFSET_GLB(deathmatch);
	DO_OFFSET_GLB(coop);
	DO_OFFSET_GLB(teamplay);
	DO_OFFSET_GLB(serverflags);
	DO_OFFSET_GLB(found_secrets);
	DO_OFFSET_GLB(trace_allsolid);
	DO_OFFSET_GLB(trace_startsolid);
	DO_OFFSET_GLB(trace_fraction);
	DO_OFFSET_GLB(trace_plane_dist);
	DO_OFFSET_GLB(trace_inopen);
	DO_OFFSET_GLB(trace_inwater);
	DO_OFFSET_GLB(trace_ent);
	DO_OFFSET_GLB(v_forward);
	DO_OFFSET_GLB(v_up);
	DO_OFFSET_GLB(v_right);
	DO_OFFSET_GLB(trace_endpos);
	DO_OFFSET_GLB(trace_plane_normal);
	DO_OFFSET_GLB(vecLandmarkOffset);
	DO_OFFSET_GLB(mapname);
	DO_OFFSET_GLB(startspot);
	DO_OFFSET_GLB(pStringBase);
}

#define GET_OFFS(v,o) ((char *)v + o)

static cell AMX_NATIVE_CALL amx_glb(AMX *amx, cell *params)
{
	int iSwitch = params[1];

	if (iSwitch <= glb_start_int || iSwitch >= glb_end_pchar)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global index: %d", iSwitch);
		return 0;
	}

	int offset = g_glob_offset_table[iSwitch];
	if (offset == -1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Undefined global index: %d", iSwitch);
		return 0;
	}

	enum
	{
		Ret_Int = (1<<0),
		Ret_Float = (1<<1),
		Ret_Vec = (1<<2),
		Ret_Edict = (1<<3), 
		Ret_PChar = (1<<4)
	};

	union
	{
		int i;
		float f;
		const char *c;
	} rets;
	Vector vec;

	rets.i = 0;
	int Valtype = 0;

	if (iSwitch > glb_start_int && iSwitch < glb_end_int)
	{
		rets.i = *(int *)GET_OFFS(gpGlobals, offset);
		Valtype = Ret_Int;
	}
	else if (iSwitch > glb_start_float && iSwitch < glb_end_float)
	{
		rets.f = *(float *)GET_OFFS(gpGlobals, offset);
		Valtype = Ret_Float;
	}
	else if (iSwitch > glb_start_edict && iSwitch < glb_end_edict)
	{
		edict_t *e = *(edict_t **)GET_OFFS(gpGlobals, offset);
		rets.i = ENTINDEX(e);
		Valtype = Ret_Int | Ret_Edict;
	}
	else if (iSwitch > glb_start_vector && iSwitch < glb_end_vector)
	{
		vec = *(vec3_t *)GET_OFFS(gpGlobals, offset);
		Valtype = Ret_Vec;
	}
	else if (iSwitch > glb_start_string && iSwitch < glb_end_string)
	{
		rets.c = STRING(*(string_t *)GET_OFFS(gpGlobals, offset));
		Valtype = Ret_PChar;
	}
	else if (iSwitch > glb_start_pchar && iSwitch < glb_end_pchar)
	{
		rets.c = *(const char **)GET_OFFS(gpGlobals, offset);
		Valtype = Ret_PChar;
	}

	size_t paramnum = params[0] / sizeof(cell) - 1;

	if (paramnum == 0)
	{
		//return an int
		if (Valtype & Ret_Int)
		{
			return rets.i;
		} else {
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
			return 0;
		}
	}
	else if (paramnum == 1)
	{
		//return a byref float - usually
		cell *addr = MF_GetAmxAddr(amx, params[2]);
		if (Valtype == Ret_Float)
		{
			*addr = amx_ftoc(rets.f);
		}
		else if (Valtype == Ret_Vec)
		{
			addr[0] = amx_ftoc(vec.x);
			addr[1] = amx_ftoc(vec.y);
			addr[2] = amx_ftoc(vec.z);
		} else {
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
			return 0;
		}
		return 1;
	}
	else if (paramnum == 2)
	{
		cell size = *(MF_GetAmxAddr(amx, params[3]));
		if (Valtype == Ret_PChar)
		{
			const char *str = rets.c;
			if (!str)
				str = "";
			return MF_SetAmxString(amx, params[2], str, size);
		}
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
	}
	else if (paramnum == 3)
	{
		cell size = *(MF_GetAmxAddr(amx, params[4]));
		if (Valtype == Ret_PChar)
		{
			cell *str = MF_GetAmxAddr(amx, params[2]);
			return MF_SetAmxString(amx, params[3], STRING((int)*str), size);
		}
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return type");
	}

	//if we got here, something happened
	MF_LogError(amx, AMX_ERR_NATIVE, "Unknown global index or return combination %d", iSwitch);

	return 0;
}

AMX_NATIVE_INFO glb_natives[] = 
{
	{"global_get",		amx_glb}, 
	{NULL,				NULL},
};
