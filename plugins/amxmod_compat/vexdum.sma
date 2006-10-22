/**
 * AMX Mod Compatibility engine
 *  by the AMX Mod X Development Team
 */

#include <VexdUM_const>
#include <VexdUM_stock>

/* Forwards */
new g_FwdTouch
new g_FwdThink
new g_FwdSpawn
new g_FwdClientPreThink
new g_FwdClientPostThink
new g_FwdEmitSound
new g_FwdEmitAmbientSound
new g_FwdSetModel
new g_FwdTraceLine
new g_FwdSetCliKeyValue
new g_FwdKeyValue
new g_PlayerModels[33][64]
new g_PlayerModeled[33]

new g_LastTrace = 0

VexdUM_Register()
{
	/* Fakemeta Hooks */
	register_forward(FM_EmitSound, 				"Hook_FM_EmitSound")
	register_forward(FM_EmitAmbientSound,		"Hook_FM_EmitAmbientSound")
	register_forward(FM_SetModel, 				"Hook_FM_SetModel")
	register_forward(FM_TraceLine, 				"Hook_FM_TraceLine")
	register_forward(FM_SetClientKeyValue,		"Hook_FM_SetClientKeyValue")
	register_forward(FM_KeyValue, 				"Hook_FM_KeyValue")
	register_forward(FM_Touch, 					"Hook_FM_Touch")
	register_forward(FM_Think,					"Hook_FM_Think")
	register_forward(FM_Spawn,					"Hook_FM_Spawn")
	register_forward(FM_PlayerPreThink,			"Hook_FM_PlayerPreThink")
	register_forward(FM_PlayerPostThink,		"Hook_FM_PlayerPostThink")
	register_forward(FM_ClientUserInfoChanged,	"Hook_ClientUserInfoChanged")
	
	/* Global Forwards */
	g_FwdTouch = 			CreateMultiForwardEx("entity_touch", 		ET_STOP, FORWARD_ONLY_OLD, FP_CELL, FP_CELL)
	g_FwdThink = 			CreateMultiForwardEx("entity_think", 		ET_STOP, FORWARD_ONLY_OLD, FP_CELL)
	g_FwdSpawn = 			CreateMultiForwardEx("entity_spawn", 		ET_STOP, FORWARD_ONLY_OLD, FP_CELL)
	g_FwdClientPreThink = 	CreateMultiForwardEx("client_prethink", 	ET_IGNORE, FORWARD_ONLY_OLD, FP_CELL)
	g_FwdClientPostThink =	CreateMultiForwardEx("client_postthink", 	ET_IGNORE, FORWARD_ONLY_OLD, FP_CELL)
	g_FwdEmitSound = 		CreateMultiForwardEx("emitsound", 			ET_STOP, FORWARD_ONLY_OLD, FP_CELL, FP_STRING)
	g_FwdEmitAmbientSound = CreateMultiForwardEx("emitambientsound",	ET_STOP, FORWARD_ONLY_OLD, FP_CELL, FP_STRING)
	g_FwdSetModel = 		CreateMultiForwardEx("set_model", 			ET_STOP, FORWARD_ONLY_OLD, FP_CELL, FP_STRING)
	g_FwdTraceLine = 		CreateMultiForwardEx("traceline", 			ET_STOP, FORWARD_ONLY_OLD, FP_CELL)
	g_FwdSetCliKeyValue =	CreateMultiForwardEx("setclientkeyvalue",	ET_STOP, FORWARD_ONLY_OLD, FP_CELL, FP_STRING, FP_STRING)
	g_FwdKeyValue = 		CreateMultiForwardEx("keyvalue", 			ET_STOP, FORWARD_ONLY_OLD, FP_CELL)
}

VexdUM_Natives()
{
	/* implicit compatibility */
	register_native("is_entity", 			"__is_entity")
	register_native("find_entity", 			"__find_entity")
	register_native("find_entity_sphere",	"__find_entity_sphere")
	register_native("in_view_cone",			"__in_view_cone")
	register_native("get_offset_int",		"__get_offset_int")
	register_native("set_offset_int",		"__set_offset_int")
	register_native("trace_line",			"__trace_line")
	register_native("traceline_get_int",	"__traceline_get_int")
	register_native("traceline_set_int",	"__traceline_set_int")
	register_native("traceline_get_edict",	"__traceline_get_edict")
	register_native("traceline_set_edict",	"__traceline_set_edict")
	register_native("traceline_set_float",	"__traceline_set_float")
	register_native("can_see",				"__can_see")
	register_native("user_spawn",			"__user_spawn")
	register_native("get_maxentities",		"__get_maxentities")
	register_native("PointContents",		"__PointContents")
	register_native("DispatchKeyValue",		"__DispatchKeyValue")
	
	if (g_ModType == MOD_CSTRIKE)
	{
		register_native("set_user_model",	"__cs_set_user_model")
	} else {
		register_native("set_user_model",	"__set_user_model")
	}
}

VexdUM_ClientConnect(id)
{
	g_PlayerModels[id][0] = 0
	g_PlayerModeled[id] =0
}

SetClientKeyValue(id, const key[], const value[])
{
	new buffer = engfunc(EngFunc_GetInfoKeyBuffer, id)
	
	return engfunc(EngFunc_SetClientKeyValue, buffer, key, value)
}

GetClientKeyValue(id, const key[], value[], maxlen)
{
	new buffer = engfunc(EngFunc_GetInfoKeyBuffer, id)
	
	engfunc(EngFunc_InfoKeyValue, buffer, key, value, maxlen)
}

public __is_entity(plid, num)
{
	new ent = get_param(1)
	return is_entity(ent)
}

public __find_entity(plid, num)
{
	static entstr[256]
	new startEnt, type
	
	startEnt = get_param(1)
	get_string(2, entstr, 255)
	type = get_param(3)
	
	return find_entity(startEnt, entstr, type)
}

public __find_entity_sphere(plid, num)
{
	new ent
	new Float:orig[3]
	new Float:radius
	
	ent = get_param(1)
	get_array_f(2, orig, 3)
	radius = get_param_f(3)
	
	return find_entity_sphere(ent, orig, radius)
}

public __in_view_cone(plid, num)
{
	new ent
	new Float:orig[3]
	
	ent = get_param(1)
	get_array_f(2, orig, 3)
	
	return in_view_cone(ent, orig)
}

public __get_offset_int(plid, num)
{
	new ent = get_param(1)
	new offs = get_param(2)
	new linux = get_param(3)
	
	return get_pdata_int(ent, offs, linux)
}

public __set_offset_int(plid, num)
{
	return set_offset_int(get_param(1), get_param(2), get_param(3), get_param(4))
}

public __trace_line(plid, num)
{	
	new ent = get_param(1)
	
	new Float:vStart[3], Float:vEnd[3], Float:vReturn[3]
	
	get_array_f(2, vStart, 3)
	get_array_f(3, vEnd, 3)
	
	if (ent == FM_NULLENT)
		engfunc(EngFunc_TraceLine, vStart, vEnd, IGNORE_MONSTERS, 0, 0)
	else
		engfunc(EngFunc_TraceLine, vStart, vEnd, DONT_IGNORE_MONSTERS, ent, 0)
		
	get_tr2(0, TraceResult:TR_vecEndPos, vReturn)
	
	set_array_f(4, vReturn, 3)
	
	new traceHit = get_tr2(0, TraceResult:TR_pHit)
	
	if (!pev_valid(traceHit))
		return FM_NULLENT
		
	return traceHit
}

public __traceline_get_int(plid, num)
{
	new iSet = get_param(1)
	new iValue = 0
	
	switch (iSet)
	{
		case TR_INT_fAllSolid:
			iValue = get_tr2(g_LastTrace, TraceResult:TR_AllSolid)
		case TR_INT_fStartSolid:
			iValue = get_tr2(g_LastTrace, TraceResult:TR_StartSolid)
		case TR_INT_fInOpen:
			iValue = get_tr2(g_LastTrace, TraceResult:TR_InOpen)
		case TR_INT_fInWater:
			iValue = get_tr2(g_LastTrace, TraceResult:TR_InWater)
		case TR_INT_iHitgroup:
			iValue = get_tr2(g_LastTrace, TraceResult:TR_iHitgroup)
		default:
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
	}
	
	return iValue
}

public __traceline_set_int(plid, num)
{
	new iSet = get_param(1)
	new iValue = get_param(2)
	
	switch (iSet)
	{
		case TR_INT_fAllSolid:
			set_tr2(g_LastTrace, TraceResult:TR_AllSolid, iValue)
		case TR_INT_fStartSolid:
			set_tr2(g_LastTrace, TraceResult:TR_StartSolid, iValue)
		case TR_INT_fInOpen:
			set_tr2(g_LastTrace, TraceResult:TR_InOpen, iValue)
		case TR_INT_fInWater:
			set_tr2(g_LastTrace, TraceResult:TR_InWater, iValue)
		case TR_INT_iHitgroup:
			set_tr2(g_LastTrace, TraceResult:TR_iHitgroup, iValue)
		default:
		{
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
			return 0
		}
	}
	
	return 1
}

public __traceline_get_edict(plid, num)
{
	new iSet = get_param(1)
	new iValue = 0
	
	switch (iSet)
	{
		case TR_ENT_pHit:
			iValue = get_tr2(g_LastTrace, TraceResult:TR_pHit)
		default:
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
	}
	
	return iValue
}

public __traceline_set_edict(plid, num)
{
	new iSet = get_param(1)
	new iValue = get_param(2)
	
	switch (iSet)
	{
		case TR_ENT_pHit:
			set_tr2(g_LastTrace, TraceResult:TR_pHit, iValue)
		default:
		{
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
			return 0
		}
	}
	
	return 1
}

public Float:__traceline_get_float(plid, num)
{
	new iSet = get_param(1)
	new Float:fValue = 0.0
	
	switch (iSet)
	{
		case TR_FL_flFraction:
			get_tr2(g_LastTrace, TraceResult:TR_flFraction, fValue)
		case TR_FL_flPlaneDist:
			get_tr2(g_LastTrace, TraceResult:TR_flPlaneDist, fValue)
		default:
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
	}
	
	return fValue
}

public __traceline_set_float(plid, num)
{
	new iSet = get_param(1)
	new Float:fValue = get_param_f(2)
	
	switch (iSet)
	{
		case TR_FL_flFraction:
			set_tr2(g_LastTrace, TraceResult:TR_flFraction, fValue)
		case TR_FL_flPlaneDist:
			get_tr2(g_LastTrace, TraceResult:TR_flPlaneDist, fValue)
		default:
		{
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
			return 0
		}
	}
	
	return 1
}

public __traceline_get_vector(plid, num)
{
	new iSet = get_param(1)
	new Float:vValue[3]
	
	switch (iSet)
	{
		case TR_VEC_vecEndPos:
			get_tr2(g_LastTrace, TraceResult:TR_vecEndPos, vValue)
		case TR_VEC_vecPlaneNormal:
			get_tr2(g_LastTrace, TraceResult:TR_vecPlaneNormal, vValue)
		default:
		{
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
			return 0
		}
	}
	
	set_array_f(2, vValue, 3)
	
	return 1
}

public __traceline_set_vector(plid, num)
{
	new iSet = get_param(1)
	new Float:vValue[3]
	
	get_array_f(2, vValue, 3)
	
	switch (iSet)
	{
		case TR_VEC_vecEndPos:
			set_tr2(g_LastTrace, TraceResult:TR_vecEndPos, vValue)
		case TR_VEC_vecPlaneNormal:
			set_tr2(g_LastTrace, TraceResult:TR_vecPlaneNormal, vValue)
		default:
		{
			log_error(AMX_ERR_NATIVE, "Invalid TR_ parameter")
			return 0
		}
	}
	
	return 1
}

public __can_see(plid, num)
{
	return can_see(get_param(1), get_param(2))
}

public __user_spawn(plid, num)
{
	return dllfunc(DLLFunc_Spawn, get_param(1))
}

public __set_user_model(plid, num)
{
	new id = get_param(1)
	if (id < 1 || id > g_MaxPlayers)
	{
		return 0
	}
	
	new model[64]
	get_string(2, model, 63)
	if (model[0] == 0)
	{
		if (!g_PlayerModeled[id])
		{
			return 0
		}
		g_PlayerModeled[id] = 0
		g_PlayerModels[id][0] = 0
		dllfunc(DLLFunc_ClientUserInfoChanged, id)
	} else {
		copy(g_PlayerModels[id], 63, model)
		g_PlayerModeled[id] = 1
		SetClientKeyValue(id, "model", model)
	}
	
	return 1
}

public __cs_set_user_model(plid, num)
{
	new id = get_param(1)
	new model[64]
	
	get_string(2, model, 63)
	
	return cs_set_user_model(id, model)
}

public __get_maxentities(plid, num)
{
	return get_maxentities()
}

public __PointContents(plid, num)
{
	new Float:vCheckAt[3]
	
	get_array_f(1, vCheckAt, 3)
	
	return point_contents(vCheckAt)
}

public __DispatchKeyValue(plid, num)
{
	new ent = get_param(1)
	
	new szClassname[32], szKey[32], szValue[32]
	
	if (pev_valid(ent))
	{
		get_string(2, szKey, 31)
		get_string(3, szValue, 31)
		pev(ent, pev_classname, szClassname, 31)
		
		set_kvd(0, KV_ClassName, szClassname)
		set_kvd(0, KV_KeyName, szKey)
		set_kvd(0, KV_Value, szValue)
		set_kvd(0, KV_fHandled, 0)
		
		dllfunc(DLLFunc_KeyValue, ent, 0)
	}
	
	return 1
}


/*********************************
 ***** HOOKS *********************
 *********************************/
 
public Hook_ClientUserInfoChanged(id, buffer)
{
	if (g_PlayerModeled[id] && (pev(id, pev_deadflag) == DEAD_NO))
	{
		return FMRES_SUPERCEDE
	}
	
	return FMRES_IGNORED
}

public Hook_FM_EmitSound(entid, channel, const sample[]) //we don't care about the rest
{
	new ret
	
	ExecuteForward(g_FwdEmitSound, ret, entid, sample)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_EmitAmbientSound(entid, Float:pos[3], const sample[]) //we don't care about the rest
{
	new ret
	
	ExecuteForward(g_FwdEmitAmbientSound, ret, entid, sample)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_SetModel(entid, const model[])
{
	new ret
	
	ExecuteForward(g_FwdSetModel, ret, entid, model)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_TraceLine(Float:v1[3], Float:v2[3], noMonsters, skip_ent, ptr)
{
	g_LastTrace = ptr
	
	engfunc(EngFunc_TraceLine, v1, v2, noMonsters, skip_ent, ptr)
	
	new ret
	
	ExecuteForward(g_FwdTraceLine, ret, skip_ent)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_SetClientKeyValue(id, const infobuffer[], const key[], const value[])
{
	new ret
	
	ExecuteForward(g_FwdSetCliKeyValue, ret, id, key, value)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_KeyValue(ent, kvd)
{
	new ret
	
	ExecuteForward(g_FwdKeyValue, ret, ent)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_Touch(ent1, ent2)
{
	new ret
	
	ExecuteForward(g_FwdTouch, ret, ent1, ent2)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_Think(entid)
{
	new ret
	
	ExecuteForward(g_FwdThink, ret, entid)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_Spawn(entid)
{
	new ret
	
	ExecuteForward(g_FwdSpawn, ret, entid)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_PlayerPreThink(id)
{
	new ret
	
	ExecuteForward(g_FwdClientPreThink, ret, id)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}

public Hook_FM_PlayerPostThink(id)
{
	new ret
	
	if (g_PlayerModeled[id])
	{
		new model[64]
		GetClientKeyValue(id, "model", model, 63)
		if (!equal(g_PlayerModels[id], model))
		{
			SetClientKeyValue(id, "model", g_PlayerModels[id])	
		}
	}
	
	ExecuteForward(g_FwdClientPostThink, ret, id)
	
	return (ret == PLUGIN_HANDLED) ? FMRES_SUPERCEDE : FMRES_IGNORED
}
