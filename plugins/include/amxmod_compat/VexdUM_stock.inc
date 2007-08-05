/* VexdUM stocks backwards compatibility
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */

#if defined _vexd_bcompat_stocks_included
  #endinput
#endif
#define _vexd_bcompat_stocks_included

#if !defined _engine_included
	#include <engine>
#endif

stock is_entity(ent)
{
	return pev_valid(ent);
}

stock get_offset_int(ent, offset, linos = 5)
{
	return get_pdata_int(ent, offset, linos);
}

stock set_offset_int(ent, offset, value, linos = 5)
{
	return set_pdata_int(ent, offset, value, linos);
}

stock in_view_cone(ent, Float:Orig[3])
{
	return is_in_viewcone(ent, Orig);
}

stock get_maxentities()
{
	return global_get(glb_maxEntities);
}

stock can_see(ent1, ent2)
{
	if (is_entity(ent1) && is_entity(ent2))
	{
		new flags = pev(ent1, pev_flags);
		if (flags & EF_NODRAW || flags & FL_NOTARGET)
		{
			return 0;
		}
		
		new Float:lookerOrig[3];
		new Float:targetOrig[3];
		new Float:temp[3];
		
		pev(ent1, pev_origin, lookerOrig);
		pev(ent1, pev_view_ofs, temp);
		lookerOrig[0] += temp[0];
		lookerOrig[1] += temp[1];
		lookerOrig[2] += temp[2];
		
		pev(ent2, pev_origin, targetOrig);
		pev(ent2, pev_view_ofs, temp);
		targetOrig[0] += temp[0];
		targetOrig[1] += temp[1];
		targetOrig[2] += temp[2];
		
		engfunc(EngFunc_TraceLine, lookerOrig, targetOrig, 0, ent1, 0);
		if (get_tr2(0, TraceResult:TR_InOpen) && get_tr2(0, TraceResult:TR_InWater))
		{
			return 0;
		} else {
			new Float:flFraction;
			get_tr2(0, TraceResult:TR_flFraction, flFraction);
			if (flFraction == 1.0 || (get_tr2(0, TraceResult:TR_pHit) == ent2))
			{
				return 1;
			}
		}
	}
	
	return 0;
}

//From AMX Mod:
// Find an entity in the world, will return -1 if nothing is found
// type = 0: "classname"
// type = 1: "globalname"
// type = 2: "model"
// type = 3: "target"
// type = 4: "targetname"
// type = 5: "netname"
// type = 6: "message"
// type = 7: "noise"
// type = 8: "noise1"
// type = 9: "noise2"
// type = 10: "noise3"
// type = 11: "viewmodel"
// type = 12: "weaponmodel"

stock vexd_find_entity(ent, szValue[], type=0)
{
	static _g_FindEntTypes[13][] = 
	{
		"classname",
		"globalname",
		"model",
		"target",
		"targetname",
		"netname",
		"messages",
		"noise",
		"noise1",
		"noise2",
		"noise3",
		"viewmodel",
		"weaponmodel"
	};
	
	if (type < 0 || type >= 13)
	{
		type = 0;
	}
	
	return engfunc(EngFunc_FindEntityByString, ent, _g_FindEntTypes[type], szValue);
}

#define find_entity vexd_find_entity

//From AMX Mod:
// Find an entity within a given origin and radius
stock find_entity_sphere(ent, Float:Orig[3], Float:Rad)
{
	return engfunc(EngFunc_FindEntityInSphere, ent, Orig, Rad);
}
