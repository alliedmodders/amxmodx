/* NS2AMXHELPERS.INC
 * Provides many functions to ease developing for the NS2AMX module.
 * Version 1.0
 */
#if defined __ns2amxxhelpers__
 	#endinput
#endif
#define __ns2amxxhelpers__ 02

#include <fun>
#include <engine>
#include <ns2amxx>
#include <ns2amxxdefines>

stock ns_getammo(id,Weapon)
{
	if (id > get_playersnum() || id < 1)
	{
		return 0
	}
	if (Weapon == WEAPON_MG)
	{
		return get_offset_int(id,PRIVATE_AMMO_LMG)
	}
	if (Weapon == WEAPON_HMG)
	{
		return get_offset_int(id,PRIVATE_AMMO_HMG)
	}
	if (Weapon == WEAPON_GRENADE_GUN)
	{
		return get_offset_int(id,PRIVATE_AMMO_GL)
	}
	if (Weapon == WEAPON_PISTOL)
	{
		return get_offset_int(id,PRIVATE_AMMO_PISTOL)
	}
	if (Weapon == WEAPON_SHOTGUN)
	{
		return get_offset_int(id,PRIVATE_AMMO_SHOTGUN)
	}
	return 1
}
stock ns_setammo(id,Weapon,Value)
{
	if (id > get_playersnum() || id < 1)
	{
		return 0
	}
	if (Weapon == WEAPON_MG)
	{
		set_offset_int(id,PRIVATE_AMMO_LMG,Value)
		return 1
	}
	if (Weapon == WEAPON_HMG)
	{
		set_offset_int(id,PRIVATE_AMMO_HMG,Value)
		return 1
	}
	if (Weapon == WEAPON_GRENADE_GUN)
	{
		set_offset_int(id,PRIVATE_AMMO_GL,Value)
		return 1
	}
	if (Weapon == WEAPON_PISTOL)
	{
		set_offset_int(id,PRIVATE_AMMO_PISTOL,Value)
		return 1
	}
	if (Weapon == WEAPON_SHOTGUN)
	{
		set_offset_int(id,PRIVATE_AMMO_SHOTGUN,Value)
		return 1
	}
	return 0
}
stock ns_giveitem(id,svClassname[])
{
	new entity = create_entity(svClassname)
	ns_moveto(entity,id)
	entity_set_int(entity, EV_INT_team, entity_get_int(id, EV_INT_team))
	spawn(entity)
	fake_touch(entity,id)

}
stock ns_moveto(idMoved,idDest)
{
	new Float:origin[3]
	new Float:dest[3]
	entity_get_vector(idMoved, EV_VEC_origin, origin)
	entity_get_vector(idDest, EV_VEC_origin, dest)
	entity_set_vector(idMoved, dest)
}
stock ns_isdigesting(id)
{
	return ns_get_mask(id,MASK_DIGESTING)
}
stock ns_gethives()
{
	return ns_get_build("team_hive",1)
}
stock ns_inrange(ida,idb,range)
{
	if (entity_get_range(ida,idb) <= range)
	{
		return 1
	}
	return 0
}
stock ns_nspopup(id,svMessage[190])
{
	message_begin(MSG_ONE,get_user_msgid("HudText2"),{0,0,0},id)
	write_string(svMessage)
	write_byte(0)
	message_end()
}
stock ns_setres(id,value)
{
	if (entity_get_int(id,EV_INT_team) == 2) // this only works for aliens
	{
		set_offset_float(id,PRIVATE_ALIEN_RES,float(value))
		return 0;
	}
	return 1;
}
stock ns_getenergy(id)
{
	if (entity_get_int(id,EV_INT_iuser3) >= 3 && entity_get_int(id,EV_INT_iuser3) <= 7)
	{
		new Float:energy
		entity_set_float(id,EV_FL_fuser3,energy)
		return floatround(energy / 10.0)
	}
	return 0
}
stock ns_setenergy(id,energy)
{
	if (entity_get_int(id,EV_INT_iuser3) >= 3 && entity_get_int(id,EV_INT_iuser3) <= 7)
	{
		entity_set_float(id,EV_FL_fuser3,float(energy * 10))
	}
}
stock ns_getjpfuel(id)
{
	if (get_offset_int(id,eni_team) == 1)
	{
		return floatround(get_offset_float(id,enf_fuser3) / 10.0)
	}
	return 0
}
stock ns_setjpfuel(id,fuel)
{
	if (entity_get_int(id,EV_INT_team) == 1)
	{
		entity_set_float(id,EV_FL_fuser3, float(fuel * 10))
		return 1
	}
	return 0
}
stock ns_get_mask(id,mask)
{
  return (entity_get_int(id,EV_INT_iuser4) & mask)
}
stock ns_set_mask(id,mask,value)
{
  if (value)
  {
    if (ns_get_special(id,mask))
      return
    entity_set_int(id,EV_INT_iuser3,entity_get_int(id,EV_INT_iuser4) + mask)
    return
  }
  if (ns_get_special(id,mask))
    entity_set_int(id,EV_INT_iuser4,entity_get_int(id,EV_INT_iuser4) - mask)
}
stock ns_get_special(id,mask)
{
	if (entity_get_int(id,EV_INT_iuser4) & mask)
		return 1
	return 0
}
stock ns_get_res(id)
{
	if (id < 1 && id > get_maxplayers())
		return 0
	if (!is_user_connected(id))
		return 0
#if NS_VERSION == 3
	if (entity_get_int(id,EV_INT_iuser1) > 0)
	{
		// This is only reported to be affected in first person spectator, but might as well be safe
		new Float:res = get_offset_float(id,PRIVATE_ALIEN_RES)
		return floatround(res)
	}
	else
	{
		// They aren't in spectator mode...
		new Float:res[3]
		entity_get_vector(id,EV_VEC_vuser4,res)
		res[2] = (res[2] + 1.0) / 100
		return floatround(res[2])
	}
	return 0
#else
	new Float:res[3]
	entity_get_vector(id,EV_VEC_vuser4, res)
	res[2] = (res[2] + 1.0) / 100
	return floatround(res[2])
#endif

}
stock ns_get_class(id)
{
  new iuser3 = entity_get_int(id,EV_INT_iuser3)
  if (entity_get_int(id,EV_INT_deadflag) > 0)
    return CLASS_DEAD
  if (entity_get_int(id,EV_INT_team) == 0)
    return CLASS_NOTEAM
  if (iuser3 == 1)
  {
    // Light armor marine..
    if (entity_get_int(id,EV_INT_iuser4) & MASK_HEAVYARMOR)
      return CLASS_HEAVY
    if (entity_get_int(id,EV_INT_iuser4) & MASK_JETPACK)
      return CLASS_JETPACK
    return CLASS_MARINE
  }
  if (iuser3 == 2)
    return CLASS_COMMANDER
  if (iuser3 == 3)
    return CLASS_SKULK
  if (iuser3 == 4)  
    return CLASS_GORGE
  if (iuser3 == 5)
    return CLASS_LERK
  if (iuser3 == 6)
    return CLASS_FADE
  if (iuser3 == 7)
    return CLASS_ONOS
  if (iuser3 == 8)
    return CLASS_GESTATE
  return CLASS_UNKNOWN
}
