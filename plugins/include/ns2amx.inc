/* NS2AMX Utility backwards compatibility
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _ns2amx_included
  #endinput
#endif
#define _ns2amx_included

#include <engine>
#include <ns>

stock is_entity(id)
{
	return is_valid_ent(id)
}

stock get_build(classname[], value, number=0, NS_Version=NS_VERSION)
{
	return ns_get_build(classname, value, number, NS_Version)
}

stock get_private_i(index, offset, linuxdiff=5)
{
	return get_offset_int(index, offset, linuxdiff)
}

stock set_private_i(index, offset, value, linuxdiff=5)
{
	return set_private_i(index, offset, value, linuxdiff)
}

stock Float:get_private_f(index, offset, linuxdiff=5)
{
	return get_private_f(index, offset, linuxdiff)
}

stock Float:set_private_f(index, offset, Float:value, linuxdiff=5)
{
	return set_private_f(index, offset, value, linuxdiff)
}

stock make_string(value[])
{
	return 0
}

stock string(value, ret[])
{
	return 0
}

stock gpgobals_time()
{
	return halflife_time()
}

stock get_range(ida, idb)
{
	return entity_get_range(ida, idb)
}

stock supercede()
{
	return 0
}

stock register_clientkill()
{
	return 0
}

stock register_changelvl()
{
	return 0
}

stock register_msgblock(msgName[])
{
	return set_msg_block(get_user_msgid(msgName), BLOCK_SET)
}

stock register_msgedit(msgName[], cmd[])
{
	return register_message(get_user_msgid(msgName), cmd)
}

stock register_playback(event, cmd[])
{
	return 0
}

stock get_spwan(type, number=0, Float:ret[3])
{
	return ns_get_spawn(type, number, ret)
}

stock has_weapon(index, weapon, setweapon=-1)
{
	return user_has_weapon(index, weapon, setweapon)
}

stock gpglobals_v(type, Float:ret[3])
{
	return get_global_vector(type, ret)
}

stock pev(index, value, ...)
{
	return 0
}

stock set_pev(index, value, ...)
{
	return 0
}

stock msg_args()
{
	return get_msg_args()
}

stock Float:msg_loc(vec)
{
	new Float:Ret[3]
	get_msg_origin(Ret)
	if (vec < 0 || vec > 3)
		return float(0)
	else
		return Ret[vec]
}

stock msg_dest()
{
	return 0
}

stock msg_type()
{
	return 0
}

stock msg_name()
{
	return 0
}

stock msg_set_s(number, value[])
{
	return set_msg_arg_string(number, value)
}

stock msg_set_f(number, Float:value)
{
	return set_msg_arg_float(number, value)
}

stock msg_set_i(number, value)
{
	return set_msg_arg_int(number, value)
}

stock msg_data_type(value)
{
	return get_msg_arg_type(value)
}

stock msg_strdata(value)
{
	return 0
}

stock msg_data(value, ...)
{
	return 0
}

stock get_filename(szFile[], len=-1)
{
	new name[16], version[16], author[16], status[16]
	new res = get_plugin(0, szFile, len, name, 16, version, 16, author, 16, status, 16)
	return res
}

stock get_speedchange(id)
{
	return ns_get_speedchange(id)
}

stock set_speedchange(id, speed)
{
	return ns_set_speedchange(id)
}

stock get_maxspeed(id)
{
	return ns_get_maxspeed(id)
}

stock set_ns_menu(id,r=NSMENU_R,g=NSMENU_G,b=NSMENU_B,Float:x=NSMENU_X,Float:y=NSMENU_Y, effects=0, Float:fadeintime=0.0, Float:fadeouttime=0.0,channel1=NSMENU_CHAN1,channel2=NSMENU_CHAN2)
{
	return ns_set_menu(id, r, g, b, x, y, effects, fadeintime, fadeouttime, channel1, channel2)
}

stock show_ns_menu(id, cmd[], text[], keys, time)
{
	ns_show_menu(id, cmd, text, keys, time)
}

stock set_player_model(id, model[]="")
{
	return ns_set_player_model(id, model)
}

stock set_player_skin(id, skin=-1)
{
	return ns_set_player_skin(id, skin)
}

stock set_player_body(id, body=-1)
{
	return ns_set_player_body(id, body)
}

stock ns2amx_version()
{
	return ns_version()
}

stock set_kvhandled()
{
	return 0
}

stock ns2amx_getammo(id,Weapon)
{
	return ns_getammo(id, Weapon)
}
stock ns2amx_setammo(id,Weapon,Value)
{
	return ns_setammo(id, Weapon, Value)
}
stock ns2amx_giveitem(id,svClassname[])
{
	return ns_giveitem(id, svClassname)

}
stock ns2amx_moveto(idMoved,idDest)
{
	return ns_moveto(idMoved, idDest)
}
/* Returns whether or not the player has the MASK_DIGESTING flag set. */
stock ns2amx_isdigesting(id)
	return ns_get_mask(id,MASK_DIGESTING)

/* Returns total # of active hives. */
stock ns2amx_gethives()
	return ns_get_build("team_hive",1)

/* Returns 1 if the two entities are within the given range. */
stock ns2amx_inrange(ida,idb,range)
{
	if (entity_get_range(ida,idb) <= range)
		return 1
	return 0
}

stock ns2amx_nspopup(id,svMessage[190])
{
	return ns_nspopup(id, svMessage)
}
stock ns2amx_setres(id,value)
{
	return ns_setres(id, value)
}
stock ns2amx_getenergy(id)
{
	return ns_getenergy(id)
}
stock ns2amx_setenergy(id,energy)
{
	return ns_setenergy(id, energy)
}
stock ns2amx_getjpfuel(id)
{
	return ns_getjpfuel(id)
}
stock ns2amx_setjpfuel(id,fuel)
{
	return ns_setjpfuel(id, fuel)
}
stock get_mask(id,mask)
{
  return ns_get_mask(id, mask)
}
stock set_mask(id,mask,value)
{
  return ns_set_mask(id,mask,value)
}
stock get_special(id,mask)
{
	return ns_get_special(id,mask)
}
stock get_res(id)
{
	return ns_get_res(id)
}
stock get_class(id)
{
  return ns_get_class(id)



stock is_combat()
{
	return ns_is_combat()
}
