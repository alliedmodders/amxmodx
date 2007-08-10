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

#include <engine>     // various engine calls
#include <fakemeta>   // pev/engfunc/dllfunc/various calls which rely on engfunc/dllfunc
#include <ns>         // ns specifics

stock is_entity(id)
	return is_valid_ent(id);

/* The end of the native is buffered incase the plugin is including an NS_VERSION (no longer supported), ignore it */
stock get_build(classname[], value, number=0,any:...)
	return ns_get_build(classname, value, number);

stock get_private_i(index, offset, linuxdiff=5)
	return get_pdata_int(index, offset, linuxdiff);

stock set_private_i(index, offset, value, linuxdiff=5)
{
	return set_pdata_int(index, offset, value, linuxdiff);
}

stock Float:get_private_f(index, offset, linuxdiff=5)
{
	return get_pdata_float(index, offset, linuxdiff);
}

stock set_private_f(index, offset, Float:value, linuxdiff=5)
{
	return set_pdata_float(index, offset, value, linuxdiff);
}

stock make_string(value[])
	return engfunc(EngFunc_AllocString,value);

stock string(value, ret[])
{
  new szString[128];
  engfunc(EngFunc_SzFromIndex,value,szString,127);
  copy(ret,127,szString);
}

stock gpgobals_time()
	return floatround(halflife_time());

stock Float:get_range(ida, idb)
	return entity_range(ida, idb);

stock supercede()
  return 0;

stock register_clientkill()
	return 0;

stock register_changelvl()
	return 0;

stock register_msgblock(msgName[])
	return set_msg_block(get_user_msgid(msgName), BLOCK_SET);

stock register_msgedit(msgName[], cmd[])
	return register_message(get_user_msgid(msgName), cmd);

stock register_playback(event, cmd[])
	return 0;

stock get_spawn(type, number=0, Float:ret[3])
	return ns_get_spawn(type, number, ret);

stock has_weapon(index, weapon, setweapon=-1)
	return ns_has_weapon(index, weapon, setweapon);

stock gpglobals_v(type, Float:ret[3])
{
  new v_type=0;
  switch (type)
  {
  case 1:
    v_type = GL_v_forward;
  case 2:
    v_type = GL_v_right;
  case 3:
    v_type = GL_v_up;
  }
  if (!v_type)
    return 0;
  return get_global_vector(v_type, ret);
}

stock pev_i(_index,_field)
	return pev(_index,_field);

stock set_pev_i(_index, _field, _val)
	return set_pev(_index,_field,_val);
	
stock Float:pev_f(_index,_field)
{
  new Float:f;
  pev(_index,_field,f);
  return f;
}
stock set_pev_f(_index,_field,Float:_val)
  return set_pev(_index,_field,_val);
  
  
stock msg_args()
	return get_msg_args();

stock Float:msg_loc(vec)
{
	new Float:Ret[3];
	get_msg_origin(Ret);
	if (vec < 0 || vec > 3)
		return float(0);
	else
		return Ret[vec];
		
	return 0.0; // make compiler happy!
}

stock msg_dest()
	return 0;

stock msg_type()
	return 0;

stock msg_name()
	return 0;

stock msg_set_s(number, value[])
	return set_msg_arg_string(number, value);

stock msg_set_f(number, Float:value)
	return set_msg_arg_float(number, get_msg_argtype(number), value);

stock msg_set_i(number, value)
	return set_msg_arg_int(number, get_msg_argtype(number), value);

stock msg_data_type(value)
	return get_msg_argtype(value);

stock msg_strdata(value)
	return 0;

stock msg_data(value, ...)
{
	return (0*value);
}

stock get_filename(szFile[], len=-1)
{
	new name[16], version[16], author[16], status[16];
	new res = get_plugin(0, szFile, len, name, 16, version, 16, author, 16, status, 16);
	return res;
}

stock get_speedchange(id)
	return ns_get_speedchange(id);

stock set_speedchange(id, speed)
	return ns_set_speedchange(id,speed);

stock get_maxspeed(id)
	return ns_get_maxspeed(id);

stock set_player_model(id, model[]="")
	return ns_set_player_model(id, model);

stock set_player_skin(id, skin=-1)
	return ns_set_player_skin(id, skin);

stock set_player_body(id, body=-1)
	return ns_set_player_body(id, body);

stock ns2amx_version()
	return 0;

stock set_kvhandled()
	return 0;

stock ns2amx_getammo(id,Weapon)
	return ns_get_weap_reserve(id, Weapon);

stock ns2amx_setammo(id,Weapon,Value)
	return ns_set_weap_reserve(id, Weapon, Value);

stock ns2amx_giveitem(id,svClassname[])
	return ns_give_item(id, svClassname);

stock ns2amx_moveto(idMoved,idDest)
{
  new Float:origin[3];
  entity_get_vector(idDest, EV_VEC_origin,origin);
  entity_set_origin(idMoved, origin);
  return 1;
}
/* Returns whether or not the player has the MASK_DIGESTING flag set. */
stock ns2amx_isdigesting(id)
	return ns_get_mask(id,MASK_DIGESTING);

/* Returns total # of active hives. */
stock ns2amx_gethives()
	return ns_get_build("team_hive",1);

/* Returns 1 if the two entities are within the given range. */
stock ns2amx_inrange(ida,idb,range)
{
	if (entity_range(ida,idb) <= range)
		return 1;
	return 0;
}

stock ns2amx_nspopup(id,svMessage[190]) {
  new szMessage[180];
  copy(szMessage,179,svMessage);
  return ns_popup(id, szMessage);
}

stock ns2amx_setres(id,value)
	return ns_set_res(id, float(value));

stock ns2amx_getenergy(id)
	return floatround(ns_get_energy(id));

stock ns2amx_setenergy(id,energy)
	return ns_set_energy(id, float(energy));

stock ns2amx_getjpfuel(id)
	return floatround(ns_get_jpfuel(id));

stock ns2amx_setjpfuel(id,fuel)
	return ns_set_jpfuel(id, float(fuel));

stock get_mask(id,mask)
  return ns_get_mask(id, mask);

stock set_mask(id,mask,value)
  return ns_set_mask(id,mask,value);

stock get_special(id,mask)
{
  if (pev(id,pev_iuser4) & mask)
     return 1;
  return 0;
}

stock get_res(id)
	return floatround(ns_get_res(id));

stock get_class(id)
  return ns_get_class(id);

stock is_combat()
	return ns_is_combat();
