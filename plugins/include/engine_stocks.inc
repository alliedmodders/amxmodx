/* Engine stocks
*
* by the AMX Mod X Development Team
*  thanks to AssKicR, Freecode and T(+)rget
*
* This file is provided as is (no warranties).
*/

#if defined _engine_stocks_included
  #endinput
#endif
#define _engine_stocks_included

//wrapper for find_ent_by_class
stock find_ent(iStart, szClassname[])
{
	return find_ent_by_class(iStart, szClassname)
}

/* Changes an integer vec to a floating vec */

stock IVecFVec(IVec[3], Float:FVec[3])
{
	FVec[0] = float(IVec[0])
	FVec[1] = float(IVec[1])
	FVec[2] = float(IVec[2])

	return 1
}

/* Changes a float vec to an integer vec */
stock FVecIVec(Float:FVec[3], IVec[3])
{
	IVec[0] = floatround(FVec[0])
	IVec[1] = floatround(FVec[1])
	IVec[2] = floatround(FVec[2])

	return 1
}

/* Get the Button(s) user is pressing */
stock get_user_button(id)
	return entity_get_int(id, EV_INT_button)

stock get_user_oldbutton(id)
	return entity_get_int(id, EV_INT_oldbuttons)

/* Get flags an entity is flagged with */
stock get_entity_flags(ent)
	return entity_get_int(ent, EV_INT_flags)

/* Get the distance between two entities */
stock get_entity_distance(ent1, ent2)
{
	new Float:orig1[3], Float:orig2[3], origin1[3], origin2[3]
	entity_get_vector(ent1, EV_VEC_origin, orig1)
	for(new a = 0; a < 3; a++)
		origin1[a] = floatround(orig1[a])

	entity_get_vector(ent2, EV_VEC_origin, orig2)
	for(new b = 0; b < 3; b++)
		origin2[b] = floatround(orig2[b])

	return get_distance(origin1, origin2)
}

/* Get grenade thrown by this user */
stock get_grenade(id)
{
	new iGrenade = find_ent_by_class(-1, "grenade")
	while(iGrenade > 0)
	{
		if(entity_get_edict(iGrenade, EV_ENT_owner) == id)
			return iGrenade

		iGrenade = find_ent_by_class(iGrenade, "grenade")
	}

	return 0
}

/* Get origin of a brush entity */
stock get_brush_entity_origin(ent, Float:orig[3])
{
	new Float:Min[3], Float:Max[3]
	entity_get_vector(ent, EV_VEC_mins, Min)
	entity_get_vector(ent, EV_VEC_maxs, Max)
	for(new a = 0; a < 3; a++)
		orig[a] = (Min[a] + Max[a]) / 2

	return orig[0] && orig[1] && orig[2]
}

/* Remove entity by name */
stock remove_entity_name(eName[])
{
	new iEntity = FindEntity(-1, eName)
	while (iEntity > 0)
	{
		remove_entity(iEntity)
		iEntity = find_ent_by_class(-1, eName)
	}

	return 1
}

/* Get the contents of the point a user is aiming at */
stock ViewContents(id)
{
	new origin[3],Float:Orig[3]
	get_user_origin( id, origin, 3 )
	for(new a = 0; a < 3; a++)
		Orig[a] = float(origin[a])

	return PointContents( Orig )
}

stock get_speed(ent)
{
	new Float:Vel[3], rVel[3]
	entity_get_vector(ent, EV_VEC_velocity, Vel)
	for(new i = 0; i < 3; i++)
		rVel[i] = floatround(Vel[i])

	return sqroot(rVel[0] * rVel[0] + rVel[1] * rVel[1] + rVel[2] * rVel[2])
}

/* Creates a death message. */
stock make_deathmsg(killer,victim,headshot,weapon[])
{
	message_begin(MSG_ALL,get_user_msgid("DeathMsg"),{0,0,0},0)
	write_byte( killer )
	write_byte( victim )
	write_byte( headshot )
	write_string( weapon[] )
	message_end()

	return 1
}

/* Kills a user without a message. */
stock user_silentkill(index)
{
	set_msg_block(get_user_msgid("DeathMsg"),BLOCK_ONCE)
	user_kill(index,1)

	return 1
}

/*  Set endering of an entity */
stock set_rendering(index,fx=kRenderFxNone, r=255,g=255,b=255, render=kRenderNormal,amount=16)
{
	entity_set_int(index,EV_INT_renderfx,fx)
	new Float:RenderColor[3]
	RenderColor[0] = float(r)
	RenderColor[1] = float(g)
	RenderColor[2] = float(b)
	entity_set_vector(index,EV_VEC_rendercolor,RenderColor)
	entity_set_int(index,EV_INT_rendermode,render)
	entity_set_float(index,EV_FL_renderamt,float(amount))

	return 1
}

/* Set flags on an entity */
stock set_entity_flags(ent,flag,onoff)
{
	if ((entity_get_int(ent,EV_INT_flags)&flag) > 0)
	{
		if (onoff == 1)
		{
			return 2
		}
		else
		{
			entity_set_int(ent,EV_INT_flags,entity_get_int(ent,EV_INT_flags)-flag)
			return 1
		}
	}
	else
	{
		if (onoff == 0)
		{
			return 2
		}
		else
		{
			entity_set_int(ent,EV_INT_flags,entity_get_int(ent,EV_INT_flags)+flag)
			return 1
		}
	}

	return 0
}

/* If visible = 1, entity will be set to be visible, else invisible. */
stock set_entity_visibility(entity, visible = 1) {
	entity_set_int(entity, EV_INT_effects, visible == 1 ? entity_get_int(entity, EV_INT_effects) & ~EF_NODRAW : entity_get_int(entity, EV_INT_effects) | EF_NODRAW)

	return 1
}

/* Returns 1 if entity is visible. */
stock get_entity_visibility(entity) {
	return (entity_get_int(entity, EV_INT_effects) & EF_NODRAW)
}