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

#if !defined _amxmodx_included
  #include <amxmodx>
#endif

#if !defined _engine_included
  #include <engine>
#endif

stock fakedamage(idvictim,const szClassname[],Float:takedmgdamage,damagetype)
{
	new entity = create_entity("trigger_hurt");
	if (entity)
	{
		DispatchKeyValue(entity,"classname","trigger_hurt");
		new szDamage[16];
		// Takedamages only do half damage per attack (damage is damage per second, and it's triggered in 0.5 second intervals).
		// Compensate for that.
		format(szDamage,15,"%f",takedmgdamage * 2);
		DispatchKeyValue(entity,"dmg",szDamage);
		format(szDamage,15,"%i",damagetype);
		DispatchKeyValue(entity,"damagetype",szDamage);
		DispatchKeyValue(entity,"origin","8192 8192 8192");
		DispatchSpawn(entity);
		entity_set_string(entity, EV_SZ_classname, szClassname);
		fake_touch(entity,idvictim);
		remove_entity(entity);
		return 1;
	}
	return 0;
}

//wrapper for find_ent_by_class
stock find_ent(iStart, const szClassname[])
{
	return find_ent_by_class(iStart, szClassname);
}

/* Get the Button(s) user is pressing */
stock get_user_button(id)
{
	return entity_get_int(id, EV_INT_button);
}

stock get_user_oldbutton(id)
{
	return entity_get_int(id, EV_INT_oldbuttons);
}

/* Get flags an entity is flagged with */
stock get_entity_flags(ent)
{
	return entity_get_int(ent, EV_INT_flags);
}

/* Get the distance between two entities */
stock get_entity_distance(ent1, ent2)
{
	return floatround(entity_range(ent1, ent2));
}

/* Get grenade thrown by this user */
stock get_grenade(id)
{
	new iGrenade = find_ent_by_class(-1, "grenade");
	while(iGrenade > 0)
	{
		if(entity_get_edict(iGrenade, EV_ENT_owner) == id)
			return iGrenade;

		iGrenade = find_ent_by_class(iGrenade, "grenade");
	}

	return 0;
}

/* Get origin of a brush entity */
stock get_brush_entity_origin(ent, Float:orig[3])
{
	new Float:Min[3], Float:Max[3];

	entity_get_vector(ent, EV_VEC_origin, orig);
	entity_get_vector(ent, EV_VEC_mins, Min);
	entity_get_vector(ent, EV_VEC_maxs, Max);
	
	orig[0] += (Min[0] + Max[0]) * 0.5;
	orig[1] += (Min[1] + Max[1]) * 0.5;
	orig[2] += (Min[2] + Max[2]) * 0.5;

	return 1;
}

/* Remove entity by name */
stock remove_entity_name(const eName[])
{
	new iEntity = find_ent_by_class(-1, eName);
	while (iEntity > 0)
	{
		remove_entity(iEntity);
		iEntity = find_ent_by_class(-1, eName);
	}

	return 1;
}

/* Get the contents of the point a user is aiming at */
stock ViewContents(id)
{
	new origin[3], Float:Orig[3];
	get_user_origin(id, origin, 3);
	Orig[0] = float(origin[0]);
	Orig[1] = float(origin[1]);
	Orig[2] = float(origin[2]);

	return point_contents(Orig);
}

stock get_speed(ent)
{
	new Float:Vel[3];
	entity_get_vector(ent, EV_VEC_velocity, Vel);

	return floatround(vector_length(Vel));
}

/*  Set rendering of an entity */
stock set_rendering(index, fx=kRenderFxNone, r=255, g=255, b=255, render=kRenderNormal, amount=16)
{
	entity_set_int(index,EV_INT_renderfx,fx);
	new Float:RenderColor[3];
	RenderColor[0] = float(r);
	RenderColor[1] = float(g);
	RenderColor[2] = float(b);
	entity_set_vector(index,EV_VEC_rendercolor,RenderColor);
	entity_set_int(index,EV_INT_rendermode,render);
	entity_set_float(index,EV_FL_renderamt,float(amount));

	return 1;
}

/* Set flags on an entity */
stock set_entity_flags(ent,flag,onoff)
{
	if ((entity_get_int(ent,EV_INT_flags)&flag) > 0)
	{
		if (onoff == 1)
		{
			return 2;
		}
		else
		{
			entity_set_int(ent,EV_INT_flags,entity_get_int(ent,EV_INT_flags)-flag);
			return 1;
		}
	}
	else
	{
		if (onoff == 0)
		{
			return 2;
		}
		else
		{
			entity_set_int(ent,EV_INT_flags,entity_get_int(ent,EV_INT_flags)+flag);
			return 1;
		}
	}

	return 0;
}

/* If visible = 1, entity will be set to be visible, else invisible. */
stock set_entity_visibility(entity, visible = 1) 
{
	entity_set_int(entity, EV_INT_effects, visible == 1 ? entity_get_int(entity, EV_INT_effects) & ~EF_NODRAW : entity_get_int(entity, EV_INT_effects) | EF_NODRAW);

	return 1;
}

/* Returns 1 if entity is visible. */
stock get_entity_visibility(entity) 
{
	return !(entity_get_int(entity, EV_INT_effects) & EF_NODRAW);
}

stock set_user_velocity(entity, const Float:vec[3])
{
	return entity_set_vector(entity, EV_VEC_velocity, vec);
}

stock get_user_velocity(entity, Float:vec[3])
{
	return entity_get_vector(entity, EV_VEC_velocity, vec);
}

/* Backwards compatible */
/* Hurts/Kills players in a sphere, like an explosion, Multiplier determines damage. */
stock RadiusDamage(const Float:fExplodeAt[3], iDamageMultiplier, iRadiusMultiplier) 
{
	return radius_damage(fExplodeAt, iDamageMultiplier, iRadiusMultiplier);
}
/* Gives you a velocity in the direction a player is looking, iVelocity is the multiplier. */
stock VelocityByAim(iIndex,iVelocity, Float:vRetValue[3]) 
{
	return velocity_by_aim(iIndex,iVelocity,vRetValue);
}
/* Will return the contents of a point (inside map? in sky? outside map? etc.). */
stock PointContents(const Float:fCheckAt[3]) 
{
	return point_contents(fCheckAt);
}

stock set_size(index, const Float:mins[3], const Float:maxs[3]) 
{
	return entity_set_size(index,mins,maxs);
}

//by Twilight Suzuka, request addition at29428
//"Lifted from HLSDK"
stock IsInWorld( ent )
{
   new Float:origin[3];
   entity_get_vector(ent,EV_VEC_origin,origin);

   if (origin[0] >= 4096.0) return 0;
   if (origin[1] >= 4096.0) return 0;
   if (origin[2] >= 4096.0) return 0;
   if (origin[0] <= -4096.0) return 0;
   if (origin[1] <= -4096.0) return 0;
   if (origin[2] <= -4096.0) return 0;

   new Float:velocity[3];
   entity_get_vector(ent,EV_VEC_velocity,velocity);

   if (velocity[0] >= 2000) return 0;
   if (velocity[1] >= 2000) return 0;
   if (velocity[2] >= 2000) return 0;
   if (velocity[0] <= -2000) return 0;
   if (velocity[1] <= -2000) return 0;
   if (velocity[2] <= -2000) return 0;

   return 1;
}
