/* Engine functions
*
* by the AMX Mod X Development Team
*  thanks to Vexd and mahnsawce
*
* This file is provided as is (no warranties).
*/

#if defined _engine_included
  #endinput
#endif
#define _engine_included

#include <engine_const>

#pragma library engine

native traceresult(type,{Float,Sql,Result,_}:...);

/* Registers a client impulse to a function.  Function is passed the ID of the user. */
native register_impulse(impulse, function[]);

/* Registers a touch action to a function by classnames.  Use * to specify any classname. */
native register_touch(Toucher[], Touched[], function[]);

/* Registers a think action to a function by classname. */
native register_think(Classname[], function[]);

/* Lets you directly hook a message in the engine!
 * You can overwrite the message before anything happens and either let the message continue
 * or fully block it. Here is how it works:
 * If you hook a message, the message is stored but not sent. You have the opportunity to
 * not only execute code, but to get/set the contents of the message, before you choose to 
 * either block it or let it go on its way. The hooked function will be passed a msg_id, msg_dest, and entity index. */
native register_message(iMsgId, szFunction[]);

/* The get/set _msg commands will fail if used outside a hooked message scope.
 * They should never be used unless inside a registered message function.
 * There are eight different ways of sending a message, five are ints, two are floats, and one is string.
 * These are denoted by iArgType.  argn is the number
 * of the argument. Exceeding the bounds of 1 to get_msg_args() is a bad idea. 
 * As of AMX Mod X 1.5, the middle parameter of set_* no longer does anything.
 * You cannot change the message argument type (as this would crash the mod anyway)
 */

/* Gets number of arguments that were passed to this message */
native get_msg_args();

/* Gets the argument type of argument argn */
native get_msg_argtype(argn);

/* Gets the value of argn. */
native get_msg_arg_int(argn);
native Float:get_msg_arg_float(argn);
native get_msg_arg_string(argn, szReturn[], iLength);

/* sets the value of argn. */
native set_msg_arg_int(argn, argtype, iValue);
native set_msg_arg_float(argn, argtype, Float:fValue);
native set_msg_arg_string(argn, szString[]);

/* Gets the origin of a message */
native get_msg_origin(Float:_Origin[3]);

/* NOTE: In old engine versions, this was not the case.  Values are now WINDOWS values.
 * You must pass with the windows offset (e.g. if 230 on windows, pass 230 no matter what) 
 * The module will automatically add +5 for Linux.
 */

/* Precaches any file. */
native precache_generic(szFile[]);
/* Precaches an event. */
native precache_event(type, Name[], {Float,Sql,Result,_}:...);

/* set/get a user's speak flags */
native set_speak(iIndex, iSpeakFlags)
native get_speak(iIndex)

/* Drops an entity to the floor (work?) */
native drop_to_floor(entity)

/* Get whole buffer containing keys and their data. */
native get_info_keybuffer(id, buffer[], length);

/* Use an entity with another entity. "used" could be a hostage, "user" a player. */
native force_use(pPlayer, pEntity);

/* Get globals from server. */
native Float:get_global_float(variable);
native get_global_int(variable);
native get_global_string(variable, string[], maxlen);
native get_global_vector(variable, Float:vector[3]);
native get_global_edict(variable);

/* Set entity bounds. */
native entity_set_size(index, Float:mins[3], Float:maxs[3]);

/* Get decal index */
native get_decal_index(const szDecalName[]);

/* Returns the distance between two entities. */
native Float:entity_range(ida,idb);

/* Sets/gets things in an entities Entvars Struct. */
native entity_get_int(iIndex, iKey);
native entity_set_int(iIndex, iKey, iVal);
native Float:entity_get_float(iIndex, iKey);
native entity_set_float(iIndex, iKey, Float:iVal);
native entity_get_vector(iIndex, iKey, Float:vRetVector[3]);
native entity_set_vector(iIndex, iKey, Float:vNewVector[3]);
native entity_get_edict(iIndex, iKey);
native entity_set_edict(iIndex, iKey, iNewIndex);
native entity_get_string(iIndex, iKey, szReturn[], iRetLen);
native entity_set_string(iIndex, iKey, const szNewVal[]);
native entity_get_byte(iIndex, iKey);
native entity_set_byte(iIndex, iKey, iVal);

/* Creates an entity, will return the index of the created entity. ClassName must be valid. */
native create_entity(szClassname[]);

/* Finds an entity in the world, will return 0 if nothing is found */
native find_ent_by_class(iIndex, szClass[]);
//optionally you can set a jghg2 type
// 1: target, 2:targetname, 0:classname (default)
native find_ent_by_owner(iIndex, szClass[], iOwner, iJghgType=0);
native find_ent_by_target(iIndex, szClass[]);
native find_ent_by_tname(iIndex, szClass[]);
native find_ent_by_model(iIndex, szClass[], szModel[]);
native find_ent_in_sphere(start_from_ent, Float:origin[3], Float:radius);

//this will CBaseEntity::Think() or something from the entity
native call_think(entity)

/* Mirrors CBaseEntity->TakeDamage() - Forces an entity to take damage.
   Potential for crash.  If you have problems with this, I suggest using fakedamage(). */
native take_damage(idVictim,idInflictor,idAttacker,Float:takedmgdamage,damagetype);

/* Is entity valid? */
native is_valid_ent(iIndex);

/* Proper origin setting, keeps updated with Half-Life engine. */
native entity_set_origin(iIndex, Float:fNewOrigin[3]);

/* Sets the model of an Entity. */
native entity_set_model(iIndex, const szModel[]);

/* Remove an entity from the world. */
native remove_entity(iIndex);

/* Return current number of entities in the map */
native entity_count();

/* Simulate two entities colliding/touching. */
native fake_touch(entTouched, entToucher);

/* 2 formats.
   Format: DispatchKeyValue("KeyName","Value") - sets keyvalues for the entity specified in the keyvalue() forward.
   Format: DispatchKeyValue(index,"KeyName","Value") - Sets keyvalue for entity not specified in keyvalue() forward. */
native DispatchKeyValue(...);

native get_keyvalue(entity, szKey[], value[], maxLength);

native copy_keyvalue(szClassName[],sizea,szKeyName[],sizeb,szValue[],sizec);

/* Runs the GameDLL's DispatchSpawn for an entity, I think it's used with DispatchKeyValue. */
native DispatchSpawn(iIndex);

/* Hurts/Kills players in a sphere, like an explosion, Multiplier determines damage. */
native radius_damage(Float:fExplodeAt[3], iDamageMultiplier, iRadiusMultiplier);

/* Gives you a velocity in the direction a player is looking, iVelocity is the multiplier. */
native velocity_by_aim(iIndex, iVelocity, Float:vRetValue[3]);

/* Will return the contents of a point (inside map? in sky? outside map? etc.). */
native point_contents(Float:fCheckAt[3]);

/* Trace a line from Start(X, Y, Z) to End(X, Y, Z), will return the point hit in vReturn[3]
 * and an entity index if an entity is hit. */
native trace_line(iIgnoreEnt, Float:fStart[3], Float:fEnd[3], Float:vReturn[3]);

/* Traces a hull. */
native trace_hull(Float:origin[3],hull,ignoredent=0,ignoremonsters=0);

/* Traces a line, and returns the normal to the plane hit in vReturn.
 * Returns 0 if theres no normal. */
native trace_normal(iIgnoreEnt, Float:fStart[3], Float:fEnd[3], Float:vReturn[3]);

/* Changes a Vector to an Angle vector. */
native vector_to_angle(Float:fVector[3], Float:vReturn[3]);

native angle_vector(Float:vector[3],FRU,Float:ret[3])

/* Gets the length of a vector (float[3]). */
native Float:vector_length(Float:vVector[3]);

/* Gets the distance between 2 vectors (float[3]). */
native Float:vector_distance(Float:vVector[3], Float:vVector2[3]);

/* Gets the ID of a grenade. */
native get_grenade_id(id, model[], len, grenadeid = 0);

/* Gets gpGlobals->time from Half-Life */
native Float:halflife_time();

/* Sets map lighting, #OFF to disable. */
native set_lights(const Lighting[]);

/* Sets/Gets what engine messages are blocked. */
native set_msg_block(iMessage, iMessageFlags);
native get_msg_block(iMessage);

/* Sets Player's View to entity iTargetIndex. */
native attach_view(iIndex, iTargetIndex);

/* Sets Player's View Mode. */
native set_view(iIndex, ViewType);

/* Direct copy of PLAYBACK_EVENT_FULL from Metamod/HLSDK.  If you don't know how that works, you probably shouldn't be using it. */
native playback_event(flags,invoker,eventindex,Float:delay,Float:origin[3],Float:angles[3],Float:fparam1,Float:fparam2,iparam1,iparam2,bparam1,bparam2);

/* Gets parameters sent from CmdStart.
   Note that you will receive modified values if any other plugin have
   changed them. */
native get_usercmd(type,{Float,Sql,Result,_}:...);

/* Sets the parameters sent from CmdStart.
   Note that your changes will be seen by any other plugin doing get_usercmd() */
native set_usercmd(type,{Float,Sql,Result,_}:...);

/* Converts a string offset into a real string. Some of the forwards in fakemeta
   uses string offsets. (FM_CreateNamedEntity) */
native eng_get_string(_string, _returnString[], _len);

/* FORWARDS 
 **********/

/* Called when 2 entities touch. 
 * ptr - touched entity
 * ptd - toucher entity
 */
forward pfn_touch(ptr, ptd);

/* Called once every server frame. May cause lag. */
forward server_frame();

/* Called when a client types kill in console. */
forward client_kill(id);

/* Forward for PreThink()/PostThink() on a player. */
forward client_PreThink(id);
forward client_PostThink(id);

/* Forward for impulses. */
forward client_impulse(id, impulse);

/* Called when an entity "thinks" (DispatchThink) */
forward pfn_think(entid);

/* Called when an event is played */
forward pfn_playbackevent(flags, entid, eventid, Float:delay, Float:Origin[3], Float:Angles[3], Float:fparam1, Float:fparam2, iparam1, iparam2, bparam1, bparam2);

/* Called when an entity gets a keyvalue set on it from the engine (ie: map load)  Use copy_keyvalue to get the keyvalue information */
forward pfn_keyvalue(entid);

/* Called when an entity is spawned */
forward pfn_spawn(entid);

//from jghg2
/* As above, but returns number of ents stored in entlist. Use to find a specific type of entity classname (specify in _lookforclassname) around a
 * certain entity specified in aroundent. All matching ents are stored in entlist. Specify max amount of entities to find in maxents.
 * If aroundent is 0 its origin is not used, but origin in 6th parameter. Ie, do not specify 6th parameter (origin) if you specified an entity
 * in aroundent.
 */
native find_sphere_class(aroundent, _lookforclassname[], Float:radius, entlist[], maxents, Float:origin[3] = {0.0, 0.0, 0.0});

//SDK function - checks if an origin is in an entity's view cone
native is_in_viewcone(entity, Float:origin[3]);

//SDK function - checks if an entity is visible to an entity
native is_visible(entity, target);

//Added at twistedeuphoria's request, see funcwiki for details
native trace_forward(Float:start[3], Float:angle[3], Float:give, ignoreEnt, &Float:hitX, &Float:hitY, &Float:shortestDistance, &Float:shortestDistLow, &Float:shortestDistHigh);

#include <engine_stocks>
