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

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib engine
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib engine
 #endif
#else
 #pragma library engine
#endif

native traceresult(type,any:...);

/* Registers a client impulse to a function.  Function is passed the ID of the user. */
native register_impulse(impulse, const function[]);

/* Registers a touch action to a function by classnames.  Use * to specify any classname. */
native register_touch(const Touched[], const Toucher[], const function[]);

/* Registers a think action to a function by classname. */
native register_think(const Classname[], const function[]);

/* NOTE: In old engine versions, this was not the case.  Values are now WINDOWS values.
 * You must pass with the windows offset (e.g. if 230 on windows, pass 230 no matter what) 
 * The module will automatically add +5 for Linux.
 */

/* Precaches an event. */
native precache_event(type, const Name[], any:...);

/* set/get a user's speak flags */
native set_speak(iIndex, iSpeakFlags);
native get_speak(iIndex);

/* Drops an entity to the floor (work?) */
native drop_to_floor(entity);

/* Get whole buffer containing keys and their data. */
native get_info_keybuffer(id, buffer[], length);

/* Use an entity with another entity. "entUsed" could be a hostage, "entUser" a player. */
native force_use(entUsed, entUser);

/* Get globals from server. */
native Float:get_global_float(variable);
native get_global_int(variable);
native get_global_string(variable, string[], maxlen);
native get_global_vector(variable, Float:vector[3]);
native get_global_edict(variable);

/* Set entity bounds. */
native entity_set_size(index, const Float:mins[3], const Float:maxs[3]);

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
native entity_set_vector(iIndex, iKey, const Float:vNewVector[3]);
native entity_get_edict(iIndex, iKey);
native entity_set_edict(iIndex, iKey, iNewIndex);
native entity_get_string(iIndex, iKey, szReturn[], iRetLen);
native entity_set_string(iIndex, iKey, const szNewVal[]);
native entity_get_byte(iIndex, iKey);
native entity_set_byte(iIndex, iKey, iVal);

/* Creates an entity, will return the index of the created entity. ClassName must be valid. */
native create_entity(const szClassname[]);

/* Finds an entity in the world, will return 0 if nothing is found */
native find_ent_by_class(iIndex, const szClass[]);
//optionally you can set a jghg2 type
// 1: target, 2:targetname, 0:classname (default)
native find_ent_by_owner(iIndex, const szClass[], iOwner, iJghgType=0);
native find_ent_by_target(iIndex, const szClass[]);
native find_ent_by_tname(iIndex, const szClass[]);
native find_ent_by_model(iIndex, const szClass[], const szModel[]);
native find_ent_in_sphere(start_from_ent, const Float:origin[3], Float:radius);

//this will CBaseEntity::Think() or something from the entity
native call_think(entity);

/* Is entity valid? */
native is_valid_ent(iIndex);

/* Proper origin setting, keeps updated with Half-Life engine. */
native entity_set_origin(iIndex, const Float:fNewOrigin[3]);

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
#if !defined AMXMOD_BCOMPAT
native DispatchKeyValue(...);
#endif

native get_keyvalue(entity, const szKey[], value[], maxLength);

native copy_keyvalue(szClassName[],sizea,szKeyName[],sizeb,szValue[],sizec);

/* Runs the GameDLL's DispatchSpawn for an entity, I think it's used with DispatchKeyValue. */
native DispatchSpawn(iIndex);

/* Hurts/Kills players in a sphere, like an explosion, Multiplier determines damage. */
#if !defined AMXMOD_BCOMPAT
native radius_damage(const Float:fExplodeAt[3], iDamageMultiplier, iRadiusMultiplier);
#endif

/* Will return the contents of a point (inside map? in sky? outside map? etc.). */
native point_contents(const Float:fCheckAt[3]);

/* Trace a line from Start(X, Y, Z) to End(X, Y, Z), will return the point hit in vReturn[3]
 * and an entity index if an entity is hit. */
#if !defined AMXMOD_BCOMPAT
native trace_line(iIgnoreEnt, const Float:fStart[3], const Float:fEnd[3], Float:vReturn[3]);
#endif

/* Traces a hull. */
native trace_hull(const Float:origin[3],hull,ignoredent=0,ignoremonsters=0);

/* Traces a line, and returns the normal to the plane hit in vReturn.
 * Returns 0 if theres no normal. */
native trace_normal(iIgnoreEnt, const Float:fStart[3], const Float:fEnd[3], Float:vReturn[3]);

/* Gets the ID of a grenade. */
native get_grenade_id(id, model[], len, grenadeid = 0);

/* Gets gpGlobals->time from Half-Life */
native Float:halflife_time();

/* Sets map lighting, #OFF to disable. */
native set_lights(const Lighting[]);

/* Sets Player's View to entity iTargetIndex. */
native attach_view(iIndex, iTargetIndex);

/* Sets Player's View Mode.
 * rpgrocket.mdl must be precached in order for this function to work */
native set_view(iIndex, ViewType);

/* Direct copy of PLAYBACK_EVENT_FULL from Metamod/HLSDK.  If you don't know how that works, you probably shouldn't be using it. */
native playback_event(flags,invoker,eventindex,Float:delay,const Float:origin[3],const Float:angles[3],Float:fparam1,Float:fparam2,iparam1,iparam2,bparam1,bparam2);

/* Gets parameters sent from CmdStart.
   Note that you will receive modified values if any other plugin have
   changed them. */
native get_usercmd(type,any:...);

/* Sets the parameters sent from CmdStart.
   Note that your changes will be seen by any other plugin doing get_usercmd() */
native set_usercmd(type,any:...);

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
native find_sphere_class(aroundent, const _lookforclassname[], Float:radius, entlist[], maxents, const Float:origin[3] = {0.0, 0.0, 0.0});

/* SDK function - checks if an origin is in an entity's view cone
 * Set use3d to 1 to do the calculation in 3D. Otherwise it will be in 2D.
 */
native is_in_viewcone(entity, const Float:origin[3], use3d = 0);

//SDK function - checks if an entity is visible to an entity
native is_visible(entity, target);

//Added at twistedeuphoria's request, see funcwiki for details
native trace_forward(const Float:start[3], const Float:angle[3], Float:give, ignoreEnt, &Float:hitX, &Float:hitY, &Float:shortestDistance, &Float:shortestDistLow, &Float:shortestDistHigh);

#include <engine_stocks>
