// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Special thanks to Vexd and mahnsawce.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Engine Functions
//

#if defined _engine_included
	#endinput
#endif
#define _engine_included

#include <engine_const>

#pragma reqlib engine
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib engine
#endif

/**
 * Retrieves a result from the global engine module trace handle.
 *
 * @note For a list of trace results available see the TR_* constants in
 *       engine_const.inc.
 * @note Usage examples:
 *       value = traceresult(TR_AllSolid);
 *       traceresult(TR_Fraction, floatvalue);
 *       traceresult(TR_EndPos, vector);
 *
 * @param type      Result to retrieve
 * @param ...       Depending on the result type a different number of
 *                  additional parameters should be provided:
 *                     int      - Returns the result integer value directly, no
 *                                additional parameters required
 *                     float    - Stores the result float value into the
 *                                variable provided as the second parameter
 *                     vector   - Copies the result vector to the Float:array[3]
 *                                provided in the second parameter
 *
 * @return          Changes depending on the result type:
 *                     int      - Returns the result integer value
 *                     float    - Returns 1
 *                     vector   - Returns 1
 */
native traceresult(type, any:...);

/**
 * Registers a function to be called on a client impulse.
 *
 * @note The function will be called in the following manner:
 *
 * public impulse_handler(client, impulse)
 *
 *   client     - Client index
 *   impulse    - Impulse triggered by the client
 *
 * @note The callback should return PLUGIN_CONTINUE to ignore the impulse,
 *       PLUGIN_HANDLED or higher to nullify it (CmdStart() is not blocked).
 * @note When returning PLUGIN_HANDLED or higher from the callback, Engine will
 *       still fire other impulse functions. This includes the client_impulse()
 *       and client_cmdStart() forwards.
 *
 * @param impulse   Impulse to hook
 * @param function  Name of callback function
 *
 * @return          Impulse forward id
 */
native register_impulse(impulse, const function[]);

/**
 * Registers a function to be called on a touch action between entities of
 * specified classes.
 *
 * @note The function will be called in the following manner:
 *
 * public touch_handler(touched, toucher)
 *
 *   touched    - Index of entity being touched
 *   toucher    - Index of entity touching
 *
 * @note The callback should return PLUGIN_CONTINUE to ignore the touch,
 *       PLUGIN_HANDLED or higher to block it.
 * @note When returning PLUGIN_HANDLED from the callback, Engine will still fire
 *       other touch functions like the pfn_touch() forward before actually
 *       blocking the touch. To immediately block return PLUGIN_HANDLED_MAIN
 *       instead.
 *
 * @param Touched   Entity classname being touched, "*" or "" for any class
 * @param Toucher   Entity classname touching, "*" or "" for any class
 * @param function  Name of callback function
 *
 * @return          Touch forward id
 */
native register_touch(const Touched[], const Toucher[], const function[]);

/**
 * Registers a function to be called on entity think on all entities of a
 * specified class.
 *
 * @note The function will be called in the following manner:
 *
 * public think_handler(entity)
 *
 *   entity    - Index of entity thinking
 *
 * @note The callback should return PLUGIN_CONTINUE to ignore the think,
 *       PLUGIN_HANDLED or higher to block it.
 * @note When returning PLUGIN_HANDLED from the callback, Engine will still fire
 *       other think functions like the pfn_think() forward before actually
 *       blocking the think. To immediately block return PLUGIN_HANDLED_MAIN
 *       instead.
 *
 * @param Classname	Entity classname to hook
 * @param function	Name of callback function
 *
 * @return 	        Think forward id
 */
native register_think(const Classname[], const function[]);

/**
 * Removes a previously registered impulse hook.
 *
 * @param registerid    Impulse forward id
 *
 * @return              1 on success, 0 if nothing was removed
 */
native unregister_impulse(registerid);

/**
 * Removes a previously registered touch hook.
 *
 * @param registerid    Touch forward id
 *
 * @return              1 on success, 0 if nothing was removed
 */
native unregister_touch(registerid);

/**
 * Removes a previously registered think hook.
 *
 * @param registerid    Think forward id
 *
 * @return              1 on success, 0 if nothing was removed
 */
native unregister_think(registerid);

/**
 * Sets the engine module speak flags on a client.
 *
 * @note For a list of available flags see the SPEAK_* constants in
 *       engine_const.inc
 *
 * @param iIndex        Client index
 * @param iSpeakFlags   New flags to set
 *
 * @noreturn
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native set_speak(iIndex, iSpeakFlags);

/**
 * Returns the engine module speak flags currently set on a client.
 *
 * @note For a list of available flags see the SPEAK_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Client index
 *
 * @return          Client speak flags
 * @error           If the client index is not within the range of 1 to
 *                  MaxClients, or the client is not connected, an error will be
 *                  thrown.
 */
native get_speak(iIndex);

/**
 * Uses the DROP_TO_FLOOR() engine function on an entity, which attempts to put
 * it down on the floor.
 *
 * @note This engine function traces 256 units straight downwards from the
 *       entity origin. If the trace hits the floor, the origin is updated to
 *       the end position of the trace, FL_ONGROUND is added to the flags and
 *       EV_ENT_groundentity is updated. When the trace does not hit anything or
 *       the entity would be stuck inside something, the function does nothing
 *       and returns 0.
 *
 * @param entity    Entity index
 *
 * @return          1 if entity is on the floor, 0 otherwise
 */
native drop_to_floor(entity);

/**
 * Retrieves keyvalue buffer from a client or the server.
 *
 * @note There are three different types of keyvalue buffers, depending on the
 *       index passed:
 *           -1 - "local" buffer (various server information and config values)
 *            0 - server buffer (usually contains "*gamedir" only)
 *           >0 - client buffer ("name", "rate" and other client info)
 * @note The buffer is formatted as "\key1\value1\key2\value2\...\keyN\valueN"
 *
 * @param id        Server/client index
 * @param buffer    Buffer to copy keybuffer to
 * @param length    Maximum size of buffer
 *
 * @return          Number of cells written to buffer
 * @error           If an invalid entity index is provided or, if the index is a
 *                  client index, the client is not connected, an error will be
 *                  thrown.
 */
native get_info_keybuffer(id, buffer[], length);

/**
 * Forces an entity (such as a player) to use another entity (such as a button).
 *
 * @param entUsed   Index of entity being used
 * @param entUser   Index of entity using
 *
 * @noreturn
 * @error           If an invalid entity index is provided or, if either index
 *                  is a client index, that client is not connected, an error
 *                  will be thrown.
 */
native force_use(entUsed, entUser);

/**
 * Returns a float type value from the server globals.
 *
 * @note For a list of valid float type entries, see the GL_* constants in
 *       engine_const.inc under the "Float" section.
 *
 * @param variable  Entry to retrieve from
 *
 * @return          Value of specified entry
 * @error           If an invalid entry is provided, an error will be thrown.
 */
native Float:get_global_float(variable);

/**
 * Returns a integer type value from the server globals.
 *
 * @note For a list of valid integer type entries, see the GL_* constants in
 *       engine_const.inc under the "Int" section.
 *
 * @param variable  Entry to retrieve from
 *
 * @return          Value of specified entry
 * @error           If an invalid entry is provided, an error will be thrown.
 */
native get_global_int(variable);

/**
 * Retrieves a global string type value from the server.
 *
 * @note For a list of valid string type entries, see the GL_* constants in
 *       engine_const.inc under the "String" section.
 *
 * @param variable  Entry to retrieve from
 * @param string    Buffer to copy value to
 * @param maxlen    Maximum size of buffer
 *
 * @return          Number of cells written to buffer
 * @error           If an invalid entry is provided, an error will be thrown.
 */
native get_global_string(variable, string[], maxlen);

/**
 * Returns a vector type value from the server globals.
 *
 * @note For a list of valid vector type entries, see the GL_* constants in
 *       engine_const.inc under the "Vector" section.
 *
 * @param variable  Entry to retrieve from
 * @param vector    Array to store vector in
 *
 * @noreturn
 * @error           If an invalid entry is provided, an error will be thrown.
 */
native get_global_vector(variable, Float:vector[3]);

/**
 * Returns a edict type value from the server globals.
 *
 * @note For a list of valid edict type entries, see the GL_* constants in
 *       engine_const.inc under the "Edict" section.
 * @note This native returns 0 as an error value if the edict retrieved is an
 *       invalid entity. As 0 is an entity index that is considered to be a
 *       valid value for some globals ("worldspawn"), this native can
 *       potentially return a misleading value. Use get_global_edict2() for a
 *       safe version.
 *
 * @param variable  Entry to retrieve from
 *
 * @return          Value of specified entry
 * @error           If an invalid entry is provided, an error will be thrown.
 */
native get_global_edict(variable);

/**
 * Returns a edict type value from the server globals.
 *
 * @note For a list of valid edict type entries, see the GL_* constants in
 *       engine_const.inc under the "Edict" section.
 * @note This native returns -1 as a safe error value if the edict retrieved is
 *       an invalid entity. Otherwise it is identical to get_global_edict().
 *
 * @param variable  Entry to retrieve from
 *
 * @return          Value of specified entry
 * @error           If an invalid entry is provided, an error will be thrown.
 */
native get_global_edict2(variable);

/**
 * Sets the size of the entity bounding box, as described by the minimum and
 * maximum vectors relative to the origin.
 *
 * @param index     Entity index
 * @param mins      Vector containing the minimum point relative to the origin
 * @param maxs      Vector containing the maximum point relative to the origin
 *
 * @noreturn
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_set_size(index, const Float:mins[3], const Float:maxs[3]);

/**
 * Returns the index of a decal.
 *
 * @param szDecalName   Decal name
 *
 * @return              Decal index >= 0, or -1 if decal was not found
 */
native get_decal_index(const szDecalName[]);

/**
 * Returns the distance between two entities.
 *
 * @param ida   Entity index 1
 * @param idb   Entity index 2
 *
 * @return      Distance between the entities
 * @error       If an invalid entity index is provided or, if either index is a
 *              client index, that client is not connected, an error will be
 *              thrown.
 */
native Float:entity_range(ida, idb);

/**
 * Returns if two entities bounding boxes intersect by comparing their absolute
 * minimum and maximum origins.
 *
 * @param entity    Entity index 1
 * @param other     Entity index 2
 *
 * @return          True if entities intersect, false otherwise
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native bool:entity_intersects(entity, other);

/**
 * Returns an integer type value from an entities entvar struct.
 *
 * @note For a list of valid integer type entries, see the EV_INT_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 *
 * @return          Value of specified entry
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_get_int(iIndex, iKey);

/**
 * Sets an integer type value in an entities entvar struct.
 *
 * @note For a list of valid integer type entries, see the EV_INT_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to write to
 * @param iVal      Value to set
 *
 * @return          1 if value was sucessfully set, 0 if an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_set_int(iIndex, iKey, iVal);

/**
 * Returns a float type value from an entities entvar struct.
 *
 * @note For a list of valid float type entries, see the EV_FL_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 *
 * @return          Value of specified entry, or 0 if an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native Float:entity_get_float(iIndex, iKey);

/**
 * Sets a float type value in an entities entvar struct.
 *
 * @note For a list of valid float type entries, see the EV_FL_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to write to
 * @param iVal      Value to set
 *
 * @return          1 if value was sucessfully set, 0 if an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_set_float(iIndex, iKey, Float:iVal);

/**
 * Retrieves a vector type value from an entities entvar struct.
 *
 * @note For a list of valid vector type entries, see the EV_VEC_* constants in
 *       engine_const.inc
 *
 * @param iIndex        Entity index
 * @param iKey          Entry to retrieve from
 * @param vRetVector    Array to store vector in
 *
 * @return              1 if value was sucessfully retrieved, 0 if an invalid
 *                      entry was specified
 * @error               If an invalid entity index is provided, an error will be
 *                      thrown.
 */
native entity_get_vector(iIndex, iKey, Float:vRetVector[3]);

/**
 * Sets a vector type value in an entities entvar struct.
 *
 * @note For a list of valid vector type entries, see the EV_VEC_* constants in
 *       engine_const.inc
 *
 * @param iIndex        Entity index
 * @param iKey          Entry to write to
 * @param vNewVector    Array to copy to the entity
 *
 * @return              1 if value was sucessfully set, 0 if an invalid entry
 *                      was specified
 * @error               If an invalid entity index is provided, an error will be
 *                      thrown.
 */
native entity_set_vector(iIndex, iKey, const Float:vNewVector[3]);

/**
 * Returns an edict type value from an entities entvar struct.
 *
 * @note For a list of valid edict type entries, see the EV_ENT_* constants in
 *       engine_const.inc
 * @note This native returns 0 as an error value if the edict retrieved from the
 *       entvar is an invalid entity. As 0 is an entity index that is
 *       considered to be a valid value for some entvars ("worldspawn"), this
 *       native can potentially return a misleading value. Use
 *       entity_get_edict2() for a safe version.
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 *
 * @return          Entity index in specified entry, 0 if the edict in the
 *                  entvar is not a valid entity or an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_get_edict(iIndex, iKey);

/**
 * Returns an edict type value from an entities entvar struct.
 *
 * @note For a list of valid edict type entries, see the EV_ENT_* constants in
 *       engine_const.inc
 * @note This native returns -1 as a safe error value if the edict retrieved
 *       from the entvar is an invalid entity. Otherwise it is identical to
 *       entity_get_edict().
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 *
 * @return          Entity index in specified entry, -1 if the edict in the
 *                  entvar is not a valid entity or an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_get_edict2(iIndex, iKey);

/**
 * Sets an edict type value in an entities entvar struct.
 *
 * @note For a list of valid edict type entries, see the EV_ENT_* constants in
 *       engine_const.inc
 * @note This native will crash the server if an invalid entity index is
 *       provided in iNewIndex.
 *
 * @param iIndex        Entity index
 * @param iKey          Entry to write to
 * @param iNewIndex     Entity index to set
 *
 * @return              1 if value was sucessfully set, 0 if an invalid entry
 *                      was specified
 * @error               If an invalid entity index is provided, an error will be
 *                      thrown.
 */
native entity_set_edict(iIndex, iKey, iNewIndex);

/**
 * Retrieves a string type value from an entities entvar struct.
 *
 * @note For a list of valid string type entries, see the EV_SZ_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 * @param szReturn  Buffer to copy value to
 * @param iRetLen   Maximum size of buffer
 *
 * @return          Number of cells written to buffer,  0 if an invalid entry
 *                  was specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_get_string(iIndex, iKey, szReturn[], iRetLen);

/**
 * Sets a string type value in an entities entvar struct.
 *
 * @note For a list of valid string type entries, see the EV_SZ_* constants in
 *       engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 * @param szNewVal  String to copy to the entity
 *
 * @return          1 if value was sucessfully set, 0 if an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_set_string(iIndex, iKey, const szNewVal[]);

/**
 * Returns a bytearray type value from an entities entvar struct.
 *
 * @note For a list of valid bytearray type entries, see the EV_BYTE_* constants
 *       in engine_const.inc
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to retrieve from
 *
 * @return          Value of specified entry, 0 if an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_get_byte(iIndex, iKey);

/**
 * Sets a bytearray type value in an entities entvar struct.
 *
 * @note For a list of valid bytearray type entries, see the EV_BYTE_* constants
 *       in engine_const.inc
 * @note The value is automatically clamped to [0,255].
 *
 * @param iIndex    Entity index
 * @param iKey      Entry to write to
 * @param iVal      Value to set
 *
 * @return          1 if value was sucessfully set, 0 if an invalid entry was
 *                  specified
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_set_byte(iIndex, iKey, iVal);

/**
 * Creates an entity.
 *
 * @note When creating an entity the classname has to be valid in the mod, as
 *       the engine needs to link the entity to an existing class internally.
 *       The classname string that is stored in the entvar struct
 *       (EV_SZ_classname) is separate from this association and can later be
 *       freely changed to serve other purposes.
 *
 * @param szClassname   Entity classname
 *
 * @return              Entity index > 0 on success, 0 otherwise
 */
native create_entity(const szClassname[]);

/**
 * Removes an entity from the world.
 *
 * @param iIndex    Entity index
 *
 * @return          1 if entity was sucessfully removed, 0 if an invalid entity
 *                  was provided
 * @error           If an entity index in the range of 0 to MaxClients is
 *                  provided, an error will be thrown.
 */
native remove_entity(iIndex);

/**
 * Returns the current number of entities in the world.
 *
 * @return  Number of entities
 */
native entity_count();

/**
 * Returns if an entity index is valid (as required by other engine natives).
 *
 * @note Engine considers an entity index valid if it is in the range between 1
 *       and the maximum number of entities possible. The index also has to
 *       point to an existing entity or, if it is a client index, the client has
 *       to be connected.
 *
 * @param iIndex    Entity index
 *
 * @return          1 if entity is valid, 0 otherwise
 */
native is_valid_ent(iIndex);

/**
 * Searches entities in the world, starting at a specified index and matching by
 * classname.
 *
 * @param iIndex    Entity index to start from
 * @param szClass   Classname to match
 *
 * @return          Entity index if an entity was found, 0 otherwise
 */
native find_ent_by_class(iIndex, const szClass[]);

/**
 * Searches entities in the world, starting at a specified index, matching by
 * owner and a configurable entity field.
 *
 * @param iIndex        Entity index to start from
 * @param szClass       String to match
 * @param iOwner        Owner entity index to match
 * @param iJghgType     Entity field to match string against:
 *                          0 - Classname
 *                          1 - Target
 *                          2 - Targetname
 *
 * @return              Entity index if an entity was found, 0 otherwise
 */
native find_ent_by_owner(iIndex, const szClass[], iOwner, iJghgType = 0);

/**
 * Searches entities in the world, starting at a specified index and matching by
 * target.
 *
 * @param iIndex    Entity index to start from
 * @param szClass   Target to match
 *
 * @return          Entity index if an entity was found, 0 otherwise
 */
native find_ent_by_target(iIndex, const szClass[]);

/**
 * Searches entities in the world, starting at a specified index and matching by
 * targetname.
 *
 * @param iIndex    Entity index to start from
 * @param szClass   Targetname to match
 *
 * @return          Entity index if an entity was found, 0 otherwise
 */
native find_ent_by_tname(iIndex, const szClass[]);

/**
 * Searches entities in the world, starting at a specified index and matching by
 * classname and model.
 *
 * @param iIndex    Entity index to start from
 * @param szClass   Classname to match
 * @param szModel   Model to match
 *
 * @return          Entity index if an entity was found, 0 otherwise
 */
native find_ent_by_model(iIndex, const szClass[], const szModel[]);

/**
 * Searches for entities inside a sphere, starting at a specified index.
 *
 * @param start_from_ent    Entity index to start from
 * @param origin            Center of sphere
 * @param radius            Sphere radius
 *
 * @return                  Entity index if an entity was found, 0 otherwise
 */
native find_ent_in_sphere(start_from_ent, const Float:origin[3], Float:radius);

/**
 * Searches for entities inside a sphere around a specified entity or origin,
 * matching by classname.
 *
 * @note This native always starts searching from entity index 0, there is no
 *       way to specify the starting point. If the entlist array is not big
 *       enough to accomodate all entities, the results will be truncated.
 *
 * @param aroundent             Entity index to center sphere around, < 1 to use
 *                              origin
 * @param _lookforclassname     Classname to match
 * @param radius                Sphere radius
 * @param entlist               Array to store entities in
 * @param maxents               Maximum size of array
 * @param origin                Center of sphere, used if aroundent < 1
 *
 * @return                      Number of entities stored in entlist
 * @error                       If an invalid entity index is provided or, if
 *                              the index is a client index, the client is not
 *                              connected, an error will be thrown.
 */
native find_sphere_class(aroundent, const _lookforclassname[], Float:radius, entlist[], maxents, const Float:origin[3] = {0.0, 0.0, 0.0});

/**
 * Sets the origin of an entity.
 *
 * @note This native uses engine functions to set the origin, keeping it
 *       properly updated with the game. Directly writing to EV_VEC_origin is an
 *       error and will cause problems.
 *
 * @param iIndex        Entity index
 * @param fNewOrigin    New origin
 *
 * @noreturn
 * @error               If an invalid entity index is provided, an error will be
 *                      thrown.
 */
native entity_set_origin(iIndex, const Float:fNewOrigin[3]);

/**
 * Sets the model of an entity.
 *
 * @note This native uses an engine function to set the model, keeping it
 *       properly updated with the game. Simply writing to EV_SZ_model is an
 *       error and will cause problems.
 *
 * @param iIndex    Entity index
 * @param szModel   Model to set
 *
 * @noreturn
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native entity_set_model(iIndex, const szModel[]);

/**
 * Sets rendering options of an entity.
 *
 * @note For a list of valid rendering effects see the kRenderFx* constants in
 *       amxconst.inc
 * @note For a list of valid rendering modes see the kRender* constants in
 *       amxconst.inc
 * @note Rendering amount has different meanings depending on the rendering
 *       effect and mode used on the entity.
 *
 * @param index     Entity index
 * @param fx        Rendering effect
 * @param r         Red component of rendering color
 * @param g         Green component of rendering color
 * @param b         Blue component of rendering color
 * @param render    Rendering mode
 * @param amount    Rendering amount
 *
 * @noreturn
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native set_ent_rendering(index, fx = kRenderFxNone, r = 0, g = 0, b = 0, render = kRenderNormal, amount = 0);

/**
 * Calls the DispatchThink() game DLL function on an entity, triggering it to
 * think if applicable.
 *
 * @note DispatchThink() checks the entity for the FL_DORMANT flag - if it is
 *       set, the entity will not proceed to think. It will first call the
 *       class-specific think function and eventually CBaseEntity::Think(), thus
 *       triggering other think hooks and forwards.
 *
 * @param entity    Entity index
 *
 * @noreturn
 * @error           If an invalid entity index is provided, an error will be
 *                  thrown.
 */
native call_think(entity);

/**
 * Forces an entity to touch another entity.
 *
 * @note This calls the game touch function even when the entities do not
 *       intersect. It doesn't change their origins and/or bounding boxes.
 *
 * @param entTouched    Index of entity being touched
 * @param entToucher    Index of entity touching
 *
 * @noreturn
 * @error               If an invalid entity index is provided or, if the index
 *                      is a client index, the client is not connected, an error
 *                      will be thrown.
 */
native fake_touch(entTouched, entToucher);

/**
 * Calls the spawn function on an entity.
 *
 * @param iIndex    Entity index
 *
 * @noreturn
 * @error           If an invalid entity index is provided or, if the index is a
 *                  client index, the client is not connected, an error will be
 *                  thrown.
 */
native DispatchSpawn(iIndex);

/**
 * Fires/sets a keyvalue on an entity.
 *
 * @param ...   (1) To fire a new keyvalue struct, three parameters should be
 *                  provided in the following manner:
 *                      DispatchKeyValue(entity, "KeyName", "Value");
 *                  The "szClassName" value will automatically use the classname
 *                  of the specified entity, "fHandled" will be set to 0.
 *              (2) Inside the pfn_keyvalue() forward this native can be used to
 *                  modify the keyvalue struct inline, two parameters should be
 *                  provided in the following manner:
 *                     DispatchKeyValue("KeyName", "Value");
 *                  The "szClassName" or "fHandled" values can not be changed.
 *
 * @noreturn
 * @error       For variant (1), if an invalid entity index is provided, an
 *              error will be thrown. For variant (2), if it is used outside of
 *              the pfn_keyvalue() forward, an error will be thrown.
 */
native DispatchKeyValue(...);

/**
 * Retrieves a value from an entities keyvalues.
 *
 * @param entity        Entity index
 * @param szKey         Key to retrieve value of
 * @param value         Buffer to copy value to
 * @param maxLength     Maximum size of buffer
 *
 * @return              Number of cells written to buffer
 * @error               If an invalid entity index is provided or, if the index
 *                      is a client index, the client is not connected, an error
 *                      will be thrown.
 */
native get_keyvalue(entity, const szKey[], value[], maxLength);

/**
 * Retrieves buffers from the keyvalue structure.
 *
 * @note Can only be used inside the pfn_keyvalue() forward.
 *
 * @param szClassName   Buffer to copy classname to
 * @param sizea         Maximum size of classname buffer
 * @param szKeyName     Buffer to copy keyname to
 * @param sizeb         Maximum size of keyname buffer
 * @param szVlaue       Buffer to copy value to
 * @param sizec         Maximum size of value buffer
 *
 * @return              1 on success, 0 if used outside the pfn_keyvalue()
 *                      forward
 */
native copy_keyvalue(szClassName[], sizea, szKeyName[], sizeb, szValue[], sizec);

/**
 * Hurts (and kills, if applicable) players in a sphere.
 *
 * @note Players that have the DAMAGE_NO flag set in EV_INT_flags will be
 *       ignored.
 * @note The sphere has four different damage zones. Below is pseudo-code of the
 *       algorithm, indicating how damage will be dealt to players:
 *       if (distance <= 5 * radius) damage(10 + random(1 * dmg_multi))
 *       if (distance <= 4 * radius) damage(25 + random(2 * dmg_multi))
 *       if (distance <= 3 * radius) damage(50 + random(3 * dmg_multi))
 *       if (distance <= 2 * radius) kill()
 *
 * @param fExplodeAt            Center origin of sphere
 * @param iDamageMultiplier     Damage multiplier
 * @param iRadiusMultiplier     Sphere radius
 *
 * @noreturn
 */
native radius_damage(const Float:fExplodeAt[3], iDamageMultiplier, iRadiusMultiplier);

/**
 * Returns the contents value of an origin.
 *
 * @note For a list of valid contents values see the CONTENTS_* constants in
 *       hlsdk_const.inc
 *
 * @param fCheckAt  Origin to retrieve contents of
 *
 * @return          Contents value
 */
native point_contents(const Float:fCheckAt[3]);

/**
 * Returns if an origin is in an entities view cone. Derived from SDK.
 *
 * @note This uses the entities EV_FL_fov value in the calculations and applies
 *       it on all axes. It might be unreliable depending on the use-case.
 *
 * @param entity    Entity index
 * @param origin    Origin
 * @param use3d     If zero the calculation will ignore the z axis (height), if
 *                  nonzero it is done in 3D
 *
 * @return          1 if origin is in view code, 0 otherwise
 */
native is_in_viewcone(entity, const Float:origin[3], use3d = 0);

/**
 * Returns if an entity is visible to another entity. Derived from SDK.
 *
 * @note If the target entity has the FL_NOTARGET flag set, this native always
 *       returns 0.
 * @note This native fires a traceline between the view-offset origins of the
 *       entities. If the traceline is unobstructed it returns true. This is not
 *       a full 3D visibility check.
 *
 * @param entity    Entity index
 * @param target    Target entity index
 *
 * @return          1 if entity is visible, 0 otherwise
 * @error           If an invalid entity index is provided or, if the index is a
 *                  client index, the client is not connected, an error will be
 *                  thrown.
 */
native is_visible(entity, target);

/**
 * Fires a trace line between two origins, retrieving the end point and entity
 * hit.
 *
 * @note This native writes to the global engine module trace handle. Additional
 *       trace results can be retrieved using traceresult().
 * @note This native returns 0 if the trace did not hit anything. As 0 is an
 *       entity index that is considered to be a valid value for a trace hit
 *       ("worldspawn"), this native can potentially return a misleading value.
 *       Check other components of the trace result to verify the entity index.
 *
 * @param iIgnoreEnt    Entity index that trace will ignore, -1 if trace should
 *                      not ignore any entities
 * @param fStart        Trace starting point
 * @param fEnd          Trace target point
 * @param vReturn       Vector to copy trace end point to
 *
 * @return              Entity index if trace hit an entity, 0 otherwise
 */
native trace_line(iIgnoreEnt, const Float:fStart[3], const Float:fEnd[3], Float:vReturn[3]);

/**
 * Fires a trace line between two origins, retrieving the trace normal.
 *
 * @note This native writes to the global engine module trace handle. Additional
 *       trace results can be retrieved using traceresult().
 *
 * @param iIgnoreEnt    Entity index that trace will ignore, -1 if trace should
 *                      not ignore any entities
 * @param fStart        Trace starting point
 * @param fEnd          Trace target point
 * @param vReturn       Vector to copy trace normal to
 *
 * @return              1 if a normal is available (trace hit something), 0
 *                      otherwise
 */
native trace_normal(iIgnoreEnt, const Float:fStart[3], const Float:fEnd[3], Float:vReturn[3]);

/**
 * Fires a trace hull on a specified origin or between two origins.
 *
 * @note This native writes to the global engine module trace handle. Additional
 *       trace results can be retrieved using traceresult().
 * @note For a list of valid hull types see the HULL_* constants in
 *       hlsdk_const.inc
 * @note For a list of valid ignore types see the *IGNORE_* constants in
 *       hlsdk_const.inc
 *
 * @param origin            Trace start point (and end point if not specified)
 * @param hull              Hull type
 * @param ignoredent        Entity index that trace will ignore
 * @param ignoremonsters    Entity ignore type
 * @param end               Trace end point, pass NULL_VECTOR to use start point
 *
 * @return                  Custom bitflag sum of relevant trace results
 *                          StartSolid (1), AllSolid (2) and InOpen (4)
 */
native trace_hull(const Float:origin[3], hull, ignoredent = 0, ignoremonsters = 0, const Float:end[3] = NULL_VECTOR);

/**
 * Attempts to describe an obstacle by firing trace lines in a specified
 * direction, offset on the z-axis around an origin.
 *
 * @note The functionality of this native can mostly be replaced by a single
 *       hull trace. This native does not write to the global engine module
 *       trace handle.
 * @note This native is intended to examine an obstacle in front of a standing
 *       player. Start should usually be the origin of a client while angle
 *       should be its forward angle vector. 73 traces are fired, each offset by
 *       one unit on the z-axis from the last, starting at -36 and moving up to
 *       +36. This is because a standing player model is 72 units high, so 73
 *       units of clearance are required to fit them. The values stored in the
 *       various parameters then attempt to describe the obstacle.
 * @note To fully understand the nuances of the algorithm it is necessary to
 *       view its source code located in engine.cpp of the engine module.
 *
 * @param start             Starting origin
 * @param angle             Trace line direction
 * @param give              Units that a trace line can be longer than the
 *                          shortest trace line to still be considered hitting
 *                          the same obstacle
 * @param ignoreEnt         Entity index that traces will ignore, -1 if traces
 *                          should not ignore any entities
 * @param hitX              Variable to store X axis value of shortest trace
 *                          line endpoint in
 * @param hitY              Variable to store Y axis value of shortest trace
 *                          line endpoint in
 * @param shortestDistance  Variable to store length of shortest trace line in
 * @param shortestDistLow   Variable to store Z axis offset of shortest trace
 *                          line in
 * @param shortestDistHigh  Variable to store Z axis offset of highest trace
 *                          line that satisfies "give" condition in
 *
 * @noreturn
 */
native trace_forward(const Float:start[3], const Float:angle[3], Float:give, ignoreEnt, &Float:hitX, &Float:hitY, &Float:shortestDistance, &Float:shortestDistLow, &Float:shortestDistHigh);

/**
 * Finds a grenade entity, matching by owner.
 *
 * @param id            Owner entity index to match
 * @param model         Buffer to copy grenade model to
 * @param len           Maximum length of buffer
 * @param grenadeid     Entity index to start searching from
 *
 * @return              Grenade entity index > 0 if found, 0 otherwise
 * @error               If an invalid entity index is provided, an error will be
 *                      thrown.
 */
native get_grenade_id(id, model[], len, grenadeid = 0);

/**
 * Returns the game time based on the game tick.
 *
 * @note This time is counted up from map start. If the engine is not processing
 *       this function will return the same value between calls, which makes it
 *       unusable for profiling purposes.
 *
 * @return      Game time, in seconds
 */
native Float:halflife_time();

/**
 * Sets the map lighting level.
 *
 * @note After setting the map lighting level, the engine module enforces it by
 *       continuously re-applying it until it is reset.
 *
 * @param Lighting  Map lighting level (described by a character a-z), #OFF to
 *                  reset
 *
 * @noreturn
 */
native set_lights(const Lighting[]);

/**
 * Attaches a clients viewport to an entity.
 *
 * @note To reset the clients viewport, call this function with the client index
 *       as the target entity.
 *
 * @param iIndex        Client index
 * @param iTargetIndex  Index of entity to attach to
 *
 *
 * @error               If the client index is not within the range of 1 to
 *                      MaxClients, or the client is not connected, an error
 *                      will be thrown.
 */
native attach_view(iIndex, iTargetIndex);

/**
 * Sets the engine module view mode on a client.
 *
 * @note For a list of valid view modes see the CAMERA_* constants in
 *       engine_const.inc
 * @note The engine module uses a custom entity to achieve the camera effects
 *       and requires "models/rpgrocket.mdl" to be precached by the plugin.
 *
 * @param iIndex    Client index
 * @param ViewType  View mode
 */
native set_view(iIndex, ViewType);

/**
 * Plays back an event on the client. Most prominently used for gun firing
 * animations.
 *
 * @note Event indexes can be acquired using precache_event() with the sc dummy
 *       files in the events folder.
 *
 * @param flags         Event flags
 * @param invoker       Index of entity to invoke event on
 * @param eventindex    Index of event in the precache table
 * @param delay         Time until the event is played
 * @param origin        Origin to play event from
 * @param angles        Angles to play event with
 * @param fparam1       Float parameter 1 to pass along into/with the event
 * @param fparam2       Float parameter 2 to pass along into/with the event
 * @param iparam1       Integer parameter 1 to pass along into/with the event
 * @param iparam2       Integer parameter 2 to pass along into/with the event
 * @param bparam1       Boolean parameter 1 to pass along into/with the event
 * @param bparam2       Boolean parameter 2 to pass along into/with the event
 *
 * @noreturn
 */
native playback_event(flags, invoker, eventindex, Float:delay, const Float:origin[3], const Float:angles[3], Float:fparam1, Float:fparam2, iparam1, iparam2, bparam1, bparam2);

/**
 * Retrieves a value from a usercmd struct.
 *
 * @note This native can only be used inside the client_cmdStart() forward. If
 *       it is used outside this forward it will not retrieve any results and
 *       always return 0.
 * @note For a list of valid usercmd entries see the usercmd_* constants in
 *       engine_const.inc
 *
 * @param type  Entry to retrieve from
 * @param ...   Depending on the entry type a different number of
 *              additional parameters should be provided:
 *                 int      - Returns the entry integer value directly, no
 *                            additional parameters required
 *                 float    - Stores the entry float value into the
 *                            variable provided as the second parameter
 *                 vector   - Copies the entry vector to the Float:array[3]
 *                            provided in the second parameter
 *
 * @return      Changes depending on the entry type:
 *                 int      - Returns the entry integer value
 *                 float    - Returns 1
 *                 vector   - Returns 1
 */
native get_usercmd(type, any:...);

/**
 * Sets a value in a usercmd struct.
 *
 * @note This native can only be used inside the client_cmdStart() forward.
 * @note For a list of valid usercmd entries see the usercmd_* constants in
 *       engine_const.inc
 * @note Changes will be immediately reflected in get_usercmd() for all plugins.
 *
 * @param type  Entry to write to
 * @param ...   Depending on the entry type a different additional parameter
 *              should be provided:
 *                 int      - Second parameter should be an integer variable
 *                 float    - Second parameter should be a float variable
 *                 vector   - Second parameter should be a Float:array[3]
 *
 * @noreturn
 */
native set_usercmd(type, any:...);

/**
 * Retrieves a string from the engine string table.
 *
 * @param _string           String table index
 * @param _returnString     Buffer to copy string to
 * @param _len              Maximum size of buffer
 *
 * @return                  Number of cells written to buffer
 */
native eng_get_string(_string, _returnString[], _len);

/**
 * @section Forwards
 */

/**
 * Called when two entities touch.
 *
 * @param ptr   Index of entity being touched
 * @param ptd   Index of entity touching
 *
 * @return      PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward pfn_touch(ptr, ptd);

/**
 * Called at the start of every server frame.
 *
 * @note Using his forward can easily become performance-critical. More specific
 *       hooks and forwards should be used whenever possible.
 *
 * @noreturn
 */
forward server_frame();

/**
 * Called when a client types kill in console.
 *
 * @param id    Client index
 *
 * @return      PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward client_kill(id);

/**
 * Called at the start of each client think.
 *
 * @note Using his forward can easily become performance-critical. More specific
 *       hooks and forwards should be used whenever possible.
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_PreThink(id);

/**
 * Called after each client think.
 *
 * @note Using his forward can easily become performance-critical. More specific
 *       hooks and forwards should be used whenever possible.
 *
 * @param id    Client index
 *
 * @noreturn
 */
forward client_PostThink(id);

/**
 * Called when a client triggers an impulse.
 *
 * @param id        Client index
 * @param impulse   Impulse triggered by client
 *
 * @param           PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to
 *                  nullify impulse (CmdStart() is not blocked)
 */
forward client_impulse(id, impulse);

/**
 * Called for CmdStart() on a client.
 *
 * @note Use [get|set]_usercmd() to read and modify information in the usercmd
 *       struct.
 *
 * @param id    Client index
 *
 * @return      PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward client_cmdStart(id);

/**
 * Called when an entity thinks.
 *
 * @param entid     Entity index
 *
 * @return          PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward pfn_think(entid);

/**
 * Called when an event is played.
 *
 * @param flags     Event flags
 * @param entid     Index of entity to invoke event on
 * @param eventid   Index of event in the precache table
 * @param delay     Time until the event is played
 * @param Origin    Origin to play event from
 * @param Angles    Angles to play event with
 * @param fparam1   Float parameter 1 to pass along into/with the event
 * @param fparam2   Float parameter 2 to pass along into/with the event
 * @param iparam1   Integer parameter 1 to pass along into/with the event
 * @param iparam2   Integer parameter 2 to pass along into/with the event
 * @param bparam1   Boolean parameter 1 to pass along into/with the event
 * @param bparam2   Boolean parameter 2 to pass along into/with the event
 *
 * @return          PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward pfn_playbackevent(flags, entid, eventid, Float:delay, Float:Origin[3], Float:Angles[3], Float:fparam1, Float:fparam2, iparam1, iparam2, bparam1, bparam2);

/**
 * Called when a keyvalue pair is sent to an entity.
 *
 * @note Use copy_keyvalue() to retrieve the keyvalue information, and
 *       DispatchKeyVaue() to modify it.
 *
 * @param entid     Entity index
 *
 * @return          PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward pfn_keyvalue(entid);

/**
 * Called when an entity is spawned.
 *
 * @param entid     Entity index
 *
 * @return          PLUGIN_CONTINUE to ignore, PLUGIN_HANDLED or higher to block
 */
forward pfn_spawn(entid);

/**
 * @endsection
 */

#include <engine_stocks>
