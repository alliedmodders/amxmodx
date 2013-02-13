/* FakeMeta functions
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _fakemeta_included
	#endinput
#endif
#define _fakemeta_included

#include <fakemeta_const> 

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib fakemeta
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib fakemeta
 #endif
#else
 #pragma library fakemeta
#endif

/**
 * Returns entvar data from an entity.  Use the pev_* enum (in fakemeta_const.inc) to specify which data you want retrieved.
 *
 * @note	This function uses "read_data" style data syntax.  It returns integer values,
 *				by-references float data, and sets a buffer for string data.
 *
 * @note	If retrieving strings, you may optionally get a pointer into the global string table. Depending on
 * 			your situation, there are two ways to do this.
 * 			1: This simply gets the pointer.
 * 			   new ptr = pev(entid, pev_classname)
 * 			2: The pointer will be stored in ptr AND the actual string is retrieved.
 * 			   new ptr, classname[32]
 * 			   pev(entid, pev_classname, ptr, classname, 31)
 *
 * @param	_index		The entity index to lookup.
 * @param	_value		The pev field to lookup (look in fakemeta_const.inc)
 */
native pev(_index,_value,any:...);

/**
 * Sets entvar data for an entity.  Use the pev_* enum from fakemeta_const.inc for reference.
 *
 * @note	Setting string data will automatically allocate a new string (via AllocString)
 *			If you have a string already allocated with your own call to AllocString, use
 *			set_pev_string_ptr instead.
 *
 * @param	_index		The entity index to set the value on.
 * @param	_value		The pev field to set, (look in fakemeta_const.inc)
 */
native set_pev(_index,_value,any:...);

/**
 * Use this native to set a pev field to a string that is already allocated (via a function such
 * as EngFunc_AllocString).
 *
 * @note	If you specify _value as anything other than string fields, an error will be thrown.
 * @note	Pass 0 as the _string field to set it to an empty string.
 *
 * @param	_index		The entity index to set the value on.
 * @param	_value		The pev field to set - MUST be a string field.
 * @param	_string		The string handle, retrieved from places like AllocString.
 */
native set_pev_string(_index, _value, _string);


/**
 * Checks the validity of an entity.
 *
 * @param entindex		The entity id to check.
 *
 * @return 			0 on invalid entity
 *						1 on entity is valid
 *						2 on entity is valid and it has private data (safe to use pdata natives on).
 */
native pev_valid(entindex);

/**
 * Returns the serial number for each entity.  The serial number is a unique identity 
 * generated when an entity is created.
 * 
 * @param	entindex	The entity id.
 *
 * @return				The serial number for the entity.
 */
native pev_serial(entindex);

/* Returns any global variable inside globalvars_t structure. Use the glb_* enum.
 *
 * When returning data from glb_pStringBase (the global string table), you may give a pointer into that table
 * in order to get different strings.
 * Example:
 * new model[128]
 * new ptr = pev(id, pev_viewmodel)
 * global_get(glb_pStringBase, ptr, model, 127)
 */
native global_get(_value, any:...);

/* Returns an integer from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native get_pdata_int(_index,_Offset,_linuxdiff=5,_macdiff=5);

/* Sets an integer from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native set_pdata_int(_index,_Offset,_Value,_linuxdiff=5,_macdiff=5);

/* Returns a float from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native Float:get_pdata_float(_index,_Offset,_linuxdiff=5,_macdiff=5);

/* Sets a float from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native set_pdata_float(_index,_Offset,Float:_Value,_linuxdiff=5,_macdiff=5);

/**
 * Tries to retrieve an edict (entity encapsulation) pointer from an entity's private data.
 *
 * This function is byte-addressable.  Unlike get_pdata_int() which searches in byte increments of 4, 
 * get_pdata_end searches in increments of 1.
 *
 * @param _index		Entity index.
 * @param _offset		Offset to search.
 * @param _linuxdiff		Linux difference.
 * @param _macdiff		Mac OS X difference.
 * @return				-2 if an invalid entity was found.
 *						-1 if an empty entity was found.
 * 						Otherwise, an entity index is returned.
 */
native get_pdata_ent(_index, _offset, _linuxdiff=20, _macdiff=20);

/* Registers a forward.
 * Returns an id you can pass to unregister_forward
 */
native register_forward(_forwardType,const _function[],_post=0);

/* Unregisters a forward.
 * The registerId must be from register_forward, and
 * post/forwardtype must match what you registered the forward as.
 */
native unregister_forward(_forwardType, registerId, post=0);

/* Returns data for metamod */
native forward_return(type,any:...);

/* Returns the original return value of an engine function. 
 * This is only valid in forwards that were registered as post.
 *
 * get_orig_retval() - no params, retrieves integer return value
 * get_orig_retval(&Float:value) - retrieves float return value by reference
 * get_orig_retval(value[], len) - retrives string return value
 */
native get_orig_retval({Float,_}:...);

native engfunc(type,any:...);
native dllfunc(type,any:...);

//only use this with functions that pass a Trace
// get: zero extra params - return int, one extra param = byref float or vector
// set: use anything
native get_tr(TraceResult:tr_member, {Float,_}:...);
native set_tr(TraceResult:tr_member, {Float,_}:...);

//Upgraded version takes in a TraceResult handle, optionally passed in as the last parameter to the
//TraceResult forward.  Use 0 to specify the global traceresult handle set from calling 
// some of the Engfucs.
native get_tr2(tr_handle, {TraceResult,_}:tr_member, {Float,_}:...);
native set_tr2(tr_handle, {TraceResult,_}:tr_member, {Float,_}:...);

/**
 * Creates a traceresult handle.  This value should never be altered.
 * The handle can be used in get/set_tr2 and various traceresult engine functions.
 *
 * NOTE: You must call free_tr2() on every handle made with create_tr2().
 *
 * @return		A new TraceResult handle.
 */
native create_tr2();

/** 
 * Frees a traceresult handle created with free_tr2().  Do not call
 * this more than once per handle, or on handles not created through
 * create_tr2().
 *
 * @param tr_handle	TraceResult handle created via create_tr2().
 * @noreturn
 */
native free_tr2(tr_handle);

//Same as above, use either a kvd_handle or 0 for global reserved kvd data
//kvd_handle is passed by the kvd hook, last param
native get_kvd(kvd_handle, KeyValueData:member, {Float,_}:...);

//Using set_kvd with the handle from the hook for anything under KV_fHandled
// is considered an undefined operation (it could crash).  You should fire a new
// keyvalues structure rather than changing the internal engine strings.
native set_kvd(kvd_handle, KeyValueData:member, {Float,_}:...);

// These functions are used with the clientdata data structure (FM_UpdateClientData)
// Get: 0 extra params - Return integer; 1 extra param - by ref float or vector; 2 extra params - string and length
// Set: Use anything
// Use 0 for cd_handle to specify the global clientdata handle
native get_cd(cd_handle, ClientData:member, {Float,_}:...);
native set_cd(cd_handle, ClientData:member, {Float,_}:...);

// These functions are used with the entity_state data structure (FM_AddToFullPack)
// Get: 0 extra params - Return integer; 1 extra param - by ref float or vector or array
// Set: Use anything
// Use 0 for es_handle to specify the global entity_state handle
native get_es(es_handle, EntityState:member, {Float,_}:...);
native set_es(es_handle, EntityState:member, {Float,_}:...);

// These functions are used with the usercmd data structure (FM_CmdStart)
// Get: 0 extra params - Return integer; 1 extra param - by ref float or vector
// Set: Use anything
// Use 0 for uc_handle to specify the global usercmd handle
native get_uc(uc_handle, UserCmd:member, {Float,_}:...);
native set_uc(uc_handle, UserCmd:member, {Float,_}:...);

//NOTE that for the string offsets below, on AMD64, a byref (char **) offset is NOT the same as an int offset
//In fact it's QWORD aligned rather than DWORD aligned, so the offset will be exactly half.
//Gets a string from a private offset.  If byref is false, the string is treated as static rather than dynamic.
//linux value is what to add to the offset for linux servers.
//mac value is what to add to the offset for os x servers. Default (cellmin) means that linux value will be used.
//this cannot use a default value due to older version using an awkward default value.
native get_pdata_string(entity, offset, dest[], maxlength, byref=1, linux, mac=cellmin);

//Sets a string in a private offset.
//realloc = -1 - nonbyref copy (static
//realloc = 0 - copy byref, no realloc *(char **)
//realloc = 1 - reallocate new string with free+malloc
//realloc = 2 - reallocate new string with delete[]+new[]
//linux value is what to add to the offset for linux servers.
//mac value iswhat to add to the offset for os x servers.
//this cannot use a default value due to older version using an awkward default value.
native set_pdata_string(entity, offset, const source[], realloc=2, linux, mac=cellmin);

// Copies the given infoBuffer pointer into out[]
// An infoBuffer pointer is returned by EngFunc_GetInfoKeyBuffer
native copy_infokey_buffer(infoBuffer, out[], maxlen);


/**
 * Looks up the sequence for the entity.
 *
 * @param entity		The entity id to lookup.
 * @param name			The sequence name to lookup, case insensitive. ("JUMP" would match "jump")
 * @param framerate		The framerate of the sequence, if found.
 * @param loops			Whether or not the sequence loops.
 * @param groundspeed	The groundspeed setting of the sequence.
 * @return				-1 on failed lookup, the sequence number on successful lookup.
 */
native lookup_sequence(entity, const name[], &Float:framerate = 0.0, &bool:loops = false, &Float:groundspeed = 0.0);

/**
 * Sets a bone controller with the specified value.
 *
 * @param entity		The entity id to set the value on.
 * @param controller	Which controller to set (0 through 3).
 * @param value			The value to set it to.
 * @return				The percentage that the controller is extended (0.0 through 1.0)
 */
native Float:set_controller(entity, controller, Float:value);
