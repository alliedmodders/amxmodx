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

/* Returns entvar data from an entity   Use the pev_* enum to specify which form of data you want returned.
 *
 * If retrieving strings, you may optionally get a pointer into the global string table. Depending on
 * your situation, there are two ways to do this.
 * 1: This simply gets the pointer.
 *    new ptr = pev(entid, pev_classname)
 * 2: The pointer will be stored in ptr AND the actual string is retrieved.
 *    new ptr, classname[32]
 *    pev(entid, pev_classname, ptr, classname, 31)
 */
native pev(_index,_value,{Float,Sql,Result,_}:...);

/* Sets entvar data for an entity.  Use the pev_* enum */
native set_pev(_index,_value,{Float,Sql,Result,_}:...);

/* returns 0 if ent is invalid, >0 if valid
 * (1 == valid, 2 == valid+pvPrivateData valid)
 */
native pev_valid(entindex);

/* Returns any global variable inside globalvars_t structure. Use the glb_* enum.
 *
 * When returning data from glb_pStringBase (the global string table), you may give a pointer into that table
 * in order to get different strings.
 * Example:
 * new model[128]
 * new ptr = pev(id, pev_viewmodel)
 * global_get(glb_pStringBase, ptr, model, 127)
 */
native global_get(_value, {Float,Sql,Result,_}:...);

/* Returns an integer from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native get_pdata_int(_index,_Offset,_linuxdiff=5);

/* Sets an integer from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native set_pdata_int(_index,_Offset,_Value,_linuxdiff=5);

/* Returns a float from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native Float:get_pdata_float(_index,_Offset,_linuxdiff=5);

/* Sets a float from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native set_pdata_float(_index,_Offset,Float:_Value,_linuxdiff=5);


/* Registers a forward.
 * Returns an id you can pass to unregister_forward
 */
native register_forward(_forwardType,_function[],_post=0);

/* Unregisters a forward.
 * The registerId must be from register_forward, and
 * post/forwardtype must match what you registered the forward as.
 */
native unregister_forward(_forwardType, registerId, post=0);

/* Returns data for metamod */
native forward_return(type,{Float,Sql,Result,_}:...);

/* Returns the original return value of an engine function. 
 * This is only valid in forwards that were registered as post.
 *
 * get_orig_retval() - no params, retrieves integer return value
 * get_orig_retval(&Float:value) - retrieves float return value by reference
 * get_orig_retval(value[], len) - retrives string return value
 */
native get_orig_retval({Float,_}:...)

native engfunc(type,{Float,Sql,Result,AlertType,_}:...);
native dllfunc(type,{Float,Sql,Result,_}:...);

//only use this with functions that pass a Trace
// get: zero extra params - return int, one extra param = byref float or vector
// set: use anything
native get_tr(TraceResult:tr_member, {Float,_}:...);
native set_tr(TraceResult:tr_member, {Float,_}:...);

//Upgraded version takes in a TraceResult handle, optionally passed in as the last parameter to the
//TraceResult forward.  Use 0 to specify the global traceresult handle set from calling 
// some of the Engfucs.
native get_tr2(tr_handle, TraceResult:tr_member, {Float,_}:...);
native set_tr2(tr_handle, TraceResult:tr_member, {Float,_}:...);

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
native get_pdata_string(entity, offset, dest[], maxlength, byref=1, linux=-5);

//Sets a string in a private offset.
//realloc = -1 - nonbyref copy (static
//realloc = 0 - copy byref, no realloc *(char **)
//realloc = 1 - reallocate new string with free+malloc
//realloc = 2 - reallocate new string with delete[]+new[]
native set_pdata_string(entity, offset, const source[], realloc=2, linux=-5);

// Copies the given infoBuffer pointer into out[]
// An infoBuffer pointer is returned by EngFunc_GetInfoKeyBuffer
native copy_infokey_buffer(infoBuffer, out[], maxlen);
