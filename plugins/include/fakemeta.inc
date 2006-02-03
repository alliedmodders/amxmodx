/* FakeMeta functions
 * This file is provided as-is with no warrenties.
 */

#if defined _fakemeta_included
	#endinput
#endif
#define _fakemeta_included

#include <fakemeta_const> 

#pragma library fakemeta

/* Returns entvar data from an entity   Use the pev_* enum to specify which form of data you want returned. */
native pev(_index,_value,{Float,Sql,Result,_}:...);

/* Sets entvar data for an entity.  Use the pev_* enum */
native set_pev(_index,_value,{Float,Sql,Result,_}:...);

/* returns 0 if ent is invalid, >0 if valid
 * (1 == valid, 2 == valid+pvPrivateData valid)
 */
native pev_valid(entindex);

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

native engfunc(type,{Float,Sql,Result,_}:...);
native dllfunc(type,{Float,Sql,Result,_}:...);

//only use this with functions that pass a Trace
// get: zero extra params - return int, one extra param = byref float or vector
// set: use anything
native get_tr(TraceResult:tr_member, {Float,_}:...);
native set_tr(TraceResult:tr_member, {Float,_}:...);

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
