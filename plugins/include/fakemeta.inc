/* FakeMeta functions
 * This file is provided as-is with no warrenties.
 */

#if defined _fakemeta_included
	#endinput
#endif
#define _fakemeta_included

#include <fakemeta_const> 

/* Returns entvar data from an entity   Use the pev_* enum to specify which form of data you want returned. */
native pev(_index,_value,{Float,_}:...)

/* Sets entvar data for an entity.  Use the pev_* enum */
native set_pev(_index,_value,{Float,_}:...)

/* Returns an integer from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native get_pdata_int(_index,_Offset,_linuxdiff=5);

/* Sets an integer from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native set_pdata_int(_index,_Offset,_Value,_linuxdiff=5);

/* Returns a float from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native Float:get_pdata_float(_index,_Offset,_linuxdiff=5);

/* Sets a float from private data.  _linuxdiff is added into the _Offset if it's used on a linux server. */
native set_pdata_float(_index,_Offset,Float:_Value,_linuxdiff=5);


/* Registers a forward */
native register_forward(_forwardType,_function[],_post=0);

/* Returns data for metamod */
native forward_return(type,{Float,_}:...);

native engfunc(type,{Float,_}:...);
native dllfunc(type,{Float,_}:...);