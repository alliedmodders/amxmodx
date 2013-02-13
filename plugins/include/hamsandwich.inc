/**
 * Ham Sandwich module include file.
 *   (c) 2007, The AMX Mod X Development Team
 *
 * -
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of this program with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

/**
 * Ham Sandwich is a module that is used to hook and call virtual functions of 
 *  entities.
 * Virtual functions are mod-specific functions.  This means that in order
 *  for this to work on a mod, it needs to be configured with the hamdata.ini
 *  file.
 * Be very careful with parameter passing to these functions.
 */

#if defined _hamsandwich_included
	#endinput
#endif
#define _hamsandwich_included

#include <ham_const>

#if AMXX_VERSION_NUM >= 175
	#pragma reqlib hamsandwich
	#if !defined AMXMODX_NOAUTOLOAD
		#pragma loadlib hamsandwich
	#endif
#else
	#pragma library hamsandwich
#endif

/**
 * Hooks the virtual table for the specified entity class.
 * An example would be: RegisterHam(Ham_TakeDamage, "player", "player_hurt");
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to hook.
 * @param EntityClass	The entity classname to hook.
 * @param callback		The forward to call.
 * @param post			Whether or not to forward this in post.
 * @return 				Returns a handle to the forward.  Use EnableHamForward/DisableHamForward to toggle the forward on or off.
 */
native HamHook:RegisterHam(Ham:function, const EntityClass[], const Callback[], Post=0);

/**
 * Hooks the virtual table for the specified entity's class.
 * An example would be: RegisterHam(Ham_TakeDamage, id, "player_hurt");
 * Look at the Ham enum for parameter lists.
 * Note: This will cause hooks for the entire internal class that the entity is
 *       not exclusively for the provided entity.
 *
 * @param function		The function to hook.
 * @param EntityId		The entity classname to hook.
 * @param callback		The forward to call.
 * @param post			Whether or not to forward this in post.
 * @return 				Returns a handle to the forward.  Use EnableHamForward/DisableHamForward to toggle the forward on or off.
 */
native HamHook:RegisterHamFromEntity(Ham:function, EntityId, const Callback[], Post=0);


/**
 * Stops a ham forward from triggering.
 * Use the return value from RegisterHam as the parameter here!
 *
 * @param fwd			The forward to stop.
 */
native DisableHamForward(HamHook:fwd);

/**
 * Starts a ham forward back up.
 * Use the return value from RegisterHam as the parameter here!
 *
 * @param fwd			The forward to re-enable.
 */
native EnableHamForward(HamHook:fwd);

/**
 * Executes the virtual function on the entity.
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to call.
 * @param id			The id of the entity to execute it on.
 */
native ExecuteHam(Ham:function, this, any:...);

/**
 * Executes the virtual function on the entity, this will trigger all hooks on that function.
 * Be very careful about recursion!
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to call.
 * @param id			The id of the entity to execute it on.
 */
native ExecuteHamB(Ham:function, this, any:...);

/**
 * Gets the return status of the current hook.
 * This is useful to determine what return natives to use.
 *
 * @return				The current status of the hook (such as HAM_SUPERCEDE).
 */
native GetHamReturnStatus();

/**
 * Gets the return value of a hook for hooks that return integers or booleans.
 *
 * @param output		The variable to store the value in.
 */
native GetHamReturnInteger(&output);

/**
 * Gets the return value of a hook for hooks that return float.
 *
 * @param output		The variable to store the value in.
 */
native GetHamReturnFloat(&Float:output);

/**
 * Gets the return value of a hook for hooks that return Vectors.
 *
 * @param output		The variable to store the value in.
 */
native GetHamReturnVector(Float:output[3]);

/**
 * Gets the return value of a hook for hooks that return entities.
 *
 * @param output		The variable to store the value in. Will be -1 on null.
 */
native GetHamReturnEntity(&output);

/**
 * Gets the return value of a hook for hooks that return strings.
 *
 * @param output		The buffer to store the string in.
 * @param size			The string size of the buffer.
 */
native GetHamReturnString(output[], size);

/**
 * Gets the original return value of a hook for hooks that return integers or booleans.
 *
 * @param output		The variable to store the value in.
 */
native GetOrigHamReturnInteger(&output);

/**
 * Gets the original return value of a hook for hooks that return floats.
 *
 * @param output		The variable to store the value in.
 */
native GetOrigHamReturnFloat(&Float:output);

/**
 * Gets the original return value of a hook for hooks that return Vectors.
 *
 * @param output		The variable to store the value in.
 */
native GetOrigHamReturnVector(Float:output[3]);

/**
 * Gets the original return value of a hook for hooks that return entities.
 *
 * @param output		The variable to store the value in. -1 on null.
 */
native GetOrigHamReturnEntity(&output);

/**
 * Gets the original return value of a hook for hooks that return strings.
 *
 * @param output		The buffer to store the string in.
 * @param size			The size of the buffer.
 */
native GetOrigHamReturnString(output[], size);


/**
 * Sets the return value of a hook that returns an integer or boolean.
 * This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.
 *
 * @param value				The value to set the return to.
 */
native SetHamReturnInteger(value);

/**
 * Sets the return value of a hook that returns a float.
 * This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.
 *
 * @param value				The value to set the return to.
 */
native SetHamReturnFloat(Float:value);

/**
 * Sets the return value of a hook that returns a Vector.
 * This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.
 *
 * @param value				The value to set the return to.
 */
native SetHamReturnVector(const Float:value[3]);

/**
 * Sets the return value of a hook that returns an entity.  Set to -1 for null.
 * This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.
 *
 * @param value				The value to set the return to.
 */
native SetHamReturnEntity(value);

/**
 * Sets the return value of a hook that returns a string.
 * This needs to be used in conjunction with HAM_OVERRIDE or HAM_SUPERCEDE.
 *
 * @param value				The value to set the return to.
 */
native SetHamReturnString(const value[]);


/**
 * Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
 * Use this on parameters that are integers.
 *
 * @param which				Which parameter to change.  Starts at 1, and works up from the left to right.  1 is always "this".
 * @param value				The value to change it to.
 */
native SetHamParamInteger(which, value);

/**
 * Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
 * Use this on parameters that are floats.
 *
 * @param which				Which parameter to change.  Starts at 1, and works up from the left to right.  1 is always "this".
 * @param value				The value to change it to.
 */
native SetHamParamFloat(which, Float:value);

/**
 * Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
 * Use this on parameters that are Vectors.
 *
 * @param which				Which parameter to change.  Starts at 1, and works up from the left to right.  1 is always "this".
 * @param value				The value to change it to.
 */
native SetHamParamVector(which, const Float:value[3]);

/**
 * Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
 * Use this on parameters that are entities.
 *
 * @param which				Which parameter to change.  Starts at 1, and works up from the left to right.  1 is always "this".
 * @param value				The value to change it to.
 */
native SetHamParamEntity(which, value);

/**
 * Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
 * Use this on parameters that are strings.
 *
 * @param which				Which parameter to change.  Starts at 1, and works up from the left to right.  1 is always "this".
 * @param value				The value to change it to.
 */
native SetHamParamString(which, const output[]);

/**
 * Sets a parameter on the fly of the current hook.  This has no effect in post hooks.
 * Use this on parameters that are trace result handles.
 *
 * @param which				Which parameter to change.  Starts at 1, and works up from the left to right.  1 is always "this".
 * @param value				The value to change it to.
 */
native SetHamParamTraceResult(which, tr_handle);


/**
 * Returns whether or not the function for the specified Ham is valid.
 * Things that would make it invalid would be bounds (an older module version
 *  may not have all of the functions), and the function not being found in
 *  the mod's hamdata.ini file.
 *
 * @param function		The function to look up.
 * @return				true if the function is valid, false otherwise.
 */
native bool:IsHamValid(Ham:function);

/**
 * This is used to compliment fakemeta's {get,set}_pdata_{int,float,string}.
 * This requires the mod to have the pev and base fields set in hamdata.ini.
 * Note this dereferences memory! Improper use of this will crash the server.
 * This will return an index of the corresponding cbase field in private data.
 * Returns -1 on a null entry.
 *
 * @param id			The entity to examine the private data.
 * @param offset		The windows offset of the data.
 * @param linuxdiff		The linux difference of the data.
 * @param macdiff		The mac os x difference of the data.
 * @return				The index of the corresponding pdata field. -1 for none set.
 */
native get_pdata_cbase(id, offset, linuxdiff=5, macdiff=5);

/**
 * This is used to compliment fakemeta's {get,set}_pdata_{int,float,string}.
 * This requires the mod to have the pev and base fields set in hamdata.ini.
 * This will set the corresponding cbase field in private data with the index.
 * Pass -1 to null the entry.
 *
 * @param id			The entity to examine the private data.
 * @param offset		The windows offset of the data.
 * @param value			The index to store, -1 for invalid
 * @param linuxdiff		The linux difference of the data.
 * @param macdiff		The mac os x difference of the data.
 */
native set_pdata_cbase(id, offset, value, linuxdiff=5, macdiff=5);

/**
 * This is similar to the get_pdata_cbase, however it does not dereference memory.
 * This is many times slower than get_pdata_cbase, and this should only be used 
 * for testing and finding of offsets, not actual release quality plugins.
 * This will return an index of the corresponding cbase field in private data.
 * Returns -1 on a null entry. -2 on an invalid entry.
 *
 * @param id			Entry to examine the private data.
 * @param offset		The windows offset of the data.
 * @param linuxdiff		The linux difference of the data.
 * @param macdiff		The mac os x difference of the data.
 * @return				The index of the corresponding pdata field, -1 for null, -2 for invalid.
 */
native get_pdata_cbase_safe(id, offset, linuxdiff=5, macdiff=5);




// This is the callback from the module, this handles any fatal errors.
// This will in turn call the "HamFilter(Ham:id, HamError:err, const reason[])" public, if it exists.
// Return PLUGIN_HANDLED from within the HamFilter to stop the plugin from failing.
// Any other return value will fail the plugin.
// You do not need to have a HamFilter, if there is none, all fatal errors will fail the plugin.
// Do not modify this!
public __fatal_ham_error(Ham:id, HamError:err, const reason[])
{
	
	new func=get_func_id("HamFilter", -1);
	new bool:fail=true;
	
	if (func != -1 && callfunc_begin_i(func, -1)==1)
	{
		callfunc_push_int(_:id);
		callfunc_push_int(_:err);
		callfunc_push_str(reason, false);
		if (callfunc_end()==PLUGIN_HANDLED)
		{
			fail=false;
		}
	}
	if (fail)
	{
		set_fail_state(reason);
	}
	
}
