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
 * Ham Sandwich is a module that is used to hook virtual functions of entities.
 * Virtual functions are mod-specific functions.  This means that in order
 *  for this to work on a mod, it needs to be configured with the hamdata.ini
 *  file.
 * Be very careful with parameter passing to these functions.
 */

#if defined _hamsandwich_included
	#endinput
#endif
#define _hamsandwich_included

#if AMXX_VERSION_NUM >= 175
	#pragma reqlib hamsandwich
	#if !defined AMXMODX_NOAUTOLOAD
		#pragma loadlib hamsandwich
	#endif
#else
	#pragma library hamsandwich
#endif


/**
 * A few notes about all of the following functions:
 *   - Not all functions will do as you expect on all mods.
 *     If a function does not do what you would believe it should
 *     DO NOT file a bug report, you will be ignored.
 *
 *   - Passing invalid parameters has potential to crash the server
 *     So be careful, and adequately test!
 *
 *   - All functions take (and pass) a "this" index as the first param.
 *     This is the entity from which the function is being executed on.
 *
 *   - All functions and forwards (eg: {Register,Execute}Ham[B]) require
 *     the mod to have the pev and base keys in addition to the function
 *     keys for the corresponding mod/operating system in hamdata.ini
 */

enum Ham
{
	/**
     * Description: 	This is typically called whenever an entity is created. 
	 *					It is the virtual equivilent of spawn from the engine.
	 *					Some mods call this on player spawns too.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Spawn, this);
	 */
	Ham_Spawn = 0, 
						
	/**
     * Description: 	This is typically called on map change.
	 *					This will typically precache all assets required by the entity.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Precache, this);
	 */
	Ham_Precache,
	
	/**
	 * Description:		Typically this is similar to an engine keyvalue call.
	 *					Use the kvd natives from fakemeta to handle the kvd_handle passed.
	 *					NOTE: Do not pass handle 0 to this! Use get_kvd_handle(0) from fakemeta instead!
	 * Forward params:	function(this, kvd_handle);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Keyvalue, this, kvd_handle);
	 */
	Ham_Keyvalue,
	
	/**
	 * Description:		Returns flags for how an entity can be used (FCAP_* constants in hlsdk_const.inc)
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ObjectCaps, this);
	 */
	Ham_ObjectCaps,
	
	/**
	 * Description:		Usually called to activate some objects.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Activate, this);
	 */
	Ham_Activate,
	
	/**
	 * Description:		Usually called after the engine call with the same name.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SetObjectCollisionBox, this);
	 */
	Ham_SetObjectCollisionBox,
	
	/**
	 * Description:		Returns an integer number that corresponds with what type of entity this is.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_Classify, this);
	 */
	Ham_Classify,
	
	/**
	 * Description:		Typically called when an entity dies to notify any children entities about the death.
	 * Forward params:	function(this, idchild)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_DeathNotice, this, idchild)
	 */
	Ham_DeathNotice,
	
	/**
	 * Description:		Usually called whenever an entity gets attacked by a hitscan (such as a gun) weapon.
	 *					Use the get/set tr2 natives in fakemeta to handle the traceresult data.
	 *					Do not use a handle of 0 as a traceresult in execution, use get_tr_handle(0) instead.
	 * Forward params:	function(this, idattacker, Float:damage, Float:direction[3], traceresult, damagebits)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TraceAttack, this, idattacker, Float:damage, Float:direction[3], tracehandle, damagebits);
	 */
	Ham_TraceAttack,
	
	/**
	 * Description:		Usually called whenever an entity takes any kind of damage.
	 *					Inflictor is the entity that caused the damage (such as a gun).
	 *					Attacker is the entity that tirggered the damage (such as the gun's owner).
	 * Forward params:	function(this, idinflictor, idattacker, Float:damage, damagebits);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_TakeDamage, this, idinflictor, idattacker, Float:damage, damagebits);
	 */
	Ham_TakeDamage,
	
	/**
	 * Description:		Usually called whenever an entity gets a form of a heal.
	 * Forward params:	function(this, Float:health, damagebits);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_TakeHealth, this, Float:health, damagebits);
	 */
	Ham_TakeHealth,
	
	/**
	 * Description:		Normally called whenever an entity dies.
	 * Forward params:	function(this, idattacker, shouldgib)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Killed, this, idattacker, shouldgib);
	 */
	Ham_Killed,
	
	/**
	 * Description:		Normally returns the blood color of the entity.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_BloodColor, this)
	 */
	Ham_BloodColor,
	
	
	Ham_TraceBleed,
	Ham_IsTriggered,
	Ham_GetToggleState,
	
	/**
	 * Description:		Typically adds points to the entity.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_BloodColor, this, points, bool:cangonegative);
	 */
	Ham_AddPoints,

	/**
	 * Description:		Typically adds points to the entity's team.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_BloodColor, this, points, bool:cangonegative);
	 */
	Ham_AddPointsToTeam,
	
	/**
	 * Description:		Adds an item to the player's inventory.
	 * Forward params:	function(this, idother);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_AddPlayerItem, this, idother);
	 */
	Ham_AddPlayerItem,
	
	/**
	 * Description:		Removes an item to the player's inventory.
	 * Forward params:	function(this, idother);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_RemovePlayerItem, this, idother);
	 */
	Ham_RemovePlayerItem,
	Ham_GiveAmmo,
	Ham_GetDelay,
	
	/** 
	 * Description:		Whether or not the entity is moving.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsMoving, this);
	 */
	Ham_IsMoving,
	Ham_OverrideReset,
	Ham_DamageDecal,
	Ham_SetToggleState,
	
	/**
	 * Description:		Not entirely sure what this does.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_StartSneaking, this);
	 */
	Ham_StartSneaking,

	/**
	 * Description:		Not entirely sure what this does.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_StopSneaking, this);
	 */
	Ham_StopSneaking,
	Ham_OnControls,
	
	/** 
	 * Description:		Whether or not the entity is sneaking.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_IsSneaking, this);
	 */
	Ham_IsSneaking,
	
	/** 
	 * Description:		Whether or not the entity is alive.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsAlive, this);
	 */
	Ham_IsAlive,
	
	/** 
	 * Description:		Whether or not the entity uses a BSP model.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsBSPModel, this);
	 */
	Ham_IsBSPModel,
	
	/** 
	 * Description:		Whether or not the entity can reflect gauss shots..
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ReflectGauss, this);
	 */
	Ham_ReflectGauss,
	Ham_HasTarget,
	
	/** 
	 * Description:		Whether or not the entity is in the world.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsInWorld, this);
	 */
	Ham_IsInWorld,
	
	/** 
	 * Description:		Whether or not the entity is a player.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsPlayer, this);
	 */
	Ham_IsPlayer,

	/** 
	 * Description:		Whether or not the entity is a net client.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsNetClient, this);
	 */
	Ham_IsNetClient,

	/** 
	 * Description:		Get the entity's team id.
	 * Forward params:	function(this);
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_IsPlayer, this, buffer[], size);
	 */
	Ham_TeamId,
	
	/**
	 * Description:		Returns the next target of this.
	 * Forward params:	function(this);
	 * Execute params:	ExecuteHam(Ham_GetNextTarget, this);
	 */
	Ham_GetNextTarget,
	
	/**
	 * Description:		Called whenever an entity thinks.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Think, this);
	 */
	Ham_Think,
	
	/**
	 * Description:		Called whenever two entities touch.
	 * Forward params:	function(this, idother);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Touch, this, idother);
	 */
	Ham_Touch,
	
	/**
	 * Description:		Called whenver one entity uses another.
	 * Forward params:	function(this, idcaller, idactivator, use_type, Float:value)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Use, this, idcaller, idactivator, use_type, Float:value);
	 */
	Ham_Use,
	
	/**
	 * Description:		Normally called whenever one entity blocks another from moving.
	 * Forward params:	function(this, idother);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Blocked, this, idother);
	 */
	Ham_Blocked,
	
	/**
	 * Description:		Normally called when a map-based item respawns, such as a health kit or something.
	 * Forward params:	function(this);
	 * Return type:		CBaseEntity.
	 * Execute params:	ExecuteHam(Ham_Respawn, this);
	 */
	Ham_Respawn,
	
	/**
	 * Description:		Used in Half-Life to update a monster's owner.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_UpdateOwner, this);
	 */
	Ham_UpdateOwner,
	
	/**
	 * Description:		Normally called whenever a barnacle grabs the entity.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_FBecomeProne, this);
	 */
	Ham_FBecomeProne,
	
	/**
	 * Description:		Returns the center of the entity.
	 * Forward params:	function(this);
	 * Return type:		Vector (byref'd in Execute).
	 * Execute params:	ExecuteHam(Ham_Center, this, Float:output[3]);
	 */
	Ham_Center,
	
	/**
	 * Description:		Returns the eye position of the entity.
	 * Forward params:	function(this);
	 * Return type:		Vector (byref'd in Execute).
	 * Execute params:	ExecuteHam(Ham_EyePosition, this, Float:output[3]);
	 */
	Ham_EyePosition,

	/**
	 * Description:		Returns the ear position of the entity.
	 * Forward params:	function(this);
	 * Return type:		Vector (byref'd in Execute).
	 * Execute params:	ExecuteHam(Ham_EarPosition, this, Float:output[3]);
	 */
	Ham_EarPosition,
	
	/**
	 * Description:		Position to shoot at.
	 * Forward params:	function(this, Float:srcvector[3]);
	 * Return type:		Vector (byref'd in Execute).
	 * Execute params:	ExecuteHam(Ham_BodyTarget, Float:srcvector[3], Float:returnvector[3])
	 */
	Ham_BodyTarget,
	Ham_Illumination,
	Ham_FVisible,
	Ham_FVecVisible,

	Ham_TS_BreakableRespawn,
	Ham_TS_CanUsedThroughWalls,
	Ham_TS_RespawnWait,

	/**
	 * Description:		This is called on a map reset for most map based entities.
	 * Forward params:	function(this);
	 * Execute params:	ExecuteHam(Ham_Respawn, this);
	 */
	Ham_CS_Restart,

	Ham_DOD_RoundRespawn,
	Ham_DOD_RoundRespawnEnt,
	Ham_DOD_RoundStore,
	Ham_DOD_AreaSetIndex,
	Ham_DOD_AreaSendStatus,
	Ham_DOD_GetState,
	Ham_DOD_GetStateEnt,

	Ham_TFC_DbGetItemName,
	Ham_TFC_EngineerUse,
	Ham_TFC_Finished,
	Ham_TFC_EmpExplode,
	Ham_TFC_CalcEmpDmgRad,
	Ham_TFC_TakeEmpBlast,
	Ham_TFC_EmpRemove,
	Ham_TFC_TakeConcussionBlast,
	Ham_TFC_Concuss,

	Ham_NS_GetPointValue,
	Ham_NS_AwardKill,
	Ham_NS_ResetEntity,
	Ham_NS_UpdateOnRemove,


	/**
	 * DONT USE ME LOL
	 */
	HAM_LAST_ENTRY_DONT_USE_ME_LOL
};


/**
 * Ham return types.
 * -
 * Return these from hooks to disable calling the target function.
 * Numbers match up with fakemeta's FMRES_* for clarity.  They are interchangable.
 * 0 (or no return) is also interpretted as HAM_IGNORED.
 */
#define HAM_IGNORED		1	/**< Calls target function, returns normal value */
#define HAM_HANDLED		2	/**< Tells the module you did something, still calls target function and returns normal value */
#define HAM_OVERRIDE	3	/**< Still calls the target function, but returns whatever is set with SetHamReturn*() */
#define HAM_SUPERCEDE	4	/**< Block the target call, and use your return value (if applicable) (Set with SetHamReturn*()) */

/**
 * Hooks the virtual table for the specified entity class.
 * An example would be: RegisterHam(Ham_TakeDamage, "player_hurt", "player");
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to hook.
 * @param callback		The forward to call.
 * @param entity		The entity classname to hook.
 * @param post			Whether or not to forward this in post.
 * @return 				Returns a handle to the forward.  Use EnableHamForward/DisableHamForward to toggle the forward on or off.
 */
native HamHook:RegisterHam(Ham:function, const callback[], const entity[], post=0);

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


native GetHamReturnInteger(&output);
native GetHamReturnFloat(&Float:output);
native GetHamReturnVector(Float:output[3]);
native GetHamReturnCbase(&output);
native GetHamReturnString(output[], size);
native GetOrigHamReturnInteger(&output);
native GetOrigHamReturnFloat(&Float:output);
native GetOrigHamReturnVector(Float:output[3]);
native GetOrigHamReturnCbase(&output);
native GetOrigHamReturnString(output[], size);
native SetHamReturnInteger(output);
native SetHamReturnFloat(Float:output);
native SetHamReturnVector(const Float:output[3]);
native SetHamReturnCbase(output);
native SetHamReturnString(const output[]);


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
 * This requires the mod to have the pev field set in hamdata.ini.
 * Note this dereferences memory! Improper use of this will crash the server.
 * This will return an index of the corresponding cbase field in private data.
 *
 * @param id			The entity to examine the private data.
 * @param offset		The windows offset of the data.
 * @param linuxdiff		The linux difference of the data.
 * @return				The index of the corresponding pdata field. -1 for none set.
 */
native get_pdata_cbase(id, offset, linuxdiff=5);

/**
 * This is used to compliment fakemeta's {get,set}_pdata_{int,float,string}.
 * This requires the mod to have the pev field set in hamdata.ini.
 * Note this dereferences memory! Improper use of this will crash the server.
 * This will set the corresponding cbase field in private data with the index.
 *
 * @param id			The entity to examine the private data.
 * @param offset		The windows offset of the data.
 * @param value			The index to store.
 * @param linuxdiff		The linux difference of the data.
 * @return				The index of the corresponding pdata field. -1 for none set.
 */
native set_pdata_cbase(id, offset, value, linuxdiff=5);



 


enum HamError
{
	HAM_OK = 0,
	
	HAM_INVALID_FUNC,			// The function is not valid
	HAM_FUNC_NOT_CONFIGURED,	// This function is not configured in hamdata.ini
	
	HAM_ERR_END
};

// This is the callback from the module, this handles any fatal errors.
// This will in turn call the "HamFilter(Ham:id, HamError:err, const reason[])" public, if it exists.
// Return PLUGIN_HANDLED from within the HamFilter to stop the plugin from failing.
// Any other return value will fail the plugin.
// You do not need to have a HamFilter, if there is none, all fatal errors will fail the plugin.
public __fatal_ham_error(Ham:id, HamError:err, const reason[])
{
	
	new func=get_func_id("HamFilter", -1);
	new bool:fail=true;
	
	if (callfunc_begin_i(func, -1)==1)
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