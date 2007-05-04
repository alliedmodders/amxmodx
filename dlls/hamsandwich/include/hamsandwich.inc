#if defined _hamsandwich_included
	#endinput
#endif

#define _hamsandwich_included

enum Ham
{
	Ham_Spawn = 0,
	Ham_Precache,
	Ham_Keyvalue,
	Ham_ObjectCaps,
	Ham_Activate,
	Ham_SetObjectCollisionBox,
	Ham_Classify,
	Ham_DeathNotice,
	Ham_TraceAttack,
	Ham_TakeDamage,
	Ham_TakeHealth,
	Ham_Killed,
	Ham_BloodColor,
	Ham_TraceBleed,
	Ham_IsTriggered,
	Ham_GetToggleState,
	Ham_AddPoints,
	Ham_AddPointsToTeam,
	Ham_AddPlayerItem,
	Ham_RemovePlayerItem,
	Ham_GiveAmmo,
	Ham_GetDelay,
	Ham_IsMoving,
	Ham_OverrideReset,
	Ham_DamageDecal,
	Ham_SetToggleState,
	Ham_StartSneaking,
	Ham_StopSneaking,
	Ham_OnControls,
	Ham_IsSneaking,
	Ham_IsAlive,
	Ham_IsBSPModel,
	Ham_ReflectGauss,
	Ham_HasTarget,
	Ham_IsInWorld,
	Ham_IsPlayer,
	Ham_IsNetClient,
	Ham_TeamId,
	Ham_GetNextTarget,
	Ham_Think,
	Ham_Touch,
	Ham_Use,
	Ham_Blocked,
	Ham_Respawn,
	Ham_UpdateOwner,
	Ham_FBecomeProne,
	Ham_Center,
	Ham_EyePosition,
	Ham_EarPosition,
	Ham_BodyTarget,
	Ham_Illumination,
	Ham_FVisible,
	Ham_FVecVisible,

	Ham_TS_BreakableRespawn,
	Ham_TS_CanUsedThroughWalls,
	Ham_TS_RespawnWait,

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


	HAM_LAST_ENTRY_DONT_USE_ME_LOL
};


/**
 * Hooks the virtual table for the specified entity class.
 * An example would be: RegisterHam(Ham_TakeDamage, "player_hurt", "player");
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to hook.
 * @param callback		The forward to call.
 * @param entity		The entity classname to hook.
 * @param post			Whether or not to forward this in post.
 */
 

native RegisterHam(Ham:function, const callback[], const entity[], post=0);

/**
 * Executes the virtual function on the entity.
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to call.
 * @param id			The id of the entity to execute it on.
 */
native ExecuteHam(Ham:function, id, any:...);

/**
 * Executes the virtual function on the entity, this will trigger all hooks on that function.
 * Be very careful about recursion!
 * Look at the Ham enum for parameter lists.
 *
 * @param function		The function to call.
 * @param id			The id of the entity to execute it on.
 */
native ExecuteHamB(Ham:function, id, any:...);


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