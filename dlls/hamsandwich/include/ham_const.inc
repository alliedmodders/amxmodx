#if defined _ham_const_included
	#endinput
#endif
#define _ham_const_included

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
	Ham_MyMonsterPointer,
	Ham_MySquadMonsterPointer,
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

	Ham_Player_Jump,
	Ham_Player_Duck,
	Ham_Player_PreThink,
	Ham_Player_PostThink,
	Ham_Player_GetGunPosition,
	Ham_Player_ShouldFadeOnDeath,
	Ham_Player_ImpulseCommands,
	Ham_Player_UpdateClientData,
	
	/**
	 * Items have all the attributes of normal entities in addition to these.
	 */
	Ham_Item_AddToPlayer,
	Ham_Item_AddDuplicate,
	Ham_Item_GetItemInfo,
	Ham_Item_CanDeploy,
	Ham_Item_Deploy,
	Ham_Item_CanHolster,
	Ham_Item_Holster,
	Ham_Item_UpdateItemInfo,
	Ham_Item_PreFrame,
	Ham_Item_PostFrame,
	Ham_Item_Drop,
	Ham_Item_Kill,
	Ham_Item_AttachToPlayer,
	Ham_Item_PrimaryAmmoIndex,
	Ham_Item_SecondaryAmmoIndex,
	Ham_Item_UpdateClientData,
	Ham_Item_GetWeaponPtr,
	Ham_Item_ItemSlot,
	
	
	/**
	 * Weapons have all the attributes to Ham_Item_*, in addition to these.
	 */
	Ham_Weapon_ExtractAmmo,
	Ham_Weapon_ExtractClipAmmo,
	Ham_Weapon_AddWeapon,
	Ham_Weapon_PlayEmptySound,
	Ham_Weapon_ResetEmptySound,
	Ham_Weapon_SendWeaponAnim,
	Ham_Weapon_IsUsable,
	Ham_Weapon_PrimaryAttack,
	Ham_Weapon_SecondaryAttack,
	Ham_Weapon_Reload,
	Ham_Weapon_WeaponIdle,
	Ham_Weapon_RetireWeapon,
	Ham_Weapon_ShouldWeaponIdle,
	Ham_Weapon_UseDecrement,
	 
	
	Ham_TS_BreakableRespawn,
	Ham_TS_CanUsedThroughWalls,
	Ham_TS_RespawnWait,

	/**
	 * Description:		This is called on a map reset for most map based entities.
	 * Forward params:	function(this);
	 * Execute params:	ExecuteHam(Ham_CS_Restart, this);
	 */
	Ham_CS_Restart,
	
	/**
	 * Description:		Respawn function for players/bots only! Do not use this on non player/bot entities!
	 * Forward params:	function(this);
	 * Execute params:	ExecuteHam(Ham_CS_RoundRespawn, this);
	 */
	Ham_CS_RoundRespawn,

	Ham_DOD_RoundRespawn,
	Ham_DOD_RoundRespawnEnt,
	Ham_DOD_RoundStore,
	Ham_DOD_AreaSetIndex,
	Ham_DOD_AreaSendStatus,
	Ham_DOD_GetState,
	Ham_DOD_GetStateEnt,

	Ham_TFC_EngineerUse,
	Ham_TFC_Finished,
	Ham_TFC_EmpExplode,
	Ham_TFC_CalcEmpDmgRad,
	Ham_TFC_TakeEmpBlast,
	Ham_TFC_EmpRemove,
	Ham_TFC_TakeConcussionBlast,
	Ham_TFC_Concuss,

	Ham_ESF_IsEnvModel, // Only valid in ESF Open Beta
	Ham_ESF_TakeDamage2, // Only valid in ESF Open Beta

	Ham_NS_GetPointValue,
	Ham_NS_AwardKill,
	Ham_NS_ResetEntity,
	Ham_NS_UpdateOnRemove,


	/**
	 * DONT USE ME LOL
	 */
	HAM_LAST_ENTRY_DONT_USE_ME_LOL
};

enum HamError
{
	HAM_OK = 0,
	
	HAM_INVALID_FUNC,			// The function is not valid
	HAM_FUNC_NOT_CONFIGURED,	// This function is not configured in hamdata.ini
	
	HAM_ERR_END
};
