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
 *
 *   - Some functions that return booleans may need to be logically ANDed
 *     to get the results desired.  e.g: if (ExecuteHam(Ham_TS_IsObjective, this) & 0x0000FFFF != 0) { // true.. }
 *     because the module will return the full integer value.
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
	 *					Do not use a handle of 0 as a traceresult in execution, use create_tr2() from Fakemeta
	 *					to pass a custom handle instead.  (Don't forget to free the handle when you're done.)
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
	
	/**
	 * Description:		Traces where blood should appear.
	 * Forward params:	function(this, Float:Damage, Float:Direction[3], trace_handle, damagebits);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TraceBleed, this, Float:damage, Float:direction[3], trace_handle, damagebits);
	 */
	Ham_TraceBleed,
	
	/**
	 * Description:		Returns whether an entity is activated.
	 * Forward params:	function(this, idActivator);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsTriggered, this, idActivator);
	 */
	Ham_IsTriggered,
	
	/**
	 * Description:		Returns the id of the entity if its class is derived off of CBaseMonster, -1 otherwise.
	 * Forward params:	function(this)
	 * Return type:		Entity.
	 * Execute params:	ExecuteHam(Ham_MyMonsterPointer, this);
	 */
	Ham_MyMonsterPointer,
	
	/**
	 * Description:		Returns the id of the entity if its class is derived off of CBaseSquadMonster, -1 otherwise.
	 * Forward params:	function(this)
	 * Return type:		Entity.
	 * Execute params:	ExecuteHam(Ham_MySquadMonsterPointer, this);
	 */
	Ham_MySquadMonsterPointer,
	
	/**
	 * Description:		Returns the toggle state of the entity.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_GetToggleState, this);
	 */
	Ham_GetToggleState,
	
	/**
	 * Description:		Typically adds points to the entity.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_AddPoints, this, points, bool:cangonegative);
	 */
	Ham_AddPoints,

	/**
	 * Description:		Typically adds points to everybody on the entity's team.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_AddPointsToTeam, this, points, bool:cangonegative);
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
	
	/**
	 * Description:		Gives ammo to the entity.
	 * Forward params:	function(this, Amount, const Name[], Max)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_GiveAmmo, this, amount, "type", max);
	 */
	Ham_GiveAmmo,
	
	/**
	 * Description:		Unsure, I believe this is the delay between activation for an entity.
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_GetDelay, this, Float:output)
	 */
	Ham_GetDelay,
	
	/** 
	 * Description:		Whether or not the entity is moving.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsMoving, this);
	 */
	Ham_IsMoving,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_OverrideReset, this)
	 */
	Ham_OverrideReset,
	
	/**
	 * Description:		Returns the damage decal of the entity for the damage type.
	 * Forward params:	function(this, damagebits)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_DamageDecal, this);
	 */
	Ham_DamageDecal,
	
	/**
	 * Description:		Sets the toggle state of the entity.
	 * Forward params:	function(this, state)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SetToggleState, this, state);
	 */
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
	
	/**
	 * Description:		Not entirely sure.
	 * Forward params:	function(this, idOn)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_OnControls, this, idOn);
	 */
	Ham_OnControls,
	
	/** 
	 * Description:		Whether or not the entity is sneaking.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsSneaking, this);
	 */
	Ham_IsSneaking,
	
	/** 
	 * Description:		Whether or not the entity is alive.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsAlive, this);
	 */
	Ham_IsAlive,
	
	/** 
	 * Description:		Whether or not the entity uses a BSP model.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsBSPModel, this);
	 */
	Ham_IsBSPModel,
	
	/** 
	 * Description:		Whether or not the entity can reflect gauss shots..
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ReflectGauss, this);
	 */
	Ham_ReflectGauss,
	
	/**
	 * Description:		Whether or not the target is the same as the one passed.
	 *					Note the strindex parameter is a string passed that has been allocated by the engine.
	 *					Use fakemeta's EngFunc_SzFromIndex to convert to a normal string, or fakemeta's
	 *					EngFunc_AllocString to create a new string.
	 * Forward params:	function(this, strindex).
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_HasTarget, this, strindex);
	 */
	Ham_HasTarget,
	
	/** 
	 * Description:		Whether or not the entity is in the world.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsInWorld, this);
	 */
	Ham_IsInWorld,
	
	/** 
	 * Description:		Whether or not the entity is a player.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsPlayer, this);
	 */
	Ham_IsPlayer,

	/** 
	 * Description:		Whether or not the entity is a net client.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsNetClient, this);
	 */
	Ham_IsNetClient,

	/** 
	 * Description:		Get the entity's team id.
	 * Forward params:	function(this);
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_TeamId, this, buffer[], size);
	 */
	Ham_TeamId,
	
	/**
	 * Description:		Returns the next target of this.
	 * Forward params:	function(this);
	 * Return type:		Entity.
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
	 * Return type:		Entity.
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
	
	/**
	 * Description:		Returns the illumination of the entity.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_Illumination, this);
	 */
	Ham_Illumination,
	
	/**
	 * Description:		Unsure, I assume it is whether or not the other entity is visible to this entity.
	 * Forward params:	function(this, idOther);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FVisible, this, idOther);
	 */
	Ham_FVisible,
	
	/**
	 * Description:		Unsure, I assume it is whether or not the target vector is visible to this entity.
	 * Forward params:	function(this, const Float:origin[3]);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FVecVisible, this, const Float:origin[3]);
	 */
	Ham_FVecVisible,

	
	/**
	 * Players have all the attributes of normal entities, in addition to these.
	 */
	 
	/**
	 * Description:		Typically called every frame when a player has jump held.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Player_Jump, this);
	 */
	Ham_Player_Jump,

	/**
	 * Description:		Typically called every frame when a player has duck held.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Player_Duck, this);
	 */
	Ham_Player_Duck,
	
	/**
	 * Description:		Typically called every frame during PlayerPreThink engine call.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Player_PreThink, this);
	 */
	Ham_Player_PreThink,

	/**
	 * Description:		Typically called every frame during PlayerPostThink engine call.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Player_PostThink, this);
	 */
	Ham_Player_PostThink,
	
	/**
	 * Description:		Returns a vector that tells the gun position.
	 * Forward params:	function(this)
	 * Return type:		Vector, byreffed in execute.
	 * Execute params:	ExecuteHam(Ham_Player_GetGunPosition, this, Float:output[3]);
	 */
	Ham_Player_GetGunPosition,
	
	/**
	 * Description:		Whether or not the player should fade on death.
	 * Forward param:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Player_ShouldFadeOnDeath, this);
	 */
	Ham_Player_ShouldFadeOnDeath,

	/**
	 * Description:		Called whenever an impulse command is executed.
	 * Forward param:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Player_ImpulseComands, this);
	 */
	Ham_Player_ImpulseCommands,
	
	/**
	 * Description:		Updates the client's data for hud changes (such as ammo).  Usually called every frame.
	 * Forward param:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Player_UpdateClientData, this);
	 */
	Ham_Player_UpdateClientData,
	
	/**
	 * Items have all the attributes of normal entities in addition to these.
	 */
	 
	/**
	 * Description:		Adds the item to the player.
	 * Forward params:	function(this, idPlayer);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_AddToPlayer, this, idPlayer);
	 */
	Ham_Item_AddToPlayer,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, idOriginal);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_AddDuplicate, this, idOriginal);
	 */
	Ham_Item_AddDuplicate,
	
	/**
	 * Description:		Whether or not this entity can be deployed.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_CanDeploy, this);
	 */
	Ham_Item_CanDeploy,
	
	/**
	 * Description:		Deploys the entity (usually a weapon).
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_Deploy, this);
	 */
	Ham_Item_Deploy,
	
	/**
	 * Description:		Whether or not the entity can be holstered.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_CanHolster, this);
	 */
	Ham_Item_CanHolster,
	
	/**
	 * Description:		Whether or not the entity (usually weapon) can be holstered.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_Holster, this);
	 */
	Ham_Item_Holster,
	
	/**
	 * Description:		Updates the HUD info about this item.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_UpdateItemInfo, this);
	 */
	Ham_Item_UpdateItemInfo,
	
	/**
	 * Description:		Called each frame for an item, normally only on active items.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Item_PreFrame, this);
	 */
	Ham_Item_PreFrame,
	
	/**
	 * Description:		Called each frame for an item, normally only on active items.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Item_PostFrame, this);
	 */
	Ham_Item_PostFrame,
	
	/**
	 * Description:		Called when an item gets dropped, normally on death only.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Item_Drop, this);
	 */
	Ham_Item_Drop,
	
	/**
	 * Description:		Normally called when an item gets deleted.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Item_Drop, this);
	 */
	Ham_Item_Kill,
	
	/**
	 * Description:		Called when an entity starts being attached to (normally invisible and "following") a player.
	 * Forward params:	function(this, idPlayer)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Item_AttachToPlayer, this, idPlayer)
	 */
	Ham_Item_AttachToPlayer,
	
	/**
	 * Description:		Returns the ammo index of the item.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_Item_PrimaryAmmoIndex, this);
	 */
	Ham_Item_PrimaryAmmoIndex,
	
	/**
	 * Description:		Returns the secondary ammo index of the item.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_Item_SecondaryAmmoIndex, this);
	 */
	Ham_Item_SecondaryAmmoIndex,

	/**
	 * Description:		Updates item data for the client.
	 * Forward params:	function(this, idPlayer)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_Item_UpdateClientData, this, idPlayer);
	 */
	Ham_Item_UpdateClientData,
	
	/**
	 * Description:		Returns the entity index if the item is a weapon, -1 otherwise.
	 * Forward params:	function(this)
	 * Return type:		Entity.
	 * Execute Params:	ExecuteHam(Ham_Item_GetWeaponPtr, this)
	 */
	Ham_Item_GetWeaponPtr,
	
	/**
	 * Description:		Returns the item slot for the item.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_Item_ItemSlot, this)
	 */
	Ham_Item_ItemSlot,
	
	
	/**
	 * Weapons have all the attributes to Ham_Item_*, in addition to these.
	 */
	 
	/**
	 * Description:		Gets ammo from the target weapon.
	 * Forward params:	function(this, idTarget)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_Weapon_ExtractAmmo, this, idTarget)
	 */
	Ham_Weapon_ExtractAmmo,

	/**
	 * Description:		Gets clip ammo from the target weapon.
	 * Forward params:	function(this, idTarget)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_Weapon_ExtractAmmo, this, idTarget)
	 */
	Ham_Weapon_ExtractClipAmmo,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Weapon_AddWeapon, this);
	 */
	Ham_Weapon_AddWeapon,
	
	/**
	 * Description:		Plays the weapon's empty sound.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Weapon_PlayEmptySound, this);
	 */
	Ham_Weapon_PlayEmptySound,

	/**
	 * Description:		Sets the weapon so that it can play empty sound again.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_ResetEmptySound, this);
	 */
	Ham_Weapon_ResetEmptySound,
	
	/**
	 * Description:		Sends an animation event for the weapon.
	 * Forward params:	function(this, iAnim, skiplocal, body);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_SendWeaponAnim, this, iAnim, skiplocal, body);
	 */
	Ham_Weapon_SendWeaponAnim,
	
	/**
	 * Description:		Whether or not the weapon is usable (has ammo, etc.)
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Weapon_IsUsable, this)
	 */
	Ham_Weapon_IsUsable,
	
	/**
	 * Description:		Called when the main attack of a weapon is triggered.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_PrimaryAttack, this);
	 */
	Ham_Weapon_PrimaryAttack,
	
	/**
	 * Description:		Called when the secondary attack of a weapon is triggered.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_SecondaryAttack, this);
	 */
	Ham_Weapon_SecondaryAttack,
	
	/**
	 * Description:		Called when the weapon is reloaded.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_Reload, this);
	 */
	Ham_Weapon_Reload,
	
	/**
	 * Description:		Displays the idle animation for the weapon.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_WeaponIdle, this);
	 */
	Ham_Weapon_WeaponIdle,
	
	/**
	 * Description:		There is no more ammo for this gun, so switch to the next best one.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * ExecuteParams:	ExecuteHam(Ham_Weapon_RetireWeapon, this)
	 */
	Ham_Weapon_RetireWeapon,
	
	/**
	 * Description:		Whether or not the weapon should idle.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute Params:	ExecuteHam(Ham_Weapon_ShouldWeaponIdle, this)
	 */
	Ham_Weapon_ShouldWeaponIdle,
	
	/**
	 * Description:		Not sure.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Weapon_UseDecrement, this);
	 */
	Ham_Weapon_UseDecrement,
	 
	/**
	 * Description:		-
	 * Forward params:	function(this, someboolvalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TS_BreakableRespawn, this, someboolvalue);
	 */
	Ham_TS_BreakableRespawn,
	
	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean)
	 * Execute params:	ExecuteHam(Ham_TS_CanUsedThroughWalls, this);
	 */
	Ham_TS_CanUsedThroughWalls,
	
	/**
	 * Description:		Unsure - this was removed in TS 3.0 (and thus is deprecated).
	 * Forward params:	function(this)
	 * Return type:		Integer (I think...)
	 * Execute params:	ExecuteHam(Ham_TS_RespawnWait, this);
	 */
	Ham_TS_RespawnWait,

	/**
	 * Description:		This is called on a map reset for most map based entities.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CS_Restart, this);
	 */
	Ham_CS_Restart,
	
	/**
	 * Description:		Respawn function for players/bots only! Do not use this on non player/bot entities!
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CS_RoundRespawn, this);
	 */
	Ham_CS_RoundRespawn,
	/**
	 * Description:		Whether or not the player can drop the specified item.
	 * Forward params:	function(this)
	 * Return type:		Integer
	 * Execute params:	ExecuteHam(Ham_CS_Item_CanDrop, this);
	 */
	Ham_CS_Item_CanDrop,
	
	/**
	 * Description:		Gets the maximum speed for whenever a player has the item deployed.
	 * Forward params:	function(this);
	 * Return type:		Float, byrefed in execute.
	 * Execute params:	ExecuteHam(Ham_CS_Item_GetMaxSpeed, this, Float:output);
	 */
	Ham_CS_Item_GetMaxSpeed,

	/**
	 * Description:		I assume this spawns players at the start of a new round.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_RoundRespawn, this);
	 */
	Ham_DOD_RoundRespawn,

	/**
	 * Description:		I assume this spawns entities (like func_breakables) at the start of a new round.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_RoundRespawnEnt, this);
	 */
	Ham_DOD_RoundRespawnEnt,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		None, I think...
	 * Execute params:	ExecuteHam(Ham_DOD_RoundStore, this);
	 */
	Ham_DOD_RoundStore,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, someintegervalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_DOD_AreaSetIndex, this, someintegervalue)
	 */
	Ham_DOD_AreaSetIndex,
	
	/**
	 * Description:		Unsure
	 * Forward params:	function(this, idPlayer)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_AreaSendStatus, this, idPlayer);
	 */
	Ham_DOD_AreaSendStatus,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_GetState, this);
	 */
	Ham_DOD_GetState,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, idtarget)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_GetStateEnt, this, idtarget);
	 */
	Ham_DOD_GetStateEnt,
	
	/**
	 * Description:		Whether or not a player can drop this item.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute Params:	ExecuteHam(Ham_DOD_Item_CanDrop, this);
	 */
	Ham_DOD_Item_CanDrop,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, iduser)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_TFC_EngineerUse, this, iduser)
	 */
	Ham_TFC_EngineerUse,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_Finished, this);
	 */
	Ham_TFC_Finished,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, entityid, Float:floata, Float:floatb)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_EmpExplode, this, entityid, Float:floata, Float:floatb)
	 */
	Ham_TFC_EmpExplode,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, Float:floata, Float:floatb)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_CalcEmpDmgRad, this, Float:floata, Float:floatb)
	 */
	Ham_TFC_CalcEmpDmgRad,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, entityid)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_TakeEmpBlast, this, entityid);
	 */
	Ham_TFC_TakeEmpBlast,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_EmpRemove, this);
	 */
	Ham_TFC_EmpRemove,
	
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, entityid, Float:floata)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_TakeConcussionBlast, this, entityid, Float:floata);
	 */
	Ham_TFC_TakeConcussionBlast,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, entityid)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_Concuss, this, entityid);
	 */
	Ham_TFC_Concuss,

	
	/**
	 * Description:		Unsure.
	 *					Is only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsEnvModel, this);
	 */
	Ham_ESF_IsEnvModel, 
	
	/**
	 * Description:		Unsure.
	 *					Is only in ESF Open Beta.
	 * Forward params:	function(this, entityida, entityidb, Float:floata, Float:floatb, dmgbits)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_TakeDamage2, this, entityida, entityidb, Float:floata, Float:floatb, dmgbits);
	 */
	Ham_ESF_TakeDamage2,

	/**
	 * Description:		Returns how many points each entity is worth.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_NS_GetPointValue, this);
	 */
	Ham_NS_GetPointValue,
	
	/**
	 * Description:		Unsure.  Probably awards this with the killing of idvictim.
	 * Forward params:	function(this, idvictim)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_AwardKill, this, idvictim);
	 */
	Ham_NS_AwardKill,
	
	/**
	 * Description:		Unsure, probably whenever an entity resets after a new round.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_ResetEntity, this);
	 */
	Ham_NS_ResetEntity,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_UpdateOnRemove, this)
	 */
	Ham_NS_UpdateOnRemove,


	/** Virtual functions added to TS in TS 3 */
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TS_GiveSlowMul, this)
	 */
	Ham_TS_GiveSlowMul,
	
	/**
	 * Description:		Unsure.  The second paramater is actually a char.
	 * Forward params:	function(this, Float:someval, someotherval)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TS_GoSlow, this, Float:someval, someotherval)
	 */
	Ham_TS_GoSlow,
	
	/**
	 * Description:		Probably returns true if the user is in slow mo.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_TS_InSlow, this)
	 */
	Ham_TS_InSlow,
	
	/**
	 * Description:		Returns true if the entity is an objective.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_TS_IsObjective, this)
	 */
	Ham_TS_IsObjective,
	
	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, bool:someval)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TS_EnableObjective, this, bool:someval)
	 */
	Ham_TS_EnableObjective,
	
	/**
	 * Description:		Probably called when the engine call to OnEntFreePrivateData is called (the entity destructor.)
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TS_OnEntFreePrivateData, this)
	 */
	Ham_TS_OnFreeEntPrivateData,
	
	/**
	 * Description:		Probably called when the engine call to ShouldCollide is called.
	 * Forward params:	function(this, otherEntity)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_TS_ShouldCollide, this, otherEntity)
	 */
	Ham_TS_ShouldCollide,


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
