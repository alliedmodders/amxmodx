// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Ham Sandwich Constants
//

#if defined _ham_const_included
	#endinput
#endif
#define _ham_const_included

/**
 * Ham return types.
 *
 * @note Return these from hooks to disable calling the target function.
 *       Numbers match up with fakemeta's FMRES_* for clarity. They are
 *       interchangable. 0 (or no return) is also interpretted as HAM_IGNORED.
 */
#define HAM_IGNORED		1	/**< Calls target function, returns normal value */
#define HAM_HANDLED		2	/**< Tells the module you did something, still calls target function and returns normal value */
#define HAM_OVERRIDE	3	/**< Still calls the target function, but returns whatever is set with SetHamReturn*() */
#define HAM_SUPERCEDE	4	/**< Block the target call, and use your return value (if applicable) (Set with SetHamReturn*()) */

/**
 * List of virtual functions made available through the hamsandwich module.
 *
 * @note Not all functions will do as you expect on all mods. If a function does
 *       not do what you would believe it should DO NOT file a bug report, you
 *       will be ignored.
 * @note Passing invalid parameters has potential to crash the server, so be
 *       careful, and test carefully and adequately!
 * @note All functions take (and pass) a "this" index as the first param.
 *       This is the entity from which the function is being executed on.
 * @note All functions and forwards (eg: {Register,Execute}Ham[B]) require
 *       the mod to have the pev and base keys in addition to the function
 *       keys for the corresponding mod/operating system in gamedata
 * @note Some functions that return booleans may need to be logically ANDed
 *       to get the desired results because the mod will return the full integer
 *       value. E.g.: (ExecuteHam(Ham_TS_IsObjective, this) & 0x0000FFFF) != 0
 */

enum Ham
{
	/**
	 * Description:		This is typically called whenever an entity is created.
	 *					It is the virtual equivilent of spawn from the engine.
	 *					Some mods call this on player spawns too.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Spawn, this);
	 */
	Ham_Spawn = 0,

	/**
	 * Description:		This is typically called on map change.
	 *					This will typically precache all assets required by the entity.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Precache, this);
	 */
	Ham_Precache,

	/**
	 * Description:		Typically this is similar to an engine keyvalue call.
	 *					Use the kvd natives from fakemeta to handle the kvd_handle passed.
	 *					NOTE: Do not pass handle 0 to this! Use create_kvd() from fakemeta instead!
	 * Forward params:	function(this, kvd_handle);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Keyvalue, this, kvd_handle);
	 */
	Ham_Keyvalue,

	/**
	 * Description:		Returns flags for how an entity can be used.
	 * Forward params:	function(this)
	 * Return type:		Integer (FCAP_* constants, see hlsdk_const.inc).
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
	 *					For Team Fortress Classic mod, see Ham_TFC_Killed.
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
	 *					This function is not supported by Day Of Defeat.
	 *					This function has different version for the following mods:
	 *					    Team Fortress Classic, see Ham_TFC_IsTriggered instead.
	 *						Sven-Coop 5.0+, see Ham_SC_IsTriggered instead.
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
	 *					This function is not supported for the following mods:
	 *						Team Fortress Classic.
	 *						Sven-Coop 5.0-, see Ham_SC_AddPoints instead.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_AddPoints, this, points, bool:cangonegative);
	 */
	Ham_AddPoints,

	/**
	 * Description:		Typically adds points to everybody on the entity's team.
	 *					This function is not supported for the following mods:
	 *						Team Fortress Classic.
	 *						Sven-Coop 5.0-, see Ham_SC_AddPointsToTeam instead.
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
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_RemovePlayerItem instead.
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_RemovePlayerItem, this, idother);
	 */
	Ham_RemovePlayerItem,

	/**
	 * Description:		Gives ammo to the entity.
	 *					This function is not supported for the following mods:
	 *						Earth's Special Forces.
	 *						Sven-Coop 4.8+, see Ham_SC_GiveAmmo instead.
	 *						Team Fortress Classic, see Ham_TFC_GiveAmmo instead.
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
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsMoving instead.
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
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_OnControls instead.
	 * Forward params:	function(this, idOn)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_OnControls, this, idOn);
	 */
	Ham_OnControls,

	/**
	 * Description:		Whether or not the entity is sneaking.
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsSneaking instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsSneaking, this);
	 */
	Ham_IsSneaking,

	/**
	 * Description:		Whether or not the entity is alive.
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsAlive instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsAlive, this);
	 */
	Ham_IsAlive,

	/**
	 * Description:		Whether or not the entity uses a BSP model.
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsBSPModel instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsBSPModel, this);
	 */
	Ham_IsBSPModel,

	/**
	 * Description:		Whether or not the entity can reflect gauss shots..
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_ReflectGauss instead.
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
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_HasTarget instead.
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_HasTarget, this, strindex);
	 */
	Ham_HasTarget,

	/**
	 * Description:		Whether or not the entity is in the world.
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsInWorld instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsInWorld, this);
	 */
	Ham_IsInWorld,

	/**
	 * Description:		Whether or not the entity is a player.
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsPlayer instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsPlayer, this);
	 */
	Ham_IsPlayer,

	/**
	 * Description:		Whether or not the entity is a net client.
	 *					This function is not supported for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_IsNetClient instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_IsNetClient, this);
	 */
	Ham_IsNetClient,

	/**
	 * Description:		Get the entity's team id.
	 *					This function is not supported by Team Fortress Classic mod.
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
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_FBecomeProne instead.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_FBecomeProne, this);
	 */
	Ham_FBecomeProne,

	/**
	 * Description:		Returns the center of the entity.
	 * Forward params:	function(this);
	 * Return type:		Vector (byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_Center, this, Float:output[3]);
	 */
	Ham_Center,

	/**
	 * Description:		Returns the eye position of the entity.
	 * Forward params:	function(this);
	 * Return type:		Vector (byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_EyePosition, this, Float:output[3]);
	 */
	Ham_EyePosition,

	/**
	 * Description:		Returns the ear position of the entity.
	 * Forward params:	function(this);
	 * Return type:		Vector (byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_EarPosition, this, Float:output[3]);
	 */
	Ham_EarPosition,

	/**
	 * Description:		Position to shoot at.
	 * Forward params:	function(this, Float:srcvector[3]);
	 * Return type:		Vector (byref'd in ExecuteHam).
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
	 * Description:		Returns true if a line can be traced from the caller's eyes to the target.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_FVisible instead.
	 * Forward params:	function(this, idOther);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FVisible, this, idOther);
	 */
	Ham_FVisible,

	/**
	 * Description:		Returns true if a line can be traced from the caller's eyes to given vector.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_FVecVisible instead.
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
	 *					This function is not supported in Earth's Special Forces mod.
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
	 *					This function is not supported by Team Fortress Classic.
	 * Forward params:	function(this)
	 * Return type:		Vector (byreffed in ExecuteHam);.
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
	 *					This function is not supported by Team Fortress Classic mod.
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
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Item_AddToPlayer instead.
	 * Forward params:	function(this, idPlayer);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_AddToPlayer, this, idPlayer);
	 */
	Ham_Item_AddToPlayer,

	/**
	 * Description:		Unsure.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Item_AddDuplicate instead.
	 * Forward params:	function(this, idOriginal);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_AddDuplicate, this, idOriginal);
	 */
	Ham_Item_AddDuplicate,

	/**
	 * Description:		Whether or not this entity can be deployed.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Item_CanDeploy instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_CanDeploy, this);
	 */
	Ham_Item_CanDeploy,

	/**
	 * Description:		Deploys the entity (usually a weapon).
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Item_Deploy instead.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_Deploy, this);
	 */
	Ham_Item_Deploy,

	/**
	 * Description:		Whether or not the entity can be holstered.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Item_CanHolster instead.
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
	 * Execute params:	ExecuteHam(Ham_Item_Kill, this);
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
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Item_UpdateClientData instead.
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
	 *					This function is not supported in Earth's Special Forces mod.
	 * Forward params:	function(this, idTarget)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_Weapon_ExtractAmmo, this, idTarget)
	 */
	Ham_Weapon_ExtractAmmo,

	/**
	 * Description:		Gets clip ammo from the target weapon.
	 *					This function is not supported in Earth's Special Forces mod.
	 * Forward params:	function(this, idTarget)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_Weapon_ExtractAmmo, this, idTarget)
	 */
	Ham_Weapon_ExtractClipAmmo,

	/**
	 * Description:		Unsure.
	 *					This function is not supported in Earth's Special Forces mod.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Weapon_AddWeapon instead.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Weapon_AddWeapon, this);
	 */
	Ham_Weapon_AddWeapon,

	/**
	 * Description:		Plays the weapon's empty sound.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Weapon_PlayEmptySound instead.
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
	 *					This function has different versions for the following mods:
	 *						Counter-Strike: see Ham_CS_Weapon_SendWeaponAnim.
	 *						Team Fortress Classic: see Ham_TFC_Weapon_SendWeaponAnim.
	 *						Day Of Defeat: see Ham_DOD_Weapon_SendWeaponAnim.
	 * Forward params:	function(this, iAnim, skiplocal, body);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Weapon_SendWeaponAnim, this, iAnim, skiplocal, body);
	 */
	Ham_Weapon_SendWeaponAnim,

	/**
	 * Description:		Whether or not the weapon is usable (has ammo, etc.)
	 *					This function is not supported in Earth's Special Forces mod.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Weapon_IsUsable instead.
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
	 *					This function is not supported in Earth's Special Forces mod.
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
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Weapon_ShouldWeaponIdle instead.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute Params:	ExecuteHam(Ham_Weapon_ShouldWeaponIdle, this)
	 */
	Ham_Weapon_ShouldWeaponIdle,

	/**
	 * Description:		Unsure.
	 *					This function is not supported by Team Fortress Classic.
	 *					This function has different version for the following mods:
	 *						Sven-Coop 5.0+, see Ham_SC_Weapon_UseDecrement instead.
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Weapon_UseDecrement, this);
	 */
	Ham_Weapon_UseDecrement,


	/**
	 * The following functions are specific to The Specialists.
	 */

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
	 * The following functions are specific to Counter-Strike.
	 */

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
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_CS_Item_CanDrop, this);
	 */
	Ham_CS_Item_CanDrop,

	/**
	 * Description:		Gets the maximum speed for whenever a player has the item deployed.
	 * Forward params:	function(this);
	 * Return type:		Float (byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_CS_Item_GetMaxSpeed, this, Float:output);
	 */
	Ham_CS_Item_GetMaxSpeed,


	/**
	 * The following functions are specific to Day Of Defeat.
	 */

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
	 * Description:		Unsure.
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
	 * The following functions are specific to The Team Fortress Classic.
	 */

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, playerId)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_TFC_EngineerUse, this, playerId)
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
	 * Forward params:	function(this, grenId, Float:damage, Float:radius)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_EmpExplode, this, grenId, Float:damage, Float:radius)
	 */
	Ham_TFC_EmpExplode,

	/**
	 * Description:		Unsure.
	 *					'damage' and 'radius' are byref'd in ExecuteHam.
	 * Forward params:	function(this, Float:damage, Float:radius)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_CalcEmpDmgRad, this, Float:damage, Float:radius)
	 */
	Ham_TFC_CalcEmpDmgRad,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, grenId)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_TakeEmpBlast, this, grenId);
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
	 * Forward params:	function(this, grenId, Float:bounceMax)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_TakeConcussionBlast, this, grenId, Float:bounceMax);
	 */
	Ham_TFC_TakeConcussionBlast,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this, grenId)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_Concuss, this, grenId);
	 */
	Ham_TFC_Concuss,


	/**
	 * The following functions are specific to Earth's Special Forces.
	 */

	/**
	 * Description:		Unsure.
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsEnvModel, this);
	 */
	Ham_ESF_IsEnvModel,

	/**
	 * Description:		Unsure.
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, entityida, entityidb, Float:floata, Float:floatb, dmgbits)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_TakeDamage2, this, entityida, entityidb, Float:floata, Float:floatb, dmgbits);
	 */
	Ham_ESF_TakeDamage2,


	/**
	 * The following functions are specific to Natural Selection.
	 */

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


	/**
	 * The following functions are specific to The Specialists.
	 */

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
	 * LATE ADDITIONS (2011)
	 */

	/**
	 * Description:		Turns a monster towards its ideal_yaw.
	 * Forward params:	function(this, speed);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ChangeYaw, this, speed);
	 */
	Ham_ChangeYaw,

	/**
	 * Description:		Returns if monster has human gibs.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_HasHumanGibs, this);
	 */
	Ham_HasHumanGibs,

	/**
	 * Description:		Returns if monster has alien gibs.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_HasAlienGibs, this);
	 */
	Ham_HasAlienGibs,

	/**
	 * Description:		Slowly fades a entity out, then removes it.
	 *					Using this on player will crash your server.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_FadeMonster, this);
	 */
	Ham_FadeMonster,

	/**
	 * Description:		Create some gore and get rid of a monster's model.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_GibMonster, this);
	 */
	Ham_GibMonster,

	/**
	 * Description:		Called when monster dies and prepares its entity to become a corpse.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_BecomeDead, this);
	 */
	Ham_BecomeDead,

	/**
	 * Description:		Checks relation ship between two monsters.
	 * Forward params:	function(this, idother);
	 * Return type:		Integer (R_* constants, see HLDSK).
	 * Execute params:	ExecuteHam(Ham_IRelationship, this, idother);
	 */
	Ham_IRelationship,

	/**
	 * Description:		Called when monster is about to emit pain sound.
	 *					Not guaranteed to actually emit sound. (random, time, etc..)
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_PainSound, this);
	 */
	Ham_PainSound,

	/**
	 * Description:		Prints debug information about monster to console. (state, activity, and other)
	 *					This function is called by impulse 103.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ReportAIState, this);
	 */
	Ham_ReportAIState,

	/**
	 * Description:		Called when monster has died.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_MonsterInitDead, this);
	 */
	Ham_MonsterInitDead,

	/**
	 * Description:		Function to find enemies or food by sight.
	 *					distance is maximum distance (in units) monster can see.
	 * Forward params:	function(this, distance);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Look, this, distance);
	 */
	Ham_Look,

	/**
	 * Description:		This functions searches the link list whose head is the caller's m_pLink field.
	 * Forward params:	function(this);
	 * Return type:		Integer (entity).
	 * Execute params:	ExecuteHam(Ham_BestVisibleEnemy, this);
	 */
	Ham_BestVisibleEnemy,

	/**
	 * Description:		Returns true if the passed ent is in the caller's forward view cone.
	 *					The dot product is performed in 2d, making the view cone infinitely tall.
	 * Forward params:	function(this, idOther);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FInViewCone, this, idOther);
	 */
	Ham_FInViewCone,

	/**
	 * Description:		Returns true if the passed ent is in the caller's forward view cone.
	 *					The dot product is performed in 2d, making the view cone infinitely tall.
	 * Forward params:	function(this, const Float:origin[3]);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FVecInViewCone, this, const Float:origin[3]);
	 */
	Ham_FVecInViewCone,

	/**
	 * Description:		Determines the best type of death animation to play.
	 * Forward params:	function(this);
	 * Return type:		Integer (ACT_* constants, see HLDSK. It might different depending the mod).
	 * Execute params:	ExecuteHam(Ham_GetDeathActivity, this);
	 */
	Ham_GetDeathActivity,


	/**
	 * The following functions are not supported by Counter-Strike, The Specialists and Natural Selection mods
	 */

	/**
	 * Description:		Runs core AI functions.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_RunAI, this);
	 */
	Ham_RunAI,

	/**
	 * Description:		Calls out to core AI functions and handles this monster's specific animation events.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_MonsterThink, this);
	 */
	Ham_MonsterThink,

	/**
	 * Description:		After a monster is spawned, it needs to be dropped into the world, checked for mobility problems
	 *					and put on the proper path, if any. This function does all of those things after the monster spawns.
	 *					Any initialization that should take place for all monsters goes here.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_MonsterInit, this);
	 */
	Ham_MonsterInit,

	/**
	 * Description:		Check validity of a straight move through space.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, const Float:start[3], const Float:end[3], target, Float:dist)
	 * Return type:		Integer (See LOCALMOVE_* constants).
	 * Execute params:	ExecuteHam(Ham_CheckLocalMove, this, const Float:start[3], const Float:end[3], target, Float:dist);
	 */
	Ham_CheckLocalMove,

	/**
	 * Description:		Takes a single step towards the next ROUTE location.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:interval)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Move, this, Float:interval);
	 */
	Ham_Move,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, targetEnt, const Float:dir[3], Float:interval)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_MoveExecute, this, targetEnt, const Float:dir[3], Float:interval);
	 */
	Ham_MoveExecute,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:waypointDist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ShouldAdvanceRoute, this, Float:waypointDist);
	 */
	Ham_ShouldAdvanceRoute,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this)
	 * Return type:		Integer (ACT_* constants, see HLDSK. It might different depending the mod).
	 * Execute params:	ExecuteHam(Ham_GetStoppedActivity, this);
	 */
	Ham_GetStoppedActivity,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_Stop, this);
	 */
	Ham_Stop,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CheckRangeAttack1, this, Float:dot, Float:dist);
	 */
	Ham_CheckRangeAttack1,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CheckRangeAttack2, this, Float:dot, Float:dist);
	 */
	Ham_CheckRangeAttack2,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CheckMeleeAttack1, this, Float:dot, Float:dist);
	 */
	Ham_CheckMeleeAttack1,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CheckMeleeAttack2, this, Float:dot, Float:dist);
	 */
	Ham_CheckMeleeAttack2,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ScheduleChange, this, Float:dot, Float:dist);
	 */
	Ham_ScheduleChange,

	/**
	 * Description:		Determines whether or not the monster can play the scripted sequence or AI sequence that is
	 *					trying to possess it. If DisregardState is set, the monster will be sucked into the script
	 *					no matter what state it is in. ONLY Scripted AI ents should allow this.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 *					This function has different versions for the following mod:
	 *						Sven Coop: see Ham_SC_CanPlaySequence
	 * Forward params:	function(this, bool:disregardState, interruptLevel);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CanPlaySequence, this, bool:disregardState, interruptLevel);
	 */
	Ham_CanPlaySequence,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 *					This function has different versions for the following mod:
	 *						Sven Coop: see Ham_SC_CanPlaySentence2
	 * Forward params:	function(this, bool:disregardState);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CanPlaySentence2, this, bool:disregardState);
	 */
	Ham_CanPlaySentence2,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, const sentence[], Float:duration, Float:volume, Float:attenuation);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_PlaySentence, this, const sentence[], Float:duration, Float:volume, Float:attenuation);
	 */
	Ham_PlaySentence,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 *					This function has different version for Sven Coop, see Ham_SC_PlayScriptedSentence instead.
	 * Forward params:	function(this, const sentence[], Float:duration, Float:volume, Float:attenuation, bool:concurrent, idListener);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_PlayScriptedSentence, this, const sentence[], Float:duration, Float:volume, Float:attenuation, bool:concurrent, idListener);
	 */
	Ham_PlayScriptedSentence,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SentenceStop, this);
	 */
	Ham_SentenceStop,

	/**
	 * Description:		Surveys the Conditions information available and finds the best new state for a monster.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Integer (MONSTERSTATE_* constants, see HLDSK).
	 * Execute params:	ExecuteHam(Ham_GetIdealState, this);
	 */
	Ham_GetIdealState,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, newActivity);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SetActivity, this, newActivity);
	 */
	Ham_SetActivity,

	/**
	 * Description:		Part of the condition collection process gets and stores data and conditions
	 *					pertaining to a monster's enemy. Returns TRUE if Enemy LKP was updated.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, idEnemy);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CheckEnemy, this, idEnemy);
	 */
	Ham_CheckEnemy,

	/**
	 * Description:		Tries to overcome local obstacles by triangulating a path around them.
	 *					'dist' is how far the obstruction that we are trying to triangulate around is from the monster.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, const Float:start[3], const Float:end[3], Float:dist, targetEnt, Float:apex[3]);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_FTriangulate, this, const Float:start[3], const Float:end[3], Float:dist, targetEnt, Float:apex[3]);
	 */
	Ham_FTriangulate,

	/**
	 * Description:		Allows each sequence to have a different turn rate associated with it.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SetYawSpeed, this);
	 */
	Ham_SetYawSpeed,

	/**
	 * Description:		Tries to build a route as close to the target as possible, even if there isn't a path to the final point.
	 *					If supplied, search will return a node at least as far away as MinDist from vecThreat, but no farther than minDist.
	 *					If maxDist isn't supplied, it defaults to a reasonable value.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_BuildNearestRoute, this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist);
	 */
	Ham_BuildNearestRoute,

	/**
	 * Description:		Tries to find a nearby node that will hide the caller from its enemy.
	 *					If supplied, search will return a node at least as far away as MinDist from vecThreat, but no farther than minDist.
	 *					If maxDist isn't supplied, it defaults to a reasonable value.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FindCover, this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist);
	 */
	Ham_FindCover,

	/**
	 * Description:		Default cover radius.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_CoverRadius, this);
	 */
	Ham_CoverRadius,

	/**
	 * Description:		Prequalifies a monster to do more fine checking of potential attacks.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FCanCheckAttacks, this);
	 */
	Ham_FCanCheckAttacks,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CheckAmmo, this);
	 */
	Ham_CheckAmmo,

	/**
	 * Description:		Before a set of conditions is allowed to interrupt a monster's schedule, this function removes
	 *					conditions that we have flagged to interrupt the current schedule, but may not want to interrupt
	 *					the schedule every time. (Pain, for instance)
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IgnoreConditions, this);
	 */
	Ham_IgnoreConditions,

	/**
	 * Description:		Tells use whether or not the monster cares about the type of Hint Node given.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, hint);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FValidateHintType, this, hint);
	 */
	Ham_FValidateHintType,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_FValidateHintType, this);
	 */
	Ham_FCanActiveIdle,

	/**
	 * Description:		Returns a bit mask indicating which types of sounds this monster regards.
	 *					In the base class implementation, monsters care about all sounds, but no scents.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ISoundMask, this);
	 */
	Ham_ISoundMask,

	/**
	 * Description:		-
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_HearingSensitivity, this);
	 */
	Ham_HearingSensitivity,

	/**
	 * Description:		Called by Barnacle victims when the barnacle pulls their head into its mouth.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this, idBarnacle);
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_BarnacleVictimBitten, this, idBarnacle);
	 */
	Ham_BarnacleVictimBitten,

	/**
	 * Description:		Called by barnacle victims when the host barnacle is killed.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_BarnacleVictimReleased, this);
	 */
	Ham_BarnacleVictimReleased,

	/**
	 * Description:		Runs after conditions are collected and before scheduling code is run.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_PrescheduleThink, this);
	 */
	Ham_PrescheduleThink,

	/**
	 * Description:		Plays death sounds.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_DeathSound, this);
	 */
	Ham_DeathSound,

	/**
	 * Description:		Plays alert sounds.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_AlertSound, this);
	 */
	Ham_AlertSound,

	/**
	 * Description:		Plays idle sounds.
	 *					This function is not supported by Counter-Strike, The Specialists and Natural Selection mods.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_IdleSound, this);
	 */
	Ham_IdleSound,

	/**
	 * Description:		This should stop a monster following a target.
	 * Forward params:	function(this, bool:clearSchedule)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_StopFollowing, this, bool:clearSchedule);
	 */
	Ham_StopFollowing,


	/**
	 * The following functions are specific to Counter-Strike.
	 */

	/**
	 * Description:		Sends an animation event for the weapon. skiplocal is 1 if client is predicting weapon animations.
	 * Forward params:	function(this, anim, skiplocal);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CS_Weapon_SendWeaponAnim, this, anim, skiplocal);
	 */
	Ham_CS_Weapon_SendWeaponAnim,

	/**
	 * Description:		Resets the player's max speed.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CS_Player_ResetMaxSpeed, this);
	 */
	Ham_CS_Player_ResetMaxSpeed,

	/**
	 * Description:		Whether or not the player is a bot.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CS_Player_IsBot, this);
	 */
	Ham_CS_Player_IsBot,

	/**
	 * Description:		Returns a vector that tells the autoaim direction.
	 *					Set crosshair position to point to enemey if sv_aim is 1.
	 * Forward params:	function(this, Float:delta)
	 * Return type:		Vector, byreffed in execute.
	 * Execute params:	ExecuteHam(Ham_CS_Player_GetAutoaimVector, this, Float:delta, Float:output[3]);
	 */
	Ham_CS_Player_GetAutoaimVector,

	/**
	 * Description:		Whether or not the player is being flashing. (flashbang grenade explosion)
	 *					blindTime is the time you are considered as being blind. (holdTime * 0.33).
	 * Forward params:	function(this, Float:blindTime, Float:duration, Float:holdTime, alpha);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CS_Player_Blind, this, Float:blindTime, Float:duration, Float:holdTime, alpha);
	 */
	Ham_CS_Player_Blind,

	/**
	 * Description:		Whether or not the player is touching a weapon on the ground.
	 * Forward params:	function(this, entityid);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_CS_Player_OnTouchingWeapon, this, entityid);
	 */
	Ham_CS_Player_OnTouchingWeapon,


	/**
	 * The following functions are specific to Day Of Defeat.
	 */

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_SetScriptReset, this);
	 */
	Ham_DOD_SetScriptReset,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_Item_SpawnDeploy, this);
	 */
	Ham_DOD_Item_SpawnDeploy,

	/**
	 * Description:		-
	 * Forward params:	function(this, Float:someValue)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_Item_SetDmgTime, this, Float:someValue);
	 */
	Ham_DOD_Item_SetDmgTime,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_Item_DropGren, this);
	 */
	Ham_DOD_Item_DropGren,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_IsUseable, this);
	 */
	Ham_DOD_Weapon_IsUseable,

	/**
	 * Description:		-
	 * Forward params:	function(this, Float:accuracyFactor, idother, shared_rand)
	 * Return type:		Vector, byreffed in execute.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_Aim, this, Float:accuracyFactor, idother, shared_rand, Float:output[3]);
	 */
	Ham_DOD_Weapon_Aim,

	/**
	 * Description:		-
	 * Forward params:	function(this, Float:accuracyFactor, idother)
	 * Return type:		Float.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_flAim, this, Float:accuracyFactor, idother);
	 */
	Ham_DOD_Weapon_flAim,

	/**
	 * Description:		-
	 * Forward params:	function(this, Float:amount, targetEnt)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_RemoveStamina, this, Float:amount, targetEnt);
	 */
	Ham_DOD_Weapon_RemoveStamina,

	/**
	 * Description:		-
	 * Forward params:	function(this, fov)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_ChangeFOV, this, fov);
	 */
	Ham_DOD_Weapon_ChangeFOV,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_ZoomOut, this);
	 */
	Ham_DOD_Weapon_ZoomOut,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_ZoomIn, this);
	 */
	Ham_DOD_Weapon_ZoomIn,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_GetFOV, this);
	 */
	Ham_DOD_Weapon_GetFOV,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_IsWaterSniping, this);
	 */
	Ham_DOD_Weapon_PlayerIsWaterSniping,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_UpdateZoomSpeed, this);
	 */
	Ham_DOD_Weapon_UpdateZoomSpeed,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute Params:	ExecuteHam(Ham_DOD_Weapon_Special, this);
	 */
	Ham_DOD_Weapon_Special,


	/**
	 * The following functions are specific to Team Fortress Classic.
	 */

	/**
	 * Description:		Get the item name.
	 * Forward params:	function(this, const buffer[]);
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_TFC_DB_GetItemName, this, buffer[], size);
	 */
	Ham_TFC_DB_GetItemName,

	/**
	 * Description:		This entity is exploding, or otherwise needs to inflict damage upon entities within a certain range.
	 * Forward params:	function(this, inflictorId, attackerId, Float:damage, classIgnore, bitsDamageType)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_RadiusDamage, this, inflictorId, attackerId, Float:damage, classIgnore, bitsDamageType);
	 */
	Ham_TFC_RadiusDamage,

	/**
	 * Description:		This entity is exploding, or otherwise needs to inflict damage upon entities within a certain range.
	 * Forward params:	function(this, const Float:source[3], inflictorId, attackerId, Float:damage, classIgnore, bitsDamageType)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_RadiusDamage2, this, const Float:source[3], inflictorId, attackerId, Float:damage, classIgnore, bitsDamageType);
	 */
	Ham_TFC_RadiusDamage2,


	/**
	 * The following functions are specific to Earth's Special Forces.
	 */

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsFighter, this);
	 */
	Ham_ESF_IsFighter,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsBuddy, this);
	 */
	Ham_ESF_IsBuddy,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, const char sample[], somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_EmitSound, this, const sample[], somevalue);
	 */
	Ham_ESF_EmitSound,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_EmitNullSound, this, somevalue);
	 */
	Ham_ESF_EmitNullSound,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, someentid, somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_IncreaseStrength, this, someentid, somevalue);
	 */
	Ham_ESF_IncreaseStrength,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_IncreasePL, this, somevalue);
	 */
	Ham_ESF_IncreasePL,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetPowerLevel, this, somevalue);
	 */
	Ham_ESF_SetPowerLevel,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetMaxPowerLevel, this, somevalue);
	 */
	Ham_ESF_SetMaxPowerLevel,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, anim)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StopAniTrigger, this, anim);
	 */
	Ham_ESF_StopAniTrigger,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StopFly, this);
	 */
	Ham_ESF_StopFly,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_HideWeapon, this);
	 */
	Ham_ESF_HideWeapon,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_HideWeapon, this, somevalue);
	 */
	Ham_ESF_ClientRemoveWeapon,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, const model[])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SendClientsCustomModel, this, const model[]);
	 */
	Ham_ESF_SendClientsCustomModel,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanTurbo, this);
	 */
	Ham_ESF_CanTurbo,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanPrimaryFire, this);
	 */
	Ham_ESF_CanPrimaryFire,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanSecondaryFire, this);
	 */
	Ham_ESF_CanSecondaryFire,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanStopFly, this);
	 */
	Ham_ESF_CanStopFly,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanBlock, this);
	 */
	Ham_ESF_CanBlock,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanRaiseKi, this);
	 */
	Ham_ESF_CanRaiseKi,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanRaiseStamina, this);
	 */
	Ham_ESF_CanRaiseStamina,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanTeleport, this);
	 */
	Ham_ESF_CanTeleport,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanStartFly, this);
	 */
	Ham_ESF_CanStartFly,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanStartPowerup, this);
	 */
	Ham_ESF_CanStartPowerup,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanJump, this);
	 */
	Ham_ESF_CanJump,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_CanWallJump, this);
	 */
	Ham_ESF_CanWallJump,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsSuperJump, this);
	 */
	Ham_ESF_IsSuperJump,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsMoveBack, this);
	 */
	Ham_ESF_IsMoveBack,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_CheckWallJump, this);
	 */
	Ham_ESF_CheckWallJump,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, const Float:somevalue[3])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_EnableWallJump, this, const Float:somevalue[3]);
	 */
	Ham_ESF_EnableWallJump,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_DisableWallJump, this);
	 */
	Ham_ESF_DisableWallJump,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_ResetWallJumpVars, this);
	 */
	Ham_ESF_ResetWallJumpVars,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, const value[], const Float:somevalue[3], const someothervalue[])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_GetWallJumpAnim, const value[], const Float:somevalue[3], const someothervalue[]);
	 */
	Ham_ESF_GetWallJumpAnim,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, const somevalue[], const someothervalue[])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_GetWallJumpAnim2, this, const somevalue[], const someothervalue[]);
	 */
	Ham_ESF_GetWallJumpAnim2,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetFlyMoveType, this);
	 */
	Ham_ESF_SetWallJumpAnimation,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetFlyMoveType, this);
	 */
	Ham_ESF_SetFlyMoveType,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsFlyMoveType, this);
	 */
	Ham_ESF_IsFlyMoveType,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_IsWalkMoveType, this);
	 */
	Ham_ESF_IsWalkMoveType,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetWalkMoveType, this);
	 */
	Ham_ESF_SetWalkMoveType,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_DrawChargeBar, this, somevalue);
	 */
	Ham_ESF_DrawChargeBar,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StartBlock, this);
	 */
	Ham_ESF_StartBlock,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StopBlock, this);
	 */
	Ham_ESF_StopBlock,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StartFly, this);
	 */
	Ham_ESF_StartFly,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, Float:value)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_ESF_GetMaxSpeed, this, Float:value);
	 */
	Ham_ESF_GetMaxSpeed,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, anim)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetAnimation, this, anim);
	 */
	Ham_ESF_SetAnimation,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_PlayAnimation, this);
	 */
	Ham_ESF_PlayAnimation,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_GetMoveForward, this);
	 */
	Ham_ESF_GetMoveForward,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_GetMoveRight, this);
	 */
	Ham_ESF_GetMoveRight,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_GetMoveUp, this);
	 */
	Ham_ESF_GetMoveUp,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_AddBlindFX, this);
	 */
	Ham_ESF_AddBlindFX,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_RemoveBlindFX, this);
	 */
	Ham_ESF_RemoveBlindFX,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_DisablePSBar, this);
	 */
	Ham_ESF_DisablePSBar,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_AddBeamBoxCrosshair, this, somevalue);
	 */
	Ham_ESF_AddBeamBoxCrosshair,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_RemoveBeamBoxCrosshair, this);
	 */
	Ham_ESF_RemoveBeamBoxCrosshair,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_DrawPSWinBonus, this);
	 */
	Ham_ESF_DrawPSWinBonus,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, Float:value, Float:othervalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_DrawPSBar, this, Float:value, Float:othervalue);
	 */
	Ham_ESF_DrawPSBar,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_LockCrosshair, this);
	 */
	Ham_ESF_LockCrosshair,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_UnLockCrosshair, this);
	 */
	Ham_ESF_UnLockCrosshair,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_RotateCrosshair, this);
	 */
	Ham_ESF_RotateCrosshair,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_UnRotateCrosshair, this);
	 */
	Ham_ESF_UnRotateCrosshair,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_WaterMove, this);
	 */
	Ham_ESF_WaterMove,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_CheckTimeBasedDamage, this);
	 */
	Ham_ESF_CheckTimeBasedDamage,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_DoesSecondaryAttack, this);
	 */
	Ham_ESF_DoesSecondaryAttack,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_ESF_DoesPrimaryAttack, this);
	 */
	Ham_ESF_DoesPrimaryAttack,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_RemoveSpecialModes, this);
	 */
	Ham_ESF_RemoveSpecialModes,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StopTurbo, this);
	 */
	Ham_ESF_StopTurbo,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_TakeBean, this);
	 */
	Ham_ESF_TakeBean,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_GetPowerLevel, this);
	 */
	Ham_ESF_GetPowerLevel,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_RemoveAllOtherWeapons, this);
	 */
	Ham_ESF_RemoveAllOtherWeapons,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_StopSwoop, this);
	 */
	Ham_ESF_StopSwoop,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetDeathAnimation, this);
	 */
	Ham_ESF_SetDeathAnimation,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_SetModel, this);
	 */
	Ham_ESF_SetModel,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_AddAttacks, this);
	 */
	Ham_ESF_AddAttacks,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this, const value[], const othervalue[], somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_EmitClassSound, this, const value[], const othervalue[], somevalue);
	 */
	Ham_ESF_EmitClassSound,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_CheckLightning, this);
	 */
	Ham_ESF_CheckLightning,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_FreezeControls, this);
	 */
	Ham_ESF_FreezeControls,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_UnFreezeControls, this);
	 */
	Ham_ESF_UnFreezeControls,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_UpdateKi, this);
	 */
	Ham_ESF_UpdateKi,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_ESF_UpdateHealth, this);
	 */
	Ham_ESF_UpdateHealth,

	/**
	 * Description:		-
	 *					This is available only in ESF Open Beta.
	 * Forward params:	function(this)
	 * Return type:		Vector.
	 * Execute params:	ExecuteHam(Ham_ESF_GetTeleportDir, this, output[3]);
	 */
	Ham_ESF_GetTeleportDir,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_ESF_Weapon_HolsterMeleed, this);
	 */
	Ham_ESF_Weapon_HolsterWhenMeleed,


	/**
	 * The following functions are specific to Natural Selection.
	 */

	/**
	 * Description:		-
	 * Forward params:	function(this, controller, Float:value)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_SetBoneController, this, controller, Float:value)
	 */
	Ham_NS_SetBoneController,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_SaveDataForReset, this)
	 */
	Ham_NS_SaveDataForReset,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_NS_GetHull, this)
	 */
	Ham_NS_GetHull,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_NS_GetMaxWalkSpeed, this)
	 */
	Ham_NS_GetMaxWalkSpeed,

	/**
	 * Description:		-
	 * Forward params:	function(this, const teamID[])
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_NS_SetTeamID, this, const teamID[])
	 */
	Ham_NS_SetTeamID,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_NS_GetPlayerClass, this)
	 */
	Ham_NS_GetEffectivePlayerClass,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_NS_GetAuthenticationMask, this)
	 */
	Ham_NS_GetAuthenticationMask,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_EffectivePlayerClassChanged, this)
	 */
	Ham_NS_EffectivePlayerClassChanged,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_NeedsTeamUpdate, this)
	 */
	Ham_NS_NeedsTeamUpdate,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_SendTeamUpdate, this)
	 */
	Ham_NS_SendTeamUpdate,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_SendWeaponUpdate, this)
	 */
	Ham_NS_SendWeaponUpdate,

	/**
	 * Description:		-
	 * Forward params:	function(this, idOther)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_InitPlayerFromSpawn, this, idOther)
	 */
	Ham_NS_InitPlayerFromSpawn,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_PackDeadPlayerItems, this)
	 */
	Ham_NS_PackDeadPlayerItems,

	/**
	 * Description:		Gets sequence name based on index.
	 *					animationName are passed by reference in ExecuteHam. outputLength is the max output length.
	 * Forward params:	function(this, activity, const animationName[], bool:somevalue)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_GetAnimationForActivity, this, activity, output[], bool:somevalue, outputLength)
	 */
	Ham_NS_GetAnimationForActivity,

	/**
	 * Description:		-
	 * Forward params:	function(this, const Float:position[3], const Float:viewAngles[3])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_StartObserver, this, const Float:position[3], const Float:viewAngles[3])
	 */
	Ham_NS_StartObserver,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_StopObserver, this)
	 */
	Ham_NS_StopObserver,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_NS_GetAdrenalineFactor, this)
	 */
	Ham_NS_GetAdrenalineFactor,

	/**
	 * Description:		-
	 * Forward params:	function(this, const name[], bool:showpickup))
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_GiveNamedItem, this, const name[], bool:showpickup)
	 */
	Ham_NS_GiveNamedItem,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_Suicide, this)
	 */
	Ham_NS_Suicide,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_NS_GetCanUseWeapon, this)
	 */
	Ham_NS_GetCanUseWeapon,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_GetWeapPrimeTime, this)
	 */
	Ham_NS_Weapon_GetWeaponPrimeTime,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_PrimeWeapon, this)
	 */
	Ham_NS_Weapon_PrimeWeapon,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_GetIsWeaponPrimed, this)
	 */
	Ham_NS_Weapon_GetIsWeaponPrimed,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_GetIsWeapPriming, this)
	 */
	Ham_NS_Weapon_GetIsWeaponPriming,

	/**
	 * Description:		-
	 * Forward params:	function(this, const viewModel[], const weaponModel[], anim, const animExt[], skiplocal, body)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_DefaultDeploy, this, const viewModel[], const weaponModel[], anim, const animExt[], skiplocal, body)
	 */
	Ham_NS_Weapon_DefaultDeploy,

	/**
	 * Description:		-
	 * Forward params:	function(this, clipsize, anim, Float:delay, body)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_DefaultReload, this, clipsize, anim, Float:delay, body)
	 */
	Ham_NS_Weapon_DefaultReload,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_NS_Weapon_GetDeployTime, this)
	 */
	Ham_NS_Weapon_GetDeployTime,


	/**
	 * The following functions are specific to Sven Co-op.
	 */

	/**
	 * Description:		Returns the type of group (i.e, "houndeye", or "human military"
	 *					so that monsters with different classnames still realize that they are teammates.
	 *					(overridden for monsters that form groups)
	 *					Classes list:
	 *						CLASS_NONE				0
	 *						CLASS_MACHINE			1
	 *						CLASS_PLAYER			2
	 * 						CLASS_HUMAN_PASSIVE		3
	 *						CLASS_HUMAN_MILITARY	4
	 *						CLASS_ALIEN_MILITARY	5
	 *						CLASS_ALIEN_PASSIVE		6
	 *						CLASS_ALIEN_MONSTER		7
	 *						CLASS_ALIEN_PREY		8
	 *						CLASS_ALIEN_PREDATOR	9
	 *						CLASS_INSECT			10
	 *						CLASS_PLAYER_ALLY		11
	 *						CLASS_PLAYER_BIOWEAPON	12
	 *						CLASS_ALIEN_BIOWEAPON	13
	 *						CLASS_XRACE_PITDRONE	14
	 *						CLASS_XRACE_SHOCK		15
	 *						CLASS_BARNACLE			99
	 * Forward params:	function(this, class)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_GetClassification, this, class)
	 */
	Ham_SC_GetClassification,

	/**
	 * Description:		Whether entity is a monter.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsMonster, this)
	 */
	Ham_SC_IsMonster,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 *                  Whether entity uses PhysX feature.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsPhysX, this)
	 */
	Ham_SC_IsPhysX,

	/**
	 * Description:		Whether this is a point entity.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsPointEntity, this)
	 */
	Ham_SC_IsPointEntity,

	/**
	 * Description:		Whether entity is a machine.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsMachine, this)
	 */
	Ham_SC_IsMachine,

	/**
	 * Description:		Removes the entity and all its content in critical situation.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CriticalRemove, this)
	 */
	Ham_SC_CriticalRemove,

	/**
	 * Description:		Updates global tables that need to know about entities being removed.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(UpdateOnRemove, this)
	 */
	Ham_SC_UpdateOnRemove,

	/**
	 * Description:		Returns true if a line can be traced from the caller's eyes to the target vector.
	 * Forward params:	function(this, entity, bool:ignoreGlass)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FVisible, this, entity, bool:ignoreGlass)
	 */
	Ham_SC_FVisible,

	/**
	 * Description:		Returns true if a line can be traced from the given point to the target point.
	 * Forward params:	function(this, const Float:target[3], const Float:start[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FVisibleFromPos, this, const Float:target[3], const Float:start[3])
	 */
	Ham_SC_FVisibleFromPos,

	/**
	 * Description:		Returns true if passed in entity is facing current entity.
	 *					Some examples of dotProduct value :
	 *					VIEW_FIELD_FULL -1.0
	 *					VIEW_FIELD_WIDE -0.7
	 *					VIEW_FIELD_NARROW 0.7
	 *					VIEW_FIELD_ULTRA_NARROW 0.9
	 * Forward params:	function(this, entTest, Float:dotProduct)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsFacing, this, entTest, Float:dotProduct)
	 */
	Ham_SC_IsFacing,

	/**
	 * Description:		Gets points without killing an entity.
	 * Forward params:	function(this, Float:damage)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_SC_GetPointsForDamage, this, Float:damage)
	 */
	Ham_SC_GetPointsForDamage,

	/**
	 * Description:		Gets points for making some damage.
	 * Forward params:	function(this, attacker, inflictor, Float:damage)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_GetDamagePoints, this, attacker, inflictor, Float:damage)
	 */
	Ham_SC_GetDamagePoints,

	/**
	 * Description:		Constructor.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_OnCreate, this)
	 */
	Ham_SC_OnCreate,

	/**
	 * Description:		Desctructor.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_OnDestroy, this)
	 */
	Ham_SC_OnDestroy,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 *                  Returns false if the entity is somehow invalid.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsValidEntity, this)
	 */
	Ham_SC_IsValidEntity,

	/**
	 * Description:		Checks if this monster should fade out.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_ShouldFadeOnDeath, this)
	 */
	Ham_SC_ShouldFadeOnDeath,

	/**
	 * Description:		Sets up a friendly monster.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_SetupFriendly, this)
	 */
	Ham_SC_SetupFriendly,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 *                  Tries to revive a monster.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_ReviveThink, this)
	 */
	Ham_SC_ReviveThink,

	/**
	 * Description:		Revives a monster.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Revive, this)
	 */
	Ham_SC_Revive,

	/**
	 * Description:		Final bit of initization before a monster is turned over to the AI.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_StartMonster, this)
	 */
	Ham_SC_StartMonster,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CheckRangeAttack1_Move, this, Float:dot, Float:dist)
	 */
	Ham_SC_CheckRangeAttack1_Move,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CheckRangeAttack2_Move, this, Float:dot, Float:dist)
	 */
	Ham_SC_CheckRangeAttack2_Move,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CheckMeleeAttack1_Move, this, Float:dot, Float:dist)
	 */
	Ham_SC_CheckMeleeAttack1_Move,

	/**
	 * Description:		Surveys conditions and set appropriate conditions bits for attack types.
	 * Forward params:	function(this, Float:dot, Float:dist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CheckMeleeAttack2_Move, this, Float:dot, Float:dist)
	 */
	Ham_SC_CheckMeleeAttack2_Move,

	/**
	 * Description:		Checks tank usage.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CheckTankUsage, this)
	 */
	Ham_SC_CheckTankUsage,

	/**
	 * Description:		Sets a monster's gait activity.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_SetGaitActivity, this)
	 */
	Ham_SC_SetGaitActivity,

	/**
	 * Description:		Tries to overcome local obstacles by triangulating a path around them.
	 * Forward params:	function(this, const Float:start[3], const Float:end[3], Float:dist, targetEnt, Float:apex[3], Float:apex2[3], bool:coverPath)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FTriangulate, this, const Float:start[3], const Float:end[3], Float:dist, targetEnt, Float:apex[3], Float:apex2[3], bool:coverPath )
	 */
	Ham_SC_FTriangulate,

	/**
	 * Description:		Tries to overcome local obstacles by triangulating a path around them.
	 * Forward params:	function(this, const Float:start[3], const Float:end[3], Float:dist, targetEnt, Float:apex[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FTriangulateExtension, this, const Float:start[3], const Float:end[3], Float:dist, targetEnt, Float:apex[3])
	 */
	Ham_SC_FTriangulateExtension,

	/**
	 * Description:		Tries to find a nearby node that will hide the caller from its enemy.
	 *					If supplied, search will return a node at least as far away as minDist, but no farther than maxDist.
	 *					If maxDist isn't supplied, it defaults to a reasonable value.
	 * Forward params:	function(this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FindCoverGrenade, this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist)
	 */
	Ham_SC_FindCoverGrenade,

	/**
	 * Description:		Tries to find a nearby node that will hide the caller from its enemy.
	 *					If supplied, search will return a node at least as far away as minDist, but no farther than maxDist.
	 *					If maxDist isn't supplied, it defaults to a reasonable value.
	 * Forward params:	function(this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FindCoverDistance, this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist)
	 */
	Ham_SC_FindCoverDistance,

	/**
	 * Description:		Tries to find a nearby node that will hide the caller from its enemy.
	 *					If supplied, search will return a node at least as far away as minDist, but no farther than maxDist.
	 *					If maxDist isn't supplied, it defaults to a reasonable value.
	 * Forward params:	function(this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FindAttackPoint, this, const Float:threat[3], const Float:viewOffset[3], Float:minDist, Float:maxDist)
	 */
	Ham_SC_FindAttackPoint,

	/**
	 * Description:		Determines whether or not the chosen cover location is a good one to move to.
	 *					Currently based on proximity to others in the squad.
	 * Forward params:	function(this, const Float:coverLocation[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FValidateCover, this, const Float:coverLocation[3])
	 */
	Ham_SC_FValidateCover,

	/**
	 * Description:		Checks for possibility of friendly fire.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_NoFriendlyFire, this)
	 */
	Ham_SC_NoFriendlyFire1,

	/**
	 * Description:		Checks for possibility of friendly fire.
	 * Forward params:	function(this, const Float:pos[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_NoFriendlyFire2, this, const Float:pos[3])
	 */
	Ham_SC_NoFriendlyFire2,

	/**
	 * Description:		Checks for possibility of friendly fire.
	 * Forward params:	function(this, const Float:pos[3], targetEnt)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_NoFriendlyFire3, this, const Float:pos[3], targetEnt)
	 */
	Ham_SC_NoFriendlyFire3,

	/**
	 * Description:		Checks for possibility of friendly fire from the calling monster's origin to toPos.
	 * Forward params:	function(this, const Float:toPos[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_NoFriendlyFireToPos, this, const Float:toPos[3])
	 */
	Ham_SC_NoFriendlyFireToPos,

	/**
	 * Description:		Same as FVisible but from gun position.
	 * Forward params:	function(this, entity, Float:pos[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FVisibleGunPos, this, entity, Float:pos[3])
	 */
	Ham_SC_FVisibleGunPos,

	/**
	 * Description:		Checks for monsters in this generic cone.
	 * Forward params:	function(this, entity, const Float:pos[3])
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FInBulletCone, this, entity, const Float:pos[3])
	 */
	Ham_SC_FInBulletCone,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_CallGibMonster, this)
	 */
	Ham_SC_CallGibMonster,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_CheckTimeBasedDamage, this)
	 */
	Ham_SC_CheckTimeBasedDamage,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsMoving, this)
	 */
	Ham_SC_IsMoving,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsPlayerFollowing, this)
	 */
	Ham_SC_IsPlayerFollowing,

	/**
	 * Description:		-
	 * Forward params:	function(this, idleader, bool:noSound)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_StartPlayerFollowing, this, idleader, bool:noSound)
	 */
	Ham_SC_StartPlayerFollowing,

	/**
	 * Description:		-
	 * Forward params:	function(this, bool:clearSchedule, bool:noSound)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_StopPlayerFollowing, this, bool:clearSchedule, bool:noSound)
	 */
	Ham_SC_StopPlayerFollowing,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_UseSound, this)
	 */
	Ham_SC_UseSound,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_UnUseSound, this)
	 */
	Ham_SC_UnUseSound,

	/**
	 * Description:		-
	 * Forward params:	function(this, idOther)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_RideMonster, this, idOther)
	 */
	Ham_SC_RideMonster,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_CheckAndApplyGenericAttacks, this)
	 */
	Ham_SC_CheckAndApplyGenericAttacks,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_CheckScared, this)
	 */
	Ham_SC_CheckScared,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_CheckCreatureDanger, this)
	 */
	Ham_SC_CheckCreatureDanger,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_CheckFallDamage, this)
	 */
	Ham_SC_CheckFallDamage,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_CheckRevival, this)
	 */
	Ham_SC_CheckRevival,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_MedicCallSound, this)
	 */
	Ham_SC_MedicCallSound,


	/**
	 * Description:		(!) This function is no more available in the mod.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_MenuInputPerformed, this)
	 */
	Ham_SC_Player_MenuInputPerformed,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_IsMenuInputDone, this)
	 */
	Ham_SC_Player_IsMenuInputDone,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_SpecialSpawn, this)
	 */
	Ham_SC_Player_SpecialSpawn,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Player_IsValidInfoEntity, this)
	 */
	Ham_SC_Player_IsValidInfoEntity,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_LevelEnd, this)
	 */
	Ham_SC_Player_LevelEnd,

	/**
	 * Description:		-
	 * Forward params:	function(this, voteType)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_VoteStarted, this, voteType)
	 */
	Ham_SC_Player_VoteStarted,

	/**
	 * Description:		-
	 * Forward params:	function(this, voteType)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Player_CanStartNextVote, this, voteType)
	 */
	Ham_SC_Player_CanStartNextVote,

	/**
	 * Description:		-
	 * Forward params:	function(this, voteInput)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_Vote, this, voteInput)
	 */
	Ham_SC_Player_Vote,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Player_HasVoted, this)
	 */
	Ham_SC_Player_HasVoted,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_ResetVote, this)
	 */
	Ham_SC_Player_ResetVote,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_Player_LastVoteInput, this)
	 */
	Ham_SC_Player_LastVoteInput,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_InitVote, this)
	 */
	Ham_SC_Player_InitVote,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_SC_Player_StartNextVote, this)
	 */
	Ham_SC_Player_TimeToStartNextVote,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_ResetView, this)
	 */
	Ham_SC_Player_ResetView,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_SC_Player_GetLogFrequency, this)
	 */
	Ham_SC_Player_GetLogFrequency,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Player_LogPlayerStats, this)
	 */
	Ham_SC_Player_LogPlayerStats,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 * Forward params:	function(this, idPlayer, Float:time)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_DisableCollision, this, idPlayer, Float:time)
	 */
	Ham_SC_Player_DisableCollisionWithPlayer,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 * Forward params:	function(this, idPlayer, bool:testIntersection)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_EnableCollision, this, idPlayer, bool:testIntersection)
	 */
	Ham_SC_Player_EnableCollisionWithPlayer,

	/**
	 * Description:		(!) This function is no more available in the mod.
	 * Forward params:	function(this, idPlayer)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Player_CanTouchPlayer, this, idPlayer)
	 */
	Ham_SC_Player_CanTouchPlayer,


	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Item_Materialize, this)
	 */
	Ham_SC_Item_Materialize,


	/**
	 * Description:		-
	 * Forward params:	function(this, const Float:moving[3], const Float:standing[3], const Float:crouched[3])
	 * Return type:		Vector, byreffed in execute.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_BulletAccuracy, this, const Float:moving[3], const Float:standing[3], const Float:crouched[3], Float:output[3])
	 */
	Ham_SC_Weapon_BulletAccuracy,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_TertiaryAttack, this)
	 */
	Ham_SC_Weapon_TertiaryAttack,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_BurstSupplement, this)
	 */
	Ham_SC_Weapon_BurstSupplement,

	/**
	 * Description:		-
	 * Forward params:	function(this, const alternative[] = "")
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_GetP_Model, this, const alternative[] = "", buffer[], size)
	 */
	Ham_SC_Weapon_GetP_Model,

	/**
	 * Description:		-
	 * Forward params:	function(this, const alternative[] = "")
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_GetW_Model, this, const alternative[] = "", buffer[], size)
	 */
	Ham_SC_Weapon_GetW_Model,

	/**
	 * Description:		-
	 * Forward params:	function(this, const alternative[] = "")
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_GetV_Model, this, const alternative[] = "", buffer[], size)
	 */
	Ham_SC_Weapon_GetV_Model,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_PrecacheCustomMdl, this)
	 */
	Ham_SC_Weapon_PrecacheCustomModels,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_IsMultiplayer, this)
	 */
	Ham_SC_Weapon_IsMultiplayer,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_FRunfuncs, this)
	 */
	Ham_SC_Weapon_FRunfuncs,

	/**
	 * Description:		-
	 * Forward params:	function(this, fov)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_SetFOV, this, fov)
	 */
	Ham_SC_Weapon_SetFOV,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_FCanRun, this)
	 */
	Ham_SC_Weapon_FCanRun,

	/**
	 * Description:		-
	 * Forward params:	function(this, Float:frametime)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_CustomDecrement, this, Float:frametime)
	 */
	Ham_SC_Weapon_CustomDecrement,

	/**
	 * Description:		-
	 * Forward params:	function(this, const model[])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_SetV_Model, this, const model[])
	 */
	Ham_SC_Weapon_SetV_Model,

	/**
	 * Description:		-
	 * Forward params:	function(this, const model[])
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_SetP_Model, this, const model[])
	 */
	Ham_SC_Weapon_SetP_Model,

	/**
	 * Description:		-
	 * Forward params:	function(this, skin)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_ChangeWeaponSkin, this, skin)
	 */
	Ham_SC_Weapon_ChangeWeaponSkin,


	/**
	 * LATE ADDITIONS (2013)
	 */

	/**
	 * Description:		Normally called whenever an entity dies.
	 * Forward params:	function(this, idinflictor, idattacker, shouldgib)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_Killed, this, idinflictor, idattacker, shouldgib);
	 */
	Ham_TFC_Killed,

	/**
	 * Description:		Returns whether an entity is activated.
	 * Forward params:	function(this, idActivator);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_IsTriggered, this, idActivator);
	 */
	Ham_TFC_IsTriggered,

	/**
	 * Description:		Sends an animation event for the weapon. skiplocal is 1 if client is predicting weapon animations.
	 * Forward params:	function(this, anim, skiplocal);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TFC_Weapon_SendWeaponAnim, this, anim, skiplocal);
	 */
	Ham_TFC_Weapon_SendWeaponAnim,

	/**
	 * Description:		Gets next attack delay.
	 * Forward params:	function(this, Float:delay)
	 * Return type:		Float (byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_TFC_Weapon_GetNextAttackDelay, this, Float:delay, Float:output);
	 */
	Ham_TFC_Weapon_GetNextAttackDelay,


	/**
	 * Description:		Usually called whenever an entity gets a form of a heal.
	 * Forward params:	function(this, Float:health, damagebits, health_cap);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_TakeHealth, this, Float:health, damagebits, health_cap);
	 */
	Ham_SC_TakeHealth,

	/**
	 * Description:		Usually called whenever an entity gets a form of armor.
	 * Forward params:	function(this, Float:armor, damagebits, armor_cap);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_TakeArmor, this, Float:armor, damagebits, armor_cap);
	 */
	Ham_SC_TakeArmor,

	/**
	 * Description:		Gives ammo to the entity.
	 * Forward params:	function(this, amount, const name[], max, const bool:fromPlayer)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_GiveAmmo, this, amount, "type", max, fromPlayer);
	 */
	Ham_SC_GiveAmmo,

	/**
	 * Description:		Determines if we should ignore damage.
	 * Forward params:	function(this, idattacker);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_CheckAttacker, this, idattacker);
	 */
	Ham_SC_CheckAttacker,

	/**
	 * Description:		Determines if a player is connected.
	 * Forward params:	function(this);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_Player_IsConnected, this);
	 */
	Ham_SC_Player_IsConnected,


	/**
	 * Description:		Sends an animation event for the weapon. skiplocal is 1 if client is predicting weapon animations.
	 * Forward params:	function(this, anim, skiplocal);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_DOD_Weapon_SendWeaponAnim, this, anim, skiplocal);
	 */
	Ham_DOD_Weapon_SendWeaponAnim,


	/**
	 * Description:		-
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CS_Item_IsWeapon, this);
	 */
	Ham_CS_Item_IsWeapon,


	/**
	 * Description:		Returns the id of the entity if its class is derived off of CBaseSquadTalkMonster, -1 otherwise.
	 * Forward params:	function(this)
	 * Return type:		Entity.
	 * Execute params:	ExecuteHam(Ham_OPF_MySquadTalkMonsterPointer, this);
	 */
	Ham_OPF_MySquadTalkMonsterPointer,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float (byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_OPF_WeaponTimeBase, this);
	 */
	Ham_OPF_WeaponTimeBase,


	/**
	 * Description:		Called when the alternate attack of a weapon is triggered.
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_TS_Weapon_AlternateAttack, this);
	 */
	Ham_TS_Weapon_AlternateAttack,


	/**
	 * Description:		Gets item infos.
	 * Forward params:	function(this, iteminfo_handle);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_Item_GetItemInfo, this, iteminfo_handle);
	 *					Use CreateHamItemInfo() to pass a new ItemInfo handle.
	 */
	Ham_Item_GetItemInfo,


	/**
	 * LATE ADDITIONS (2017)
	 */

	/**
	 * Description:		Performs checks that must occur before Spawn itself is called. Always call baseclass version first.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_PreSpawn, this);
	 */
	Ham_SC_PreSpawn,

	/**
	 * Description:		Performs checks that must occur after Spawn itself is called. Always call baseclass version first.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_PostSpawn, this);
	 */
	Ham_SC_PostSpawn,

	/**
	 * Description:		-
	 * Forward params:	function(this, key);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_PostSpawn, this, const key[]);
	 */
	Ham_SC_OnKeyValueUpdate,

	/**
	 * Description:		-
	 * Forward params:	function(this, classification);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_SetClassification, this, classification);
	 */
	Ham_SC_SetClassification,

	/**
	 * Description:		Returns whether an entity is activated.
	 *					This function is not supported by Day Of Defeat.
	 *					This function has different version for Team Fortress Classic, see Ham_TFC_IsTriggered instead.
	 * Forward params:	function(this, idActivator);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_IsTriggered, this, idActivator);
	 */
	Ham_SC_IsTriggered,

	/**
	 * Description:		Returns the id of the entity if its class is derived off of ICustomEntity, -1 otherwise.
	 * Forward params:	function(this)
	 * Return type:		Entity.
	 * Execute params:	ExecuteHam(Ham_SC_MyCustomPointer, this);
	 */
	Ham_SC_MyCustomPointer,

	/**
	 * Description:		Returns the id of the entity if its class is derived off of CBasePlayerItem, -1 otherwise.
	 * Forward params:	function(this)
	 * Return type:		Entity.
	 * Execute params:	ExecuteHam(Ham_SC_MyItemPointer, this);
	 */
	Ham_SC_MyItemPointer,

	/**
	 * Description:		Typically adds points to the entity.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_AddPoints, this, points, bool:cangonegative);
	 */
	Ham_SC_AddPoints,

	/**
	 * Description:		Typically adds points to everybody on the entity's team.
	 * Forward params:	function(this, points, bool:cangonegative);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_AddPointsToTeam, this, points, bool:cangonegative);
	 */
	Ham_SC_AddPointsToTeam,

	/**
	 * Description:		Removes an item to the player's inventory.
	 * Forward params:	function(this, idother);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_RemovePlayerItem, this, idother);
	 */
	Ham_SC_RemovePlayerItem,

	/**
	 * Description:		Not entirely sure.
	 * Forward params:	function(this, idOn)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_OnControls, this, idOn);
	 */
	Ham_SC_OnControls,

	/**
	 * Description:		Whether or not the entity is sneaking.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsSneaking, this);
	 */
	Ham_SC_IsSneaking,

	/**
	 * Description:		Whether or not the entity is alive.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsAlive, this);
	 */
	Ham_SC_IsAlive,

	/**
	 * Description:		Whether or not the entity uses a BSP model.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsBSPModel, this);
	 */
	Ham_SC_IsBSPModel,

	/**
	 * Description:		Whether or not the entity can reflect gauss shots.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_ReflectGauss, this);
	 */
	Ham_SC_ReflectGauss,

	/**
	 * Description:		Whether or not the target is the same as the one passed.
	 *					Note the strindex parameter is a string passed that has been allocated by the engine.
	 *					Use fakemeta's EngFunc_SzFromIndex to convert to a normal string, or fakemeta's
	 *					EngFunc_AllocString to create a new string.
	 * Forward params:	function(this, strindex).
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_HasTarget, this, strindex);
	 */
	Ham_SC_HasTarget,

	/**
	 * Description:		Whether or not the entity is in the world.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsInWorld, this);
	 */
	Ham_SC_IsInWorld,

	/**
	 * Description:		Whether or not the entity is a player.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Sc_IsPlayer, this);
	 */
	Ham_Sc_IsPlayer,

	/**
	 * Description:		Whether or not the entity is a net client.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsNetClient, this);
	 */
	Ham_SC_IsNetClient,

	/**
	 * Description:		Whether or not the entity is a brush entity breakable.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsBreakable, this);
	 */
	Ham_SC_IsBreakable,

	/**
	 * Description:		-
	 * Forward params:	function(this, idActivator, useType, value);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_SUB_UseTargets, this, idActivator, useType, value);
	 */
	Ham_SC_SUB_UseTargets,

	/**
	 * Description:		-
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsLockedByMaster, this);
	 */
	Ham_SC_IsLockedByMaster,

	/**
	 * Description:		Normally called whenever a barnacle grabs the entity.
	 * Forward params:	function(this, idOther);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FBecomeProne, this, idOther);
	 */
	Ham_SC_FBecomeProne,

	/**
	 * Description:		Returns true if a line can be traced from the caller's eyes to given vector.
	 * Forward params:	function(this, const Float:origin[3]);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_FVecVisible, this, const Float:origin[3]);
	 */
	Ham_SC_FVecVisible,

	/**
	 * Description:		Sets the player ally state
	 * Forward params:	function(this, bool:state);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_SetPlayerAlly, this, bool:state);
	 */
	Ham_SC_SetPlayerAlly,

	/**
	 * Description:		Callback after trigger_setorigin has moved the entity.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_OnSetOriginByMap, this);
	 */
	Ham_SC_OnSetOriginByMap,

	/**
	 * Description:		Return true if you want to be revivable.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_IsRevivable, this);
	 */
	Ham_SC_IsRevivable,

	/**
	 * Description:		-
	 *                  timeUntilRevive is the time until the actual revive event occurs.
	 * Forward params:	function(this, Float:timeUntilRevive);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_BeginRevive, Float:timeUntilRevive);
	 */
	Ham_SC_BeginRevive,

	/**
	 * Description:		-
	 *                  timeUntilRevive is the time before the monster is supposed to be revived
	 * Forward params:	function(this, Float:timeUntilRevive);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_EndRevive, Float:timeUntilRevive);
	 */
	Ham_SC_EndRevive,

	/**
	 * Description:		Determines whether or not the monster can play the scripted sequence or AI sequence that is
	 *					trying to possess it. If DisregardState is set, the monster will be sucked into the script
	 *					no matter what state it is in. ONLY Scripted AI ents should allow this.
	 * Forward params:	function(this, bool:disregardState, interruptLevel);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CanPlaySequence, this, bool:disregardState, interruptLevel);
	 */
	Ham_SC_CanPlaySequence,

	/**
	 * Description:		-
	 * Forward params:	function(this, bool:disregardState);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_CanPlaySentence2, this, bool:disregardState);
	 */
	Ham_SC_CanPlaySentence2,

	/**
	 * Description:		-
	 * Forward params:	function(this, const sentence[], Float:duration, Float:volume, Float:attenuation, bool:concurrent, idListener);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_PlayScriptedSentence, this, const sentence[], Float:duration, Float:volume, Float:attenuation, bool:concurrent, idListener);
	 */
	Ham_SC_PlayScriptedSentence,


	/**
	 * Items have all the attributes of normal entities in addition to these.
	 */

	/**
	 * Description:		Adds the item to the player.
	 * Forward params:	function(this, idPlayer);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_Item_AddToPlayer, this, idPlayer);
	 */
	Ham_SC_Item_AddToPlayer,

	/**
	 * Description:		Returns true if you want your duplicate removed from world.
	 * Forward params:	function(this, idOriginal);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_AddDuplicate, this, idOriginal);
	 */
	Ham_SC_Item_AddDuplicate,

	/**
	 * Description:		-
	 * Forward params:	function(this, idOther);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_AddAmmoFromItem, this, idOther);
	 */
	Ham_SC_Item_AddAmmoFromItem,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		String (string length returned and string byref'd in ExecuteHam).
	 * Execute params:	ExecuteHam(Ham_SC_Item_GetPickupSound, this)
	 */
	Ham_SC_Item_GetPickupSound,

	/**
	 * Description:		-
	 *					Collect Types:
	 *						COLLECT_TOUCH			0
	 *						COLLECT_USE_DIRECT		1
	 *						COLLECT_USE_INDIRECT	2
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_CanCollect, this, idOther, collectType);
	 */
	Ham_SC_Item_CanCollect,

	/**
	 * Description:		-
	 * Forward params:	function(this, idOther, collectType);
	 *					Collect Types:
	 *						COLLECT_TOUCH			0
	 *						COLLECT_USE_DIRECT		1
	 *						COLLECT_USE_INDIRECT	2
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Item_Collect, this, idOther, collectType);
	 */
	Ham_SC_Item_Collect,

	/**
	 * Description:		Gets item infos.
	 * Forward params:	function(this, iteminfo_handle);
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_Item_GetItemInfo, this, iteminfo_handle);
	 *					Use CreateHamItemInfo() to pass a new ItemInfo handle.
	 */
	Ham_SC_Item_GetItemInfo,

	/**
	 * Description:		Whether or not this entity can be deployed.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_CanDeploy, this);
	 */
	Ham_SC_Item_CanDeploy,

	/**
	 * Description:		Deploys the entity (usually a weapon).
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_Deploy, this);
	 */
	Ham_SC_Item_Deploy,

	/**
	 * Description:		Whether or not the entity can be holstered.
	 * Forward params:	function(this);
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_CanHolster, this);
	 */
	Ham_SC_Item_CanHolster,

	/**
	 * Description:		Called each frame by the player PreThink if inactive.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Item_InactiveItemPreFrame, this);
	 */
	Ham_SC_Item_InactiveItemPreFrame,

	/**
	 * Description:		Called each frame by the player PostThink if inactive.
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Item_InactiveItemPostFrame, this);
	 */
	Ham_SC_Item_InactiveItemPostFrame,

	/**
	 * Description:		-
	 * Forward params:	function(this);
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Item_DetachFromPlayer, this);
	 */
	Ham_SC_Item_DetachFromPlayer,

	/**
	 * Description:		Updates item data for the client.
	 * Forward params:	function(this, idPlayer)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_Item_UpdateClientData, this, idPlayer);
	 */
	Ham_SC_Item_UpdateClientData,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Float.
	 * Execute params:	ExecuteHam(Ham_SC_Item_GetRespawnTime, this);
	 */
	Ham_SC_Item_GetRespawnTime,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Item_CanHaveDuplicates, this);
	 */
	Ham_SC_Item_CanHaveDuplicates,


	/**
	 * Weapons have all the attributes to Ham_Item_*, in addition to these.
	 */

	/**
	 * Description:		-
	 * Forward params:	function(this, idOriginal)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_ExtractAmmoFromItem, idOriginal);
	 */
	Ham_SC_Weapon_ExtractAmmoFromItem,

	/**
	 * Description:		Unsure.
	 *					This function is not supported in Earth's Special Forces mod.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_AddWeapon, this);
	 */
	Ham_SC_Weapon_AddWeapon,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_GetAmmo1Drop);
	 */
	Ham_SC_Weapon_GetAmmo1Drop,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_GetAmmo2Drop, idOriginal);
	 */
	Ham_SC_Weapon_GetAmmo2Drop,

	/**
	 * Description:		Plays the weapon's empty sound.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_PlayEmptySound, this);
	 */
	Ham_SC_Weapon_PlayEmptySound,

	/**
	 * Description:		Whether or not the weapon is usable (has ammo, etc.)
	 *					This function is not supported in Earth's Special Forces mod.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_IsUsable, this)
	 */
	Ham_SC_Weapon_IsUsable,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_FinishReload, idOriginal);
	 */
	Ham_SC_Weapon_FinishReload,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_ShouldReload, idOriginal);
	 */
	Ham_SC_Weapon_ShouldReload,

	/**
	 * Description:		Whether or not the weapon should idle.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute Params:	ExecuteHam(Ham_SC_Weapon_ShouldWeaponIdle, this)
	 */
	Ham_SC_Weapon_ShouldWeaponIdle,

	/**
	 * Description:		Unsure.
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Weapon_UseDecrement, this);
	 */
	Ham_SC_Weapon_UseDecrement,


	/**
	 * Players have all the attributes of normal entities, in addition to these.
	 */

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_EnteredObserver, this);
	 */
	Ham_SC_Player_EnteredObserver,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		None.
	 * Execute params:	ExecuteHam(Ham_SC_Player_LeftObserver, this);
	 */
	Ham_SC_Player_LeftObserver,

	/**
	 * Description:		-
	 * Forward params:	function(this)
	 * Return type:		Integer (boolean).
	 * Execute params:	ExecuteHam(Ham_SC_Player_IsObserver, this);
	 */
	Ham_SC_Player_IsObserver,


	/**
	 * DONT USE ME LOL
	 */
	HAM_LAST_ENTRY_DONT_USE_ME_LOL
};

/**
 * Ham error types.
 */
enum HamError
{
	HAM_OK = 0,

	HAM_INVALID_FUNC,			// The function is not valid
	HAM_FUNC_NOT_CONFIGURED,	// This function is not configured in gamedata
	HAM_FUNC_NOT_AVAILABLE,		// This function is not more available in the mod

	HAM_ERR_END
};

/**
 * Constants for usage with [Get|Set]HamItemInfo
 */
enum HamItemInfo
{
	Ham_ItemInfo_iSlot,
	Ham_ItemInfo_iPosition,
	Ham_ItemInfo_pszAmmo1,
	Ham_ItemInfo_iMaxAmmo1,
	Ham_ItemInfo_pszAmmo2,
	Ham_ItemInfo_iMaxAmmo2,
	Ham_ItemInfo_pszName,
	Ham_ItemInfo_iMaxClip,
	Ham_ItemInfo_iId,
	Ham_ItemInfo_iFlags,
	Ham_ItemInfo_iWeight
};

