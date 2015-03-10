// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module Functions
//

#if defined NS_INC
	#endinput
#endif
#define NS_INC

#pragma reqlib ns
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib ns
#endif

#include <ns_const>


/**
 * Called whenever the client's class is changed.
 *
 * @param	id			The index of the player who changed.
 * @param 	newclass	The class the client changed to.  Check the class enum in ns_const.inc.
 * @param 	oldclass	The class the client changed from.  Check the class enum in ns_const.inc.
 * @noreturn
 */
forward client_changeclass(id, newclass, oldclass);

/**
 * Called whenever the client builds a structure.
 *
 * @param idPlayer		The player index who triggered the building.
 * @param idStructure	The structure index that was created.
 * @param type			The type of structure that was built (1 for marine, 2 for alien).
 * @param impulse		The impulse command that was issued to build this structure.
 * @noreturn
 */
forward client_built(idPlayer, idStructure, type, impulse);

/**
 * Tell whether or not the map is combat.
 *
 * @return		1 if combat, 0 otherwise.
 */
native ns_is_combat();

/**
 * Returns the gameplay type for the currently active map.
 * Refer to ns_const.inc's NSGameplay enum for details.
 *
 * @note		The earliest this is guaranteed to be accurate is during plugin_init().  It needs
 *				the info_gameplay entity to be properly set within the map, or it will return "Unknown",
 *				or "Cantfind".
 *
 * @return		Return the gameplay mode, as accurate as the module can tell.
 */
native NSGameplay:ns_get_gameplay();

/**
 * Exact syntax as get_user_team, but should be more accurate.
 *
 * @param id	Player id.
 * @param buff	Buffer to store team name in.
 * @param len	Buffer length.
 * @return		The pev_team setting for the player.
 */
native ns_get_user_team(id, buff[], len);

/**
 * Send an NS-style popup message.
 * 
 * @param target		The client to receive the message.  Set to 0 to send to everybody.
 * @param szMsg		The message to send, 180 characters max.
 * @param ah			Whether to only display the message on clients who have the cvar "cl_autohelp" set to 1.
 * @noreturn
 */
native ns_popup(target, const szMsg[180], ah=0);

/**
 * Sets a player model.  Omit the second parameter to return to default
 *
 * @note				The model does not revert on death, teamswitch, gestation, etc.
 *
 * @param id			The player id to change.
 * @param szModel		The model to change to.
 * @noreturn
 */
native ns_set_player_model(id, const szModel[]="");

/**
 * Sets a player skin.  Omit the second parameter to return to default
 *
 * @note				The skin does not revert on death, teamswitch, gestation, etc.
 *
 * @param id			The player id to change.
 * @param skin			The skin number to change to.
 * @noreturn
 */
native ns_set_player_skin(id, skin=-1);

/**
 * Sets a player body.  Omit the second parameter to return to default
 *
 * @note				The body does not revert on death, teamswitch, gestation, etc.
 *
 * @param id			The player id to change.
 * @param body			The body number to change to.
 * @noreturn
 */
native ns_set_player_body(id, body=-1);

/**
 * Set this to modify the player's speed by a certain amount.
 *
 * @note				The speed does not revert on death, teamswitch, gestation, etc.
 *
 * @param id			The player id to change.
 * @param speedchange	The speed to modify the player speed by.  Set to 0 to revert to default speed.
 * @noreturn
 */
native ns_set_speedchange(id, speedchange=0);

/**
 * Returns a client's current speed modifier.
 *
 * @param	id			The client id to check.
 * @return				The module's current speed modifier for the client.
 */
native ns_get_speedchange(id);

/**
 * Returns a client's maxspeed before the speed change modifier is factored in.
 *
 * @param	id			The client id to check.
 * @return				The maxspeed for the client.
 */
native ns_get_maxspeed(id);

/* Returns whether or not this mask is set from the entity's iuser4 field.  Use the "mask" enum for reference. */
native ns_get_mask(id,mask);

/* Sets or removes the mask from the entity's iuser4 field. Set "value" to 1 to turn the mask on, 0 to turn it off. */
native ns_set_mask(id,mask,value);

/* Returns built/unbuilt structures.
	If:
		builtOnly is 1 (default):
			Only fully built structures are counted.
		builtOnly is 0:
			Any structure meeting the classname is counted.
		
		Number is 0 (default):
			The total number of matching structures is returned.
		Number is any other value:
			The index of the #th matching structure is returned.
*/
native ns_get_build(const classname[],builtOnly=1,Number=0);

/* Returns if the player has the weapon or not in their pev->weapons field.
   set "setweapon" to 0 to turn the bit off, set to 1 to turn it on. Or omit it to just return the value. */
native ns_has_weapon(id,weapon,setweapon=-1);

/* Gets spawn point for specified team (type). 
	If:
		Team is equal to 0:
			Ready room spawns are returned.
		Team is greater than 0:
			Spawns for the team are returned.
			
		Number is equal to 0:
			Total number of spawns is returned.
		Number is greater than 0:
			The location of the specified spawn is returned.
*/
native ns_get_spawn(team,number=0,Float:ret[3]);

/* Returns the class of the player.  Look in the classes enum in ns_const.inc for the value's meaning. */
native ns_get_class(id);

/**
 * Gets the player's jetpack fuel reserve.
 *
 * @param id		The player to get fuel from.
 * @return			The amount of fuel in the player's reserve. (0.0 through 100.0)
 */
native Float:ns_get_jpfuel(id);

/**
 * Sets the player's jetpack fuel reserve.
 *
 * @param id		The player to set fuel.
 * @param fuel		The amount of fuel to set, as a percentage (0.0 through 100.0)
 * @noreturn
 */
native ns_set_jpfuel(id, Float:fuel);

/**
 * Adds to the player's jetpack fuel reserve.
 *
 * @param id		The player to add fuel to.
 * @param amount	The amount of fuel to add, as a percentage (0.0 through 100.0)
 * @return			The new amount of fuel in the player's reserve. (0.0 through 100.0)
 */
native Float:ns_add_jpfuel(id, Float:amount);

/**
 * Gets the player's energy percentage.
 *
 * @param id		The player to get the energy from.
 * @return			The amount of energy the player has (0.0 through 100.0)
 */
native Float:ns_get_energy(id);

/**
 * Sets the player's energy percentage.
 *
 * @param id		The player to set the energy on.
 * @param energy	The amount of energy to set (0.0 through 100.0)
 * @noreturn
 */
native ns_set_energy(id, Float:energy);

/**
 * Adds to the player's energy percentage.
 *
 * @param id		The player to add the energy to.
 * @param amount	The amount of energy to add to the player.
 * @return			The new amount of energy the player has (0.0 through 100.0)
 */
native Float:ns_add_energy(id, Float:amount);


/**
 * Returns a player's resources.
 *
 * @note			This is only for alien players.
 * @param id		The id of the player to check.
 * @return			Amount of resources this player has.
 */
native Float:ns_get_res(id);

/**
 * Sets a player's resources.
 *
 * @note			This is only for alien players.
 * @param id		The id of the player to set.
 * @param res		Amount of resources to set on this player.
 * @noreturn
 */
native ns_set_res(id, Float:res);

/**
 * Adds an amount of resources to the player.
 *
 * @note			This is only for alien players.
 * @param id		The id of the player to add resources to.
 * @param amount	The amount to add to the player.
 * @return			The new amount of resources the player has.
 */
native Float:ns_add_res(id, Float:amount);

/**
 * Returns the team's resources.
 * 
 * @param Team		1 for teama, 2 for teamb. (eg: in MvA maps, 1 is marines,
					2 is aliens.  In mvm, 1 is marine1, 2 is marine2)
 * @return			The amount of resources in this team's resource pool.
 */
native Float:ns_get_teamres(Team);

/**
 * Sets the team's resources in the resource pool.
 * 
 * @note			If this is used on an alien team, the resources will be
 *					distributed between all of the players who need resources.
 * @param Team		1 for teama, 2 for teamb. (eg: in MvA maps, 1 is marines,
 *					2 is aliens.  In mvm, 1 is marine1, 2 is marine2)
 * @param value	The amount to set the resources to set to.
 * @noreturn
 */
native ns_set_teamres(Team, Float:value);

/**
 * Adds to the team's resources in the resource pool.
 * 
 * @note			If this is used on an alien team, the resources will be
 *					distributed between all of the players who need resources.
 * @param Team		1 for teama, 2 for teamb. (eg: in MvA maps, 1 is marines,
 *					2 is aliens.  In mvm, 1 is marine1, 2 is marine2)
 * @param value	The amount to set the resources to add to the pool
 * @return			The new amount of resources in the resource pool.
 */
native Float:ns_add_teamres(Team,Float:value);


/**
 * Returns the player's experience.
 *
 * @note			Combat only.
 * @param id		The player to get experience value from.
 * @return			The amount of experience this player has.
 */
native Float:ns_get_exp(id);

/**
 * Sets the player's experience.
 *
 * @note			Combat only.
 * @param id		The player to set experience value on.
 * @param exp		The amount of experience this player will have.
 * @noreturn
 */
native ns_set_exp(id,Float:exp);

/**
 * Adds to the player's experience.
 *
 * @note			Combat only.
 * @param id		The player to add experience value to.
 * @param value	The amount of experience this player will receive.
 * @return			The new amount of experience this player has.
 */
native Float:ns_add_exp(id, Float:value);

/**
 * Gets the player's points spent count in combat.
 *
 * @param id		The player to check.
 * @return			The amount of points this player has spent.
 */
native ns_get_points(id);

/**
 * Sets the player's points spent count in combat.
 *
 * @param id		The player to set this on.
 * @param points	The amount to set this to.
 * @noreturn
 */
native ns_set_points(id, points);

/**
 * Adds to the player's points spent count in combat.
 *
 * @param id		The player to add this to.
 * @param value	The value to add to the points spent.
 * @return			The new value of the points spent variable.
 */
native ns_add_points(id,points);

/**
 * Gets the damage for this weapon.
 *
 * @note			Use weapon index, not player index!
 * @param idWeapon	The entity index of the weapon to check.
 * @return			The damage this weapon does.
 */
native Float:ns_get_weap_dmg(idWeapon);

/**
 * Sets the damage for this weapon.
 *
 * @note			Use weapon index, not player index!
 * @param idWeapon	The entity index of the weapon to set.
 * @param damage	The damage to make this weapon cause.
 * @noreturn
 */
native ns_set_weap_dmg(idWeapon, Float:damage);

/**
 * Gets the maximum range for this weapon.
 *
 * @note			Use weapon index, not player index!
 * @param idWeapon	The entity index of the weapon to check.
 * @return			The maximum range this weapon has.
 */
native Float:ns_get_weap_range(idWeapon);

/**
 * Sets the maximum range for this weapon.
 *
 * @note			Use weapon index, not player index!
 * @param idWeapon	The entity index of the weapon to set.
 * @param range	The maximum range this weapon will have.
 * @noreturn
 */
native ns_set_weap_range(idWeapon, Float:range);

/**
 * Gets the weapon's clip ammo.
 *
 * @note			Use weapon index, not player index!
 * @param idWeapon	The weapon to get the clip ammo from.
 * @return			The amount of ammunition in the weapon's clip.
 */
native ns_get_weap_clip(idWeapon);

/**
 * Sets the weapon's ammo in the clip.
 *
 * @note			Use weapon index, not player index!
 * @param idWeapon	The weapon to set the clip ammo on.
 * @param clipsize	The amount of ammunition to set in the weapon's clip.
 * @noreturn
 */
native ns_set_weap_clip(idWeapon, clipsize);

/**
 * Gets the player's weapon reserve (backpack ammo) for the specified
 * type of weapon.
 *
 * @note			Use player index, not weapon index!
 * @param id		The player id to check ammo count on.
 * @param weapon	The weapon type to check ammo count for.
 * @return			The ammunition count in the player's reserve.
 */
native ns_get_weap_reserve(id,weapon);

/**
 * Sets the player's weapon reserve (backpack ammo) for the specified
 * type of weapon.
 *
 * @note			Use player index, not weapon index!
 * @param id		The player id to set ammo count on.
 * @param weapon	The weapon type to set ammo count for.
 * @param ammo		The ammunition count to set.
 * @noreturn
 */
native ns_set_weap_reserve(id,weapon,ammo);

/**
 * Gets the player's score.
 *
 * @note			The score from level is automatically factored into the scoreboard in combat.
 * @param idPlayer	The player to get the score for.
 * @return			The player's score.
 */
native ns_get_score(idPlayer);

/**
 * Sets the player's score.
 *
 * @note			The score from level is automatically factored into the scoreboard in combat.
 * @param idPlayer	The player to get the score for.
 * @param score	What to set the player's score as.
 * @noreturn
 */
native ns_set_score(idPlayer, score);

/* Adds to a player's score
 * Returns the new score on success
 */
native ns_add_score(idPlayer,score);

/* Gets a player's death count. */
native ns_get_deaths(idPlayer);

/* Sets a player's death count. */
native ns_set_deaths(idPlayer,numdeaths);

/* Adds to a player's death count
 * Returns the new death count on success
 */
native ns_add_deaths(idPlayer,numdeaths);

/* Gets the index of the owner of a structure. -1 for no owner. */
native ns_get_struct_owner(idStructsure);

/* Sets the index of the owner of a structure. -1 for no owner. */
native ns_set_struct_owner(idStructure,indexOwner);

/* Gets the trait type tied to the hive.  Look at the hivetrait enum for the values. */
native ns_get_hive_trait(idHive);

/* Sets the trait type tied to the hive.  Look at the hivetrait enum for the values. */
native ns_set_hive_trait(idHive,trait);

/* Sets the players field of view, set "_fov" to 0.0 (or omit it) to return to normal. FOV change will persist until disconnect unless reset by a plugin */
native ns_set_fov(idPlayer,Float:_fov=0.0);

/**
 * Give the player an item.
 *
 * @param id		The player to give the item to.
 * @param class	The map-classname of the entity to give to the player.
 * @noreturn
 */
native ns_give_item(id, const class[]);

/**
 * Returns 1 if a player has the hive ability number.
 * If ability is 0, it will return the number of active hives.
 *
 * @param idPlayer	The player index to look up.
 * @param ability	The ability number to check, set to 0 to get number of active hives.
 * @return			If ability is != 0, returns 1 or 0 depending on if the client has the ability.
 *					If ability is 0, returns the number of active hives.
 */
native ns_get_hive_ability(idPlayer, ability=0);

/**
 * Triggered whenever a client's pev->team changes.
 *
 * @param id		The id of the client.
 * @param newteam	The team number of the new team.
 * @param oldteam	The team number of the old team.
 * @noreturn
 */
forward client_changeteam(id, newteam, oldteam);

/**
 * Triggered whenever a client's pev->deadflag changes from >0 to 0.
 *
 * @param id		The id of the client.
 * @noreturn
 */
forward client_spawn(id);

/**
 * Calls NS's private damage routine on the victim entity.
 *
 * @deprecated
 * @note		This is provided for backwards compatibility with peachy's module.
 *				It is suggested to use hamsandwich for this action instead.
 *
 * @param IDVictim		The victim that is taking the damage.
 * @param IDInflictor	The entity that is causing the damage (weapon, etc).
 * @param IDAttacker	The attacker who is triggering the damage (person shooting).
 * @param Damage		The amount of damage being done.
 * @param DamageType	The damage type being done (bitmask).
 */
#pragma deprecated It is suggested to use hamsandwich for this action instead.
native ns_takedamage(IDVictim, IDInflictor, IDAttacker, Float:Damage, DamageType);

/**
 * Attempts to unstick a player.
 *
 * @param id				Player to unstick.
 * @param StartDistance	Distance to start from the player to check for a new location.
 * @param MaxAttempts		How many attempts to try to find a new spot before giving up.
 * @return					1 on success, 0 on cannot find a place to move player to,
 *							-1 on invalid state (stunned/webbed), -2 on invalid class (comm/egg)
 *							-3 if the player is dead or a spectator, -4 on invalid player,
 *							-5 if the player is not connected.
 */
native ns_unstick_player(id, StartDistance=32, MaxAttempts=128);

/**
 * Whether or not there is a game in progress.
 *
 * @return		true if a game is in progress, false otherwise.
 */
native bool:ns_round_in_progress();

/**
 * Called at the approximate time that a round is started.
 *
 * @noreturn
 */
forward round_start();

/**
 * Called immediately when a round ends
 *
 * @param roundtime		The length of the round in seconds.
 * @noreturn
 */
forward round_end(Float:roundtime);

forward map_reset(isload);

native ns_get_weapon(idPlayer,weaponid,&weapontype=0);

/* Returns the location name of the provided x/y position
 * (z origin is ignored; can't have location over location)
 * -
 * Note that as of NS 3.2 beta 2, on the following maps
 * the returned string should be passed through ns_lookup_title
 * to be human readable:
 *   ns_bast, ns_hera, ns_nothing, ns_tanith,
 *   ns_nancy, ns_caged, ns_eclipse, ns_veil
 *
 * Passing the 5th parameter as non zero will auto look up
 * the title if it exists.
 */
native ns_get_locationname(Float:x, Float:y, name[], len, titlelookup=0);

/* Looks up a key from titles.txt
 * Returns -1 if the key is not found
 * Otherwise it returns the length of the output
 */
native ns_lookup_title(const KeyName[], Output[], length);

/* Forces the structure to fully build
 * Removes the ghost state from marine structures.
 * Do not use this on hives! It wont work.
 */
native ns_build_structure(idStructure);

/* Forces the structure to begin recycling 
 * Passing an index other than a marine structure will
 * have undefined results!
 * -
 * Note: This calls a private NS function!
 *       Be careful when using this!
 */
native ns_recycle(idStructure);

/* Forces the weldable to trigger
 * Passing an index other than a weldable
 * will have undefined results!
 * -
 * NS renames func_weldable to avhweldable
 * at map load.
 * -
 * Note: This calls a private NS function!
 *       Be careful when using this!
 */
native ns_finish_weldable(idWeldable);

/* Gets the total time needed to weld this
 * func_weldable shut.
 * Note: NS renames "func_weldable"s to "avhweldable"s
 * at run time!
 */
native Float:ns_get_weld_time(idWeldable);

/* Sets the total time needed to weld this
 * func_weldable shut.
 */
native ns_set_weld_time(idWeldable,Float:value);

/* Adds to the weldable's time required to open. 
 * Returns the new required time on success.
 * Note this native clamps the low value to 0.
 */
native Float:ns_add_weld_time(idWeldable,Float:value);

/* Gets the total time this func_weldable
 * has been welded.
 */
native Float:ns_get_weld_done(idWeldable);

/* Sets the total time this func_weldable
 * has been welded.
 */
native ns_set_weld_done(idWeldable,Float:value);

/* Adds to the total time this func_weldable
 * has been welded.  Returns the new value.
 * Note this native clamps the low value to 0.0
 */
native Float:ns_add_weld_done(idWeldable,Float:value);

/* Gets/sets/adds to the energy pool of this observatory. */
native Float:ns_get_obs_energy(idObs);
native ns_set_obs_energy(idObs,Float:value);
native Float:ns_add_obs_energy(idObs,Float:value);

/**
 * Removes an upgrade from the player's bought and active upgrade lists.
 * This will not refund the points spent on the upgrade, nor will it
 * immediately strip the upgrade if the player is alive.  Rather, it will
 * make it so the player no longer receives the upgrade on spawn.
 *
 * @note This only works in combat.
 * @params idPlayer		The player index to change upgrades for.
 * @params ugprade		The impulse number for the upgrade to strip.
 * @return 				2 for upgrade removed from player's bought and active list.
 *						1 for upgrade removed from player's bought list only.
 *						3 for upgrade removed from player's active list only (shouldn't happen, just incase.)
 *						0 for the player didn't have the upgrade in either list.
 */
native ns_remove_upgrade(idPlayer, upgrade);

/**
 * Particle system natives
 * -
 * The particle system emulates a map-based custom particle system.
 * Familiarity with the keyvalues from the map-based particle systems
 * is recommended! You will be lost otherwise!
 * -
 * prsearle's NSPEdit is also recommended for designing the systems:
 * http://homepage.ntlworld.com/pr.searle/NSPSEdit/NSPSEdit.html
 */

/* Creates a handle to the a particle system to configure
 * -
 * Note! this is not a particle system you can pass to 
 * ns_fire_ps()!
 */
native RawPS:ns_create_ps();

/* Sets the name of the particle system.
 * -
 * This is used for things like ns_get_ps_id()
 * and through calling another particle system
 * through the "ps_to_gen" field
 */
native ns_set_ps_name(RawPS:system, const name[]);

/* Sets the sprite to use for the particle system
 * -
 * You do NOT have to precache the sprite, BUT
 * the sprite must obviously be on the client to 
 * display.
 */
native ns_set_ps_sprite(RawPS:system, const sprite[]);

/* Finalizes the particle system.  Do not configure it after this.
 * A usable particle system handle is returned.
 */
native Particle:ns_spawn_ps(RawPS:system);

/* Draws a particle system at the given origin (and angles)
 * Flags are the FEV_* defines from hlsdk_const.inc
 * Only use handles returned by ns_spawn_ps or ns_get_ps_id here!
 */
native ns_fire_ps(Particle:system,const Float:origin[3],const Float:angles[3]={0.0,0.0,0.0}, flags=0);

/* Looks up a particle system by name
 * Returns a usable particle system handle.
 */
native Particle:ns_get_ps_id(const Name[]);

/* The following are the parameters for configuring the 
 * particle system.  Look through the fgd and NSPSEdit
 * for details!
 */
native ns_set_ps_genrate(RawPS:system, genrate);
native ns_set_ps_genshape(RawPS:system, NSPS_GenShape:genshape);
native ns_set_ps_genshape_params(RawPS:system, const params[]);
native ns_set_ps_spriteframes(RawPS:system, spriteframes);
native ns_set_ps_numparticles(RawPS:system, numparticles);
native ns_set_ps_size(RawPS:system, Float:size);
native ns_set_ps_vel_params(RawPS:system, const params[]);
native ns_set_ps_vel_shape(RawPS:system, NSPS_VelShape:shape);
native ns_set_ps_sys_life(RawPS:system, Float:lifetime);
native ns_set_ps_particle_life(RawPS:system, Float:lifetime);
native ns_set_ps_rendermode(RawPS:system, NSPS_RenderMode:rendermode);
native ns_set_ps_to_gen(RawPS:system, const name[]);
native ns_set_ps_anim_speed(RawPS:system, speed);
native ns_set_ps_spawn_flags(RawPS:system, NSPS_Flags:flags);
native ns_set_ps_base_color(RawPS:system, const colors[]);
native ns_set_ps_scale(RawPS:system, Float:scale);
native ns_set_ps_max_alpha(RawPS:system, Float:maxalpha);
