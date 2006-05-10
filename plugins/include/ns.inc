/* NS module functions
 * -
 * (c) 2004, Steve Dudenhoeffer
 * This file is provided as is (no warranties).
 */

#if defined NS_INC
	#endinput
#endif
#define NS_INC	

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib ns
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib ns
 #endif
#else
 #pragma library ns
#endif


#include <ns_const>


/* Called whenever the client's class is changed.  The classes given match get_class() output */
forward client_changeclass(id,newclass,oldclass);

/* Called whenever the client build's a structure.  If type is 1, it's a marine structure, if type is 2, it's alien. */
forward client_built(idPlayer,idStructure,type,impulse);

/* Returns if the map's combat or not. */
native ns_is_combat();

/* Sends a popup to a player.  Set target to 0 to send to everybody. Message length is 188 characters.  The last parameter, if set to 1, will only display when the player has cl_autohelp set to 1. */
native ns_popup(target,szMsg[180],ah=0)

/* Sets a player model.  Omit the second parameter to return to default.
   Note: This does *not* change back on death, team switch, gestations, etc. */
native ns_set_player_model(id,szModel[]="");

/* Sets a player's skin.  Omit second parameter to return to default.
   Note: This does *not* change back on death, team switch, gestations, etc. */
native ns_set_player_skin(id,skin=-1);

/* Sets a player's body.  Omit second parameter to return to default.
   Note: This does *not* change back on death, team switch, gestations, etc. */
native ns_set_player_body(id,body=-1);

/* Sets a player's speedchange. Omit the second parameter to return to default */
native ns_set_speedchange(id,speedchange=0);

/* Returns a player's current speedchange. */
native ns_get_speedchange(id);

/* Returns a player's max speed before the speed change is factored in. */
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
native ns_get_build(classname[],builtOnly=1,Number=0);

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

/* Returns a player's percentage of JP fuel */
native Float:ns_get_jpfuel(id);

/* Sets a player's percentage of JP fuel */
native ns_set_jpfuel(id,Float:fuel);

/* Returns a player's percentage of adrenaline (alien) */
native Float:ns_get_energy(id);

/* Sets a player's percentage of adrenaline */
native ns_set_energy(id,Float:energy);

/* Returns a player's resources. (alien only) */
native Float:ns_get_res(id);

/* Sets a player's resources. (alien only) */
native ns_set_res(id,Float:res);

/* Returns a player's experience (combat only) */
native Float:ns_get_exp(id);

/* Sets a player's experience (combat only) */
native ns_set_exp(id,Float:exp);

/* Gets a player's point spent value in combat */
native ns_get_points(id);

/* Sets a player's point spent value in combat */
native ns_set_points(id,points);

/* Gets a weapon's damage (use the weapon index, not the player) */
native Float:ns_get_weap_dmg(idWeapon);

/* Sets a weapon's damage (use the weapon index, not the player) */
native ns_set_weap_dmg(idWeapon,Float:damage);

/* Gets a weapon's range (use the weapon index, not the player) */
native Float:ns_get_weap_range(idWeapon);

/* Sets a weapon's range (use the weapon index, not the player) */
native ns_set_weap_range(idWeapon,Float:range);

/* Gets a weapon's clip ammo (use the weapon index, not the player) */
native ns_get_weap_clip(idWeapon);

/* Sets a weapon's clip ammo (use the weapon index, not the player) */
native ns_set_weap_clip(idWeapon,clipsize);

/* Gets the player's weapon reserve for the specified type of weapon.
   Look in ns_const.inc for the weapon enum. */
native ns_get_weap_reserve(id,weapon);

/* Sets the player's weapon reserve for the specified type of weapon.
   Look in ns_const.inc for the weapon enum. */
native ns_set_weap_reserve(id,weapon,ammo);

/* Gets a player's score.  Note: score from level is automatically factored into the scoreboard display in combat. */
native ns_get_score(idPlayer);

/* Sets a player's score.  Note: score from level is automatically factored into the scoreboard display in combat. */
native ns_set_score(idPlayer,score);

/* Gets a player's death count. */
native ns_get_deaths(idPlayer);

/* Sets a player's death count. */
native ns_set_deaths(idPlayer,numdeaths);

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

/* Give a player an item. */
native ns_give_item(id,const _szClassName[]);

/* Returns if a player has the hive ability number, if ability is 0 it will return the number of active hives */
native ns_get_hive_ability(idPlayer, ability=0);

/* A player's team entvar has changed */
forward client_changeteam(id,newteam,oldteam);

/* A player's deadflag entvar has changed from >0 to 0 */
forward client_spawn(id);