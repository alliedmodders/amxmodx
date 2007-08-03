/* Counter-Strike functions
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#if defined _cstrike_included
  #endinput
#endif
#define _cstrike_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib cstrike
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib cstrike
 #endif
#else
 #pragma library cstrike
#endif

/* Returns player deaths.
 */
native cs_get_user_deaths(index);

/* Sets player deaths.
 */
native cs_set_user_deaths(index, newdeaths);

/* Returns index of entity (does not have to be a player) which hostage is following. 0 is hostage doesn't follow anything.
 * Note: this native does not work on Condition Zero, which has a different hostage AI than CS.
 */
native cs_get_hostage_foll(index);

/* Set hostage to follow entity specified in followedindex. Does not have to be a player. If followedindex is 0 the hostage will stop following.
 * Note: this native does not work on Condition Zero, which has a different hostage AI than CS.
 */
native cs_set_hostage_foll(index, followedindex = 0);

/* Get unique hostage id.
 */
native cs_get_hostage_id(index);

/* Get amount of ammo in backpack on a user for a specific weapon.
 * Look in amxconst.inc for weapon types: CSW_*.
 * Weapons on the same line uses the same ammo type:
 * awm
 * scout, ak, g3
 * para
 * famas, m4a1, aug, sg550, galil, sg552
 * m3, xm
 * usp, ump, mac
 * fiveseven, p90
 * deagle
 * p228
 * glock, mp5, tmp, elites
 * flash
 * he
 * smoke
 */
native cs_get_user_bpammo(index, weapon);

/* Restock/remove ammo in a user's backpack.
 */
native cs_set_user_bpammo(index, weapon, amount);

/* Returns 1 if user has a defuse kit.
 */
native cs_get_user_defuse(index);

/* If defusekit is 1, the user will have a defuse kit.
 * You can specify a different colour for the defuse kit icon showing on hud. Default is the normal green.
 * You can specify an icon. Default is "defuser". Set flash to 1 if you want the icon to flash red.
 */
native cs_set_user_defuse(index, defusekit = 1, r = 0, g = 160, b = 0, icon[] = "defuser", flash = 0);

/* Is user in buyzone? Returns 1 when true, 0 when false.
 */
native cs_get_user_buyzone(index);

/* Returns 1 when user has a primary weapon OR a shield in inventory, else 0.
 */
native cs_get_user_hasprim(index);

/* Get user model.
 */
native cs_get_user_model(index, model[], len);

/* Set user model.
 */
native cs_set_user_model(index, const model[]);

/* Use to reset model to standard selected model.
 */
native cs_reset_user_model(index);

/* Returns users money.
 */
native cs_get_user_money(index);

/* Gives money to user. If flash is 1, the difference between new and old amount will flash red or green.
 */
native cs_set_user_money(index, money, flash = 1);

/* Does user have night vision goggles?
 */
native cs_get_user_nvg(index);

/* Set nvgoggles to 1 to give night vision goggles to index. Set it to 0 to remove them.
 */
native cs_set_user_nvg(index, nvgoggles = 1);

/* Returns 1 if user has the "skill" to plant bomb, else 0. Normally this would only be true for a terrorist carrying a bomb.
 */
native cs_get_user_plant(index);

/* If plant is 1, a user will be set to be able to plant bomb within the usual bomb target areas if having one.
 * You should use this if you give a player a weapon_c4, or he won't be able to plant it
 * without dropping it and picking it up again (only possible for terrorists).
 * If showbombicon is 1, the green C4 icon will be shown on user hud (if plant "skill" was enabled).
 */
native cs_set_user_plant(index, plant = 1, showbombicon = 1);

/* Set user team without killing player.
 * If model is anything other than CS_DONTCHANGE, that will be set as player's model.
 */
enum CsInternalModel {
	CS_DONTCHANGE = 0,
	CS_CT_URBAN = 1,
	CS_T_TERROR = 2,
	CS_T_LEET = 3,
	CS_T_ARCTIC = 4,
	CS_CT_GSG9 = 5,
	CS_CT_GIGN = 6,
	CS_CT_SAS = 7,
	CS_T_GUERILLA = 8,
	CS_CT_VIP = 9,
	CZ_T_MILITIA = 10,
	CZ_CT_SPETSNAZ = 11
};
native cs_set_user_team(index, {CsTeams,_}:team, {CsInternalModel,_}:model = CS_DONTCHANGE);

/* Get team directly from player's entity.
 * 1 = terrorist
 * 2 = counter-terrorist
 * 3 = spectator
 */
enum CsTeams {
	CS_TEAM_UNASSIGNED = 0,
	CS_TEAM_T = 1,
	CS_TEAM_CT = 2,
	CS_TEAM_SPECTATOR = 3
};
native CsTeams:cs_get_user_team(index, &{CsInternalModel,_}:model = CS_DONTCHANGE);

/* Is user vip? Returns 1 if true, 0 if false.
 */
native cs_get_user_vip(index);

/* If vip = 1, user is set to vip.
 * If model = 1, then user's model will be changed to VIP model or random CT model if vip = 0.
 * If scoreboard = 1, then scoreboard will be updated to show that user is VIP.
 * This shouldn't be used for players on teams other than CT.
 * NOTE:	this is mostly useful for unsetting vips, so they can change teams and/or buy items properly.
 * It does not alter game play; the one being VIP at start of round will retain internal status as VIP; terrorists
 * can terminate him and accomplish their objective, etc.
 */
 native cs_set_user_vip(index, vip = 1, model = 1, scoreboard = 1);

/* Returns 1 of specified user has tk:ed (team killed).
 */
native cs_get_user_tked(index);

/* Returns 1 of specified user has TKed (team killed).
 * tk = 1: player has TKed
 * tk = 0: player hasn't TKed
 * Set subtract to how many frags to subtract. Set subtract to negative value to add frags.
 */
native cs_set_user_tked(index, tk = 1, subtract = 1);

/* Returns different values depending on if user is driving a vehicle - and if so at what speed.
 * 0: no driving
 * 1: driving, but standing still
 * 2-4: driving, different positive speeds
 * 5: driving, negative speed (backing)
 * Note: these values were tested quickly, they may differ.
 */
native cs_get_user_driving(index);

/* Returns 1 if user has a shield, else 0.
 */
native cs_get_user_shield(index);

/* Returns 1 if user is using a stationary gun, else 0.
 */
native cs_get_user_stationary(index);

/* Returns armor value and sets by reference the armor type in second parameter.
 */
enum CsArmorType {
	CS_ARMOR_NONE = 0, // no armor
	CS_ARMOR_KEVLAR = 1, // armor
	CS_ARMOR_VESTHELM = 2 // armor and helmet
};
native cs_get_user_armor(index, &CsArmorType:armortype);

/* Use this instead of fun's set_user_armor.
 * Appropriate message to update client's HUD will be sent if armortype is kevlar or vesthelm.
 */
native cs_set_user_armor(index, armorvalue, CsArmorType:armortype);

/* Returns 1 if specified weapon is in burst mode.
 */
native cs_get_weapon_burst(index);

/* If burstmode = 1, weapon will be changed to burst mode, 0 and non-burst mode (semiautomatic/automatic) will be activated.
 * Only GLOCK and FAMAS can enter/leave burst mode.
 */
native cs_set_weapon_burst(index, burstmode = 1);

/* Returns 1 if weapon is silenced, else 0.
 */
native cs_get_weapon_silen(index);

/* If silence = 1, weapon will be silenced, 0 and silencer will be removed. Only USP and M4A1 can be silenced.
 */
native cs_set_weapon_silen(index, silence = 1, draw_animation = 1);

/* Returns amount of ammo in weapon's clip.
 */
native cs_get_weapon_ammo(index);

/* Set amount of ammo in weapon's clip.
 */
native cs_set_weapon_ammo(index, newammo);

/* Get weapon type. Corresponds to CSW_* in amxconst.inc: 1 is CSW_P228, 2 is CSW_SCOUT and so on...
 */
native cs_get_weapon_id(index);

/* Returns 1 if no knives mode is enabled, else 0.
 */
native cs_get_no_knives();

/* Enabled no knives mode by calling this with value 1. Disabled with 0.
 * No knives mode means that player will not be given a knife when spawning.
 * You can still give knives (ie through fun's give_item).
 */
native cs_set_no_knives(noknives = 0);

/* Spawns a Counter-Strike player
 */
native cs_user_spawn(player);

/* Get what weapon type (CSW_*) an armoury_entity is.
 */
native cs_get_armoury_type(index);

/* Set an armoury_entity to be of specified type. You will have to set the appropriate model.
 * The second argument, type, should be a CSW_* constant. Not all weapons are supported by Counter-strike.
 * Supported weapons/items: CSW_MP5NAVY, CSW_TMP, CSW_P90, CSW_MAC10, CSW_AK47, CSW_SG552, CSW_M4A1, CSW_AUG, CSW_SCOUT
 * CSW_G3SG1, CSW_AWP, CSW_M3, CSW_XM1014, CSW_M249, CSW_FLASHBANG, CSW_HEGRENADE, CSW_VEST, CSW_VESTHELM, CSW_SMOKEGRENADE
 */
native cs_set_armoury_type(index, type);

#define CS_MAPZONE_BUY 			(1<<0)
#define CS_MAPZONE_BOMBTARGET 		(1<<1)
#define CS_MAPZONE_HOSTAGE_RESCUE 	(1<<2)
#define CS_MAPZONE_ESCAPE		(1<<3)
#define CS_MAPZONE_VIP_SAFETY 		(1<<4)

/* Returns in bitwise form if the user is in a specific map zone.
 * NOTE: If user can't plant (cs_get_user_plant(index) is 0) then cs_get_user_mapzones(index) & CS_MAPZONE_BOMBTARGET will return 0 too.
 */
native cs_get_user_mapzones(index);

/* Zoom type enum. Used for get/set_user_zoom() natives.
 */
enum
{
	CS_RESET_ZOOM = 0,		// Reset any zoom blocking (when using this type, mode has no effect)
	CS_SET_NO_ZOOM,			// Disable any sort of zoom (ie: to disable zoom in all weapons use this with mode=0)
	CS_SET_FIRST_ZOOM,		// Set first zoom (awp style) 
	CS_SET_SECOND_ZOOM,		// Set second zoom (awp style)
	CS_SET_AUGSG552_ZOOM,		// Set aug/sg552 zoom style
};
/* Sets a weapon zoom type on a player, any zoom type will work for all weapons, so you can even set an awp zoom to pistols :D
 * The 2nd param has to be one of the above zoom types in the enum. Mode can only be 0 or 1.
 * If mode=0 (blocking mode), the user will be forced to use the zoom type set by the native, and wont be able to change it (even by changing weapon)
 * until the native resets the zoom with CS_RESET_ZOOM.
 * If mode=1 the user will be able to restore back to a normal view by changing weapon.
 */
native cs_set_user_zoom(index, type, mode);

/* Returns how a user is zooming during the native call. Values correspond to the above enum, but will return 0 if an error occurred.
 */
native cs_get_user_zoom(index);

/* Returns the submodel setting of the player.
 * If this is 1, then the user has a backpack or defuser on their model (depending on team)
 */
native cs_get_user_submodel(index);

/* Sets the submodel setting of the player.
 * If this is 1, then the user has a backpack or defuser on their model (depending on team)
 * 0 removes it.
 */
native cs_set_user_submodel(index, value);

/* Gets or sets the user's last activity time.  This is the time that CS's internal afk kicker
 * checks to see who has been afk too long.
 */
native Float:cs_get_user_lastactivity(index);

native cs_set_user_lastactivity(index, Float:value);

/* Gets or sets the number of hostages that a user has killed.
 */
native cs_get_user_hostagekills(index);

native cs_set_user_hostagekills(index, value);

/* Gets or sets the time that the hostage was last used.
 */
native Float:cs_get_hostage_lastuse(index);

native cs_set_hostage_lastuse(index, Float:value);

/* Gets or sets the time which the hostage can next be used.
 */
native Float:cs_get_hostage_nextuse(index);

native cs_set_hostage_nextuse(index, Float:value);

/* Gets or sets the time in which the C4 will explode.
 */
native Float:cs_get_c4_explode_time(index);

native cs_set_c4_explode_time(index, Float:value);

/* Gets or sets whether the C4 is being defused.
 */
native bool:cs_get_c4_defusing(c4index);

native cs_set_c4_defusing(c4index, bool:defusing);
/**
 * Called when CS internally fires a command to a player.  It does this for a few 
 * functions, most notably rebuy/autobuy functionality.  This is also used to pass
 * commands to CZ bots internally.
 *
 * @param id			Client index.
 * @param cmd			Command string.
 * @return				PLUGIN_HANDLED to block, PLUGIN_CONTINUE for normal operation.
 */
forward CS_InternalCommand(id, const cmd[]);
