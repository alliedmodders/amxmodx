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

/* Returns player deaths. */
native cs_get_user_deaths(index);

/* Sets player deaths. */
native cs_set_user_deaths(index, newdeaths);

/* Returns index of entity (does not have to be a player) which hostage is following. 0 is hostage doesn't follow anything. */
native cs_get_hostage_foll(index);

/* Set hostage to follow entity specified in followedindex. Does not have to be a player. If followedindex is 0 the hostage will stop following. */
native cs_set_hostage_foll(index, followedindex = 0);

/* Get unique hostage id. */
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
* smoke */
native cs_get_user_bpammo(index, weapon);

/* Restock/remove ammo in a user's backpack. */
native cs_set_user_bpammo(index, weapon, amount);

/* Returns 1 if user has a defuse kit. */
native cs_get_user_defuse(index);

/* If defusekit is 1, the user will have a defuse kit.
* You can specify a different colour for the defuse kit icon showing on hud. Default is the normal green.
* You can specify an icon. Default is "defuser". Set flash to 1 if you want the icon to flash red. */
native cs_set_user_defuse(index, defusekit = 1, r = 0, g = 160, b = 0, icon[] = "defuser", flash = 0);

/* Is user in buyzone? Returns 1 when true, 0 when false. */
native cs_get_user_buyzone(index);

/* Get user model. */
native cs_get_user_model(index, model[], len);

/* Set user model. */
native cs_set_user_model(index, const model[]);

/* Use to reset model to standard selected model. */
native cs_reset_user_model(index);

/* Returns users money. */
native cs_get_user_money(index);

/* Gives money to user. If flash is 1, the difference between new and old amount will flash red or green. */
native cs_set_user_money(index, money, flash = 1);

/* Does user have night vision goggles? */
native cs_get_user_nvg(index);

/* Set nvgoggles to 1 to give night vision goggles to index. Set it to 0 to remove them. */
native cs_set_user_nvg(index, nvgoggles = 1);

/* Returns 1 if user has the "skill" to plant bomb, else 0. Normally this would only be true for a terrorist carrying a bomb. */
native cs_get_user_plant(index);

/* If plant is 1, a user will be set to be able to plant bomb within the usual bomb target areas if having one.
* You should use this if you give a player a weapon_c4, or he won't be able to plant it
* without dropping it and picking it up again (only possible for terrorists).
* If showbombicon is 1, the green C4 icon will be shown on user hud (if plant "skill" was enabled). */
native cs_set_user_plant(index, plant = 1, showbombicon = 1);

/* Get team directly from player's entity.
* 1 = terrorist
* 2 = counter-terrorist
* 3 = spectator */
native cs_get_user_team(index);

/* Set user team without killing player (so you can move hostages, plant bomb etc as terrorist). */
native cs_set_user_team(index, team);

/* Is user vip? */
native cs_get_user_vip(index);

/* If vip = 1, user is set to vip. Note that this is useful to unset vips, so they can change teams properly.
* This will not change the player's model to/from vip, or add/remove the "VIP" text in scoreboard. */
native cs_set_user_vip(index, vip = 1);

/* Returns 1 if specified weapon is in burst mode. */
native cs_get_weapon_burst(index);

/* If burstmode = 1, weapon will be changed to burst mode, 0 and non-burst mode (semiautomatic/automatic) will be activated.
* Only GLOCK and FAMAS can enter/leave burst mode. */
native cs_set_weapon_burst(index, burstmode = 1);

/* Returns 1 if weapon is silenced, else 0. */
native cs_get_weapon_silen(index);

/* If silence = 1, weapon will be silenced, 0 and silencer will be removed. Only USP and M4A1 can be silenced. */
native cs_set_weapon_silen(index, silence = 1);

/* Returns amount of ammo in weapon's clip. */
native cs_get_weapon_ammo(index);

/* Set amount of ammo in weapon's clip. */
native cs_set_weapon_ammo(index, newammo);