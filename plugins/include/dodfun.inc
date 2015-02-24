// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// DoD Fun Functions
//

#if defined _dodfun_included
	#endinput
#endif
#define _dodfun_included

#include <dodconst>

#pragma reqlib dodfun
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib dodfun
#endif

/* Function is called after grenade throw */
forward grenade_throw(index,greindex,wId);

/* Function is called after a rocket is shot */
forward rocket_shoot(index,rocketindex,wId);

/* Example: for full stamina use dod_player_stamina(1,STAMINA_SET,100,100) */
/* value is from 0 - 100 */
native dod_set_stamina(index,set=STAMINA_SET,minvalue=0,maxvalue=100);

/* Sets fuse for grenades. Valid number is from 0.1-20.0 */
/* types : new or preprimed */
native dod_set_fuse(index,set=FUSE_SET,Float:newFuse=5.0, Type=FT_NEW);

/* Sets player class */
native dod_set_user_class(index,classId);

/* Sets player team and random class. Don't work for spectators. */
native dod_set_user_team(index,teamId,refresh=1);

/* Returns next player class. Usefull is player is using random class */
native dod_get_next_class(index);

/* Returns 1 if player choose random class */
native dod_is_randomclass(index);

/* Returns player deaths */
native dod_get_pl_deaths(index);

/* Sets player deaths. 
 * Note if you opt to refresh the scoreboard, it
 * will make the player appear as "DEAD" in the scoreboard.
 */
native dod_set_pl_deaths(index,value,refresh=1);

/* Returns player deaths. */
native dod_get_user_kills(index);

/* Sets player kills. */
native dod_set_user_kills(index,value,refresh=1);

/* Sets player score. */
native dod_set_user_score(index,value,refresh=1);

/* Sets new team name for this player */
native dod_set_pl_teamname(index,const szName[]);

/* Gets player team name */
native dod_get_pl_teamname(index,szName[],len);

/* Returns 1 is player weapon is deployed (bar,mg..) */
native dod_is_deployed(index);

/*Sets the ammo of the specified weapon entity id */
native dod_set_user_ammo(index,wid,value);

/*Gets the ammo of the specified weapon entity id */
native dod_get_user_ammo(index,wid);

/* called after first InitObj */
forward controlpoints_init();

enum CP_VALUE {
	CP_edict = 1,		// read only
	CP_area,			// read only
	CP_index,			// read only
	CP_owner,
	CP_default_owner,
	CP_visible,			// reinit after change
	CP_icon_neutral,		// reinit after change
	CP_icon_allies,		// reinit after change
	CP_icon_axis,		// reinit after change
	CP_origin_x,		// reinit after change
	CP_origin_y,		// reinit after change
	
	CP_can_touch,
	CP_pointvalue,

	CP_points_for_cap,
	CP_team_points,

	CP_model_body_neutral,
	CP_model_body_allies,
	CP_model_body_axis,

	// strings
	CP_name,
	CP_reset_capsound,
	CP_allies_capsound,
	CP_axis_capsound,
	CP_targetname,

	CP_model_neutral,
	CP_model_allies,
	CP_model_axis,
};

/* returns number of objectives */
native objectives_get_num();

/* use this function to update client(s) hud. You need to do this sometimes. Check CP_VALUE comments.
   if player is 0 , all clients will get this message */
native objectives_reinit( player=0 );

/* use this function to get info about specified control point */
native objective_get_data( index, CP_VALUE:key, szValue[]="", len=0 );

/* use this function to change control point's data */
native objective_set_data( index, CP_VALUE:key , iValue=-1, const szValue[]="" );

enum CA_VALUE {
	CA_edict = 1,
	CA_allies_numcap,
	CA_axis_numcap,
	CA_timetocap,
	CA_can_cap,

	// strings
	CA_target,
	CA_sprite,
};

/* use this function to get info about specified control point's area */
native area_get_data( index, CA_VALUE:key, szValue[]="", len=0 );

/* use this function to change control point's area data */
native area_set_data( index, CA_VALUE:key , iValue=-1, const szValue[]="" );
