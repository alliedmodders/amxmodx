/* DoDFun functions
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 */

#if defined _dodfun_included
  #endinput
#endif
#define _dodfun_included

#include <dodconst>


/* Function is called after grenade throw */
forward grenade_throw(index,greindex,wId);

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

/* Sets player deaths. */
native dod_set_pl_deaths(index,value,refresh=1);

/* Sets player score. */
native dod_set_user_score(index,value,refresh=1);

/* Sets new team name for this player */
native dod_set_pl_teamname(index,szName[]);

/* Gets player team name */
native dod_get_pl_teamname(index,szName[],len);

/* Returns 1 is player weapon is deployed (bar,mg..) */
native dod_is_deployed(index);

/*Sets the ammo of the specified weapon entity id */
native dod_set_user_ammo(index,wid,value);

/*Gets the ammo of the specified weapon entity id */
native dod_get_user_ammo(index,wid);


