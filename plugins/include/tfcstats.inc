/* TFCX Stats functions
 *
 * (c) 2004, SidLuke
 * This file is provided as is (no warranties).
 */

#if defined _tfcstats_included
  #endinput
#endif
#define _tfcstats_included

/* Gets stats from given weapon index. If wpnindex is 0
* then the stats are from all weapons. If weapon has not been used function
* returns 0 in other case 1. Fields in stats are:
* 0 - kills
* 1 - deaths
* 2 - headshots
* 3 - teamkilling
* 4 - shots
* 5 - hits
* 6 - damage
* For body hits fields see amxconst.inc. */
native get_user_wstats(index,wpnindex,stats[8],bodyhits[8]);

/* Gets round stats from given weapon index.*/
native get_user_wrstats(index,wpnindex,stats[8],bodyhits[8]);

/* Gets overall stats which are stored in file on server
* and updated on every respawn or user disconnect.
* Function returns the position in stats by diff. kills to deaths. */
native get_user_stats(index,stats[8],bodyhits[8]);

/* Gets round stats of player. */
native get_user_rstats(index,stats[8],bodyhits[8]);

/* Gets stats with which user have killed/hurt his victim. If victim is 0
* then stats are from all victims. If victim has not been hurt, function
* returns 0 in other case 1. User stats are reset on his respawn. */
native get_user_vstats(index,victim,stats[8],bodyhits[8],wpnname[]="",len=0);

/* Gets stats with which user have been killed/hurt. If killer is 0
* then stats are from all attacks. If killer has not hurt user, function
* returns 0 in other case 1. User stats are reset on his respawn. */
native get_user_astats(index,wpnindex,stats[8],bodyhits[8],wpnname[]="",len=0);

/* Resets life, weapon, victims and attackers user stats. */
native reset_user_wstats(index);

/* Gets overall stats which stored in stats.dat file in amx folder
* and updated on every mapchange or user disconnect.
* Function returns next index of stats entry or 0 if no more exists. */
native get_stats(index,stats[8],bodyhits[8],name[],len);

/* Returns number of all entries in stats. */
native get_statsnum();
