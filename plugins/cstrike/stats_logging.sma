// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Stats Logging Plugin
//

#include <amxmodx>
#include <csx>

new g_pingSum[MAX_PLAYERS + 1]
new g_pingCount[MAX_PLAYERS + 1]
new g_inGame[MAX_PLAYERS + 1]

public plugin_init()
{
	register_plugin("CS Stats Logging", AMXX_VERSION_STR, "AMXX Dev Team")
}

public client_disconnected(id)
{
	if (!g_inGame[id])
		return
		
	g_inGame[id] = 0
	
	if (is_user_bot(id))
	{
		return
	}

	remove_task(id)
	
	new szTeam[16], szName[MAX_NAME_LENGTH], szAuthid[32], iStats[STATSX_MAX_STATS], iHits[MAX_BODYHITS], szWeapon[24]
	new iUserid = get_user_userid(id)
	new _max = xmod_get_maxweapons()
	
	get_user_team(id, szTeam, charsmax(szTeam))
	get_user_name(id, szName, charsmax(szName))
	get_user_authid(id, szAuthid, charsmax(szAuthid))

	for (new i = 1 ; i < _max ; ++i)
	{
		if (get_user_wstats(id, i, iStats, iHits))
		{
			xmod_get_wpnname(i, szWeapon, charsmax(szWeapon))
			
			log_message("^"%s<%d><%s><%s>^" triggered ^"weaponstats^" (weapon ^"%s^") (shots ^"%d^") (hits ^"%d^") (kills ^"%d^") (headshots ^"%d^") (tks ^"%d^") (damage ^"%d^") (deaths ^"%d^")", 
						szName, iUserid, szAuthid, szTeam, szWeapon, iStats[STATSX_SHOTS], iStats[STATSX_HITS], iStats[STATSX_KILLS], iStats[STATSX_HEADSHOTS], iStats[STATSX_TEAMKILLS], iStats[STATSX_DAMAGE], iStats[STATSX_DEATHS])
			log_message("^"%s<%d><%s><%s>^" triggered ^"weaponstats2^" (weapon ^"%s^") (head ^"%d^") (chest ^"%d^") (stomach ^"%d^") (leftarm ^"%d^") (rightarm ^"%d^") (leftleg ^"%d^") (rightleg ^"%d^")", 
						szName, iUserid, szAuthid, szTeam, szWeapon, iHits[HIT_HEAD], iHits[HIT_CHEST], iHits[HIT_STOMACH], iHits[HIT_LEFTARM], iHits[HIT_RIGHTARM], iHits[HIT_LEFTLEG], iHits[HIT_RIGHTLEG])
		}
	}
	
	new iTime = get_user_time(id, 1)
	
	log_message("^"%s<%d><%s><%s>^" triggered ^"time^" (time ^"%d:%02d^")", szName, iUserid, szAuthid, szTeam, (iTime / 60), (iTime % 60))
	log_message("^"%s<%d><%s><%s>^" triggered ^"latency^" (ping ^"%d^")", szName, iUserid, szAuthid, szTeam, (g_pingSum[id] / (g_pingCount[id] ? g_pingCount[id] : 1)))
}

public client_connect(id)
{
	g_inGame[id] = 0
}

public client_putinserver(id)
{
	g_inGame[id] = 1
	if (!is_user_bot(id))
	{
		g_pingSum[id] = g_pingCount[id] = 0
		if (task_exists(id))
			remove_task(id)
		set_task(19.5, "getPing", id, "", 0, "b")
	}
}

public getPing(id)
{
	new iPing, iLoss
	
	get_user_ping(id, iPing, iLoss)
	g_pingSum[id] += iPing
	++g_pingCount[id]
}
