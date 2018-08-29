// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// StatsX Plugin
//

//--------------------------------
#include <amxmodx>
#include <amxmisc>
#include <csx>
#include <hamsandwich>
//--------------------------------

// Uncomment to activate log debug messages.
//#define STATSX_DEBUG

// HUD statistics duration in seconds (minimum 1.0 seconds).
#define HUD_DURATION_CVAR   "amx_statsx_duration"
#define HUD_DURATION        "12.0"

// HUD statistics stop relative freeze end in seconds.
// To stop before freeze end use a negative value.
#define HUD_FREEZE_LIMIT_CVAR   "amx_statsx_freeze"
#define HUD_FREEZE_LIMIT        "-2.0"

// HUD statistics minimum duration, in seconds, to trigger the display logic.
#define HUD_MIN_DURATION    0.2

// Config plugin constants.
#define MODE_HUD_DELAY      0   // Make a 0.1 sec delay on HUD reset process.

// You can also manualy enable or disable these options by setting them to 1
// For example:
// public ShowAttackers = 1
// However amx_statscfg command is recommended

public KillerChat           = 0 // displays killer hp&ap to victim console
                                // and screen

public ShowAttackers        = 0 // shows attackers
public ShowVictims          = 0 // shows victims
public ShowKiller           = 0 // shows killer
public ShowTeamScore        = 0 // shows team score at round end
public ShowTotalStats       = 0 // shows round total stats
public ShowBestScore        = 0 // shows rounds best scored player
public ShowMostDisruptive   = 0 // shows rounds most disruptive player

public EndPlayer            = 0 // displays player stats at the end of map
public EndTop15             = 0 // displays top15 at the end of map

public SayHP                = 0 // displays information about user killer
public SayStatsMe           = 0 // displays user's stats and rank
public SayRankStats         = 0 // displays user's rank stats
public SayMe                = 0 // displays user's stats
public SayRank              = 0 // displays user's rank
public SayReport            = 0 // report user's weapon status to team
public SayScore             = 0 // displays team's map score
public SayTop15             = 0 // displays first 15 players
public SayStatsAll          = 0 // displays all players stats and rank

public ShowStats            = 1 // set client HUD-stats switched off by default
public ShowDistHS           = 0 // show distance and HS in attackers and
                                //  victims HUD lists
public ShowFullStats        = 0 // show full HUD stats (more than 78 chars)

public SpecRankInfo         = 0 // displays rank info when spectating

// Standard Contstants.
#define MAX_TEAMS               2
#define MAX_WEAPON_LENGTH       31
#define MAX_TEXT_LENGTH         255
#define MAX_BUFFER_LENGTH       2047

// Global player flags.
new BODY_PART[MAX_BODYHITS][] =
{
	"WHOLEBODY",
	"HEAD",
	"CHEST",
	"STOMACH",
	"LEFTARM",
	"RIGHTARM",
	"LEFTLEG",
	"RIGHTLEG"
}

// Killer information, save killer info at the time when player is killed.
#define KILLED_KILLER_ID        0   // Killer userindex/user-ID
#define KILLED_KILLER_HEALTH    1   // Killer's health
#define KILLED_KILLER_ARMOUR    2   // Killer's armour
#define KILLED_TEAM             3   // Killer's team
#define KILLED_KILLER_STATSFIX  4   // Fix to register the last hit/kill

new g_izKilled[MAX_PLAYERS + 1][5]

// Menu variables and configuration
#define MAX_PPL_MENU_ACTIONS    2   // Number of player menu actions
#define PPL_MENU_OPTIONS        7   // Number of player options per displayed menu

new g_iPluginMode                                       = 0

new g_izUserMenuPosition[MAX_PLAYERS + 1]               = {0, ...}
new g_izUserMenuAction[MAX_PLAYERS + 1]                 = {0, ...}
new g_izUserMenuPlayers[MAX_PLAYERS + 1][MAX_PLAYERS]

new g_izSpecMode[MAX_PLAYERS + 1]                       = {0, ...}

new g_izShowStatsFlags[MAX_PLAYERS + 1]                 = {0, ...}
new g_izStatsSwitch[MAX_PLAYERS + 1]                    = {0, ...}
new Float:g_fzShowUserStatsTime[MAX_PLAYERS + 1]        = {0.0, ...}
new Float:g_fShowStatsTime                              = 0.0
new Float:g_fFreezeTime                                 = 0.0
new Float:g_fFreezeLimitTime                            = 0.0
new Float:g_fHUDDuration                                = 0.0

new g_iRoundEndTriggered                                = 0
new g_iRoundEndProcessed                                = 0

new g_pFreezeTime                                       = 0
new g_pRoundTime                                        = 0
new g_pHudDuration                                      = 0
new g_pHudFreezeLimit                                   = 0

new Float:g_fStartGame                                  = 0.0
new g_izTeamScore[MAX_TEAMS]                            = {0, ...}
new g_izTeamEventScore[MAX_TEAMS]                       = {0, ...}
new g_izTeamRndStats[MAX_TEAMS][STATSX_MAX_STATS]
new g_izTeamGameStats[MAX_TEAMS][STATSX_MAX_STATS]
new g_izUserUserID[MAX_PLAYERS + 1]                     = {0, ...}
new g_izUserAttackerDistance[MAX_PLAYERS + 1]           = {0, ...}
new g_izUserVictimDistance[MAX_PLAYERS + 1][MAX_PLAYERS + 1]
new g_izUserRndName[MAX_PLAYERS + 1][MAX_NAME_LENGTH]
new g_izUserRndStats[MAX_PLAYERS + 1][STATSX_MAX_STATS]
new g_izUserGameStats[MAX_PLAYERS + 1][STATSX_MAX_STATS]

// Common buffer to improve performance, as Small always zero-initializes all vars
new g_sBuffer[MAX_BUFFER_LENGTH + 1]                    = ""
new g_sScore[MAX_TEXT_LENGTH + 1]                       = ""
new g_sAwardAndScore[MAX_BUFFER_LENGTH + 1]             = ""

new t_sText[MAX_TEXT_LENGTH + 1]                        = ""
new t_sName[MAX_NAME_LENGTH + 1]                        = ""
new t_sWpn[MAX_WEAPON_LENGTH + 1]                       = ""

new g_HudSync_EndRound
new g_HudSync_SpecInfo

//--------------------------------
// Initialize
//--------------------------------
public plugin_init()
{
	// Register plugin.
	register_plugin("StatsX", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("statsx.txt")

	// Register events.
	register_event("TextMsg", "eventStartGame", "a", "2=#Game_Commencing", "2=#Game_will_restart_in")
	RegisterHamPlayer(Ham_Spawn, "eventSpawn", 1)
	register_event("RoundTime", "eventStartRound", "bc")
	register_event("SendAudio", "eventEndRound", "a", "2=%!MRAD_terwin", "2=%!MRAD_ctwin", "2=%!MRAD_rounddraw")
	register_event("TeamScore", "eventTeamScore", "a")
	register_event("30", "eventIntermission", "a")
	register_event("TextMsg", "eventSpecMode", "bd", "2&ec_Mod")
	register_event("StatusValue", "eventShowRank", "bd", "1=2")

	// Register commands.
	register_clcmd("say /hp", "cmdHp", 0, "- display info. about your killer (chat)")
	register_clcmd("say /statsme", "cmdStatsMe", 0, "- display your stats (MOTD)")
	register_clcmd("say /rankstats", "cmdRankStats", 0, "- display your server stats (MOTD)")
	register_clcmd("say /me", "cmdMe", 0, "- display current round stats (chat)")
	register_clcmd("say /score", "cmdScore", 0, "- display last score (chat)")
	register_clcmd("say /rank", "cmdRank", 0, "- display your rank (chat)")
	register_clcmd("say /report", "cmdReport", 0, "- display weapon status (say_team)")
	register_clcmd("say /top15", "cmdTop15", 0, "- display top 15 players (MOTD)")
	register_clcmd("say /stats", "cmdStats", 0, "- display players stats (menu/MOTD)")
	register_clcmd("say /switch", "cmdSwitch", 0, "- switch client's stats on or off")
	register_clcmd("say_team /hp", "cmdHp", 0, "- display info. about your killer (chat)")
	register_clcmd("say_team /statsme", "cmdStatsMe", 0, "- display your stats (MOTD)")
	register_clcmd("say_team /rankstats", "cmdRankStats", 0, "- display your server stats (MOTD)")
	register_clcmd("say_team /me", "cmdMe", 0, "- display current round stats (chat)")
	register_clcmd("say_team /score", "cmdScore", 0, "- display last score (chat)")
	register_clcmd("say_team /rank", "cmdRank", 0, "- display your rank (chat)")
	register_clcmd("say_team /report", "cmdReport", 0, "- display weapon status (say_team_team)")
	register_clcmd("say_team /top15", "cmdTop15", 0, "- display top 15 players (MOTD)")
	register_clcmd("say_team /stats", "cmdStats", 0, "- display players stats (menu/MOTD)")
	register_clcmd("say_team /switch", "cmdSwitch", 0, "- switch client's stats on or off")

	// Register menus.
	register_menucmd(register_menuid("Server Stats"), 1023, "actionStatsMenu")

	// Register special configuration setting and default value.
	register_srvcmd("amx_statsx_mode", "cmdPluginMode", ADMIN_CFG, "<flags> - sets plugin options")

#if defined STATSX_DEBUG
	register_clcmd("say /hudtest", "cmdHudTest")
#endif

	g_pHudDuration = register_cvar(HUD_DURATION_CVAR, HUD_DURATION)
	g_pHudFreezeLimit = register_cvar(HUD_FREEZE_LIMIT_CVAR, HUD_FREEZE_LIMIT)

	g_pFreezeTime = get_cvar_pointer("mp_freezetime")
	g_pRoundTime = get_cvar_pointer("mp_roundtime")

	// Init buffers and some global vars.
	g_sBuffer[0] = 0

	g_HudSync_EndRound = CreateHudSyncObj()
	g_HudSync_SpecInfo = CreateHudSyncObj()
}

public plugin_cfg()
{
	new addStast[] = "amx_statscfg add ^"%s^" %s"

	server_cmd(addStast, "ST_SHOW_KILLER_CHAT", "KillerChat")
	server_cmd(addStast, "ST_SHOW_ATTACKERS", "ShowAttackers")
	server_cmd(addStast, "ST_SHOW_VICTIMS", "ShowVictims")
	server_cmd(addStast, "ST_SHOW_KILLER", "ShowKiller")
	server_cmd(addStast, "ST_SHOW_TEAM_SCORE", "ShowTeamScore")
	server_cmd(addStast, "ST_SHOW_TOTAL_STATS", "ShowTotalStats")
	server_cmd(addStast, "ST_SHOW_BEST_SCORE", "ShowBestScore")
	server_cmd(addStast, "ST_SHOW_MOST_DISRUPTIVE", "ShowMostDisruptive")
	server_cmd(addStast, "ST_SHOW_HUD_STATS_DEF", "ShowStats")
	server_cmd(addStast, "ST_SHOW_DIST_HS_HUD", "ShowDistHS")
	server_cmd(addStast, "ST_STATS_PLAYER_MAP_END", "EndPlayer")
	server_cmd(addStast, "ST_STATS_TOP15_MAP_END", "EndTop15")
	server_cmd(addStast, "ST_SAY_HP", "SayHP")
	server_cmd(addStast, "ST_SAY_STATSME", "SayStatsMe")
	server_cmd(addStast, "ST_SAY_RANKSTATS", "SayRankStats")
	server_cmd(addStast, "ST_SAY_ME", "SayMe")
	server_cmd(addStast, "ST_SAY_RANK", "SayRank")
	server_cmd(addStast, "ST_SAY_REPORT", "SayReport")
	server_cmd(addStast, "ST_SAY_SCORE", "SayScore")
	server_cmd(addStast, "ST_SAY_TOP15", "SayTop15")
	server_cmd(addStast, "ST_SAY_STATS", "SayStatsAll")
	server_cmd(addStast, "ST_SPEC_RANK", "SpecRankInfo")

	// Update local configuration vars with value in cvars.
	get_config_cvars()
}

// Set hudmessage format.
set_hudtype_killer(Float:fDuration)
	set_hudmessage(220, 80, 0, 0.05, 0.15, 0, 6.0, fDuration, (fDuration >= g_fHUDDuration) ? 1.0 : 0.0, 1.0, -1)

set_hudtype_endround(Float:fDuration)
{
	set_hudmessage(100, 200, 0, 0.05, 0.55, 0, 0.02, fDuration, (fDuration >= g_fHUDDuration) ? 1.0 : 0.0, 1.0)
}

set_hudtype_attacker(Float:fDuration)
	set_hudmessage(220, 80, 0, 0.55, 0.35, 0, 6.0, fDuration, (fDuration >= g_fHUDDuration) ? 1.0 : 0.0, 1.0, -1)

set_hudtype_victim(Float:fDuration)
	set_hudmessage(0, 80, 220, 0.55, 0.60, 0, 6.0, fDuration, (fDuration >= g_fHUDDuration) ? 1.0 : 0.0, 1.0, -1)

set_hudtype_specmode()
{
	set_hudmessage(255, 255, 255, 0.02, 0.96, 2, 0.05, 0.1, 0.01, 3.0, -1)
}

#if defined STATSX_DEBUG
public cmdHudTest(id)
{
	new i, iLen
	iLen = 0

	for (i = 1; i < 20; i++)
		iLen += formatex(g_sBuffer[iLen], charsmax(g_sBuffer) - iLen, "....x....1....x....2....x....3....x....4....x....^n")

	set_hudtype_killer(50.0)
	show_hudmessage(id, "%s", g_sBuffer)
}
#endif

// Stats formulas
Float:accuracy(izStats[STATSX_MAX_STATS])
{
	return izStats[STATSX_SHOTS] ? (100.0 * float(izStats[STATSX_HITS]) / float(izStats[STATSX_SHOTS])) : (0.0);
}

Float:effec(izStats[STATSX_MAX_STATS])
{
	return izStats[STATSX_KILLS] ? (100.0 * float(izStats[STATSX_KILLS]) / float(izStats[STATSX_KILLS] + izStats[STATSX_DEATHS])) : (0.0);
}

// Distance formula (metric)
Float:distance(iDistance)
{
	return float(iDistance) * 0.0254
}

// Get plugin config flags.
set_plugin_mode(id, sFlags[])
{
	if (sFlags[0])
		g_iPluginMode = read_flags(sFlags)

	get_flags(g_iPluginMode, t_sText, charsmax(t_sText))
	console_print(id, "%L", id, "MODE_SET_TO", t_sText)

	return g_iPluginMode
}

// Get config parameters.
get_config_cvars()
{
	g_fFreezeTime = floatmax(get_pcvar_float(g_pFreezeTime), 0.0);

	g_fHUDDuration = floatmax(get_pcvar_float(g_pHudDuration), 1.0);

	g_fFreezeLimitTime = get_pcvar_float(g_pHudFreezeLimit)
}

// Get and format attackers header and list.
get_attackers(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
	new iAttacker
	new iFound, iLen

	iFound = 0
	sBuffer[0] = 0

	// Get and format header. Add killing attacker statistics if user is dead.
	// Make sure shots is greater than zero or division by zero will occur.
	// To print a '%', 4 of them must done in a row.
	izStats[STATSX_SHOTS] = 0
	iAttacker = g_izKilled[id][KILLED_KILLER_ID]

	if (iAttacker)
		get_user_astats(id, iAttacker, izStats, izBody)

	if (izStats[STATSX_SHOTS] && ShowFullStats)
	{
		get_user_name(iAttacker, t_sName, charsmax(t_sName))
		iLen = formatex(sBuffer, charsmax(sBuffer), "%L -- %s -- %0.2f%% %L:^n", id, "ATTACKERS", t_sName, accuracy(izStats), id, "ACC")
	}
	else
		iLen = formatex(sBuffer, charsmax(sBuffer), "%L:^n", id, "ATTACKERS")

	// Get and format attacker list.
	for (iAttacker = 1; iAttacker <= MaxClients; iAttacker++)
	{
		if (get_user_astats(id, iAttacker, izStats, izBody, t_sWpn, charsmax(t_sWpn)))
		{
			iFound = 1
			get_user_name(iAttacker, t_sName, charsmax(t_sName))

			if (izStats[STATSX_KILLS])
			{
				if (!ShowDistHS)
					iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L / %s^n", t_sName, izStats[STATSX_HITS], id, "HIT_S",
									izStats[STATSX_DAMAGE], id, "DMG", t_sWpn)
				else if (izStats[STATSX_HEADSHOTS])
					iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L / %s / %0.0f m / HS^n", t_sName, izStats[STATSX_HITS], id, "HIT_S",
									izStats[STATSX_DAMAGE], id, "DMG", t_sWpn, distance(g_izUserAttackerDistance[id]))
				else
					iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L / %s / %0.0f m^n", t_sName, izStats[STATSX_HITS], id, "HIT_S",
									izStats[STATSX_DAMAGE], id, "DMG", t_sWpn, distance(g_izUserAttackerDistance[id]))
			}
			else
				iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L^n", t_sName, izStats[STATSX_HITS], id, "HIT_S", izStats[STATSX_DAMAGE], id, "DMG")
		}
	}

	if (!iFound)
		sBuffer[0] = 0

	return iFound
}

// Get and format victims header and list
get_victims(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
	new iVictim
	new iFound, iLen

	iFound = 0
	sBuffer[0] = 0

	// Get and format header.
	// Make sure shots is greater than zero or division by zero will occur.
	// To print a '%', 4 of them must done in a row.
	izStats[STATSX_SHOTS] = 0
	get_user_vstats(id, 0, izStats, izBody)

	if (izStats[STATSX_SHOTS])
		iLen = formatex(sBuffer, charsmax(sBuffer), "%L -- %0.2f%% %L:^n", id, "VICTIMS", accuracy(izStats), id, "ACC")
	else
		iLen = formatex(sBuffer, charsmax(sBuffer), "%L:^n", id, "VICTIMS")

	for (iVictim = 1; iVictim <= MaxClients; iVictim++)
	{
		if (get_user_vstats(id, iVictim, izStats, izBody, t_sWpn, charsmax(t_sWpn)))
		{
			iFound = 1
			get_user_name(iVictim, t_sName, charsmax(t_sName))

			if (izStats[STATSX_DEATHS])
			{
				if (!ShowDistHS)
					iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L / %s^n", t_sName, izStats[STATSX_HITS], id, "HIT_S",
									izStats[STATSX_DAMAGE], id, "DMG", t_sWpn)
				else if (izStats[STATSX_HEADSHOTS])
					iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L / %s / %0.0f m / HS^n", t_sName, izStats[STATSX_HITS], id, "HIT_S",
									izStats[STATSX_DAMAGE], id, "DMG", t_sWpn, distance(g_izUserVictimDistance[id][iVictim]))
				else
					iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L / %s / %0.0f m^n", t_sName, izStats[STATSX_HITS], id, "HIT_S",
									izStats[STATSX_DAMAGE], id, "DMG", t_sWpn, distance(g_izUserVictimDistance[id][iVictim]))
			}
			else
				iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%s -- %d %L / %d %L^n", t_sName, izStats[STATSX_HITS], id, "HIT_S", izStats[STATSX_DAMAGE], id, "DMG")
		}
	}

	if (!iFound)
		sBuffer[0] = 0

	return iFound
}

// Get and format kill info.
get_kill_info(id, iKiller, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iFound, iLen

	iFound = 0
	sBuffer[0] = 0

	if (iKiller && iKiller != id)
	{
		new izAStats[STATSX_MAX_STATS], izABody[MAX_BODYHITS], izVStats[STATSX_MAX_STATS], iaVBody[MAX_BODYHITS]

		iFound = 1
		get_user_name(iKiller, t_sName, charsmax(t_sName))

		izAStats[STATSX_HITS] = 0
		izAStats[STATSX_DAMAGE] = 0
		t_sWpn[0] = 0
		get_user_astats(id, iKiller, izAStats, izABody, t_sWpn, charsmax(t_sWpn))

		izVStats[STATSX_HITS] = 0
		izVStats[STATSX_DAMAGE] = 0
		get_user_vstats(id, iKiller, izVStats, iaVBody)

		iLen = formatex(sBuffer, charsmax(sBuffer), "%L^n", id, "KILLED_YOU_DIST", t_sName, t_sWpn, distance(g_izUserAttackerDistance[id]))
		iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%L^n", id, "DID_DMG_HITS", izAStats[STATSX_DAMAGE], izAStats[STATSX_HITS], g_izKilled[id][KILLED_KILLER_HEALTH], g_izKilled[id][KILLED_KILLER_ARMOUR])
		iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%L^n", id, "YOU_DID_DMG", izVStats[STATSX_DAMAGE], izVStats[STATSX_HITS])
	}

	return iFound
}

// Get and format most disruptive.
add_most_disruptive(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iPlayer, iMaxDamageId, iMaxDamage, iMaxHeadShots

	iMaxDamageId = 0
	iMaxDamage = 0
	iMaxHeadShots = 0

	// Find player.
	for (iPlayer = 1; iPlayer <= MaxClients; iPlayer++)
	{
		if (g_izUserRndStats[iPlayer][STATSX_DAMAGE] >= iMaxDamage && (g_izUserRndStats[iPlayer][STATSX_DAMAGE] > iMaxDamage || g_izUserRndStats[iPlayer][STATSX_HEADSHOTS] > iMaxHeadShots))
		{
			iMaxDamageId = iPlayer
			iMaxDamage = g_izUserRndStats[iPlayer][STATSX_DAMAGE]
			iMaxHeadShots = g_izUserRndStats[iPlayer][STATSX_HEADSHOTS]
		}
	}

	// Format statistics.
	if (iMaxDamageId)
	{
		iPlayer = iMaxDamageId

		new Float:fGameEff = effec(g_izUserGameStats[iPlayer])
		new Float:fRndAcc = accuracy(g_izUserRndStats[iPlayer])

		formatex(t_sText, charsmax(t_sText), "%L: %s^n%d %L / %d %L -- %0.2f%% %L / %0.2f%% %L^n", id, "MOST_DMG", g_izUserRndName[iPlayer],
				g_izUserRndStats[iPlayer][STATSX_HITS], id, "HIT_S", iMaxDamage, id, "DMG", fGameEff, id, "EFF", fRndAcc, id, "ACC")
		add(sBuffer, charsmax(sBuffer), t_sText)
	}

	return iMaxDamageId
}

// Get and format best score.
add_best_score(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iPlayer, iMaxKillsId, iMaxKills, iMaxHeadShots

	iMaxKillsId = 0
	iMaxKills = 0
	iMaxHeadShots = 0

	// Find player
	for (iPlayer = 1; iPlayer <= MaxClients; iPlayer++)
	{
		if (g_izUserRndStats[iPlayer][STATSX_KILLS] >= iMaxKills && (g_izUserRndStats[iPlayer][STATSX_KILLS] > iMaxKills || g_izUserRndStats[iPlayer][STATSX_HEADSHOTS] > iMaxHeadShots))
		{
			iMaxKillsId = iPlayer
			iMaxKills = g_izUserRndStats[iPlayer][STATSX_KILLS]
			iMaxHeadShots = g_izUserRndStats[iPlayer][STATSX_HEADSHOTS]
		}
	}

	// Format statistics.
	if (iMaxKillsId)
	{
		iPlayer = iMaxKillsId

		new Float:fGameEff = effec(g_izUserGameStats[iPlayer])
		new Float:fRndAcc = accuracy(g_izUserRndStats[iPlayer])

		formatex(t_sText, charsmax(t_sText), "%L: %s^n%d %L / %d hs -- %0.2f%% %L / %0.2f%% %L^n", id, "BEST_SCORE", g_izUserRndName[iPlayer],
				iMaxKills, id, "KILL_S", iMaxHeadShots, fGameEff, id, "EFF", fRndAcc, id, "ACC")
		add(sBuffer, charsmax(sBuffer), t_sText)
	}

	return iMaxKillsId
}

// Get and format team score.
add_team_score(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new Float:fzMapEff[MAX_TEAMS], Float:fzMapAcc[MAX_TEAMS], Float:fzRndAcc[MAX_TEAMS]

	// Calculate team stats
	for (new iTeam = 0; iTeam < MAX_TEAMS; iTeam++)
	{
		fzMapEff[iTeam] = effec(g_izTeamGameStats[iTeam])
		fzMapAcc[iTeam] = accuracy(g_izTeamGameStats[iTeam])
		fzRndAcc[iTeam] = accuracy(g_izTeamRndStats[iTeam])
	}

	// Format round team stats, MOTD
	formatex(t_sText, charsmax(t_sText), "TERRORIST %d / %0.2f%% %L / %0.2f%% %L^nCT %d / %0.2f%% %L / %0.2f%% %L^n", g_izTeamScore[0],
			fzMapEff[0], id, "EFF", fzRndAcc[0], id, "ACC", g_izTeamScore[1], fzMapEff[1], id, "EFF", fzRndAcc[1], id, "ACC")
	add(sBuffer, charsmax(sBuffer), t_sText)
}

// Get and format team stats, chat version
save_team_chatscore(id, sBuffer[MAX_TEXT_LENGTH + 1])
{
	new Float:fzMapEff[MAX_TEAMS], Float:fzMapAcc[MAX_TEAMS], Float:fzRndAcc[MAX_TEAMS]

	// Calculate team stats
	for (new iTeam = 0; iTeam < MAX_TEAMS; iTeam++)
	{
		fzMapEff[iTeam] = effec(g_izTeamGameStats[iTeam])
		fzMapAcc[iTeam] = accuracy(g_izTeamGameStats[iTeam])
		fzRndAcc[iTeam] = accuracy(g_izTeamRndStats[iTeam])
	}

	// Format game team stats, chat
	formatex(sBuffer, charsmax(sBuffer), "TERRORIST %d / %0.2f%% %L / %0.2f%% %L  --  CT %d / %0.2f%% %L / %0.2f%% %L", g_izTeamScore[0],
			fzMapEff[0], id, "EFF", fzMapAcc[0], id, "ACC", g_izTeamScore[1], fzMapEff[1], id, "EFF", fzMapAcc[1], id, "ACC")
}

// Get and format total stats.
add_total_stats(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	formatex(t_sText, charsmax(t_sText), "%L: %d %L / %d hs -- %d %L / %d %L^n", id, "TOTAL", g_izUserRndStats[0][STATSX_KILLS], id, "KILL_S",
			g_izUserRndStats[0][STATSX_HEADSHOTS], g_izUserRndStats[0][STATSX_HITS], id, "HITS", g_izUserRndStats[0][STATSX_SHOTS], id, "SHOT_S")
	add(sBuffer, charsmax(sBuffer), t_sText)
}

// Get and format a user's list of body hits from an attacker.
add_attacker_hits(id, iAttacker, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iFound = 0

	if (iAttacker && iAttacker != id)
	{
		new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS], iLen

		izStats[STATSX_HITS] = 0
		get_user_astats(id, iAttacker, izStats, izBody)

		if (izStats[STATSX_HITS])
		{
			iFound = 1
			iLen = strlen(sBuffer)
			get_user_name(iAttacker, t_sName, charsmax(t_sName))

			iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%L:^n", id, "HITS_YOU_IN", t_sName)

			for (new i = 1; i < sizeof(izBody); i++)
			{
				if (!izBody[i])
					continue

				iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%L: %d^n", id, BODY_PART[i], izBody[i])
			}
		}
	}

	return iFound
}

// Get and format killed stats: killer hp, ap, hits.
format_kill_ainfo(id, iKiller, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iFound = 0

	if (iKiller && iKiller != id)
	{
		new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
		new iLen

		iFound = 1
		get_user_name(iKiller, t_sName, charsmax(t_sName))
		izStats[STATSX_HITS] = 0
		get_user_astats(id, iKiller, izStats, izBody, t_sWpn, charsmax(t_sWpn))

		iLen = formatex(sBuffer, charsmax(sBuffer), "%L (%dhp, %dap) >>", id, "KILLED_BY_WITH", t_sName, t_sWpn, distance(g_izUserAttackerDistance[id]),
						g_izKilled[id][KILLED_KILLER_HEALTH], g_izKilled[id][KILLED_KILLER_ARMOUR])

		if (izStats[STATSX_HITS])
		{
			for (new i = 1; i < sizeof(izBody); i++)
			{
				if (!izBody[i])
					continue

				iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, " %L: %d", id, BODY_PART[i], izBody[i])
			}
		}
		else
			iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, " %L", id, "NO_HITS")
	}
	else
		formatex(sBuffer, charsmax(sBuffer), "%L", id, "YOU_NO_KILLER")

	return iFound
}

// Get and format killed stats: hits, damage on killer.
format_kill_vinfo(id, iKiller, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iFound = 0
	new izStats[STATSX_MAX_STATS]
	new izBody[MAX_BODYHITS]
	new iLen

	izStats[STATSX_HITS] = 0
	izStats[STATSX_DAMAGE] = 0
	get_user_vstats(id, iKiller, izStats, izBody)

	if (iKiller && iKiller != id)
	{
		iFound = 1
		get_user_name(iKiller, t_sName, charsmax(t_sName))
		iLen = formatex(sBuffer, charsmax(sBuffer), "%L >>", id, "YOU_HIT", t_sName, izStats[STATSX_HITS], izStats[STATSX_DAMAGE])
	}
	else
		iLen = formatex(sBuffer, charsmax(sBuffer), "%L >>", id, "LAST_RES", izStats[STATSX_HITS], izStats[STATSX_DAMAGE])

	if (izStats[STATSX_HITS])
	{
		for (new i = 1; i < sizeof(izBody); i++)
		{
			if (!izBody[i])
				continue

			iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, " %L: %d", id, BODY_PART[i], izBody[i])
		}
	}
	else
		iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, " %L", id, "NO_HITS")

	return iFound
}

// Get and format top 15.
format_top15(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new iMax = get_statsnum()
	new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
	new iLen = 0

	if (iMax > 15)
		iMax = 15

	new lKills[16], lDeaths[16], lHits[16], lShots[16], lEff[16], lAcc[16]

	formatex(lKills, charsmax(lKills), "%L", id, "KILLS")
	formatex(lDeaths, charsmax(lDeaths), "%L", id, "DEATHS")
	formatex(lHits, charsmax(lHits), "%L", id, "HITS")
	formatex(lShots, charsmax(lShots), "%L", id, "SHOTS")
	formatex(lEff, charsmax(lEff), "%L", id, "EFF")
	formatex(lAcc, charsmax(lAcc), "%L", id, "ACC")

	ucfirst(lEff)
	ucfirst(lAcc)

	iLen = formatex(sBuffer, charsmax(sBuffer), "<meta charset=utf-8><body bgcolor=#000000><font color=#FFB000><pre>")
	iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%2s %-22.22s %6s %6s %6s %6s %4s %4s %4s^n", "#", "Nick", lKills, lDeaths, lHits, lShots, "HS", lEff, lAcc)

	for (new i = 0; i < iMax && charsmax(sBuffer) - iLen > 0; i++)
	{
		get_stats(i, izStats, izBody, t_sName, charsmax(t_sName))
		replace_string(t_sName, charsmax(t_sName), "<", "[")
		replace_string(t_sName, charsmax(t_sName), ">", "]")
		iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%2d %-22.22s %6d %6d %6d %6d %4d %3.0f%% %3.0f%%^n", i + 1, t_sName, izStats[STATSX_KILLS],
						izStats[STATSX_DEATHS], izStats[STATSX_HITS], izStats[STATSX_SHOTS], izStats[STATSX_HEADSHOTS], effec(izStats), accuracy(izStats))
	}
}

// Get and format rank stats.
format_rankstats(id, sBuffer[MAX_BUFFER_LENGTH + 1], iMyId = 0)
{
	new izStats[STATSX_MAX_STATS] = {0, ...}
	new izBody[MAX_BODYHITS]
	new iRankPos, iLen
	new lKills[16], lDeaths[16], lHits[16], lShots[16], lDamage[16], lEff[16], lAcc[16]

	formatex(lKills, charsmax(lKills), "%L", id, "KILLS")
	formatex(lDeaths, charsmax(lDeaths), "%L", id, "DEATHS")
	formatex(lHits, charsmax(lHits), "%L", id, "HITS")
	formatex(lShots, charsmax(lShots), "%L", id, "SHOTS")
	formatex(lDamage, charsmax(lDamage), "%L", id, "DAMAGE")
	formatex(lEff, charsmax(lEff), "%L", id, "EFF")
	formatex(lAcc, charsmax(lAcc), "%L", id, "ACC")

	ucfirst(lEff)
	ucfirst(lAcc)

	iRankPos = get_user_stats(id, izStats, izBody)
	iLen = formatex(sBuffer, charsmax(sBuffer), "<meta charset=utf-8><body bgcolor=#000000><font color=#FFB000><pre>")
	iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%L %L^n^n", id, (!iMyId || iMyId == id) ? "YOUR" : "PLAYERS", id, "RANK_IS", iRankPos, get_statsnum())
	iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%6s: %d  (%d with hs)^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %0.2f%%^n%6s: %0.2f%%^n^n",
					lKills, izStats[STATSX_KILLS], izStats[STATSX_HEADSHOTS], lDeaths, izStats[STATSX_DEATHS], lHits, izStats[STATSX_HITS], lShots, izStats[STATSX_SHOTS],
					lDamage, izStats[STATSX_DAMAGE], lEff, effec(izStats), lAcc, accuracy(izStats))

	new L_BODY_PART[MAX_BODYHITS][32]

	for (new i = 1; i < sizeof(L_BODY_PART); i++)
	{
		formatex(L_BODY_PART[i], charsmax(L_BODY_PART[]), "%L", id, BODY_PART[i])
	}

	iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%10s:^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d", "HITS",
					L_BODY_PART[HIT_HEAD], izBody[HIT_HEAD], L_BODY_PART[HIT_CHEST], izBody[HIT_CHEST], L_BODY_PART[HIT_STOMACH], izBody[HIT_STOMACH], L_BODY_PART[HIT_LEFTARM], izBody[HIT_LEFTARM], L_BODY_PART[HIT_RIGHTARM],
					izBody[HIT_RIGHTARM], L_BODY_PART[HIT_LEFTLEG], izBody[HIT_LEFTLEG], L_BODY_PART[HIT_RIGHTLEG], izBody[HIT_RIGHTLEG])
}

// Get and format stats.
format_stats(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	new izStats[STATSX_MAX_STATS] = {0, ...}
	new izBody[MAX_BODYHITS]
	new iWeapon, iLen
	new lKills[16], lDeaths[16], lHits[16], lShots[16], lDamage[16], lEff[16], lAcc[16], lWeapon[16]

	formatex(lKills, charsmax(lKills), "%L", id, "KILLS")
	formatex(lDeaths, charsmax(lDeaths), "%L", id, "DEATHS")
	formatex(lHits, charsmax(lHits), "%L", id, "HITS")
	formatex(lShots, charsmax(lShots), "%L", id, "SHOTS")
	formatex(lDamage, charsmax(lDamage), "%L", id, "DAMAGE")
	formatex(lEff, charsmax(lEff), "%L", id, "EFF")
	formatex(lAcc, charsmax(lAcc), "%L", id, "ACC")
	formatex(lWeapon, charsmax(lWeapon), "%L", id, "WEAPON")

	ucfirst(lEff)
	ucfirst(lAcc)

	get_user_wstats(id, 0, izStats, izBody)

	iLen = formatex(sBuffer, charsmax(sBuffer), "<meta charset=utf-8><body bgcolor=#000000><font color=#FFB000><pre>")
	iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%6s: %d  (%d with hs)^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %0.2f%%^n%6s: %0.2f%%^n^n",
					lKills, izStats[STATSX_KILLS], izStats[STATSX_HEADSHOTS], lDeaths, izStats[STATSX_DEATHS], lHits, izStats[STATSX_HITS], lShots, izStats[STATSX_SHOTS],
					lDamage, izStats[STATSX_DAMAGE], lEff, effec(izStats), lAcc, accuracy(izStats))
	iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%-12.12s  %6s  %6s  %6s  %6s  %6s  %4s^n", lWeapon, lKills, lDeaths, lHits, lShots, lDamage, lAcc)

	for (iWeapon = 1; iWeapon < xmod_get_maxweapons() && charsmax(sBuffer) - iLen > 0 ; iWeapon++)
	{
		if (get_user_wstats(id, iWeapon, izStats, izBody))
		{
			xmod_get_wpnname(iWeapon, t_sWpn, charsmax(t_sWpn))
			iLen += formatex(sBuffer[iLen], charsmax(sBuffer) - iLen, "%-12.12s  %6d  %6d  %6d  %6d  %6d  %3.0f%%^n", t_sWpn, izStats[STATSX_KILLS], izStats[STATSX_DEATHS],
							izStats[STATSX_HITS], izStats[STATSX_SHOTS], izStats[STATSX_DAMAGE], accuracy(izStats))
		}
	}
}

// Format round end stats
format_roundend_hudstats(id, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	sBuffer[0] = 0

	// Create round awards.
	if (ShowMostDisruptive)
		add_most_disruptive(id, sBuffer)
	if (ShowBestScore)
		add_best_score(id, sBuffer)

	// Create round score.
	// Compensate HUD message if awards are disabled.
	if (ShowTeamScore || ShowTotalStats)
	{
		if (ShowMostDisruptive && ShowBestScore)
			add(sBuffer, charsmax(sBuffer), "^n^n")
		else if (ShowMostDisruptive || ShowBestScore)
			add(sBuffer, charsmax(sBuffer), "^n^n^n^n")
		else
			add(sBuffer, charsmax(sBuffer), "^n^n^n^n^n^n")

		if (ShowTeamScore)
			add_team_score(id, sBuffer)

		if (ShowTotalStats)
			add_total_stats(id, sBuffer)
	}
}

// Show round end stats. If gametime is zero then use default duration time.
show_roundend_hudstats(id, Float:fGameTime, sBuffer[MAX_BUFFER_LENGTH + 1])
{
	// Bail out if there no HUD stats should be shown
	// for this player or end round stats not created.
	if (!g_izStatsSwitch[id]) return
	if (!sBuffer[0]) return

	// If round end timer is zero clear round end stats.
	if (g_fShowStatsTime == 0.0)
	{
		ClearSyncHud(id, g_HudSync_EndRound)
#if defined STATSX_DEBUG
		log_amx("Clear round end HUD stats for #%d", id)
#endif
	}

	// Set HUD-duration to default or remaining time.
	new Float:fDuration

	if (fGameTime == 0.0)
		fDuration = g_fHUDDuration
	else
	{
		fDuration = g_fShowStatsTime + g_fHUDDuration - fGameTime

		if (fDuration > g_fFreezeTime + g_fFreezeLimitTime)
			fDuration = g_fFreezeTime + g_fFreezeLimitTime
	}

	// Show stats only if more time left than coded minimum.
	if (fDuration >= HUD_MIN_DURATION)
	{
		set_hudtype_endround(fDuration)
		ShowSyncHudMsg(id, g_HudSync_EndRound, "%s", sBuffer)
#if defined STATSX_DEBUG
		log_amx("Show %1.2fs round end HUD stats for #%d", fDuration, id)
#endif
	}
}

// Show round end stats.
show_user_hudstats(id, Float:fGameTime)
{
	// Bail out if there no HUD stats should be shown
	// for this player or user stats timer is zero.
	if (!g_izStatsSwitch[id]) return
	if (g_fzShowUserStatsTime[id] == 0.0) return

	// Set HUD-duration to default or remaining time.
	new Float:fDuration

	if (fGameTime == 0.0)
		fDuration = g_fHUDDuration
	else
	{
		fDuration = g_fzShowUserStatsTime[id] + g_fHUDDuration - fGameTime

		if (fDuration > g_fFreezeTime + g_fFreezeLimitTime)
			fDuration = g_fFreezeTime + g_fFreezeLimitTime
	}

	// Show stats only if more time left than coded minimum.
	if (fDuration >= HUD_MIN_DURATION)
	{
		if (ShowKiller)
		{
			new iKiller

			iKiller = g_izKilled[id][KILLED_KILLER_ID]
			get_kill_info(id, iKiller, g_sBuffer)
			add_attacker_hits(id, iKiller, g_sBuffer)
			set_hudtype_killer(fDuration)
			show_hudmessage(id, "%s", g_sBuffer)
#if defined STATSX_DEBUG
			log_amx("Show %1.2fs %suser HUD k-stats for #%d", fDuration, g_sBuffer[0] ? "" : "no ", id)
#endif
		}

		if (ShowVictims)
		{
			get_victims(id, g_sBuffer)
			set_hudtype_victim(fDuration)
			show_hudmessage(id, "%s", g_sBuffer)
#if defined STATSX_DEBUG
			log_amx("Show %1.2fs %suser HUD v-stats for #%d", fDuration, g_sBuffer[0] ? "" : "no ", id)
#endif
		}

		if (ShowAttackers)
		{
			get_attackers(id, g_sBuffer)
			set_hudtype_attacker(fDuration)
			show_hudmessage(id, "%s", g_sBuffer)
#if defined STATSX_DEBUG
			log_amx("Show %1.2fs %suser HUD a-stats for #%d", fDuration, g_sBuffer[0] ? "" : "no ", id)
#endif
		}
	}
}

//------------------------------------------------------------
// Plugin commands
//------------------------------------------------------------

// Set or get plugin config flags.
public cmdPluginMode(id, level, cid)
{
	if (!cmd_access(id, level, cid, 1))
		return PLUGIN_HANDLED

	if (read_argc() > 1)
		read_argv(1, g_sBuffer, charsmax(g_sBuffer))
	else
		g_sBuffer[0] = 0

	set_plugin_mode(id, g_sBuffer)

	return PLUGIN_HANDLED
}

// Display MOTD stats.
public cmdStatsMe(id)
{
	if (!SayStatsMe)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	format_stats(id, g_sBuffer)
	get_user_name(id, t_sName, charsmax(t_sName))
	show_motd(id, g_sBuffer, t_sName)

	return PLUGIN_CONTINUE
}

// Display MOTD rank.
public cmdRankStats(id)
{
	if (!SayRankStats)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	format_rankstats(id, g_sBuffer)
	get_user_name(id, t_sName, charsmax(t_sName))
	show_motd(id, g_sBuffer, t_sName)

	return PLUGIN_CONTINUE
}

// Display MOTD top15 ranked.
public cmdTop15(id)
{
	if (!SayTop15)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	format_top15(id, g_sBuffer)
	show_motd(id, g_sBuffer, "Top 15")

	return PLUGIN_CONTINUE
}

// Display killer information.
public cmdHp(id)
{
	if (!SayHP)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	new iKiller = g_izKilled[id][KILLED_KILLER_ID]

	format_kill_ainfo(id, iKiller, g_sBuffer)
	client_print(id, print_chat, "* %s", g_sBuffer)

	return PLUGIN_CONTINUE
}

// Display user stats.
public cmdMe(id)
{
	if (!SayMe)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	format_kill_vinfo(id, 0, g_sBuffer)
	client_print(id, print_chat, "* %s", g_sBuffer)

	return PLUGIN_CONTINUE
}

// Display user rank
public cmdRank(id)
{
	if (!SayRank)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
	new iRankPos, iRankMax
	new Float:fEff, Float:fAcc

	iRankPos = get_user_stats(id, izStats, izBody)
	iRankMax = get_statsnum()

	fEff = effec(izStats)
	fAcc = accuracy(izStats)

	client_print(id, print_chat, "* %L", id, "YOUR_RANK_IS", iRankPos, iRankMax, izStats[STATSX_KILLS], izStats[STATSX_HITS], fEff, fAcc)

	return PLUGIN_CONTINUE
}

// Report user weapon status to team.
public cmdReport(id)
{
	if (!SayReport)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	new iWeapon, iClip, iAmmo, iHealth, iArmor

	iWeapon = get_user_weapon(id, iClip, iAmmo)

	if (iWeapon != 0)
		xmod_get_wpnname(iWeapon, t_sWpn, charsmax(t_sWpn))

	iHealth = get_user_health(id)
	iArmor = get_user_armor(id)

	new lWeapon[16]

	formatex(lWeapon, charsmax(lWeapon), "%L", LANG_SERVER, "WEAPON")
	strtolower(lWeapon)

	if (iClip >= 0)
	{
		formatex(g_sBuffer, charsmax(g_sBuffer), "%s: %s, %L: %d/%d, %L: %d, %L: %d", lWeapon, t_sWpn, LANG_SERVER, "AMMO", iClip, iAmmo, LANG_SERVER, "HEALTH", iHealth, LANG_SERVER, "ARMOR", iArmor)
	}
	else
		formatex(g_sBuffer, charsmax(g_sBuffer), "%s: %s, %L: %d, %L: %d", lWeapon, t_sWpn[7], LANG_SERVER, "HEALTH", iHealth, LANG_SERVER, "ARMOR", iArmor)

	engclient_cmd(id, "say_team", g_sBuffer)

	return PLUGIN_CONTINUE
}

// Display team map score
public cmdScore(id)
{
	if (!SayScore)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	save_team_chatscore(id, g_sScore)
	client_print(id, print_chat, "%L: %s", id, "GAME_SCORE", g_sScore)

	return PLUGIN_CONTINUE
}

// Client switch to enable or disable stats announcements.
public cmdSwitch(id)
{
	g_izStatsSwitch[id] = (g_izStatsSwitch[id]) ? 0 : -1
	num_to_str(g_izStatsSwitch[id], t_sText, charsmax(t_sText))
	client_cmd(id, "setinfo _amxstatsx %s", t_sText)

	new lEnDis[32]

	formatex(lEnDis, charsmax(lEnDis), "%L", id, g_izStatsSwitch[id] ? "ENABLED" : "DISABLED")
	client_print(id, print_chat, "* %L", id, "STATS_ANNOUNCE", lEnDis)

	return PLUGIN_CONTINUE
}

// Player stats menu.
public cmdStats(id)
{
	if (!SayStatsAll)
	{
		client_print(id, print_chat, "%L", id, "DISABLED_MSG")
		return PLUGIN_HANDLED
	}

	showStatsMenu(id, g_izUserMenuPosition[id] = 0)

	return PLUGIN_CONTINUE
}

//--------------------------------
// Menu
//--------------------------------

public actionStatsMenu(id, key)
{
	switch (key)
	{
		// Key '1' to '7', execute action on this option
		case 0..6:
		{
			new iOption, iIndex
			iOption = (g_izUserMenuPosition[id] * PPL_MENU_OPTIONS) + key

			if (iOption >= 0 && iOption < 32)
			{
				iIndex = g_izUserMenuPlayers[id][iOption]

				if (is_user_connected(iIndex))
				{
					switch (g_izUserMenuAction[id])
					{
						case 0: format_stats(iIndex, g_sBuffer)
						case 1: format_rankstats(iIndex, g_sBuffer, id)
						default: g_sBuffer[0] = 0
					}

					if (g_sBuffer[0])
					{
						get_user_name(iIndex, t_sName, charsmax(t_sName))
						show_motd(id, g_sBuffer, t_sName)
					}
				}
			}

			showStatsMenu(id, g_izUserMenuPosition[id])
		}
		// Key '8', change action
		case 7:
		{
			g_izUserMenuAction[id]++

			if (g_izUserMenuAction[id] >= MAX_PPL_MENU_ACTIONS)
				g_izUserMenuAction[id] = 0

			showStatsMenu(id, g_izUserMenuPosition[id])
		}
		// Key '9', select next page of options
		case 8: showStatsMenu(id, ++g_izUserMenuPosition[id])
		// Key '10', cancel or go back to previous menu
		case 9:
		{
			if (g_izUserMenuPosition[id] > 0)
				showStatsMenu(id, --g_izUserMenuPosition[id])
		}
	}

	return PLUGIN_HANDLED
}

new g_izUserMenuActionText[MAX_PPL_MENU_ACTIONS][] = {"Show stats", "Show rank stats"}

showStatsMenu(id, iMenuPos)
{
	new iLen, iKeyMask, iPlayers
	new iUserIndex, iMenuPosMax, iMenuOption, iMenuOptionMax

	get_players(g_izUserMenuPlayers[id], iPlayers)
	iMenuPosMax = ((iPlayers - 1) / PPL_MENU_OPTIONS) + 1

	// If menu pos does not excist use last menu (if players has left)
	if (iMenuPos >= iMenuPosMax)
		iMenuPos = iMenuPosMax - 1

	iUserIndex = iMenuPos * PPL_MENU_OPTIONS
	iLen = formatex(g_sBuffer, charsmax(g_sBuffer), "\y%L\R%d/%d^n\w^n", id, "SERVER_STATS", iMenuPos + 1, iMenuPosMax)
	iMenuOptionMax = iPlayers - iUserIndex

	if (iMenuOptionMax > PPL_MENU_OPTIONS)
		iMenuOptionMax = PPL_MENU_OPTIONS

	for (iMenuOption = 0; iMenuOption < iMenuOptionMax; iMenuOption++)
	{
		get_user_name(g_izUserMenuPlayers[id][iUserIndex++], t_sName, charsmax(t_sName))
		iKeyMask |= (1<<iMenuOption)
		iLen += formatex(g_sBuffer[iLen], charsmax(g_sBuffer) - iLen, "%d. %s^n\w", iMenuOption + 1, t_sName)
	}

	iKeyMask |= MENU_KEY_8|MENU_KEY_0
	iLen += formatex(g_sBuffer[iLen], charsmax(g_sBuffer) - iLen, "^n8. %s^n\w", g_izUserMenuActionText[g_izUserMenuAction[id]])

	if (iPlayers > iUserIndex)
	{
		iLen += formatex(g_sBuffer[iLen], charsmax(g_sBuffer) - iLen, "^n9. %L...", id, "MORE")
		iKeyMask |= MENU_KEY_9
	}

	if (iMenuPos > 0)
		iLen += formatex(g_sBuffer[iLen], charsmax(g_sBuffer) - iLen, "^n0. %L", id, "BACK")
	else
		iLen += formatex(g_sBuffer[iLen], charsmax(g_sBuffer) - iLen, "^n0. %L", id, "EXIT")

	show_menu(id, iKeyMask, g_sBuffer, -1, "Server Stats")

	return PLUGIN_HANDLED
}

//------------------------------------------------------------
// Plugin events
//------------------------------------------------------------

// Reset game stats on game start and restart.
public eventStartGame()
{
	read_data(2, t_sText, charsmax(t_sText))

	if (t_sText[6] == 'w')
	{
		read_data(3, t_sText, charsmax(t_sText))
		g_fStartGame = get_gametime() + float(str_to_num(t_sText))
	}
	else
		g_fStartGame = get_gametime()

	return PLUGIN_CONTINUE
}

// Round start
public eventStartRound()
{
	new iTeam, id, i

	new Float:roundtime = get_pcvar_float(g_pRoundTime)
	if (read_data(1) >= floatround(roundtime * 60.0,floatround_floor) || (roundtime == 2.3 && read_data(1) == 137)) // these round too weird for it to work through pawn, have to add an exception for it
	{
#if defined STATSX_DEBUG
		log_amx("Reset round stats")
#endif

		// Reset game stats on game start and restart.
		if (g_fStartGame > 0.0 && g_fStartGame <= get_gametime())
		{
#if defined STATSX_DEBUG
			log_amx("Reset game stats")
#endif
			g_fStartGame = 0.0

			// Clear team and game stats.
			for (iTeam = 0; iTeam < MAX_TEAMS; iTeam++)
			{
				g_izTeamEventScore[iTeam] = 0

				for (i = 0; i < sizeof(g_izTeamGameStats[]); i++)
					g_izTeamGameStats[iTeam][i] = 0
			}

			// Clear game stats, incl '0' that is sum of all users.
			for (id = 0; id <= MaxClients; id++)
			{
				for (i = 0; i < sizeof(g_izUserGameStats[]); i++)
					g_izUserGameStats[id][i] = 0
			}
		}

		// Update team score with "TeamScore" event values and
		// clear team round stats.
		for (iTeam = 0; iTeam < MAX_TEAMS; iTeam++)
		{
			g_izTeamScore[iTeam] = g_izTeamEventScore[iTeam]

			for (i = 0; i < sizeof(g_izTeamRndStats[]); i++)
				g_izTeamRndStats[iTeam][i] = 0
		}

		// Clear user round stats, incl '0' that is sum of all users.
		for (id = 0; id <= MaxClients; id++)
		{
			g_izUserRndName[id][0] = 0

			for (i = 0; i < sizeof(g_izUserRndStats[]); i++)
				g_izUserRndStats[id][i] = 0

			g_fzShowUserStatsTime[id] = 0.0
		}

		// Allow end round stats and reset end round triggered indicator.
		g_iRoundEndTriggered = 0
		g_iRoundEndProcessed = 0
		g_fShowStatsTime = 0.0

		// Update local configuration vars with value in cvars.
		get_config_cvars()
	}

	return PLUGIN_CONTINUE
}

// Reset killer info on round restart.
public eventSpawn(id)
{
	if (!is_user_alive(id))
		return HAM_IGNORED

	new args[1]
	args[0] = id

	if (g_iPluginMode & MODE_HUD_DELAY)
		set_task(0.1, "delay_spawn", 200 + id, args, sizeof(args))
	else
		delay_spawn(args)

	return HAM_IGNORED
}

public delay_spawn(args[])
{
	new id = args[0]
	new Float:fGameTime

	// Show user and score round stats after spawn
#if defined STATSX_DEBUG
	log_amx("Spawn for #%d", id)
#endif
	fGameTime = get_gametime()
	show_user_hudstats(id, fGameTime)

	if (g_izStatsSwitch[id] && g_sAwardAndScore[0])
	{
		format_roundend_hudstats(id, g_sAwardAndScore)
		show_roundend_hudstats(id, fGameTime, g_sAwardAndScore)
	}

	// Reset round stats
	g_izKilled[id][KILLED_KILLER_ID] = 0
	g_izKilled[id][KILLED_KILLER_STATSFIX] = 0
	g_izShowStatsFlags[id] = -1		// Initialize flags
	g_fzShowUserStatsTime[id] = 0.0
	g_izUserAttackerDistance[id] = 0

	for (new i = 1; i <= MaxClients; i++)
		g_izUserVictimDistance[id][i] = 0

	return PLUGIN_CONTINUE
}

// Save killer info on death.
public client_death(killer, victim, wpnindex, hitplace, TK)
{
	// Bail out if no killer.
	if (!killer)
		return PLUGIN_CONTINUE

	if (killer != victim)
	{
		new iaVOrigin[3], iaKOrigin[3]
		new iDistance

		get_user_origin(victim, iaVOrigin)
		get_user_origin(killer, iaKOrigin)

		g_izKilled[victim][KILLED_KILLER_ID] = killer
		g_izKilled[victim][KILLED_KILLER_HEALTH] = get_user_health(killer)
		g_izKilled[victim][KILLED_KILLER_ARMOUR] = get_user_armor(killer)
		g_izKilled[victim][KILLED_KILLER_STATSFIX] = 0

		iDistance = get_distance(iaVOrigin, iaKOrigin)
		g_izUserAttackerDistance[victim] = iDistance
		g_izUserVictimDistance[killer][victim] = iDistance
	}

	g_izKilled[victim][KILLED_TEAM] = get_user_team(victim)
	g_izKilled[victim][KILLED_KILLER_STATSFIX] = 1

	// Display kill stats for the player if round
	// end stats was not processed.
	if (!g_iRoundEndProcessed)
		kill_stats(victim)

	return PLUGIN_CONTINUE
}

// Display hudmessage stats on death.
// This will also update all round and game stats.
// Must be called at least once per round.
kill_stats(id)
{
	// Bail out if user stats timer is non-zero,
	// ie function already called.
	if (g_fzShowUserStatsTime[id] > 0.0)
	{
		return
	}

	new team = get_user_team(id)
	if (team < 1 || team > 2)
	{
		return
	}

	// Flag kill stats displayed for this player.
	g_fzShowUserStatsTime[id] = get_gametime()

	// Add user death stats to user round stats
	new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
	new iTeam, i
	new iKiller

	iKiller = g_izKilled[id][KILLED_KILLER_ID]

	// Get user's team (if dead use the saved team)
	if (iKiller)
		iTeam = g_izKilled[id][KILLED_TEAM] - 1
	else
		iTeam = get_user_team(id) - 1

	get_user_name(id, g_izUserRndName[id], charsmax(g_izUserRndName[]))

	if (get_user_rstats(id, izStats, izBody))
	{
		// Update user's team round stats
		if (iTeam >= 0 && iTeam < MAX_TEAMS)
		{
			for (i = 0; i < sizeof(izStats); i++)
			{
				g_izTeamRndStats[iTeam][i] += izStats[i]
				g_izTeamGameStats[iTeam][i] += izStats[i]
				g_izUserRndStats[0][i] += izStats[i]
				g_izUserGameStats[0][i] += izStats[i]
			}
		}

		// Update user's round stats
		if (g_izUserUserID[id] == get_user_userid(id))
		{
			for (i = 0; i < sizeof(izStats); i++)
			{
				g_izUserRndStats[id][i] += izStats[i]
				g_izUserGameStats[id][i] += izStats[i]
			}
		} else {
			g_izUserUserID[id] = get_user_userid(id)

			for (i = 0; i < sizeof(izStats); i++)
			{
				g_izUserRndStats[id][i] = izStats[i]
				g_izUserGameStats[id][i] = izStats[i]
			}
		}

	}	// endif (get_user_rstats())

	// Report stats in the chat section, if player is killed.
	if (KillerChat && iKiller && iKiller != id)
	{
		if (format_kill_ainfo(id, iKiller, g_sBuffer))
		{
			client_print(id, print_chat, "* %s", g_sBuffer)
			format_kill_vinfo(id, iKiller, g_sBuffer)
		}

		client_print(id, print_chat, "* %s", g_sBuffer)
	}

	// Display player stats info.
#if defined STATSX_DEBUG
	log_amx("Kill stats for #%d", id)
#endif
	show_user_hudstats(id, 0.0)
}

public eventEndRound()
{
	// Update local configuration vars with value in cvars.
	get_config_cvars()

	// If first end round event in the round, calculate team score.
	if (!g_iRoundEndTriggered)
	{
		read_data(2, t_sText, charsmax(t_sText))

		if (t_sText[7] == 't')			// Terrorist wins
			g_izTeamScore[0]++
		else if (t_sText[7] == 'c')		// CT wins
			g_izTeamScore[1]++
	}

	set_task(0.3, "ERTask", 997)

	return PLUGIN_CONTINUE
}

public ERTask()
{
	// Flag round end triggered.
	g_iRoundEndTriggered = 1

	// Display round end stats to all players.
	endround_stats()
}

endround_stats()
{
	// Bail out if end round stats has already been processed
	// or round end not triggered.
	if (g_iRoundEndProcessed || !g_iRoundEndTriggered)
		return

	new iaPlayers[MAX_PLAYERS], iPlayer, iPlayers, id

	get_players(iaPlayers, iPlayers)

	// Display attacker & victim list for all living players.
	// This will also update all round and game stats for all players
	// not killed.
#if defined STATSX_DEBUG
	log_amx("End round stats")
#endif

	// Get and save round end stats time.
	g_fShowStatsTime = get_gametime()

	for (iPlayer = 0; iPlayer < iPlayers; iPlayer++)
	{
		id = iaPlayers[iPlayer]

		if (g_fzShowUserStatsTime[id] == 0.0)
		{
			kill_stats(id)
		}

		if (!g_izStatsSwitch[id])
			continue

		format_roundend_hudstats(id, g_sAwardAndScore)
		show_roundend_hudstats(id, 0.0, g_sAwardAndScore)
	}

	// Flag round end processed.
	g_iRoundEndProcessed = 1
}

public eventTeamScore()
{
	new sTeamID[1 + 1], iTeamScore
	read_data(1, sTeamID, charsmax(sTeamID))
	iTeamScore = read_data(2)
	g_izTeamEventScore[(sTeamID[0] == 'C') ? 1 : 0] = iTeamScore

	return PLUGIN_CONTINUE
}

public eventIntermission()
{
	if (EndPlayer || EndTop15)
		set_task(1.0, "end_game_stats", 900)
}

public end_game_stats()
{
	new iaPlayers[MAX_PLAYERS], iPlayer, iPlayers, id

	if (EndPlayer)
	{
		get_players(iaPlayers, iPlayers)

		for (iPlayer = 0; iPlayer < iPlayers; iPlayer++)
		{
			id = iaPlayers[iPlayer]

			if (!g_izStatsSwitch[id])
				continue	// Do not show any stats

			cmdStatsMe(id)
		}
	}
	else if (EndTop15)
	{
		get_players(iaPlayers, iPlayers)
		format_top15(0, g_sBuffer)

		for (iPlayer = 0; iPlayer < iPlayers; iPlayer++)
		{
			id = iaPlayers[iPlayer]

			if (!g_izStatsSwitch[id])
				continue	// Do not show any stats

			show_motd(id, g_sBuffer, "Top 15")
		}
	}

	return PLUGIN_CONTINUE
}

public eventSpecMode(id)
{
	new sData[12]
	read_data(2, sData, charsmax(sData))
	g_izSpecMode[id] = (sData[10] == '2')

	return PLUGIN_CONTINUE
}

public eventShowRank(id)
{
	if (SpecRankInfo && g_izSpecMode[id])
	{
		new iPlayer = read_data(2)

		if (is_user_connected(iPlayer))
		{
			new izStats[STATSX_MAX_STATS], izBody[MAX_BODYHITS]
			new iRankPos, iRankMax

			get_user_name(iPlayer, t_sName, charsmax(t_sName))

			iRankPos = get_user_stats(iPlayer, izStats, izBody)
			iRankMax = get_statsnum()

			set_hudtype_specmode()
			ShowSyncHudMsg(id, g_HudSync_SpecInfo, "%L", id, "X_RANK_IS", t_sName, iRankPos, iRankMax)
		}
	}

	return PLUGIN_CONTINUE
}

public client_connect(id)
{
	if (ShowStats)
	{
		get_user_info(id, "_amxstatsx", t_sText, charsmax(t_sText))
		g_izStatsSwitch[id] = (t_sText[0]) ? str_to_num(t_sText) : -1
	}
	else
		g_izStatsSwitch[id] = 0

	g_izKilled[id][KILLED_KILLER_ID] = 0
	g_izKilled[id][KILLED_KILLER_STATSFIX] = 0
	g_izShowStatsFlags[id] = 0		// Clear all flags
	g_fzShowUserStatsTime[id] = 0.0

	return PLUGIN_CONTINUE
}
