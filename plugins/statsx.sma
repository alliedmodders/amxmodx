/************************************************************
  AMX mod X script. Copyright (C) 2002-2004, XAD.
  $Id: statsx.sma, v 0.9.8 2004-Mar-29
 ************************************************************/

/*************************************************************

StatsX AMX-plugin v0.9.8.
Based on "stats.sma" original written by OLO.

This file is provided as is (no warranties).

=============================================================

Description:
------------
This AMX-plugin will display game stats as HUD and chat messages.

Example of stats are: killing stats, most damage, best score, 
accuracy, efficiency, attackers list, victims list, team scores.

All effeciency stats are calculated per game, not per round.
The accuracy stats are calculated per round, except for "/score"
which is per game.

Plugin support players switching teams and awarded players 
disconnecting from server before round end. It also supports
game restart, ie all stats are resetted.

Plugin also support players switching of the stats announcements
on the HUD. The setting is a client parameter and therefore
saved between playing sessions.


Server Config:
--------------

  Duration of HUD-statistics. (Default 12 sec.)
    amx_statsx_duration < time in seconds >

  HUD-statistics display limit relative round freeze end.
  Negative time will clear the HUD-statstics before the
  round freeze time has ended. (Default -2 sec.)
    amx_statsx_freeze < time in seconds >

  When activating this plugin the Stats Settings Plugin,
  "statscfg", should also be activated. 
  Activating the Stats Settings Plugin allows configuration
  to be done either with client GUI menus or in server
  configuration files.
    amx_statscfg < "on" or "off" > < option >

    StatsX options:
      "KillerChat"     - show killer stats in the chat section
      "ShowAttackers"  - show attackers on HUD (deactivated)
      "ShowVictims"    - show victims on HUD (deactivated)
      "ShowKiller"     - show killer on HUD
      "ShowTeamScore"  - shows team score at round end (deactivated)
      "ShowTotalStats" - shows round total stats (deactivated)
      "ShowBestScore"  - shows rounds best scored player
      "ShowMostDisruptive" 
                       - shows rounds most disruptive player
      "EndPlayer"      - display player stats MOTD at map end
      "EndTop15"       - display top15 MOTD at the end of map
      "SayHP"          - allow for say /hp
      "SayStatsMe"     - allow for say /statsme
      "SayRankStats"   - allow for say /rankstats
      "SayMe"          - allow for say /me
      "SayRank"        - allow for say /rank
      "SayReport"      - allow for say /report
      "SayScore"       - allow for say /score
      "SayTop15"       - allow for say /top15
      "SayStatsAll"    - allow for say /stats
      "SayMiscFunc"    - allow for say /timeleft, /thetime, 
                         /currentmap, /ff
      "ShowStats"      - client HUD-stats switched on by default
      "ShowDistHS"     - show distance and HS in attackers and
                         victims HUD lists
      "SpecRankInfo"   - displays rank info when spectating

  Stats plugin options (flags) to define what to display to
  the clients. 
  NOTE! To deactivate all options set flags to " " 
  (one quoted space).
    amx_statsx_mode < flags >
        "a" - delay (0.01s) HUD stats on HUD reset


Client Commands:
----------------

  Display info about your killer in the chat section.
    say /hp

  Display info about your stats, on the current map, 
  in a MOTD window.
    say /statsme

  Display your rank stats, in a MOTD window.
    say /rankstats

  Display your current round hit-stats in the chat section.
    say /me

  Display your rank in the chat section.
    say /rank

  Display the game score and stats in the chat section.
    say /score
    
  Display players current weapon status, as a say team command.
    say /report
    
  Display the 15 highest ranked players in a MOTD window.
    say /top15

  Display all players stats and rank in a MOTD window. The
  displayed player is selection from a menu.
    say /stats

  Display time left on the map, as a say command.
    say /timeleft
  
  Display the current time, as a say command.
    say /thetime
  
  Display the current map, as a say command.
    say /currentmap
    
  Display the current status on friendly fire flag, 
  as a say command.
    say /ff
    
  Switch on or off all HUD-stats announcements ("_amxstatsx").
    say /switch


=============================================================

Requirements/Limitations:
-------------------------
  - AMX Mod X v0.16: Tested.
  - Replaces "stats.amx" by OLO.
  - Requires the AMX module "csstats_amx".
  - You are recommended to not run any other death or 
    end-round plugins.
  - If HUD-statistics are activated it is recommended that 
    other HUD-messages are deactivated as these otherwise 
    will disrupt HUD-statistics, such as PTB's. This is a
    HalfLife-engine limitation.
  - It is not recommended to use "mp_freezetime" less than
    1.0 sec.


Installation:
-------------
  - Copy the StatsX file "statsx.amx" to the AMX plugins 
    folder.
  - In the AMX Mod X plugin configuration file ("plugins.ini"),
    replace the line "stats.amx" with "statsx.amx".
  - Add StatsX cvar to the server or AMX Mod X configuration file.
    If menu configuration will not be used then also add 
    the "amx_statscfg" commands to one of the configuration
    files.
  - Restart server. Normally changing map is sufficient but
    restarting the server is the "safe" procedure.
  - If menu configuration is used then reconfigure the 
    stats settings using the "amx_statscfgmenu" command.


Tips:
-----
  NOTE! All statistics collection is done by the module
    "csstats_amx" and questions or issues should be directed
    to the module author.

  - To reset the player statistics in AMX Mod X 0.16, enter 
    "csstats_reset 1" in the server consol. At the next 
    mapchange the stats will be reset and the cvar 
    "csstats_reset" will be set back to "0". If you put
    this command in the server.cfg the stats will be reset
    at every mapchange.
  - In AMX Mod X 0.16 statistics are by default only saved if the
    difference between kills and deaths are greater than 0.
    

Comments:
---------
  This code is based on/parts taken from OLO's stats v0.8.
  - Added "endround" and some other functionality from
    StatsMe 2.7 source code and config files. 
  - This plugin has been updated with new functionality
    added to "stats" (by OLO), v0.8 to v0.9.6. 
  - New functions and configuration flags have been added.
  - Original "/rank" is changed to "/rankstats".
  - Attackers/victims is displayed before next round end.
  - Last shot and kill included in all stats.
  - Changed HUD number on killer stats to "2" to make 
    round end stats work with PTB.
  - All statistics collection is done by the module
    "csstats_mm" and questions or issues should be directed
    to the module author.

  Money is not supported in "say /report" (requires fun-module).

=============================================================

Modlog:
-------

0.8       2002-xx-xx  OLO  Original "stats.sma" version 0.8.
                           (c) Copyright 2002, OLO
                           This file is provided as is (no waranties).
0.8a      2002-12-xx  XAD  First modified version. Added file header.
0.8c      2002-12-20  XAD  First production tested version.
0.9a      2003-01-23  XAD  Recompiled for AMX 0.9.0.
0.9d      2003-02-14  XAD  Added player weapon stats, timeleft, thetime,
                           and currentmap.
0.9e      2003-02-15  XAD  Fixed missed friendly fire command registration.
                           Fixed divide by zero in best score and most 
                           disruptive.
0.9g      2003-02-19  XAD  Changed "/rank" to "/rankstats".
                           Added a new "/rank" command to report rank as 
                           chat message.
                           Changed "/hp" to be in the chat section instead 
                           on HUD.
                           Fixed some misspellings and changed from "say" 
                           to server chat.
                           Added "/me", your current round stats.
                           Added "/switch", client command to disable all
                           stats announcements.
0.9h      2003-02-26  XAD  Added player-rank menu (from stats/0.9.2).
                           Added config menu (from stats/0.9.3).
                           Increased global buffer to 2048 (fix in stats/0.9.3)
                           Changed to set tab equal to 4 spaces (from 6).
0.9i      2003-03-11  XAD  Added efficiency and accuracy to top15, stats, and
                           rankstats MOTD (thanks Coxton).
                           Removed deny message on the non-stats related 
                           commands so these can be overriden by other scripts.
0.9i.1    2003-03-20  XAD  Fixed bug in "/statsme" (thanks NiLuJe).
0.9i.2    2003-04-05  XAD  Fixed bug in "/statsme" (thanks NiLuJe, doh).
0.9j      2003-06-05  XAD  Support client HUD stats switched off by default,
                           mode flag "s" (thanks Troopa). 
                           Added killing distance to "/me", accuracy and HS-kills
                           for each weapons in "/statsme", killing distance and
                           HS on victim and killer HUD stats (thanks versus666).
                           Added HS-kills to "/statsme" and "/rankstats"
                           (thanks Jim66).
                           Added team score prediction to remove 0.5 sec. task
                           delay on round end stats.
                           Changed death-stats to be triggered 0.25 sec. after
                           death instead of "late" hit-event, to fix problems
                           with 3:rd party modules (thanks server_sgz).
                           Added support for HUD statistics over HUD reset with
                           time limits relative to the round freeze end.
                           Added cvar for HUD time parameters.
                           Special thanks to server_sgz for test work.
0.9.6b    2003-09-12  XAD  Adapted to AMX 0.9.6h.
                           Added config menu ("statscfg" for AMX 0.9.6) and 
                           removed old ("statsset" from AMX 0.9.3).
                           Changed to html in MOTD (thanks ootoaoo).
                           Deactivated victim list, attacker list, team score,
                           and total HUD-stats. The killer, best score, most
                           disruptive HUD-stats have been reduced in size to
                           be within the 79 character HUD-message limit.
                           Thanks for debug reports: BMJ.
0.9.8a    2003-11-30  XAD  Updated for CS release 2003-nov-26.
                           Restored all HUD-messages changed in v0.9.6b, as
                           HUD-message limit has increased to 480 characters.
                           Reactivated last-hit fix, as AMX core function
                           "CS_DeathMsg" doesn't always work.
0.9.8a1a  2004-01-03  XAD  Removed show_roundend_hudstats from kill_stats.
                           Reduced HUD_MIN_DURATION from 1 sec to 0.2 sec.
0.9.8b    2004-03-29  XAD  Updated for AMX Mod X 0.16.
                           Changed the code to support non-zero initializing
                           Small compilers.
                           Removed support for 79 char limit HUD-message.

*************************************************************/

new PLUGIN_AUTHOR[]  = "XAD (OLO)";
new PLUGIN_NAME[]    = "STATSX";
new STRING_VERSION[] = "0.9.8b";

//--------------------------------

#include <amxmodx>
#include <amxmisc>
#include <csstats>

//--------------------------------

// Uncomment to activate log debug messages.
//#define STATSX_DEBUG
//#define STATSX_DEBUG2

// Comment to activate new DeathMsg process (if fixed).
#define STATSX_OLD_DEATHMSG

// HUD statistics duration in seconds (minimum 1.0 seconds).
new HUD_DURATION_CVAR[]         = "amx_statsx_duration";
new HUD_DURATION[]              = "12.0";

// HUD statistics stop relative freeze end in seconds.
// To stop before freeze end use a negative value.
new HUD_FREEZE_LIMIT_CVAR[]     = "amx_statsx_freeze";
new HUD_FREEZE_LIMIT[]          = "-2.0";

// HUD statistics minimum duration, in seconds, to trigger the display logic.
#define HUD_MIN_DURATION    0.2

// Config plugin constants.
#define MODE_HUD_DELAY      0   // Make a 0.01 sec delay on HUD reset process.

// You can also manualy enable or disable these options by setting them to 1
// For example:
// public ShowAttackers = 1
// However amx_statscfg command is recommended

public KillerChat           = 0; // displays killer hp&ap to victim console 
                                 // and screen

public ShowAttackers        = 0; // shows attackers
public ShowVictims          = 0; // shows victims
public ShowKiller           = 0; // shows killer
public ShowTeamScore        = 0; // shows team score at round end
public ShowTotalStats       = 0; // shows round total stats
public ShowBestScore        = 0; // shows rounds best scored player
public ShowMostDisruptive   = 0; // shows rounds most disruptive player

public EndPlayer            = 0; // displays player stats at the end of map
public EndTop15             = 0; // displays top15 at the end of map

public SayHP                = 0; // displays information about user killer
public SayStatsMe           = 0; // displays user's stats and rank
public SayRankStats         = 0; // displays user's rank stats
public SayMe                = 0; // displays user's stats
public SayRank              = 0; // displays user's rank
public SayReport            = 0; // report user's weapon status to team
public SayScore             = 0; // displays team's map score
public SayTop15             = 0; // displays first 15 players
public SayStatsAll          = 0; // displays all players stats and rank
public SayMiscFunc          = 0; // displays timeleft, thetime, currentmap, ff

public ShowStats            = 0; // set client HUD-stats switched off by default
public ShowDistHS           = 0; // show distance and HS in attackers and
                                 // victims HUD lists
public ShowFullStats        = 0; // show full HUD stats (more than 78 chars)

public SpecRankInfo         = 0; // displays rank info when spectating

// Standard Contstants.
#define MAX_TEAMS               2
#define MAX_PLAYERS             32 + 1

#define MAX_NAME_LENGTH         31
#define MAX_WEAPON_LENGTH       31
#define MAX_TEXT_LENGTH         255
#define MAX_BUFFER_LENGTH       2047

// User messages.
new g_sDisabledMsg[] = "Server has disabled that option";

// User stats parms id
#define STATS_KILLS             0
#define STATS_DEATHS            1
#define STATS_HS                2
#define STATS_TKS               3
#define STATS_SHOTS             4
#define STATS_HITS              5
#define STATS_DAMAGE            6

// Global player flags.

new BODY_PART[8][] = { 
	"wholebody",
	"head",
	"chest",
	"stomach",
	"leftarm",
	"rightarm",
	"leftleg",
	"rightleg"
}

// Killer information, save killer info at the time when player is killed.
#define KILLED_KILLER_ID        0   // Killer userindex/user-ID
#define KILLED_KILLER_HEALTH    1   // Killer's health
#define KILLED_KILLER_ARMOUR    2   // Killer's armour
#define KILLED_TEAM             3   // Killer's team
#define KILLED_KILLER_STATSFIX  4   // Fix to register the last hit/kill

new g_izKilled[MAX_PLAYERS][5];

// Menu variables and configuration
#define MAX_PPL_MENU_ACTIONS    2   // Number of player menu actions
#define PPL_MENU_OPTIONS        7   // Number of player options per displayed menu

new g_iPluginMode                                   = 0;

new g_izUserMenuPosition[MAX_PLAYERS]               = { 0, ... };
new g_izUserMenuAction[MAX_PLAYERS]                 = { 0, ... };
new g_izUserMenuPlayers[MAX_PLAYERS][32];

new g_izSpecMode[MAX_PLAYERS]                       = { 0, ... };

new g_izShowStatsFlags[MAX_PLAYERS]                 = { 0, ... };
new g_izStatsSwitch[MAX_PLAYERS]                    = { 0, ... };
new Float:g_fzShowUserStatsTime[MAX_PLAYERS]        = { 0.0, ... };
new Float:g_fShowStatsTime                          = 0.0;
new Float:g_fFreezeTime                             = 0.0;
new Float:g_fFreezeLimitTime                        = 0.0;
new Float:g_fHUDDuration                            = 0.0;

new g_iRoundEndTriggered                            = 0;
new g_iRoundEndProcessed                            = 0;

new Float:g_fStartGame                              = 0.0;
new g_izTeamScore[MAX_TEAMS]                        = { 0, ... };
new g_izTeamEventScore[MAX_TEAMS]                   = { 0, ... };
new g_izTeamRndStats[MAX_TEAMS][8];
new g_izTeamGameStats[MAX_TEAMS][8];
new g_izUserUserID[MAX_PLAYERS]                     = { 0, ... };
new g_izUserAttackerDistance[MAX_PLAYERS]           = { 0, ... };
new g_izUserVictimDistance[MAX_PLAYERS][MAX_PLAYERS];
new g_izUserRndName[MAX_PLAYERS][MAX_NAME_LENGTH];
new g_izUserRndStats[MAX_PLAYERS][8];
new g_izUserGameStats[MAX_PLAYERS][8];

// Common buffer to improve performance, as Small always zero-initializes all vars
new g_sBuffer[MAX_BUFFER_LENGTH+1]                  = "";
new g_sScore[MAX_TEXT_LENGTH+1]                     = "";
new g_sAwardAndScore[MAX_BUFFER_LENGTH+1]           = "";

new t_sText[MAX_TEXT_LENGTH+1]                      = "";
new t_sName[MAX_NAME_LENGTH+1]                      = "";
new t_sWpn[MAX_WEAPON_LENGTH+1]                     = "";

//--------------------------------
//  Initialize
//--------------------------------

public plugin_init() {
	// Register plugin.
	log_message( "[%s] %s v%s", PLUGIN_NAME, "StatsX", STRING_VERSION );
	register_plugin( "StatsX", STRING_VERSION, PLUGIN_AUTHOR );

	// Register events.
	register_event( "TextMsg", "eventStartGame", "a", 
	                "2=#Game_Commencing", "2=#Game_will_restart_in" );
	register_event( "ResetHUD", "eventResetHud", "be" );
	register_event( "RoundTime", "eventStartRound", "bc" );

	#if defined STATSX_OLD_DEATHMSG
	register_event( "DeathMsg", "eventDeathMsg", "a" );
	register_event( "Damage", "eventDamage", "b", "2!0", "3=0", "4!0" );
	#else
	register_event( "CS_DeathMsg", "eventCSDeathMsg", "a" );
	#endif

	register_event( "SendAudio", "eventEndRound", "a", 
	                "2=%!MRAD_terwin", "2=%!MRAD_ctwin", "2=%!MRAD_rounddraw" );
	register_event( "TeamScore", "eventTeamScore", "a" );
	register_event( "30", "eventIntermission", "a" );

	register_event( "TextMsg", "eventSpecMode", "bd", "2&ec_Mod" );
	register_event( "StatusValue", "eventShowRank", "bd", "1=2" );

	// Register commands.
	register_clcmd( "say /hp", "cmdHp", 0, "- display info. about your killer (chat)" );
	register_clcmd( "say /statsme", "cmdStatsMe", 0, "- display your stats (MOTD)" );
	register_clcmd( "say /rankstats", "cmdRankStats", 0, "- display your server stats (MOTD)" );
	register_clcmd( "say /me", "cmdMe", 0, "- display current round stats (chat)" );
	register_clcmd( "say /score", "cmdScore", 0, "- display last score (chat)" );
	register_clcmd( "say /rank", "cmdRank", 0, "- display your rank (chat)" );
	register_clcmd( "say /report", "cmdReport", 0, "- display waepon status (say_team)" );

	register_clcmd( "say /top15", "cmdTop15", 0, "- display top 15 players (MOTD)" );
	register_clcmd( "say /stats", "cmdStats", 0, "- display players stats (menu/MOTD)" );

	register_clcmd( "say /timeleft", "cmdTimeLeft", 0, "- display time left on map (say)" );
	register_clcmd( "say /thetime", "cmdTheTime", 0, "- display the time (say)" );
	register_clcmd( "say /currentmap", "cmdCurrentMap", 0, "- display current map (say)" );
	register_clcmd( "say /ff", "cmdFf", 0, "- display friendly fire status (say)" );

	register_clcmd( "say /switch", "cmdSwitch", 0, "- switch client's stats on or off" );

	// Register menus.
	register_menucmd( register_menuid("Server Stats"), 1023, "actionStatsMenu" );

	// Register special configuration setting and default value.
	register_srvcmd( "amx_statsx_mode", "cmdPluginMode", ADMIN_CFG, "<flags> - sets plugin options" );

	#if defined STATSX_DEBUG2
	register_clcmd( "say /hudtest", "cmdHudTest" );
	#endif

	register_cvar( HUD_DURATION_CVAR, HUD_DURATION );
	register_cvar( HUD_FREEZE_LIMIT_CVAR, HUD_FREEZE_LIMIT );

	// Init buffers and some global vars.
	g_sBuffer[0] = 0;
	save_team_chatscore();

	// Update local configuration vars with value in cvars.
	get_config_cvars();

	return PLUGIN_CONTINUE;
}

new g_addStast[] = "amx_statscfg add ^"%s^" %s"

public plugin_cfg(){
	server_cmd( g_addStast, "Show killer hp&ap","KillerChat" );

	server_cmd( g_addStast, "Show Attackers", "ShowAttackers" );
	server_cmd( g_addStast, "Show Victims", "ShowVictims" );
	server_cmd( g_addStast, "Show killer", "ShowKiller" );
	server_cmd( g_addStast, "Show Team Score", "ShowTeamScore" );
	server_cmd( g_addStast, "Show Total Stats", "ShowTotalStats" );
	server_cmd( g_addStast, "Show Best Score", "ShowBestScore" );
	server_cmd( g_addStast, "Show Most Disruptive", "ShowMostDisruptive" );

	server_cmd( g_addStast, "HUD-stats default", "ShowStats" );
	server_cmd( g_addStast, "Dist&HS in HUD lists", "ShowDistHS" );
	
	server_cmd( g_addStast, "Stats at the end of map", "EndPlayer");
	server_cmd( g_addStast, "Top15 at the end of map", "EndTop15" );

	server_cmd( g_addStast, "Say /hp", "SayHP" );
	server_cmd( g_addStast, "Say /statsme", "SayStatsMe" );
	server_cmd( g_addStast, "Say /rankstats", "SayRankStats" );
	server_cmd( g_addStast, "Say /me", "SayMe" );
	server_cmd( g_addStast, "Say /rank", "SayRank" );
	server_cmd( g_addStast, "Say /report", "SayReport" );
	server_cmd( g_addStast, "Say /score", "SayScore" );
	server_cmd( g_addStast, "Say /top15", "SayTop15" );
	server_cmd( g_addStast, "Say /stats", "SayStatsAll" );
	server_cmd( g_addStast, "Misc say commands", "SayMiscFunc" );
	
	server_cmd( g_addStast, "Spec. Rank Info", "SpecRankInfo" );
}

// Set hudmessage format.
set_hudtype_killer( Float:fDuration )
	set_hudmessage( 220, 80, 0, 0.05, 0.15, 0, 6.0, fDuration, (fDuration>=g_fHUDDuration)?1.0:0.0, 1.0, 2 );
set_hudtype_endround( Float:fDuration )
	set_hudmessage( 100, 200, 0, 0.05, 0.55, 0, 0.02, fDuration, (fDuration>=g_fHUDDuration)?1.0:0.0, 1.0, 1 );
set_hudtype_attacker( Float:fDuration )
	set_hudmessage( 220, 80, 0, 0.55, 0.35, 0, 6.0, fDuration, (fDuration>=g_fHUDDuration)?1.0:0.0, 1.0, 3 );
set_hudtype_victim( Float:fDuration )
	set_hudmessage( 0, 80, 220, 0.55, 0.60, 0, 6.0, fDuration, (fDuration>=g_fHUDDuration)?1.0:0.0, 1.0, 4 );
set_hudtype_specmode() {
	set_hudmessage( 255, 255, 255, 0.02, 0.87, 2, 0.05, 0.1, 0.01, 3.0, 1);
}

#if defined STATSX_DEBUG2
public cmdHudTest( id ) {
	new i, iLen;
	iLen = 0;
	for( i = 1; i < 20; i++ )
		iLen += format( g_sBuffer[iLen], MAX_BUFFER_LENGTH - iLen, "....x....1....x....2....x....3....x....4....x....^n" );
	set_hudtype_killer( 50.0 );
	show_hudmessage( id, g_sBuffer );
}
#endif

// Stats formulas
Float:accuracy( izStats[8] ) {
	if( !izStats[STATS_SHOTS] ) return ( 0.0 );
	return ( 100.0 * float( izStats[STATS_HITS] ) / float( izStats[STATS_SHOTS] ) );
}

Float:effec( izStats[8] ) {
	if( !izStats[STATS_KILLS] ) return ( 0.0 );
	return ( 100.0 * float( izStats[STATS_KILLS] ) / float( izStats[STATS_KILLS] + izStats[STATS_DEATHS] ) );
}

// Distance formula (metric)
Float:distance( iDistance ) {
	return float( iDistance ) * 0.0254;
}

// Get plugin config flags.
set_plugin_mode( id, sFlags[] ){
	if( sFlags[0] )
		g_iPluginMode = read_flags( sFlags );
	get_flags( g_iPluginMode, t_sText, MAX_TEXT_LENGTH );
	console_print( id, "^"amx_statsx_mode^" set to ^"%s^"", t_sText );
	return g_iPluginMode;
}

// Get config parameters.
get_config_cvars() {
	g_fFreezeTime = get_cvar_float("mp_freezetime");
	if( g_fFreezeTime < 0.0 ) g_fFreezeTime = 0.0;

	g_fHUDDuration = get_cvar_float( HUD_DURATION_CVAR );
	if( g_fHUDDuration < 1.0 ) g_fHUDDuration = 1.0;

	g_fFreezeLimitTime = get_cvar_float( HUD_FREEZE_LIMIT_CVAR );

	return;
}

// Get and format attackers header and list.
get_attackers( id, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new izStats[8], izBody[8];
	new iAttacker;
	new iFound, iLen;
	new iMaxPlayer = get_maxplayers();

	iFound = 0;
	sBuffer[0] = 0;
	
	// Get and format header. Add killing attacker statistics if user is dead.
	// Make sure shots is greater than zero or division by zero will occur.	
	// To print a '%', 4 of them must done in a row.
	izStats[STATS_SHOTS] = 0;
	iAttacker = g_izKilled[id][KILLED_KILLER_ID];
	if( iAttacker )
		get_user_astats( id, iAttacker, izStats, izBody );
	if( izStats[STATS_SHOTS] && ShowFullStats ) {
		get_user_name( iAttacker, t_sName, MAX_NAME_LENGTH );
		iLen = format( sBuffer, MAX_BUFFER_LENGTH, "Attackers -- %s -- %0.2f%%%% acc.:^n",
		               t_sName, accuracy( izStats ) );
	}
	else
		iLen = format( sBuffer, MAX_BUFFER_LENGTH, "Attackers:^n" );

	// Get and format attacker list.
	for( iAttacker = 1; iAttacker <= iMaxPlayer; iAttacker++ ) {
		if( get_user_astats( id, iAttacker, izStats, izBody, t_sWpn, MAX_WEAPON_LENGTH ) ) {
			iFound = 1;
			get_user_name( iAttacker, t_sName, 32 );
			if( izStats[STATS_KILLS] ) {
				if( !ShowDistHS )
					iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
					                "%s -- %d hit(s) / %d dmg / %s^n",
					                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE], 
					                t_sWpn );
				else if( izStats[STATS_HS] )
					iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
					                "%s -- %d hit(s) / %d dmg / %s / %0.0f m / HS^n",
					                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE], 
					                t_sWpn, distance(g_izUserAttackerDistance[id]) );
				else
					iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
					                "%s -- %d hit(s) / %d dmg / %s / %0.0f m^n",
					                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE], 
					                t_sWpn, distance(g_izUserAttackerDistance[id]) );
			}
			else
				iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
				                "%s -- %d hit(s) / %d dmg^n",
				                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE] );
		}
	}
	if( !iFound )
		sBuffer[0] = 0;
	return iFound;
}


// Get and format victims header and list
get_victims( id, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new izStats[8], izBody[8];
	new iVictim;
	new iFound, iLen;
	new iMaxPlayer = get_maxplayers();

	iFound = 0;
	sBuffer[0] = 0;
	
	// Get and format header.
	// Make sure shots is greater than zero or division by zero will occur.	
	// To print a '%', 4 of them must done in a row.
	izStats[STATS_SHOTS] = 0;
	get_user_vstats( id, 0, izStats, izBody );
	if( izStats[STATS_SHOTS] )
		iLen = format( sBuffer, MAX_BUFFER_LENGTH, "Victims -- %0.2f%%%% acc.:^n",
		               accuracy( izStats ) );
	else
		iLen = format( sBuffer, MAX_BUFFER_LENGTH, "Victims:^n" );

	for( iVictim = 1; iVictim <= iMaxPlayer; iVictim++ ) {
		if( get_user_vstats( id, iVictim, izStats, izBody, t_sWpn, MAX_WEAPON_LENGTH ) ) {
			iFound = 1;
			get_user_name( iVictim, t_sName, MAX_NAME_LENGTH );
			if( izStats[STATS_DEATHS] ) {
				if( !ShowDistHS )
					iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
					                "%s -- %d hit(s) / %d dmg / %s^n",
					                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE], 
					                t_sWpn );
				else if( izStats[STATS_HS] )
					iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
					                "%s -- %d hit(s) / %d dmg / %s / %0.0f m / HS^n",
					                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE], 
					                t_sWpn, distance(g_izUserVictimDistance[id][iVictim]) );
				else
					iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
					                "%s -- %d hit(s) / %d dmg / %s / %0.0f m^n",
					                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE], 
					                t_sWpn, distance(g_izUserVictimDistance[id][iVictim]) );
			}
			else
				iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
				                "%s -- %d hit(s) / %d dmg^n",
				                t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE] );
		}
	}
	if( !iFound )
		sBuffer[0] = 0;

	return iFound;
}


// Get and format kill info.
get_kill_info( id, iKiller, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new iFound, iLen;

	iFound = 0;
	sBuffer[0] = 0;

	if( iKiller && iKiller != id ) {
		new izAStats[8], izABody[8], izVStats[8], iaVBody[8];

		iFound = 1;
		get_user_name( iKiller, t_sName, MAX_NAME_LENGTH );

		izAStats[STATS_HITS]   = 0;
		izAStats[STATS_DAMAGE] = 0;
		t_sWpn[0] = 0;
		get_user_astats( id, iKiller, izAStats, izABody, t_sWpn, MAX_WEAPON_LENGTH );

		izVStats[STATS_HITS]   = 0;
		izVStats[STATS_DAMAGE] = 0;
		get_user_vstats( id, iKiller, izVStats, iaVBody );

		iLen  = format( sBuffer, MAX_BUFFER_LENGTH,
		                "%s killed you with %s^nfrom distance of %0.2f meters.^n",
		                t_sName, t_sWpn, distance(g_izUserAttackerDistance[id]) );
		iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
		                "He did %d damage to you with %d hit(s)^nand still has %dhp and %dap.^n",
		                izAStats[STATS_DAMAGE], izAStats[STATS_HITS],	
		                g_izKilled[id][KILLED_KILLER_HEALTH], g_izKilled[id][KILLED_KILLER_ARMOUR] );
		iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
		                "You did %d damage to him with %d hit(s).^n",
		                izVStats[STATS_DAMAGE], izVStats[STATS_HITS] );
	}
	return iFound;
}


// Get and format most disruptive.
add_most_disruptive( sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new id, iMaxDamageId, iMaxDamage, iMaxHeadShots;
	
	iMaxDamageId  = 0;
	iMaxDamage    = 0;
	iMaxHeadShots = 0;

	// Find player.
	for( id = 1; id < MAX_PLAYERS; id++ ) {
		if( g_izUserRndStats[id][STATS_DAMAGE] >= iMaxDamage &&
		    ( g_izUserRndStats[id][STATS_DAMAGE] > iMaxDamage || g_izUserRndStats[id][STATS_HS] > iMaxHeadShots ) ) {
			iMaxDamageId  = id;
			iMaxDamage    = g_izUserRndStats[id][STATS_DAMAGE];
			iMaxHeadShots = g_izUserRndStats[id][STATS_HS];
		}
	}

	// Format statistics.
	if( iMaxDamageId ) {
		id = iMaxDamageId;
		new Float:fGameEff = effec( g_izUserGameStats[id] );
		new Float:fRndAcc = accuracy( g_izUserRndStats[id] );
		format( t_sText, MAX_TEXT_LENGTH,
		        "Most damage done by: %s^n%d hit(s) / %d dmg -- %0.2f%%%% eff. / %0.2f%%%% acc.^n",
		        g_izUserRndName[id], g_izUserRndStats[id][STATS_HITS], 
		        iMaxDamage, fGameEff, fRndAcc );
		add( sBuffer, MAX_BUFFER_LENGTH, t_sText );
	}
	return iMaxDamageId;
}

// Get and format best score.
add_best_score( sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new id, iMaxKillsId, iMaxKills, iMaxHeadShots;

	iMaxKillsId   = 0;
	iMaxKills     = 0;
	iMaxHeadShots = 0;

	// Find player
	for( id = 1; id < MAX_PLAYERS; id++ ) {
		if( g_izUserRndStats[id][STATS_KILLS] >= iMaxKills &&
		    ( g_izUserRndStats[id][STATS_KILLS] > iMaxKills || g_izUserRndStats[id][STATS_HS] > iMaxHeadShots ) ) {
			iMaxKillsId   = id;
			iMaxKills     = g_izUserRndStats[id][STATS_KILLS];
			iMaxHeadShots = g_izUserRndStats[id][STATS_HS];
		}
	}

	// Format statistics.
	if( iMaxKillsId ) {
		id = iMaxKillsId;
		new Float:fGameEff = effec( g_izUserGameStats[id] );
		new Float:fRndAcc = accuracy( g_izUserRndStats[id] );
		format( t_sText, MAX_TEXT_LENGTH,
		        "Best score: %s^n%d kill(s) / %d hs -- %0.2f%%%% eff. / %0.2f%%%% acc.^n",
		        g_izUserRndName[id], iMaxKills, iMaxHeadShots, fGameEff, fRndAcc );
		add( sBuffer, MAX_BUFFER_LENGTH, t_sText );
	}
	return iMaxKillsId;
}


// Get and format team score.
add_team_score( sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new Float:fzMapEff[MAX_TEAMS], Float:fzMapAcc[MAX_TEAMS], Float:fzRndAcc[MAX_TEAMS];

	// Calculate team stats
	for( new iTeam = 0; iTeam < MAX_TEAMS; iTeam++ ) {
		fzMapEff[iTeam] = effec( g_izTeamGameStats[iTeam] );
		fzMapAcc[iTeam] = accuracy( g_izTeamGameStats[iTeam] );
		fzRndAcc[iTeam] = accuracy( g_izTeamRndStats[iTeam] );
	}

	// Format round team stats, MOTD
	format( t_sText, MAX_TEXT_LENGTH,
	        "TERRORIST %i / %0.2f%%%% eff. / %0.2f%%%% acc.^nCT %i / %0.2f%%%% eff. / %0.2f%%%% acc.^n",
	        g_izTeamScore[0], fzMapEff[0], fzRndAcc[0], g_izTeamScore[1], fzMapEff[1], fzRndAcc[1] );
	add( sBuffer, MAX_BUFFER_LENGTH, t_sText );
}


// Get and format team stats, chat version
save_team_chatscore() {
	new Float:fzMapEff[MAX_TEAMS], Float:fzMapAcc[MAX_TEAMS], Float:fzRndAcc[MAX_TEAMS];

	// Calculate team stats
	for( new iTeam = 0; iTeam < MAX_TEAMS; iTeam++ ) {
		fzMapEff[iTeam] = effec( g_izTeamGameStats[iTeam] );
		fzMapAcc[iTeam] = accuracy( g_izTeamGameStats[iTeam] );
		fzRndAcc[iTeam] = accuracy( g_izTeamRndStats[iTeam] );
	}

	// Format game team stats, chat
	format( g_sScore, MAX_BUFFER_LENGTH,
	        "TERRORIST %i / %0.2f%%%% eff. / %0.2f%%%% acc.  --  CT %i / %0.2f%%%% eff. / %0.2f%%%% acc.",
	        g_izTeamScore[0], fzMapEff[0], fzMapAcc[0], g_izTeamScore[1], fzMapEff[1], fzMapAcc[1] );
}


// Get and format total stats.
add_total_stats( sBuffer[MAX_BUFFER_LENGTH+1] ) {
	format( t_sText, MAX_TEXT_LENGTH,
	        "Total: %d kill(s) / %d hs -- %d hit(s) / %d shot(s)^n",
	        g_izUserRndStats[0][STATS_KILLS], g_izUserRndStats[0][STATS_HS],
	        g_izUserRndStats[0][STATS_HITS], g_izUserRndStats[0][STATS_SHOTS] );
	add( sBuffer, MAX_BUFFER_LENGTH, t_sText );
}


// Get and format a user's list of body hits from an attacker.
add_attacker_hits( id, iAttacker, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new iFound = 0;
	if( iAttacker && iAttacker != id ) {
		new izStats[8], izBody[8], iLen;

		izStats[STATS_HITS] = 0;
		get_user_astats( id, iAttacker, izStats, izBody );

		if( izStats[STATS_HITS] ) {
			iFound = 1;
			iLen = strlen( sBuffer );
			get_user_name( iAttacker, t_sName, MAX_NAME_LENGTH );
			iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
			                "%s hits you in:^n", t_sName );
			for( new i = 1; i < 8; i++ ) {
				if( !izBody[i] ) continue;
				iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen, 
				                "%s: %d^n", BODY_PART[i], izBody[i] );
			}
		}
	}
	return iFound;
}


// Get and format killed stats: killer hp, ap, hits.
format_kill_ainfo( id, iKiller, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new iFound = 0;
	if( iKiller && iKiller != id ) {
		new izStats[8], izBody[8];
		new iLen;
		iFound = 1;
		get_user_name( iKiller, t_sName, MAX_NAME_LENGTH );

		izStats[STATS_HITS] = 0;
		get_user_astats( id, iKiller, izStats, izBody, t_sWpn, MAX_WEAPON_LENGTH );

		iLen = format( sBuffer, MAX_BUFFER_LENGTH,
		               "Killed by %s with %s @ %0.0fm (%dhp, %dap) >>", t_sName, t_sWpn,
		               distance(g_izUserAttackerDistance[id]),
		               g_izKilled[id][KILLED_KILLER_HEALTH], g_izKilled[id][KILLED_KILLER_ARMOUR] );

		if( izStats[STATS_HITS] ) {
			for( new i = 1; i < 8; i++ ) {
				if( !izBody[i] ) continue;
				iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
				                " %s: %d", BODY_PART[i], izBody[i] );
			}
		}
		else
			iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
			                " no hits" );
	}
	else
		copy( sBuffer, MAX_BUFFER_LENGTH, "You have no killer..." );
	return iFound;
}


// Get and format killed stats: hits, damage on killer.
format_kill_vinfo( id, iKiller, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new iFound = 0;
	new izStats[8];
	new izBody[8];
	new iLen;

	izStats[STATS_HITS] = 0;
	izStats[STATS_DAMAGE] = 0;
	get_user_vstats( id, iKiller, izStats, izBody );

	if( iKiller && iKiller != id ) {
		iFound = 1;
		get_user_name( iKiller, t_sName, MAX_NAME_LENGTH );
		iLen = format( sBuffer, MAX_BUFFER_LENGTH,
		               "You hit %s %d time(s), %d damage >>",
		               t_sName, izStats[STATS_HITS], izStats[STATS_DAMAGE] );
	}
	else
		iLen = format( sBuffer, MAX_BUFFER_LENGTH,
		               "Last result: %d hit(s), %d damage >>",
		               izStats[STATS_HITS], izStats[STATS_DAMAGE] );

	if( izStats[STATS_HITS] ) {
		for( new i = 1; i < 8; i++ ) {
			if( !izBody[i] ) continue;
			iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
			                " %s: %d", BODY_PART[i], izBody[i] );
		}
	}
	else
		iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
		                " no hits" );
	return iFound;
}


// Get and format top 15.
format_top15( sBuffer[MAX_BUFFER_LENGTH+1] ){
	new iMax = get_statsnum();
	new izStats[8], izBody[8];
	new iLen = 0;

	if( iMax > 15 )
		iMax = 15;
	
	iLen = format( sBuffer, MAX_BUFFER_LENGTH, 
	                "<body bgcolor=#000000><font color=#FFB000><pre>" );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%2s %-22.22s %6s %6s %6s %6s %4s %4s %4s^n",
	                "#", "Nick", "Kills", "Deaths", "Hits", "Shots", "HS", "Eff.", "Acc." );
	for( new i = 0; i < iMax && MAX_BUFFER_LENGTH - iLen > 0; i++ ) {
		get_stats( i, izStats, izBody, t_sName, MAX_NAME_LENGTH );
		iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
		                "%2d %-22.22s %6d %6d %6d %6d %4d %3.0f%% %3.0f%%^n", i+1, t_sName,
		                izStats[STATS_KILLS], izStats[STATS_DEATHS], izStats[STATS_HITS], 
		                izStats[STATS_SHOTS], izStats[STATS_HS],
		                effec( izStats ), accuracy( izStats ) );
	}
}


// Get and format rank stats.
format_rankstats( id, sBuffer[MAX_BUFFER_LENGTH+1], iMyId=0 ) {
	new izStats[8] = { 0, ... };
	new izBody[8];
	new iRankPos, iLen;
	iRankPos = get_user_stats( id, izStats, izBody );
	iLen = format( sBuffer, MAX_BUFFER_LENGTH, 
	                "<body bgcolor=#000000><font color=#FFB000><pre>" );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%s rank is %d of %d^n^n", 
    	            (!iMyId||iMyId==id)?"Your":"Players", iRankPos, get_statsnum() );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%s rank is %d of %d^n^n", 
	                (!iMyId||iMyId==id)?"Your":"Players", iRankPos, get_statsnum() );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%6s: %d  (%d with hs)^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %0.2f%%^n%6s: %0.2f%%^n^n",
	                "Kills", izStats[STATS_KILLS], izStats[STATS_HS], 
	                "Deaths", izStats[STATS_DEATHS], "Hits", izStats[STATS_HITS], 
	                "Shots", izStats[STATS_SHOTS], "Damage", izStats[STATS_DAMAGE], 
	                "Eff.", effec( izStats ), "Acc.", accuracy( izStats ) );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%10s:^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d^n%10s: %d",
	                "HITS", BODY_PART[1], izBody[1], BODY_PART[2], izBody[2], 
	                BODY_PART[3], izBody[3], BODY_PART[4], izBody[4], BODY_PART[5], izBody[5], 
	                BODY_PART[6], izBody[6], BODY_PART[7], izBody[7]);
	return;
}


// Get and format stats.
format_stats( id, sBuffer[MAX_BUFFER_LENGTH+1] ) {
	new izStats[8] = { 0, ... };
	new izBody[8];
	new iWeapon, iLen;
	get_user_wstats( id, 0, izStats, izBody );
	iLen = format( sBuffer, MAX_BUFFER_LENGTH, 
	                "<body bgcolor=#000000><font color=#FFB000><pre>" );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%6s: %d  (%d with hs)^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %d^n%6s: %0.2f%%^n%6s: %0.2f%%^n^n",
	                "Kills", izStats[STATS_KILLS], izStats[STATS_HS], 
	                "Deaths", izStats[STATS_DEATHS], "Hits", izStats[STATS_HITS], 
	                "Shots", izStats[STATS_SHOTS], "Damage", izStats[STATS_DAMAGE], 
	                "Eff.", effec( izStats ), "Acc.", accuracy( izStats ) );
	iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
	                "%-12.12s  %6s  %6s  %6s  %6s  %6s  %4s^n",
	                "Weapon", "Kills", "Deaths", "Hits", "Shots", "Damage", "Acc." );
	for( iWeapon = 1; iWeapon < 31 && MAX_BUFFER_LENGTH - iLen > 0 ; iWeapon++ ) {
		if( get_user_wstats( id, iWeapon, izStats, izBody ) ) {
			get_weaponname( iWeapon, t_sWpn, MAX_WEAPON_LENGTH );
			iLen += format( sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
			                "%-12.12s  %6d  %6d  %6d  %6d  %6d  %3.0f%%^n",
			                t_sWpn[7], izStats[STATS_KILLS], izStats[STATS_DEATHS], 
			                izStats[STATS_HITS], izStats[STATS_SHOTS], 
			                izStats[STATS_DAMAGE], accuracy( izStats ) );
		}
	}
	return;
}

// Show round end stats. If gametime is zero then use default duration time. 
show_roundend_hudstats( id, Float:fGameTime ) {
	
	// Bail out if there no HUD stats should be shown
	// for this player or end round stats not created.
	if( !g_izStatsSwitch[id] ) return;
	if( !g_sAwardAndScore[0] ) return;

	// If round end timer is zero clear round end stats.
	if( g_fShowStatsTime == 0.0 ) {
		set_hudtype_endround( 0.05 );
		show_hudmessage( id, "" );
		#if defined STATSX_DEBUG
		log_message( "[%s] Clear round end HUD stats for #%i", PLUGIN_NAME, id );	
		#endif
		return;
	}

	// Set HUD-duration to default or remaining time.
	new Float:fDuration;
	if( fGameTime == 0.0 )
		fDuration = g_fHUDDuration;
	else {
		fDuration = g_fShowStatsTime + g_fHUDDuration - fGameTime;
		if( fDuration > g_fFreezeTime + g_fFreezeLimitTime )
			fDuration = g_fFreezeTime + g_fFreezeLimitTime;
	}
	// Show stats only if more time left than coded minimum.
	if( fDuration >= HUD_MIN_DURATION ) {
		set_hudtype_endround( fDuration );
		show_hudmessage( id, g_sAwardAndScore );
		#if defined STATSX_DEBUG
		log_message( "[%s] Show %1.2fs round end HUD stats for #%i", PLUGIN_NAME, fDuration, id );	
		#endif
	}
	return;
}


// Show round end stats.
show_user_hudstats( id, Float:fGameTime ) {
	
	// Bail out if there no HUD stats should be shown
	// for this player or user stats timer is zero.
	if( !g_izStatsSwitch[id] ) return;
	if( g_fzShowUserStatsTime[id] == 0.0 ) return;

	// Set HUD-duration to default or remaining time.
	new Float:fDuration;
	if( fGameTime == 0.0 )
		fDuration = g_fHUDDuration;
	else {
		fDuration = g_fzShowUserStatsTime[id] + g_fHUDDuration - fGameTime;
		if( fDuration > g_fFreezeTime + g_fFreezeLimitTime )
			fDuration = g_fFreezeTime + g_fFreezeLimitTime;
	}
	// Show stats only if more time left than coded minimum.
	if( fDuration >= HUD_MIN_DURATION ) {
		if( ShowKiller ) {
			new iKiller;
			iKiller = g_izKilled[id][KILLED_KILLER_ID];
			get_kill_info( id, iKiller, g_sBuffer );
			add_attacker_hits( id, iKiller, g_sBuffer );
			set_hudtype_killer( fDuration );
			show_hudmessage( id, g_sBuffer );
			#if defined STATSX_DEBUG
			log_message( "[%s] Show %1.2fs %suser HUD k-stats for #%i", PLUGIN_NAME, fDuration, g_sBuffer[0]?"":"no ", id );
			#endif
		}
		if( ShowVictims ) {
			get_victims( id, g_sBuffer );
			set_hudtype_victim( fDuration );
			show_hudmessage( id, g_sBuffer );
			#if defined STATSX_DEBUG
			log_message( "[%s] Show %1.2fs %suser HUD v-stats for #%i", PLUGIN_NAME, fDuration, g_sBuffer[0]?"":"no ", id );
			#endif
		}
		if( ShowAttackers ) {
			get_attackers( id, g_sBuffer );
			set_hudtype_attacker( fDuration );
			show_hudmessage( id, g_sBuffer );
			#if defined STATSX_DEBUG
			log_message( "[%s] Show %1.2fs %suser HUD a-stats for #%i", PLUGIN_NAME, fDuration, g_sBuffer[0]?"":"no ", id );
			#endif
		}
	}
	return;
}


//------------------------------------------------------------
//  Plugin commands
//------------------------------------------------------------

// Set or get plugin config flags.
public cmdPluginMode( id, level, cid ) {
	if( !cmd_access( id, level, cid, 1 ) ) 
		return PLUGIN_HANDLED;
	if( read_argc() > 1 )
		read_argv( 1, g_sBuffer, MAX_BUFFER_LENGTH );
	else
		g_sBuffer[0] = 0;
	set_plugin_mode( id, g_sBuffer );
	return PLUGIN_HANDLED;
}

// Display MOTD stats.
public cmdStatsMe( id ) {
	if( !SayStatsMe ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	format_stats( id, g_sBuffer );
	get_user_name( id, t_sName, MAX_NAME_LENGTH );
	show_motd( id, g_sBuffer, t_sName );
	return PLUGIN_CONTINUE;
}

// Display MOTD rank.
public cmdRankStats( id ) {
	if( !SayRankStats ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	format_rankstats( id, g_sBuffer );
	get_user_name( id, t_sName, MAX_NAME_LENGTH );
	show_motd( id, g_sBuffer, t_sName );
	return PLUGIN_CONTINUE;
}

// Display MOTD top15 ranked.
public cmdTop15( id ) {
	if( !SayTop15 ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	format_top15( g_sBuffer );
	show_motd( id, g_sBuffer, "Top 15" );
	return PLUGIN_CONTINUE;
}

// Display killer information.
public cmdHp( id ) {
	if( !SayHP ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	new iKiller = g_izKilled[id][KILLED_KILLER_ID];
	format_kill_ainfo( id, iKiller, g_sBuffer );
	client_print( id, print_chat, "* %s", g_sBuffer );
	return PLUGIN_CONTINUE;
}

// Display user stats.
public cmdMe( id ) {
	if( !SayMe ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	format_kill_vinfo( id, 0, g_sBuffer );
	client_print( id, print_chat, "* %s", g_sBuffer );
	return PLUGIN_CONTINUE;
}

// Display user rank
public cmdRank( id ) {
	if( !SayRank ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	new izStats[8], izBody[8];
	new iRankPos, iRankMax;
	new Float:fEff, Float:fAcc;
	iRankPos = get_user_stats( id, izStats, izBody );
	iRankMax = get_statsnum();
	fEff = effec( izStats );
	fAcc = accuracy( izStats );
	client_print( id, print_chat,
	              "* Your rank is %d of %d with %d kill(s), %d hit(s), %0.2f%%%% eff. and %0.2f%%%% acc.", 
	              iRankPos, iRankMax, izStats[STATS_KILLS], izStats[STATS_HITS],
	              fEff, fAcc );
	return PLUGIN_CONTINUE;
}

// Report user weapon status to team.
public cmdReport( id ) { 
	if( !SayReport ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	new iWeapon, iClip, iAmmo, iHealth, iArmor;
	iWeapon = get_user_weapon( id, iClip, iAmmo ); 
	get_weaponname( iWeapon, t_sWpn, MAX_WEAPON_LENGTH );
	iHealth = get_user_health( id ); 
	iArmor = get_user_armor( id );
	if( iClip >= 0 )
		format( g_sBuffer, MAX_BUFFER_LENGTH,
		        "weapon: %s, ammo: %d/%d, health: %d, armor: %d", 
		        t_sWpn[7], iClip, iAmmo, iHealth, iArmor ); 
	else
		format( g_sBuffer, MAX_BUFFER_LENGTH,
		        "weapon: %s, health: %d, armor: %d", 
		        t_sWpn[7], iHealth, iArmor ); 
	engclient_cmd( id, "say_team", g_sBuffer );
	return PLUGIN_CONTINUE;
} 

// Display team map score
public cmdScore( id ) {
	if( !SayScore ) {
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	client_print( id, print_chat, "Game score: %s", g_sScore );
	return PLUGIN_CONTINUE;
}

// Display time left on map
public cmdTimeLeft( id ) {
	if( !SayMiscFunc )
		return PLUGIN_CONTINUE;
	new iTimeLeft = get_timeleft();
	if( iTimeLeft )
		client_print( 0, print_chat,
		              "Time remaining:  %02d:%02d", iTimeLeft / 60, iTimeLeft % 60 );
	else
		client_print( 0, print_chat, "* No Time Limit *" );
	return PLUGIN_CONTINUE;
}

// Display the time.
public cmdTheTime( id ) {
	if( !SayMiscFunc )
		return PLUGIN_CONTINUE;
	get_time( "%H:%M:%S", g_sBuffer, MAX_BUFFER_LENGTH );
	client_print( 0, print_chat, "The time:  %s", g_sBuffer );
	return PLUGIN_CONTINUE;
}

// Display current map name.
public cmdCurrentMap( id ) {
	if( !SayMiscFunc )
		return PLUGIN_CONTINUE;
	get_mapname( g_sBuffer, MAX_BUFFER_LENGTH );
	client_print( 0, print_chat, "Played map:  %s", g_sBuffer );
	return PLUGIN_CONTINUE;
}

// Display friendly fire status.
public cmdFf( id ) {
	if( !SayMiscFunc )
		return PLUGIN_CONTINUE;
	client_print( 0, print_chat, "Friendly fire:  %s",
	              get_cvar_num( "mp_friendlyfire" ) ? "ON" : "OFF" );
	return PLUGIN_CONTINUE;
}

// Client switch to enable or disable stats announcements.
public cmdSwitch( id ) {
	g_izStatsSwitch[id] = ( g_izStatsSwitch[id] ) ? 0 : -1 ;
	num_to_str( g_izStatsSwitch[id], t_sText, MAX_TEXT_LENGTH );
	client_cmd( id, "setinfo _amxstatsx %s", t_sText );
	client_print( id, print_chat, "* You have %s stats announcements",
	              g_izStatsSwitch[id] ? "enabled" : "disabled" );
	return PLUGIN_CONTINUE;
}

// Player stats menu.
public cmdStats( id ) {
	if ( !SayStatsAll ){
		client_print( id, print_chat, g_sDisabledMsg );
		return PLUGIN_HANDLED;
	}
	showStatsMenu( id, g_izUserMenuPosition[id]=0 );
	return PLUGIN_CONTINUE;
}


//--------------------------------
//  Menu
//--------------------------------

public actionStatsMenu( id, key ) {
	switch( key ) {
		// Key '1' to '7', execute action on this option
		case 0..6: {
			new iOption, iIndex;
			iOption = ( g_izUserMenuPosition[id] * PPL_MENU_OPTIONS ) + key;
			if( iOption >= 0 && iOption < 32 ) {
				iIndex = g_izUserMenuPlayers[id][iOption];
				if( is_user_connected( iIndex ) ){
					switch( g_izUserMenuAction[id] ) {
						case 0:  format_stats( iIndex, g_sBuffer );
						case 1:  format_rankstats( iIndex, g_sBuffer, id );
						default: g_sBuffer[0] = 0;
					}
					if( g_sBuffer[0] ) {
						get_user_name( iIndex, t_sName, MAX_NAME_LENGTH );
						show_motd( id, g_sBuffer, t_sName );
					}
				}
			}
			showStatsMenu( id, g_izUserMenuPosition[id] );
		}
		// Key '8', change action
		case 7: {
			g_izUserMenuAction[id]++;
			if( g_izUserMenuAction[id] >= MAX_PPL_MENU_ACTIONS )
				g_izUserMenuAction[id] = 0;
			showStatsMenu( id, g_izUserMenuPosition[id] );
		}
		// Key '9', select next page of options
		case 8:
			showStatsMenu( id, ++g_izUserMenuPosition[id] );
		// Key '10', cancel or go back to previous menu
		case 9: {
			if( g_izUserMenuPosition[id] > 0 )
				showStatsMenu( id, --g_izUserMenuPosition[id] );
		}
	}
	return PLUGIN_HANDLED;
}

new g_izUserMenuActionText[MAX_PPL_MENU_ACTIONS][]  = { "Show stats", "Show rank stats" };

showStatsMenu( id, iMenuPos ) {
	new iLen, iKeyMask, iPlayers;
	new iUserIndex, iMenuPosMax, iMenuOption, iMenuOptionMax;
	get_players( g_izUserMenuPlayers[id], iPlayers );
	iMenuPosMax = (( iPlayers - 1 ) / PPL_MENU_OPTIONS ) + 1;
	// If menu pos does not excist use last menu (if players has left)
	if ( iMenuPos >= iMenuPosMax )
		iMenuPos = iMenuPosMax - 1;
	iUserIndex = iMenuPos * PPL_MENU_OPTIONS;
	iLen = format( g_sBuffer, MAX_BUFFER_LENGTH, "\yServer Stats\R%d/%d^n\w^n",
	               iMenuPos + 1, iMenuPosMax );
	iMenuOptionMax = iPlayers - iUserIndex;
	if( iMenuOptionMax > PPL_MENU_OPTIONS ) 
		iMenuOptionMax = PPL_MENU_OPTIONS;
	for( iMenuOption = 0; iMenuOption < iMenuOptionMax; iMenuOption++ ) {
		get_user_name( g_izUserMenuPlayers[id][iUserIndex++], t_sName, MAX_NAME_LENGTH );
		iKeyMask |= (1<<iMenuOption);
		iLen += format( g_sBuffer[iLen], MAX_BUFFER_LENGTH - iLen,
		                "%d. %s^n\w", iMenuOption + 1, t_sName );
	}
	iKeyMask |= MENU_KEY_8|MENU_KEY_0;
	iLen += format( g_sBuffer[iLen], MAX_BUFFER_LENGTH - iLen, "^n8. %s^n\w",
	                g_izUserMenuActionText[ g_izUserMenuAction[id] ] );
	if( iPlayers > iUserIndex ) {
		iLen += copy( g_sBuffer[iLen], MAX_BUFFER_LENGTH - iLen, "^n9. More..." );
		iKeyMask |= MENU_KEY_9;
	}
	if( iMenuPos > 0 )
		iLen += copy( g_sBuffer[iLen], MAX_BUFFER_LENGTH - iLen, "^n0. Back" );
	else
		iLen += copy( g_sBuffer[iLen], MAX_BUFFER_LENGTH - iLen, "^n0. Exit" );
	show_menu( id, iKeyMask, g_sBuffer );
	return PLUGIN_HANDLED;
}


//------------------------------------------------------------
//  Plugin events
//------------------------------------------------------------

// Reset game stats on game start and restart.
public eventStartGame() {
	read_data( 2, t_sText, MAX_TEXT_LENGTH );
	if( t_sText[6] == 'w' ) {
		read_data( 3, t_sText, MAX_TEXT_LENGTH );
		g_fStartGame = get_gametime() + float( str_to_num( t_sText ) );
	}
	else
		g_fStartGame = get_gametime();
	return PLUGIN_CONTINUE;
}

// Round start
public eventStartRound() {
	new iTeam, id, i;
	new iRoundTime;

	iRoundTime = read_data( 1 );
	if ( iRoundTime >= get_cvar_float("mp_roundtime") * 60 ) {
		#if defined STATSX_DEBUG
		log_message( "[%s] Reset round stats", PLUGIN_NAME );	
		#endif

		// Reset game stats on game start and restart.
		if( g_fStartGame > 0.0 && g_fStartGame <= get_gametime() ) {
			#if defined STATSX_DEBUG
			log_message( "[%s] Reset game stats", PLUGIN_NAME );	
			#endif

			g_fStartGame = 0.0;

			// Clear team and game stats.
			for( iTeam = 0; iTeam < MAX_TEAMS; iTeam++ ) {
				g_izTeamEventScore[iTeam] = 0;
				for( i = 0; i < 8; i++ )
					g_izTeamGameStats[iTeam][i] = 0;
			}

			// Clear game stats, incl '0' that is sum of all users.
			for( id = 0; id < MAX_PLAYERS; id++ ) {
				for( i = 0; i < 8; i++ )
					g_izUserGameStats[id][i] = 0;
			}
		}

		// Update team score with "TeamScore" event values and
		// clear team round stats.
		for( iTeam = 0; iTeam < MAX_TEAMS; iTeam++ ) {
			g_izTeamScore[iTeam] = g_izTeamEventScore[iTeam];
			for( i = 0; i < 8; i++ )
				g_izTeamRndStats[iTeam][i] = 0;
		}

		// Clear user round stats, incl '0' that is sum of all users.
		for( id = 0; id < MAX_PLAYERS; id++ ) {
			g_izUserRndName[id][0] = 0;
			for( i = 0; i < 8; i++ )
				g_izUserRndStats[id][i] = 0;
			g_fzShowUserStatsTime[id] = 0.0;
		}
		
		// Allow end round stats and reset end round triggered indicator.
		g_iRoundEndTriggered = 0;
		g_iRoundEndProcessed = 0;
		g_fShowStatsTime = 0.0;

		// Update local configuration vars with value in cvars.
		get_config_cvars();
	}

	return PLUGIN_CONTINUE;
}

// Reset killer info on round restart.
public eventResetHud( id ) {
	new args[1];
	args[0] = id;
	if( g_iPluginMode & MODE_HUD_DELAY )
		set_task( 0.01, "delay_resethud", 200 + id, args, 1 );
	else
		delay_resethud( args );
	return PLUGIN_CONTINUE;
}

public delay_resethud( args[] ) {
	new id = args[0];
	new Float:fGameTime
	
	// Show user and score round stats after HUD-reset
	#if defined STATSX_DEBUG
	log_message( "[%s] Reset HUD for #%i", PLUGIN_NAME, id );	
	#endif
	fGameTime = get_gametime();
	show_user_hudstats( id, fGameTime );
	show_roundend_hudstats( id, fGameTime );
	
	// Reset round stats
	g_izKilled[id][KILLED_KILLER_ID] = 0;
	g_izKilled[id][KILLED_KILLER_STATSFIX] = 0;
	g_izShowStatsFlags[id] = -1;  // Initialize flags
	g_fzShowUserStatsTime[id] = 0.0;
	g_izUserAttackerDistance[id] = 0;
	for( new i = 0; i < MAX_PLAYERS; i++ )
		g_izUserVictimDistance[id][i] = 0;
	return PLUGIN_CONTINUE;
}

#if defined STATSX_OLD_DEATHMSG

// Save killer info on death.
public eventDeathMsg() {
	new iKiller = read_data( 1 );
	new iVictim = read_data( 2 );

	// Bail out if no killer.
	if( !iKiller ) return PLUGIN_CONTINUE;

	if( iKiller != iVictim ) {
		new iaVOrigin[3], iaKOrigin[3];
		new iDistance;
		get_user_origin( iVictim, iaVOrigin );
		get_user_origin( iKiller, iaKOrigin );
		g_izKilled[iVictim][KILLED_KILLER_ID] = iKiller;
		g_izKilled[iVictim][KILLED_KILLER_HEALTH] = get_user_health( iKiller );
		g_izKilled[iVictim][KILLED_KILLER_ARMOUR] = get_user_armor( iKiller );
		g_izKilled[iVictim][KILLED_KILLER_STATSFIX] = 0;
		
		iDistance = get_distance( iaVOrigin, iaKOrigin );
		g_izUserAttackerDistance[iVictim] = iDistance;
		g_izUserVictimDistance[iKiller][iVictim] = iDistance;
	}
	g_izKilled[iVictim][KILLED_TEAM] = get_user_team( iVictim );
	new args[1];
	args[0] = iVictim;
	set_task( 0.25, "delay_damage", 100 + iVictim, args, 1 );

	return PLUGIN_CONTINUE;
}

public delay_damage( args[] ) {
	new id = args[0];

	// Display stats to killed player if player
	// has not already been processed.
	if( !g_izKilled[id][KILLED_KILLER_STATSFIX] ) {
		g_izKilled[id][KILLED_KILLER_STATSFIX] = 1;

		// Display round end stats to all players if
		// round end has already been triggered.
		if( g_iRoundEndTriggered )
			endround_stats();
		// Display kill stats for the player if round
		// end stats was not processed.
		if( !g_iRoundEndProcessed )
			kill_stats( id );
	}
}

// Trigger death stats processing.
public eventDamage( id ) {
	// Bail out if player not killed or if player
	// player has already been processed.
	if( !g_izKilled[id][KILLED_KILLER_ID] || g_izKilled[id][KILLED_KILLER_STATSFIX] )
		return PLUGIN_CONTINUE;

	// Remove task if not alreay done and process.
	remove_task( 100 + id );

	// Process player deaths.
	new izData[1];
	izData[0] = id;
	delay_damage( izData );

	return PLUGIN_CONTINUE;
}

#endif  // if defined STATSX_OLD_DEATHMSG


#if !defined STATSX_OLD_DEATHMSG

// Save killer info on death.
public eventCSDeathMsg() {
	new iKiller = read_data( 1 );
	new iVictim = read_data( 2 );

	// Bail out if no killer.
	if( !iKiller ) return PLUGIN_CONTINUE;

	if( iKiller != iVictim ) {
		new iaVOrigin[3], iaKOrigin[3];
		new iDistance;
		get_user_origin( iVictim, iaVOrigin );
		get_user_origin( iKiller, iaKOrigin );
		g_izKilled[iVictim][KILLED_KILLER_ID] = iKiller;
		g_izKilled[iVictim][KILLED_KILLER_HEALTH] = get_user_health( iKiller );
		g_izKilled[iVictim][KILLED_KILLER_ARMOUR] = get_user_armor( iKiller );
		g_izKilled[iVictim][KILLED_KILLER_STATSFIX] = 0;
		
		iDistance = get_distance( iaVOrigin, iaKOrigin );
		g_izUserAttackerDistance[iVictim] = iDistance;
		g_izUserVictimDistance[iKiller][iVictim] = iDistance;
	}
	g_izKilled[iVictim][KILLED_TEAM] = get_user_team( iVictim );
	g_izKilled[iVictim][KILLED_KILLER_STATSFIX] = 1;

	// Display kill stats for the player if round
	// end stats was not processed.
	if( !g_iRoundEndProcessed )
		kill_stats( iVictim );

	return PLUGIN_CONTINUE;
}

#endif

// Display hudmessage stats on death.
// This will also update all round and game stats.
// Must be called at least once per round.
kill_stats( id ) {

	// Bail out if user stats timer is non-zero, 
	// ie function already called.
	if( g_fzShowUserStatsTime[id] > 0.0 ) return;

	// Flag kill stats displayed for this player.
	g_fzShowUserStatsTime[id] = get_gametime();

	// Add user death stats to user round stats
	new izStats[8], izBody[8];
	new iTeam, i;
	new iKiller;

	iKiller = g_izKilled[id][KILLED_KILLER_ID];

	// Get user's team (if dead use the saved team)
	if( iKiller )
		iTeam = g_izKilled[id][KILLED_TEAM] - 1;
	else
		iTeam = get_user_team( id ) - 1;
	
	get_user_name( id, g_izUserRndName[id], MAX_NAME_LENGTH );

	if( get_user_rstats( id, izStats, izBody ) ) {

		// Update user's team round stats
		if( iTeam >= 0 && iTeam < MAX_TEAMS ) {
			for( i = 0; i < 8; i++ ) {
				g_izTeamRndStats[iTeam][i]  += izStats[i];
				g_izTeamGameStats[iTeam][i] += izStats[i];
				g_izUserRndStats[0][i]      += izStats[i];
				g_izUserGameStats[0][i]     += izStats[i];
			}
		}

		// Update user's round stats
		if( g_izUserUserID[id] == get_user_userid( id ) ) {
			for( i = 0; i < 8; i++ ) {
				g_izUserRndStats[id][i]  += izStats[i];
				g_izUserGameStats[id][i] += izStats[i];
			}
		}
		else {
			g_izUserUserID[id] = get_user_userid( id );
			for( i = 0; i < 8; i++ ) {
				g_izUserRndStats[id][i]  = izStats[i];
				g_izUserGameStats[id][i] = izStats[i];
			}
		}

	}  // endif( get_user_rstats() )
	
	// Report stats in the chat section, if player is killed.
	if( KillerChat && iKiller && iKiller != id ) {
		if( format_kill_ainfo( id, iKiller, g_sBuffer ) ) {
			client_print( id, print_chat, "* %s", g_sBuffer );
			format_kill_vinfo( id, iKiller, g_sBuffer );
		}
		client_print( id, print_chat, "* %s", g_sBuffer );
	}
	
	// Display player stats info.
	#if defined STATSX_DEBUG
	log_message( "[%s] Kill stats for #%i", PLUGIN_NAME, id );	
	#endif
	show_user_hudstats( id,  0.0 )

	return;
}

public eventEndRound() {

	// Update local configuration vars with value in cvars.
	get_config_cvars();

	// If first end round event in the round, calculate team score.
	if( !g_iRoundEndTriggered ) {
		read_data( 2, t_sText, MAX_TEXT_LENGTH );
		if( t_sText[7] == 't' )  // Terrorist wins
			g_izTeamScore[0]++;
		else if( t_sText[7] == 'c' )  // CT wins
			g_izTeamScore[1]++;
	}
	
	// Flag round end triggered.
	g_iRoundEndTriggered = 1;

	// Display round end stats to all players.
	endround_stats();

	return PLUGIN_CONTINUE;
}

endround_stats() {

	// Bail out if end round stats has already been processed
	// or round end not triggered.
	if( g_iRoundEndProcessed || !g_iRoundEndTriggered ) return;
	
	new iaPlayers[32], iPlayer, iPlayers, id;

	get_players( iaPlayers, iPlayers );

	#if defined STATSX_OLD_DEATHMSG
	// Bail out if not all killers got their damage event (ie last stats)
	for( iPlayer = 0; iPlayer < iPlayers; iPlayer++ ) {
		id = iaPlayers[iPlayer];
		if( g_izKilled[id][KILLED_KILLER_ID] && !g_izKilled[id][KILLED_KILLER_STATSFIX] )
			return;
	}
	#endif

	// Display attacker & victim list for all living players.
	// This will also update all round and game stats for all players
	// not killed.
	#if defined STATSX_DEBUG
	log_message( "[%s] End round stats", PLUGIN_NAME );	
	#endif
	for( iPlayer = 0; iPlayer < iPlayers; iPlayer++ ) {
		id = iaPlayers[iPlayer];
		if( g_fzShowUserStatsTime[id] == 0.0 )
			kill_stats( id );
	}
	
	g_sAwardAndScore[0] = 0;

	// Create round awards. 
	if( ShowMostDisruptive )
		add_most_disruptive( g_sAwardAndScore );
	if( ShowBestScore )
		add_best_score( g_sAwardAndScore );

	// Create round score. 
	// Compensate HUD message if awards are disabled.
	if( ShowTeamScore || ShowTotalStats ) { 
		if( ShowMostDisruptive && ShowBestScore )
			add( g_sAwardAndScore, MAX_BUFFER_LENGTH, "^n^n" );
		else if( ShowMostDisruptive || ShowBestScore )
			add( g_sAwardAndScore, MAX_BUFFER_LENGTH, "^n^n^n^n" );
		else
			add( g_sAwardAndScore, MAX_BUFFER_LENGTH, "^n^n^n^n^n^n" );
		if( ShowTeamScore )
			add_team_score( g_sAwardAndScore );
		if( ShowTotalStats )
			add_total_stats( g_sAwardAndScore );
	}

	save_team_chatscore();
	
	// Get and save round end stats time.
	g_fShowStatsTime = get_gametime();

	// Display round end stats to all players.
	for( iPlayer = 0; iPlayer < iPlayers; iPlayer++ ) {
		id = iaPlayers[iPlayer];
		show_roundend_hudstats( id, 0.0 );
	}

	// Flag round end processed.
	g_iRoundEndProcessed = 1;

	return;
}

public eventTeamScore() {
	new sTeamID[1+1], iTeamScore;
	read_data( 1, sTeamID, 1 );
	iTeamScore = read_data(2);
	g_izTeamEventScore[ (sTeamID[0]=='C') ? 1 : 0 ] = iTeamScore;
	return PLUGIN_CONTINUE;
}

public eventIntermission() {
	if( EndPlayer || EndTop15 )
		set_task( 1.0, "end_game_stats", 900 );
}

public end_game_stats() {
	new iaPlayers[32], iPlayer, iPlayers, id;

	if( EndPlayer ) {
		get_players( iaPlayers, iPlayers );
		for( iPlayer = 0; iPlayer < iPlayers; iPlayer++ ) {
			id = iaPlayers[iPlayer];
			if( !g_izStatsSwitch[id] ) continue;  // Do not show any stats
			cmdStatsMe( iaPlayers[iPlayer] );
		}
	}	
	else if( EndTop15 ) {
		get_players( iaPlayers, iPlayers );
		format_top15( g_sBuffer );
		for( iPlayer = 0; iPlayer < iPlayers; iPlayer++ ) {
			id = iaPlayers[iPlayer];
			if( !g_izStatsSwitch[id] ) continue;  // Do not show any stats
			show_motd( iaPlayers[iPlayer], g_sBuffer, "Top 15" );
		}
	}
	return PLUGIN_CONTINUE;
}

public eventSpecMode( id ) { 
	new sData[12];
	read_data( 2, sData, 11 );
	g_izSpecMode[id] = ( sData[10] == '2' );
	return PLUGIN_CONTINUE;
} 

public eventShowRank( id ) {
	if( SpecRankInfo && g_izSpecMode[id] ) {
		new iPlayer = read_data(2);
		if( is_user_connected( iPlayer ) ) {
			new izStats[8], izBody[8];
			new iRankPos, iRankMax;
			get_user_name( iPlayer, t_sName, MAX_NAME_LENGTH );
			iRankPos = get_user_stats( iPlayer, izStats, izBody );
			iRankMax = get_statsnum();
			set_hudtype_specmode();
			show_hudmessage( id, "%s's rank is %d of %d", t_sName, iRankPos, iRankMax );
		}
	}
	return PLUGIN_CONTINUE;
}

public client_connect( id ) {
	if( ShowStats ) {
		get_user_info( id, "_amxstatsx", t_sText, MAX_TEXT_LENGTH );
		g_izStatsSwitch[id] = ( t_sText[0] ) ? str_to_num( t_sText ) : -1 ;
	}
	else
		g_izStatsSwitch[id] = 0;

	g_izKilled[id][KILLED_KILLER_ID] = 0;
	g_izKilled[id][KILLED_KILLER_STATSFIX] = 0;
	g_izShowStatsFlags[id] = 0;  // Clear all flags
	g_fzShowUserStatsTime[id] = 0.0;

	return PLUGIN_CONTINUE;
}
