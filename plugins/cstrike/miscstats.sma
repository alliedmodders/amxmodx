/* AMX Mod X
*   Misc. Stats Plugin
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is part of AMX Mod X.
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation, 
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve, 
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include <amxmodx>
#include <amxmisc>
#include <csx>

public MultiKill
public MultiKillSound
public BombPlanting
public BombDefusing
public BombPlanted
public BombDefused
public BombFailed
public BombPickUp
public BombDrop
public BombCountVoice
public BombCountDef
public BombReached
public ItalyBonusKill
public EnemyRemaining
public LastMan
public KnifeKill
public KnifeKillSound
public GrenadeKill
public GrenadeSuicide
public HeadShotKill
public HeadShotKillSound
public RoundCounterSound
public RoundCounter
public KillingStreak
public KillingStreakSound
public DoubleKill
public DoubleKillSound
public PlayerName
public FirstBloodSound

public BombPlantedSound
public BombDefusedSound
public BombFailedSound
public BombCountHUD
public LastManSound
public GrenadeKillSound
public GrenadeSuicideSound

const SOUNDFILE_PATH_MAXLEN = 64
const SOUND_SHORTPATH_MAXLEN = SOUNDFILE_PATH_MAXLEN - 10 // 64 (sound/ [ 54 ] .wav) critical value for fast dl

new g_streakKills[33][2]
new g_multiKills[33][2]
new g_C4Timer
new g_Defusing
new g_Planter 
new Float:g_LastOmg
new g_LastAnnounce
new g_roundCount
new Float:g_doubleKill
new g_doubleKillId
new g_friend[33]
new g_firstBlood
new g_center1_sync
new g_announce_sync
new g_status_sync
new g_left_sync
new g_bottom_sync
new g_he_sync

new g_pcvar_mp_c4timer, g_c4timer_value

const TASK_BOMB_TIMER = 8038
const TASK_DELAYED_NEW_ROUND = 98038

const TEAM_T = 1
const TEAM_CT = 2

new g_connected[33]

new g_MultiKillMsg[7][] =
{
	"Multi-Kill! %s^n%L %d %L (%d %L)", 
	"Ultra-Kill!!! %s^n%L %d %L (%d %L)", 
	"%s IS ON A KILLING SPREE!!!^n%L %d %L (%d %L)", 
	"RAMPAGE!!! %s^n%L %d %L (%d hs)", 
	"%s IS UNSTOPPABLE!!!^n%L %d %L (%d %L)", 
	"%s IS A MONSTER!^n%L %d %L (%d %L)", 
	"%s IS GODLIKE!!!!^n%L %d %L (%d %L)"
}

new g_Sounds[7][SOUND_SHORTPATH_MAXLEN] = 
{
	"misc/multikill",
	"misc/ultrakill",
	"misc/killingspree",
	"misc/rampage",
	"misc/unstoppable",
	"misc/monsterkill",
	"misc/godlike"
}

new g_firstbloodsound[SOUND_SHORTPATH_MAXLEN] = "misc/firstblood"
new g_lastmansound_1vsothers[SOUND_SHORTPATH_MAXLEN] = "misc/oneandonly"
new g_lastmansound_duel[SOUND_SHORTPATH_MAXLEN] = "misc/maytheforce"
new g_hssound_killer[SOUND_SHORTPATH_MAXLEN] = "misc/headshot"
new g_hssound_victim[SOUND_SHORTPATH_MAXLEN] = "misc/headshot"
new g_knifekillsound[SOUND_SHORTPATH_MAXLEN] = "misc/humiliation"
new g_doublekillsound[SOUND_SHORTPATH_MAXLEN] = "misc/doublekill"
new g_roundcountersound[SOUND_SHORTPATH_MAXLEN] = "misc/prepare"
new g_grenadekillsound[SOUND_SHORTPATH_MAXLEN] = "djeyl/grenade"
new g_grenadesuicidesound[SOUND_SHORTPATH_MAXLEN] = "djeyl/witch"
new g_bombplantedsound[SOUND_SHORTPATH_MAXLEN] = "djeyl/c4powa"
new g_bombdefusedsound[SOUND_SHORTPATH_MAXLEN] = "djeyl/laugh"
new g_bombfailedsound[SOUND_SHORTPATH_MAXLEN] = "djeyl/witch"

new g_KillingMsg[7][] =
{
	"%s: Multi-Kill!", 
	"%s: Ultra-Kill!!!", 
	"%s IS ON A KILLING SPREE!!!", 
	"%s: RAMPAGE!!!", 
	"%s IS UNSTOPPABLE!!!", 
	"%s IS A MONSTER!", 
	"%s IS GODLIKE!!!"
}

new g_KinfeMsg[4][] =
{
	"KNIFE_MSG_1", 
	"KNIFE_MSG_2", 
	"KNIFE_MSG_3", 
	"KNIFE_MSG_4"
}

new g_LastMessages[4][] =
{
	"LAST_MSG_1", 
	"LAST_MSG_2", 
	"LAST_MSG_3", 
	"LAST_MSG_4"
}

new g_HeMessages[4][] =
{
	"HE_MSG_1", 
	"HE_MSG_2", 
	"HE_MSG_3", 
	"HE_MSG_4"
}

new g_SHeMessages[4][] =
{
	"SHE_MSG_1", 
	"SHE_MSG_2", 
	"SHE_MSG_3", 
	"SHE_MSG_4"
}

new g_HeadShots[7][] =
{
	"HS_MSG_1", 
	"HS_MSG_2", 
	"HS_MSG_3", 
	"HS_MSG_4", 
	"HS_MSG_5", 
	"HS_MSG_6", 
	"HS_MSG_7"
}

new g_teamsNames[4][] =
{
	"TERRORIST", 
	"CT", 
	"TERRORISTS", 
	"CTS"
}

public plugin_init()
{
	register_plugin("CS Misc. Stats", AMXX_VERSION_STR, "AMXX Dev Team")
	register_dictionary("miscstats.txt")
	register_event("TextMsg", "eRestart", "a", "2&#Game_C", "2&#Game_w")
	register_event("SendAudio", "eEndRound", "a", "2&%!MRAD_terwin", "2&%!MRAD_ctwin", "2&%!MRAD_rounddraw")
	
	register_event("HLTV", "Event_HLTV_New_Round", "a", "1=0", "2=0") // cache c4 timer value
	register_logevent("LogEvent_Round_Start", 2, "1=Round_Start") // replaces ugly register_event("RoundTime", "eNewRound", "bc")
	register_event("StatusValue", "setTeam", "bef", "1=1")
	register_event("StatusValue", "showStatus", "bef", "1=2", "2!0")
	register_event("StatusValue", "hideStatus", "bef", "1=1", "2=0")

	new mapname[32], n = get_mapname(mapname, charsmax(mapname))
	if ((get_map_objectives() & MapObjective_Bomb) || (equali(mapname, "de_", 3) || equali(mapname, "csde_", 5)))
	{
		register_event("StatusIcon", "eGotBomb", "be", "1=1", "1=2", "2=c4")
		register_event("TextMsg", "eBombPickUp", "bc", "2&#Got_bomb")
		register_event("TextMsg", "eBombDrop", "bc", "2&#Game_bomb_d")
		register_event("BarTime", "eStopDefuse", "b", "1=0")
	}
	else if (equali(mapname, "cs_italy") || equali(mapname, "cs_italy_cz"))
	{
		register_event("23", "chickenKill", "a", "1=108", "15=4")
		register_event("23", "radioKill", "a", "1=108", n == 8 ? "15=2" : "15=8") // cz radio is wood
		
	}
	
	g_center1_sync = CreateHudSyncObj()
	g_announce_sync = CreateHudSyncObj()
	g_status_sync = CreateHudSyncObj()
	g_left_sync = CreateHudSyncObj()
	g_bottom_sync = CreateHudSyncObj()
	g_he_sync = CreateHudSyncObj()

	g_pcvar_mp_c4timer = get_cvar_pointer("mp_c4timer")
	g_c4timer_value = get_pcvar_num( g_pcvar_mp_c4timer )
}

public plugin_precache()
{
	// parse sounds and precache them
	new szConfigsDir[64], szCfgFile[64]
	get_configsdir(szConfigsDir, charsmax(szConfigsDir))
	formatex(szCfgFile, charsmax(szCfgFile), "%s/stats.ini", szConfigsDir)

	new buffer[256]

	// initialize xvars so we can know later if sounds have to be precached or not.
	// xvars gonna be initialized again in statscfg.amxx plugin_init, this is ok.
	new fp = fopen(szCfgFile, "rt")
	if( fp )
	{
		new xvarname[32], xvarid
		while( !feof(fp) )
		{
			fgets(fp, buffer, charsmax(buffer))
			trim(buffer)
			if( buffer[0] != ';' )
			{
				parse(buffer, xvarname, charsmax(xvarname))
				if ((xvarid = get_xvar_id(xvarname)) != -1)
				{
					set_xvar_num(xvarid, 1)
				}
			}
		}
		fclose(fp)
	}

	formatex(szCfgFile, charsmax(szCfgFile), "%s/miscstats.ini", szConfigsDir)
	fp = fopen(szCfgFile, "rt")
	if( fp )
	{
		new szSoundKey[32], szSoundFile[SOUNDFILE_PATH_MAXLEN]
		while( !feof(fp) )
		{
			fgets(fp, buffer, charsmax(buffer))
			trim(buffer)
			if( buffer[0] != ';' && parse(buffer, szSoundKey, charsmax(szSoundKey), szSoundFile, charsmax(szSoundFile)) == 2 )
			{
				if( equal(szSoundKey, "FirstBloodSound") )
				{
					copy_sound(g_firstbloodsound, charsmax(g_firstbloodsound), szSoundFile)
					if( FirstBloodSound ) precache_sound_custom(g_firstbloodsound)
				}
				else if( equal(szSoundKey, "LastManVsOthersSound") )
				{
					copy_sound(g_lastmansound_1vsothers, charsmax(g_lastmansound_1vsothers), szSoundFile)
					if( LastManSound ) precache_sound_custom(g_lastmansound_1vsothers)
				}
				else if( equal(szSoundKey, "LastManDuelSound") )
				{
					copy_sound(g_lastmansound_duel, charsmax(g_lastmansound_duel), szSoundFile)
					if( LastManSound ) precache_sound_custom(g_lastmansound_duel)
				}
				else if( equal(szSoundKey, "HeadShotKillSoundKiller") )
				{
					copy_sound(g_hssound_killer, charsmax(g_hssound_killer), szSoundFile)
					if( HeadShotKillSound ) precache_sound_custom(g_hssound_killer)
				}
				else if( equal(szSoundKey, "HeadShotKillSoundVictim") )
				{
					copy_sound(g_hssound_victim, charsmax(g_hssound_victim), szSoundFile)
					if( HeadShotKillSound ) precache_sound_custom(g_hssound_victim)
				}
				else if( equal(szSoundKey, "KnifeKillSound") )
				{
					copy_sound(g_knifekillsound, charsmax(g_knifekillsound), szSoundFile)
					if( KnifeKillSound ) precache_sound_custom(g_knifekillsound)
				}
				else if( equal(szSoundKey, "DoubleKillSound") )
				{
					copy_sound(g_doublekillsound, charsmax(g_doublekillsound), szSoundFile)
					if( DoubleKillSound ) precache_sound_custom(g_doublekillsound)
				}
				else if( equal(szSoundKey, "RoundCounterSound") )
				{
					copy_sound(g_roundcountersound, charsmax(g_roundcountersound), szSoundFile)
					if( RoundCounterSound ) precache_sound_custom(g_roundcountersound)
				}
				else if( equal(szSoundKey, "GrenadeKillSound") )
				{
					copy_sound(g_grenadekillsound, charsmax(g_grenadekillsound), szSoundFile)
					if( GrenadeKillSound ) precache_sound_custom(g_grenadekillsound)
				}
				else if( equal(szSoundKey, "GrenadeSuicideSound") )
				{
					copy_sound(g_grenadesuicidesound, charsmax(g_grenadesuicidesound), szSoundFile)
					if( GrenadeSuicideSound ) precache_sound_custom(g_grenadesuicidesound)
				}
				else if( equal(szSoundKey, "BombPlantedSound") )
				{
					copy_sound(g_bombplantedsound, charsmax(g_bombplantedsound), szSoundFile)
					if( BombPlantedSound ) precache_sound_custom(g_bombplantedsound)
				}
				else if( equal(szSoundKey, "BombDefusedSound") )
				{
					copy_sound(g_bombdefusedsound, charsmax(g_bombdefusedsound), szSoundFile)
					if( BombDefusedSound ) precache_sound_custom(g_bombdefusedsound)
				}
				else if( equal(szSoundKey, "BombFailedSound") )
				{
					copy_sound(g_bombfailedsound, charsmax(g_bombfailedsound), szSoundFile)
					if( BombFailedSound ) precache_sound_custom(g_bombfailedsound)
				}
				else
				{
					// KillingStreakSound and MultiKillSound
					if( equal(szSoundKey, "MultiKillSound") )
					{
						copy_sound(g_Sounds[0], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[0])
					}
					else if( equal(szSoundKey, "UltraKillSound") )
					{
						copy_sound(g_Sounds[1], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[1])
					}
					else if( equal(szSoundKey, "KillingSpreeSound") )
					{
						copy_sound(g_Sounds[2], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[2])
					}
					else if( equal(szSoundKey, "RampageSound") )
					{
						copy_sound(g_Sounds[3], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[3])
					}
					else if( equal(szSoundKey, "UnstopableSound") )
					{
						copy_sound(g_Sounds[4], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[4])
					}
					else if( equal(szSoundKey, "MonsterKillSound") )
					{
						copy_sound(g_Sounds[5], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[5])
					}
					else if( equal(szSoundKey, "GodLike") )
					{
						copy_sound(g_Sounds[6], charsmax(g_Sounds[]), szSoundFile)
						if( KillingStreakSound || MultiKillSound ) precache_sound_custom(g_Sounds[6])
					}
				}
			}
		}
		fclose(fp)
	}
}

precache_sound_custom( const sound[] )
{
	new fullpathsound[SOUNDFILE_PATH_MAXLEN]
	formatex(fullpathsound, charsmax(fullpathsound), "sound/%s.wav", sound)
	if( file_exists(fullpathsound) )
	{
		precache_sound(fullpathsound[6])
	}
	else
	{
		log_amx("Could not locate <%s> file", fullpathsound)
	}
}

copy_sound(dest[], len, src[])
{
	new n = copy(dest, len, src[ 6 * equali(src, "sound/", 6) ])
	if( n > 4 && equal(dest[n-4], ".wav") )
	{
		dest[n-4] = EOS
	}
}

public plugin_cfg()
{
	new g_addStast[] = "amx_statscfg add ^"%s^" %s"
	
	server_cmd(g_addStast, "ST_MULTI_KILL", "MultiKill")
	server_cmd(g_addStast, "ST_MULTI_KILL_SOUND", "MultiKillSound")
	server_cmd(g_addStast, "ST_BOMB_PLANTING", "BombPlanting")
	server_cmd(g_addStast, "ST_BOMB_DEFUSING", "BombDefusing")
	server_cmd(g_addStast, "ST_BOMB_PLANTED", "BombPlanted")
	server_cmd(g_addStast, "ST_BOMB_PLANTED_SOUND", "BombPlantedSound")
	server_cmd(g_addStast, "ST_BOMB_DEF_SUCC", "BombDefused")
	server_cmd(g_addStast, "ST_BOMB_DEF_SUCC_SOUND", "BombDefusedSound")
	server_cmd(g_addStast, "ST_BOMB_DEF_FAIL", "BombFailed")
	server_cmd(g_addStast, "ST_BOMB_DEF_FAIL_SOUND", "BombFailedSound")
	server_cmd(g_addStast, "ST_BOMB_PICKUP", "BombPickUp")
	server_cmd(g_addStast, "ST_BOMB_DROP", "BombDrop")
	server_cmd(g_addStast, "ST_BOMB_CD_HUD", "BombCountHUD")
	server_cmd(g_addStast, "ST_BOMB_CD_VOICE", "BombCountVoice")
	server_cmd(g_addStast, "ST_BOMB_CD_DEF", "BombCountDef")
	server_cmd(g_addStast, "ST_BOMB_SITE", "BombReached")
	server_cmd(g_addStast, "ST_ITALY_BONUS", "ItalyBonusKill")
	server_cmd(g_addStast, "ST_LAST_MAN", "LastMan")
	server_cmd(g_addStast, "ST_LAST_MAN_SOUND", "LastManSound")
	server_cmd(g_addStast, "ST_KNIFE_KILL", "KnifeKill")
	server_cmd(g_addStast, "ST_KNIFE_KILL_SOUND", "KnifeKillSound")
	server_cmd(g_addStast, "ST_HE_KILL", "GrenadeKill")
	server_cmd(g_addStast, "ST_HE_KILL_SOUND", "GrenadeKillSound")
	server_cmd(g_addStast, "ST_HE_SUICIDE", "GrenadeSuicide")
	server_cmd(g_addStast, "ST_HE_SUICIDE_SOUND", "GrenadeSuicideSound")
	server_cmd(g_addStast, "ST_HS_KILL", "HeadShotKill")
	server_cmd(g_addStast, "ST_HS_KILL_SOUND", "HeadShotKillSound")
	server_cmd(g_addStast, "ST_ROUND_CNT", "RoundCounter")
	server_cmd(g_addStast, "ST_ROUND_CNT_SOUND", "RoundCounterSound")
	server_cmd(g_addStast, "ST_KILL_STR", "KillingStreak")
	server_cmd(g_addStast, "ST_KILL_STR_SOUND", "KillingStreakSound")
	server_cmd(g_addStast, "ST_ENEMY_REM", "EnemyRemaining")
	server_cmd(g_addStast, "ST_DOUBLE_KILL", "DoubleKill")
	server_cmd(g_addStast, "ST_DOUBLE_KILL_SOUND", "DoubleKillSound")
	server_cmd(g_addStast, "ST_PLAYER_NAME", "PlayerName")
	server_cmd(g_addStast, "ST_FIRST_BLOOD_SOUND", "FirstBloodSound")
}

public client_putinserver(id)
{
	g_multiKills[id] = {0, 0}
	g_streakKills[id] = {0, 0}
	g_connected[id] = true
}

public client_disconnect(id)
{
	g_connected[id] = false
}

public client_death(killer, victim, wpnindex, hitplace, TK)
{
	if (wpnindex == CSW_C4)
		return

	new headshot = (hitplace == HIT_HEAD) ? 1 : 0
	new selfkill = (killer == victim) ? 1 : 0

	new victim_alive = is_user_alive(victim) // happens on ClientKill

	if (g_firstBlood)
	{
		g_firstBlood = 0
		if (FirstBloodSound)
			play_sound(g_firstbloodsound)
	}

	if ((KillingStreak || KillingStreakSound) && !TK)
	{
		g_streakKills[victim][1]++
		g_streakKills[victim][0] = 0

		if (!selfkill)
		{
			g_streakKills[killer][0]++
			g_streakKills[killer][1] = 0
			
			new a = g_streakKills[killer][0] - 3

			if ((a > -1) && !(a % 2))
			{
				new name[32]
				get_user_name(killer, name, charsmax(name))
				
				if ((a >>= 1) > 6)
					a = 6
				
				if (KillingStreak)
				{
					set_hudmessage(0, 100, 255, 0.05, 0.50, 2, 0.02, 6.0, 0.01, 0.1, -1)
					ShowSyncHudMsg(0, g_left_sync, g_KillingMsg[a], name)
				}
				
				if (KillingStreakSound)
				{
					play_sound(g_Sounds[a])
				}
			}
		}
	}

	if (MultiKill || MultiKillSound)
	{
		if (!selfkill && !TK && killer)
		{
			g_multiKills[killer][0]++ 
			g_multiKills[killer][1] += headshot
			
			new param[2]
			
			param[0] = killer
			param[1] = g_multiKills[killer][0]
			set_task(4.0 + float(param[1]), "checkKills", killer, param, 2)
		}
	}

	new team = get_user_team(victim)
	if (EnemyRemaining && is_user_connected(victim))
	{
		if( TEAM_T <= team <= TEAM_CT )
		{
			new ppl[32], pplnum, epplnum, a
			get_players(ppl, epplnum, "ae", team == TEAM_T ? "CT" : "TERRORIST")
			get_players(ppl, pplnum, "ae", team == TEAM_T ? "TERRORIST" : "CT")
			if( victim_alive )
			{
				for(a=0; a<pplnum; a++)
				{
					if( ppl[a] == victim )
					{
						ppl[a] = ppl[--pplnum]
						break
					}
				}
			}
			
			if (pplnum && epplnum)
			{
				new message[128], team_name[32]

				set_hudmessage(255, 255, 255, 0.02, 0.85, 2, 0.05, 0.1, 0.02, 3.0, -1)
				
				/* This is a pretty stupid thing to translate, but whatever */
				new _teamname[32]
				if (team == TEAM_T)
				{
					formatex(_teamname, charsmax(_teamname), "TERRORIST%s", (epplnum == 1) ? "" : "S")
				} else {
					formatex(_teamname, charsmax(_teamname), "CT%s", (epplnum == 1) ? "" : "S")
				}

				new id
				for (a = 0; a < pplnum; ++a)
				{
					id = ppl[a]
					formatex(team_name, charsmax(team_name), "%L", id, _teamname)
					formatex(message, charsmax(message), "%L", id, "REMAINING", epplnum, team_name)
					ShowSyncHudMsg(id, g_bottom_sync, "%s", message)
				}
			}
		}
	}

	if (LastMan || LastManSound)
	{
		new cts[32], ts[32], ctsnum, tsnum, b
		get_players(cts, ctsnum, "ae", "CT")
		get_players(ts, tsnum, "ae", "TERRORIST")
		
		if( victim_alive )
		{
			switch( team )
			{
				case TEAM_T:
				{
					for(b=0; b<tsnum; b++)
					{
						if( ts[b] == victim )
						{
							ts[b] = ts[--tsnum]
							break
						}
					}
				}
				case TEAM_CT:
				{
					for(b=0; b<ctsnum; b++)
					{
						if( cts[b] == victim )
						{
							cts[b] = cts[--ctsnum]
							break
						}
					}
				}
			}
		}

		if (ctsnum == 1 && tsnum == 1)
		{
			if( LastMan )
			{
				new ctname[32], tname[32]
				
				get_user_name(cts[0], ctname, charsmax(ctname))
				get_user_name(ts[0], tname, charsmax(tname))
				
				set_hudmessage(0, 255, 255, -1.0, 0.35, 0, 6.0, 6.0, 0.5, 0.15, -1)
				ShowSyncHudMsg(0, g_center1_sync, "%s vs. %s", ctname, tname)
			}
			
			if( LastManSound )
				play_sound(g_lastmansound_duel)
		}
		else if (!g_LastAnnounce)
		{
			new oposite = 0, _team = 0
			
			if (ctsnum == 1 && tsnum > 1)
			{
				g_LastAnnounce = cts[0]
				oposite = tsnum
				_team = 0
			}
			else if (tsnum == 1 && ctsnum > 1)
			{
				g_LastAnnounce = ts[0]
				oposite = ctsnum
				_team = 1
			}

			if (g_LastAnnounce)
			{
				if( LastMan )
				{
					new name[32]
				
					get_user_name(g_LastAnnounce, name, charsmax(name))
				
					set_hudmessage(0, 255, 255, -1.0, 0.38, 0, 6.0, 6.0, 0.5, 0.15, -1)
					ShowSyncHudMsg(0, g_center1_sync, "%s (%d HP) vs. %d %s%s: %L", name, get_user_health(g_LastAnnounce), oposite, g_teamsNames[_team], (oposite == 1) ? "" : "S", LANG_PLAYER, g_LastMessages[random_num(0, 3)])
				}
				if ( LastManSound && g_connected[g_LastAnnounce] )
				{
					client_cmd(g_LastAnnounce, "spk %s", g_lastmansound_1vsothers)
				}
			}
		}
	}

	if (wpnindex == CSW_KNIFE && (KnifeKill || KnifeKillSound))
	{
		if (KnifeKill)
		{
			new killer_name[32], victim_name[32]
			
			get_user_name(killer, killer_name, charsmax(killer_name))
			get_user_name(victim, victim_name, charsmax(victim_name))
			
			set_hudmessage(255, 100, 100, -1.0, 0.25, 1, 6.0, 6.0, 0.5, 0.15, -1)
			ShowSyncHudMsg(0, g_he_sync, "%L", LANG_PLAYER, g_KinfeMsg[random_num(0, 3)], killer_name, victim_name)
		}
		
		if (KnifeKillSound)
			play_sound(g_knifekillsound)
	}

	if (wpnindex == CSW_HEGRENADE)
	{
		new killer_name[32], victim_name[32]
		if( GrenadeKill || GrenadeSuicide )
		{
			get_user_name(killer, killer_name, charsmax(killer_name))
			get_user_name(victim, victim_name, charsmax(victim_name))
			
			set_hudmessage(255, 100, 100, -1.0, 0.25, 1, 6.0, 6.0, 0.5, 0.15, -1)
		}
		
		if (!selfkill)
		{
			if (GrenadeKill)
				ShowSyncHudMsg(0, g_he_sync, "%L", LANG_PLAYER, g_HeMessages[random_num(0, 3)], killer_name, victim_name)
			if (GrenadeKillSound)
				play_sound(g_grenadekillsound)
		}
		else
		{
			if (GrenadeSuicide)
				ShowSyncHudMsg(0, g_he_sync, "%L", LANG_PLAYER, g_SHeMessages[random_num(0, 3)], victim_name)
			if (GrenadeSuicideSound)
				play_sound(g_grenadesuicidesound)
		}
	}

	if (headshot && (HeadShotKill || HeadShotKillSound))
	{
		if (HeadShotKill && wpnindex)
		{
			new killer_name[32], victim_name[32], weapon_name[32], message[256], players[32], pnum, plr
			
			xmod_get_wpnname(wpnindex, weapon_name, charsmax(weapon_name))
			get_user_name(killer, killer_name, charsmax(killer_name))
			get_user_name(victim, victim_name, charsmax(victim_name))
			get_players(players, pnum, "ch")
			
			for (new i = 0; i < pnum; i++)
			{
				plr = players[i]
				formatex(message, charsmax(message), "%L", plr, g_HeadShots[random_num(0, 6)])
				
				replace(message, charsmax(message), "$vn", victim_name)
				replace(message, charsmax(message), "$wn", weapon_name)
				replace(message, charsmax(message), "$kn", killer_name)
				
				set_hudmessage(100, 100, 255, -1.0, 0.30, 0, 6.0, 6.0, 0.5, 0.15, -1)
				ShowSyncHudMsg(plr, g_announce_sync, "%s", message)
			}
		}
		
		if (HeadShotKillSound)
		{
			client_cmd(victim, "spk %s", g_hssound_victim)
			if( victim != killer )
				client_cmd(killer, "spk %s", g_hssound_killer)
		}
	}

	if ((DoubleKill || DoubleKillSound) && !selfkill)
	{
		new Float:nowtime = get_gametime()
		
		if (g_doubleKill == nowtime && g_doubleKillId == killer)
		{
			if (DoubleKill)
			{
				new name[32]
				
				get_user_name(killer, name, charsmax(name))
				
				set_hudmessage(255, 0, 255, -1.0, 0.35, 0, 6.0, 6.0, 0.5, 0.15, -1)
				ShowSyncHudMsg(0, g_center1_sync, "%L", LANG_PLAYER, "DOUBLE_KILL", name)
			}
			
			if (DoubleKillSound)
				play_sound(g_doublekillsound)
		}
		
		g_doubleKill = nowtime
		g_doubleKillId = killer
	}
}

public hideStatus(id)
{
	if (PlayerName)
	{
		ClearSyncHud(id, g_status_sync)
	}
}

public setTeam(id)
	g_friend[id] = read_data(2)

public showStatus(id)
{
	if( PlayerName) 
	{
		new name[32], pid = read_data(2)
	
		get_user_name(pid, name, charsmax(name))
		new color1 = 0, color2 = 0
	
		if (get_user_team(pid) == TEAM_T)
			color1 = 255
		else
			color2 = 255
		
		if (g_friend[id] == 1)	// friend
		{
			new wpnid = get_user_weapon(pid)
			new wpnname[32]
		
			if (wpnid)
				xmod_get_wpnname(wpnid, wpnname, charsmax(wpnname))
		
			set_hudmessage(color1, 50, color2, -1.0, 0.60, 1, 0.01, 3.0, 0.01, 0.01, -1)
			ShowSyncHudMsg(id, g_status_sync, "%s -- %d HP / %d AP / %s", name, get_user_health(pid), get_user_armor(pid), wpnname)
		} else {
			set_hudmessage(color1, 50, color2, -1.0, 0.60, 1, 0.01, 3.0, 0.01, 0.01, -1)
			ShowSyncHudMsg(id, g_status_sync, "%s", name)
		}
	}
}

public Event_HLTV_New_Round()
{
	g_c4timer_value = get_pcvar_num( g_pcvar_mp_c4timer )
	++g_roundCount
	g_firstBlood = 1
	g_C4Timer = 0
	
	if (RoundCounterSound)
		play_sound(g_roundcountersound)

	if (RoundCounter)
	{
		set_task(0.2, "Delayed_New_Round", TASK_DELAYED_NEW_ROUND)
	}
}

public Delayed_New_Round()
{
	set_hudmessage(200, 0, 0, -1.0, 0.30, 0, 6.0, 6.0, 0.5, 0.15, -1)
	ShowSyncHudMsg(0, g_announce_sync, "%L", LANG_PLAYER, "PREPARE_FIGHT", g_roundCount)
}

public LogEvent_Round_Start()
{
	if (KillingStreak)
	{
		new appl[32], ppl, i
		get_players(appl, ppl, "ac")
		
		for (new a = 0; a < ppl; ++a)
		{
			i = appl[a]
			
			if (g_streakKills[i][0] >= 2)
				client_print(i, print_chat, "* %L", i, "KILLED_ROW", g_streakKills[i][0])
			else if (g_streakKills[i][1] >= 2)
				client_print(i, print_chat, "* %L", i, "DIED_ROUNDS", g_streakKills[i][1])
		}
	}
}

public eRestart()
{
	eEndRound()
	g_roundCount = 0
	g_firstBlood = 1
}

public eEndRound()
{
	g_C4Timer = -2
	g_LastOmg = 0.0
	remove_task(TASK_BOMB_TIMER)
	g_LastAnnounce = 0
}

public checkKills(param[])
{
	new id = param[0]
	new a = param[1]
	
	if (a == g_multiKills[id][0])
	{
		a -= 3
		
		if (a > -1)
		{
			if (a > 6)
			{
				a = 6
			}
			
			if (MultiKill)
			{
				new name[32]
				
				get_user_name(id, name, charsmax(name))
				set_hudmessage(255, 0, 100, 0.05, 0.50, 2, 0.02, 6.0, 0.01, 0.1, -1)
				
				ShowSyncHudMsg(0, g_left_sync, g_MultiKillMsg[a], name, LANG_PLAYER, "WITH", g_multiKills[id][0], LANG_PLAYER, "KILLS", g_multiKills[id][1], LANG_PLAYER, "HS")
			}
			
			if (MultiKillSound)
			{
				play_sound(g_Sounds[a])
			}
		}
		g_multiKills[id] = {0, 0}
	}
}

public chickenKill()
{
	if (ItalyBonusKill)
		announceEvent(0, "KILLED_CHICKEN")
}

public radioKill()
{
	if (ItalyBonusKill)
		announceEvent(0, "BLEW_RADIO")
}

announceEvent(id, message[])
{
	new name[32]
	
	get_user_name(id, name, charsmax(name))
	set_hudmessage(255, 100, 50, -1.0, 0.30, 0, 6.0, 6.0, 0.5, 0.15, -1)
	ShowSyncHudMsg(0, g_announce_sync, "%L", LANG_PLAYER, message, name)
}

public eBombPickUp(id)
{
	if (BombPickUp)
		announceEvent(id, "PICKED_BOMB")
}

public eBombDrop()
{
	if (BombDrop)
		announceEvent(g_Planter, "DROPPED_BOMB")
}

public eGotBomb(id)
{
	g_Planter = id
	
	if (BombReached && read_data(1) == 2 && g_LastOmg < get_gametime())
	{
		g_LastOmg = get_gametime() + 15.0
		announceEvent(g_Planter, "REACHED_TARGET")
	}
}

public bombTimer()
{
	if (--g_C4Timer > 0)
	{
		if( BombCountHUD )
		{
			new r, g
			if( g_C4Timer < 10 )
			{
				r = 255, g = 0
			}
			else
			{
				r = (((max(g_c4timer_value - g_C4Timer - 10,0)) * 255000) / (g_c4timer_value - 10)) / 1000
				g = 255 - r
			}

			set_hudmessage(r, g, 0, -1.0, 0.75, g_C4Timer <= 10 ? 1 : 0, 0.01, 1.1, 0.001, 0.001, .channel = -1)
			show_hudmessage(0, "C4: %ds", g_C4Timer)
		}
		if (BombCountVoice)
		{
			if (g_C4Timer == 30 || g_C4Timer == 20)
			{
				new temp[64]
				
				num_to_word(g_C4Timer, temp, charsmax(temp))
				format(temp, charsmax(temp), "^"vox/%s seconds until explosion^"", temp)
				play_sound(temp)
			}
			else if (g_C4Timer < 11)
			{
				new temp[64]
				
				num_to_word(g_C4Timer, temp, charsmax(temp))
				format(temp, charsmax(temp), "^"vox/%s^"", temp)
				play_sound(temp)
			}
		}
		if (BombCountDef && g_Defusing && is_user_alive(g_Defusing))
			client_print(g_Defusing, print_center, "%d", g_C4Timer)
	}
	else
		remove_task(TASK_BOMB_TIMER)
}

public eStopDefuse(id)
{
	if( id == g_Defusing )
	{
		g_Defusing = 0
	}
}

public bomb_planted(planter)
{
	g_Defusing = 0
	
	if (BombPlanted)
		announceEvent(planter, "SET_UP_BOMB")

	if (BombPlantedSound)
		play_sound(g_bombplantedsound)
	
	g_C4Timer = g_c4timer_value + 1
	bombTimer()
	set_task(1.0, "bombTimer", TASK_BOMB_TIMER, "", 0, "b")
}

public bomb_planting(planter)
{
	if (BombPlanting)
		announceEvent(planter, "PLANT_BOMB")
}

public bomb_defusing(defuser)
{
	if (BombDefusing)
		announceEvent(defuser, "DEFUSING_BOMB")
	
	g_Defusing = defuser
}

public bomb_defused(defuser)
{
	if (BombDefused)
		announceEvent(defuser, "DEFUSED_BOMB")
	if (BombDefusedSound)
		play_sound(g_bombdefusedsound)
}

public bomb_explode(planter, defuser)
{
	if( defuser )
	{
		if (BombFailed)
			announceEvent(defuser, "FAILED_DEFU")
		if (BombFailedSound)
			play_sound(g_bombfailedsound)
	}
}

play_sound(sound[])
{
	new players[32], pnum, id
	get_players(players, pnum, "ch")

	for(--pnum; pnum>=0; pnum--)
	{
		id = players[pnum]
		if ( g_connected[id] )
			client_cmd(id, "spk %s", sound)
	}
}
