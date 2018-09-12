// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Admin Commands Plugin
//

#include <amxmodx>
#include <amxmisc>

// This is not a dynamic array because it would be bad for 24/7 map servers.
#define OLD_CONNECTION_QUEUE 10

new g_iPauseCon;
new Float:g_fPausable;
new bool:g_bPaused;
new bool:g_bPauseAllowed;

new g_pPausable;
new g_pRconPassword;
new g_pTimelimit;
new g_pTempBanMaxTime;

// Old connection queue
new g_szNames[OLD_CONNECTION_QUEUE][MAX_NAME_LENGTH];
new g_szSteamIDs[OLD_CONNECTION_QUEUE][32];
new g_szIPs[OLD_CONNECTION_QUEUE][32];
new g_iAccess[OLD_CONNECTION_QUEUE];
new g_iTracker;
new g_iSize;

public Trie:g_tTempBans;
new Trie:g_tXvarsFlags;

public plugin_init()
{
	register_plugin("Admin Commands", AMXX_VERSION_STR, "AMXX Dev Team");

	register_dictionary("admincmd.txt");
	register_dictionary("adminhelp.txt");
	register_dictionary("common.txt");

	register_concmd("amx_kick",         "cmdKick",      ADMIN_KICK,                 "AMX_KICK_SYNTAX",      .info_ml = true);
	register_concmd("amx_ban",          "cmdBan",       ADMIN_BAN|ADMIN_BAN_TEMP,   "AMX_BAN_SYNTAX",       .info_ml = true);
	register_concmd("amx_banip",        "cmdBanIP",     ADMIN_BAN|ADMIN_BAN_TEMP,   "AMX_BANIP_SYNTAX",     .info_ml = true);
	register_concmd("amx_addban",       "cmdAddBan",    ADMIN_BAN,                  "AMX_ADDBAN_SYNTAX",    .info_ml = true);
	register_concmd("amx_unban",        "cmdUnban",     ADMIN_BAN|ADMIN_BAN_TEMP,   "AMX_UNBAN_SYNTAX",     .info_ml = true);
	register_concmd("amx_slay",         "cmdSlay",      ADMIN_SLAY,                 "AMX_SLAY_SYNTAX",      .info_ml = true);
	register_concmd("amx_slap",         "cmdSlap",      ADMIN_SLAY,                 "AMX_SLAP_SYNTAX",      .info_ml = true);
	register_concmd("amx_leave",        "cmdLeave",     ADMIN_KICK,                 "AMX_LEAVE_SYNTAX",     .info_ml = true);
	register_concmd("amx_pause",        "cmdPause",     ADMIN_CVAR,                 "AMX_PAUSE_SYNTAX",     .info_ml = true);
	register_concmd("amx_who",          "cmdWho",       ADMIN_ADMIN,                "AMX_WHO_SYNTAX",       .info_ml = true);
	register_concmd("amx_cvar",         "cmdCvar",      ADMIN_CVAR,                 "AMX_CVAR_SYNTAX",      .info_ml = true);
	register_concmd("amx_xvar_float",   "cmdXvar",      ADMIN_CVAR,                 "AMX_XVAR_SYNTAX",      .info_ml = true);
	register_concmd("amx_xvar_int",     "cmdXvar",      ADMIN_CVAR,                 "AMX_XVAR_SYNTAX",      .info_ml = true);
	register_concmd("amx_plugins",      "cmdPlugins",   ADMIN_ADMIN,                "AMX_PLUGINS_SYNTAX",   .info_ml = true);
	register_concmd("amx_modules",      "cmdModules",   ADMIN_ADMIN,                "AMX_MODULES_SYNTAX",   .info_ml = true);
	register_concmd("amx_map",          "cmdMap",       ADMIN_MAP,                  "AMX_MAP_SYNTAX",       .info_ml = true);
	register_concmd("amx_extendmap",    "cmdExtendMap", ADMIN_MAP,                  "AMX_EXTENDMAP_SYNTAX", .info_ml = true);
	register_concmd("amx_cfg",          "cmdCfg",       ADMIN_CFG,                  "AMX_CFG_SYNTAX",       .info_ml = true);
	register_concmd("amx_nick",         "cmdNick",      ADMIN_SLAY,                 "AMX_NICK_SYNTAX",      .info_ml = true);
	register_concmd("amx_last",         "cmdLast",      ADMIN_BAN,                  "AMX_LAST_SYNTAX",      .info_ml = true);
	register_clcmd("amx_rcon",          "cmdRcon",      ADMIN_RCON,                 "AMX_RCON_SYNTAX",      .info_ml = true);
	register_clcmd("amx_showrcon",      "cmdShowRcon",  ADMIN_RCON,                 "AMX_RCON_SYNTAX",      .info_ml = true);
	register_clcmd("pauseAc",           "cmdLBack");

	g_pRconPassword = get_cvar_pointer("rcon_password");
	g_pPausable = get_cvar_pointer("pausable");
	g_pTimelimit = get_cvar_pointer("mp_timelimit");
	g_pTempBanMaxTime = create_cvar("amx_tempban_maxtime", "4320", FCVAR_PROTECTED, "maximum ban time for temporary bans");
	g_tTempBans = TrieCreate();

	new iFlags = get_pcvar_flags(g_pRconPassword);

	if (!(iFlags & FCVAR_PROTECTED))
	{
		set_pcvar_flags(g_pRconPassword, iFlags | FCVAR_PROTECTED);
	}
}

public plugin_end()
{
	TrieDestroy(g_tTempBans);
	TrieDestroy(g_tXvarsFlags);
}

public client_disconnected(id)
{
	if (!is_user_bot(id))
	{
		InsertInfo(id);
	}
}

public cmdKick(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new szPlayer[32];
	read_argv(1, szPlayer, charsmax(szPlayer));

	new iPlayer = cmd_target(id, szPlayer, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF);
	
	if (!iPlayer)
	{
		return PLUGIN_HANDLED;
	}

	new szReason[32];
	read_argv(2, szReason, charsmax(szReason));
	remove_quotes(szReason);

	log_amx("Kick: ^"%N^" kick ^"%N^" (reason ^"%s^")", id, iPlayer, szReason);
	show_activity_key("ADMIN_KICK_1", "ADMIN_KICK_2", fmt("%n", id), fmt("%n", iPlayer));

	if (is_user_bot(iPlayer))
	{
		server_cmd("kick #%d", get_user_userid(iPlayer));
	}
	else
	{
		if (szReason[0])
		{
			server_cmd("kick #%d ^"%s^"", get_user_userid(iPlayer), szReason);
		}
		else
		{
			server_cmd("kick #%d", get_user_userid(iPlayer));
		}
	}
	
	console_print(id, "[AMXX] %l", "ADMIN_KICK_CON", iPlayer);	
	return PLUGIN_HANDLED;
}

public cmdUnban(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szArg[32], szAuth[32];
	read_argv(1, szArg, charsmax(szArg));
	get_user_authid(id, szAuth, charsmax(szAuth));

	if (!(get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON)))
	{
		new szStoredAdminAuth[32];

		if (!TrieGetString(g_tTempBans, szArg, szStoredAdminAuth, charsmax(szStoredAdminAuth)) || !equal(szStoredAdminAuth, szAuth))
		{
			console_print(id, "[AMXX] %l", "ADMIN_MUST_TEMPUNBAN");
			return PLUGIN_HANDLED;
		}
	}
	
	if (contain(szArg, ".") != -1)
	{
		server_cmd("removeip ^"%s^";writeip", szArg);
		console_print(id, "[AMXX] %l", "IP_REMOVED", szArg);
	}
	else
	{
		if (!isCommandArgSafe(szArg))
		{
			console_print(id, "[AMXX] %l", "CL_NOT_FOUND");
			return PLUGIN_HANDLED;
		}

		server_cmd("removeid %s;writeid", szArg);
		console_print(id, "[AMXX] %l", "AUTHID_REMOVED", szArg);
	}

	show_activity_key("ADMIN_UNBAN_1", "ADMIN_UNBAN_2", fmt("%n", id), szArg);
	log_amx("Cmd: ^"%N^" unban ^"%s^"", id, szArg);

	return PLUGIN_HANDLED;
}

/* amx_addban is a special command now.
 * If a user with rcon uses it, it bans the user. No questions asked.
 * If a user without rcon but with ADMIN_BAN uses it, it will scan the old
 * connection queue, and if it finds the info for a player in it, it will
 * check their old access. If they have immunity, it will not ban.
 * If they do not have immunity, it will ban. If the user is not found,
 * it will refuse to ban the target.
 */
public cmdAddBan(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 3, true)) // check for ADMIN_BAN access
	{
		if (get_user_flags(id) & iLevel) // Getting here means they didn't input enough args
		{
			return PLUGIN_HANDLED;
		}
		if (!cmd_access(id, ADMIN_RCON, iCid, 3)) // If somehow they have ADMIN_RCON without ADMIN_BAN, continue
		{
			return PLUGIN_HANDLED;
		}
	}

	new szArg[32], szAuth[32], szMinutes[32], szReason[32];
	read_argv(1, szArg, charsmax(szArg));
	read_argv(2, szMinutes, charsmax(szMinutes));
	read_argv(3, szReason, charsmax(szReason));
	trim(szArg);
	
	if (!(get_user_flags(id) & ADMIN_RCON))
	{
		new bool:bCanBan, bool:bIsIP;
		static const szLimitedAccess[][] = { "STEAM_ID_PENDING", "STEAM_ID_LAN", "HLTV", "4294967295", "VALVE_ID_LAN", "VALVE_ID_PENDING" };

		// Limited access to this command
		for(new i; i < sizeof(szLimitedAccess); i++)
		{
			if(equali(szArg, szLimitedAccess[i]))
			{
				console_print(id, "[AMXX] %l", "ADMIN_CANNOT_BAN", szArg);
				return PLUGIN_HANDLED;
			}
		}
		
		if (contain(szArg, ".") != -1)
		{
			bIsIP = true;
		}
		
		new szName[MAX_NAME_LENGTH], iAccess;

		// Scan the disconnection queue
		if (bIsIP)
		{
			for (new szIP[16], i; i < g_iSize; i++)
			{
				GetInfo(i, szName, charsmax(szName), _, _, szIP, charsmax(szIP), iAccess);
				
				if (equal(szIP, szArg))
				{
					if (iAccess & ADMIN_IMMUNITY)
					{
						console_print(id, "[AMXX] %s : %l", szIP, "CLIENT_IMM", szName);
						return PLUGIN_HANDLED;
					}

					// User did not have immunity
					bCanBan = true;
				}
			}
		}
		else
		{
			for (new szAuth[32], i; i < g_iSize; i++)
			{
				GetInfo(i, szName, charsmax(szName), szAuth, charsmax(szAuth), _, _, iAccess);
				
				if (equal(szAuth, szArg))
				{
					if (iAccess & ADMIN_IMMUNITY)
					{
						console_print(id, "[AMXX] %s : %l", szAuth, "CLIENT_IMM", szName);
						return PLUGIN_HANDLED;
					}

					// User did not have immunity
					bCanBan = true;
				}
			}
		}
		
		if (!bCanBan)
		{
			console_print(id, "[AMXX] %l", "ADMIN_BAN_ONLY_RECENT");
			return PLUGIN_HANDLED;
		}
	}
	
	// User has access to ban their target
	if (contain(szArg, ".") != -1)
	{
		server_cmd("addip ^"%s^" ^"%s^";wait;writeip", szMinutes, szArg);
		console_print(id, "[AMXX] %l", "ADMIN_IP_ADDED", szArg);
	}
	else
	{
		if (!isCommandArgSafe(szArg))
		{
			console_print(id, "[AMXX] %l", "CL_NOT_FOUND");
			return PLUGIN_HANDLED;
		}

		server_cmd("banid ^"%s^" %s;wait;writeid", szMinutes, szArg);
		console_print(id, "[AMXX] %l", "ADMIN_AUTHID_ADDED", szArg);
	}

	get_user_authid(id, szAuth, charsmax(szAuth));
	TrieSetString(g_tTempBans, szArg, szAuth);

	show_activity_key("ADMIN_ADDBAN_1", "ADMIN_ADDBAN_2", fmt("%n", id), szArg);
	log_amx("Cmd: ^"%N^" ban ^"%s^" (minutes ^"%s^") (szReason ^"%s^")", id, szArg, szMinutes, szReason);

	return PLUGIN_HANDLED;
}

public cmdBan(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 3))
	{
		return PLUGIN_HANDLED;
	}

	new szPlayer[32], szMinutes[8], szReason[32];
	read_argv(1, szPlayer, charsmax(szPlayer));
	read_argv(2, szMinutes, charsmax(szMinutes));
	read_argv(3, szReason, charsmax(szReason));
	
	new iPlayer = cmd_target(id, szPlayer, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_NO_BOTS | CMDTARGET_ALLOW_SELF);
	
	if (!iPlayer)
	{
		return PLUGIN_HANDLED;
	}

	new iMinutes = str_to_num(szMinutes);
	new const iTempBanMaxTime = get_pcvar_num(g_pTempBanMaxTime);

	if (iMinutes < 0) // since negative values result in permanent bans
	{
		iMinutes = 0;
		szMinutes = "0";
	}

	if (!(get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON)) && (iMinutes <= 0 || iMinutes > iTempBanMaxTime))
	{
		console_print(id, "[AMXX] %l", "ADMIN_MUST_TEMPBAN", iTempBanMaxTime);
		return PLUGIN_HANDLED;
	}

	new szAuth[32], szAuth2[32], szName[MAX_NAME_LENGTH], szName2[MAX_NAME_LENGTH];
	get_user_authid(iPlayer, szAuth2, charsmax(szAuth2));
	get_user_authid(id, szAuth, charsmax(szAuth));
	get_user_name(iPlayer, szName2, charsmax(szName2));
	get_user_name(id, szName, charsmax(szName));
	
	log_amx("Ban: ^"%N^" ban and kick ^"%N^" (minutes ^"%s^") (reason ^"%s^")", id, iPlayer, szMinutes, szReason);
	TrieSetString(g_tTempBans, szAuth2, szAuth); // store all bans in case a permanent ban would override a temporary one
	
	new szTemp[64], szBanned[16];

	if (iMinutes)
	{
		formatex(szTemp, charsmax(szTemp), "%L", iPlayer, "FOR_MIN", szMinutes);
	}
	else
	{
		formatex(szTemp, charsmax(szTemp), "%L", iPlayer, "PERM");
	}

	formatex(szBanned, charsmax(szBanned), "%L", iPlayer, "BANNED");

	if (szReason[0])
	{
		server_cmd("kick #%d ^"%s (%s %s)^";wait;banid %s %s;wait;writeid", get_user_userid(iPlayer), szReason, szBanned, szTemp, szMinutes, szAuth2);
	}
	else
	{
		server_cmd("kick #%d ^"%s %s^";wait;banid %s %s;wait;writeid", get_user_userid(iPlayer), szBanned, szTemp, szMinutes, szAuth2);
	}

	// display the message to all clients

	new szMessage[256], iPlayers[MAX_PLAYERS], iPnum, iLen;
	get_players_ex(iPlayers, iPnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new iPlayer, i; i < iPnum; i++)
	{
		iPlayer = iPlayers[i];

		iLen = formatex(szMessage, charsmax(szMessage), "%L", iPlayer, "BAN");
		iLen += formatex(szMessage[iLen], charsmax(szMessage) - iLen, " %s ", szName2);

		if (iMinutes)
		{
			iLen += formatex(szMessage[iLen], charsmax(szMessage) - iLen, "%L", iPlayer, "FOR_MIN", szMinutes);
		}
		else
		{
			iLen += formatex(szMessage[iLen], charsmax(szMessage) - iLen, "%L", iPlayer, "PERM");
		}

		if (strlen(szReason) > 0)
		{
			formatex(szMessage[iLen], charsmax(szMessage) - iLen, " (%L: %s)", iPlayer, "REASON", szReason);
		}

		show_activity_id(iPlayer, id, szName, szMessage);
	}
	
	console_print(id, "[AMXX] %l", "CLIENT_BANNED", szName2);
	return PLUGIN_HANDLED;
}

public cmdBanIP(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 3))
	{
		return PLUGIN_HANDLED;
	}
	
	new szPlayer[32], szMinutes[8], szReason[32];
	read_argv(1, szPlayer, charsmax(szPlayer));
	read_argv(2, szMinutes, charsmax(szMinutes));
	read_argv(3, szReason, charsmax(szReason));
	
	new iPlayer = cmd_target(id, szPlayer, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_NO_BOTS | CMDTARGET_ALLOW_SELF);
	
	if (!iPlayer)
	{
		return PLUGIN_HANDLED;
	}

	new iMinutes = str_to_num(szMinutes);
	new const iTempBanMaxTime = get_pcvar_num(g_pTempBanMaxTime);

	if (iMinutes < 0) // since negative values result in permanent bans
	{
		iMinutes = 0;
		szMinutes = "0";
	}

	if (!(get_user_flags(id) & (ADMIN_BAN | ADMIN_RCON)) && (iMinutes <= 0 || iMinutes > iTempBanMaxTime))
	{
		console_print(id, "[AMXX] %l", "ADMIN_MUST_TEMPBAN", iTempBanMaxTime);
		return PLUGIN_HANDLED;
	}
	
	new szAuth[32], szAuth2[32], szName[MAX_NAME_LENGTH], szName2[MAX_NAME_LENGTH];
	get_user_authid(iPlayer, szAuth2, charsmax(szAuth2));
	get_user_authid(id, szAuth, charsmax(szAuth));
	get_user_name(iPlayer, szName2, charsmax(szName2));
	get_user_name(id, szName, charsmax(szName));
	
	log_amx("Ban: ^"%N^" ban and kick ^"%N^" (minutes ^"%s^") (reason ^"%s^")", id, iPlayer, szMinutes, szReason);
	TrieSetString(g_tTempBans, szAuth2, szAuth);

	new szTemp[64], szBanned[16];

	if (iMinutes)
	{
		formatex(szTemp, charsmax(szTemp), "%L", iPlayer, "FOR_MIN", szMinutes);
	}
	else
	{
		formatex(szTemp, charsmax(szTemp), "%L", iPlayer, "PERM");
	}

	formatex(szBanned, charsmax(szBanned), "%L", iPlayer, "BANNED");

	new szIP[16];
	get_user_ip(iPlayer, szIP, charsmax(szIP), 1);

	if (szReason[0])
	{
		server_cmd("kick #%d ^"%s (%s %s)^";wait;addip ^"%s^" ^"%s^";wait;writeip", get_user_userid(iPlayer), szReason, szBanned, szTemp, szMinutes, szIP);
	}
	else
	{
		server_cmd("kick #%d ^"%s %s^";wait;addip ^"%s^" ^"%s^";wait;writeip", get_user_userid(iPlayer), szBanned, szTemp, iMinutes, szIP);
	}

	// display the message to all clients

	new szMessage[256], iPlayers[MAX_PLAYERS], iPnum, iLen;
	get_players_ex(iPlayers, iPnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new iPlayer, i; i < iPnum; i++)
	{
		iPlayer = iPlayers[i];

		iLen = formatex(szMessage, charsmax(szMessage), "%L", iPlayer, "BAN");
		iLen += formatex(szMessage[iLen], charsmax(szMessage) - iLen, " %s ", szName2);

		if (iMinutes)
		{
			formatex(szMessage[iLen], charsmax(szMessage) - iLen, "%L", iPlayer, "FOR_MIN", szMinutes);
		}
		else
		{
			formatex(szMessage[iLen], charsmax(szMessage) - iLen, "%L", iPlayer, "PERM");
		}

		if (strlen(szReason) > 0)
		{
			formatex(szMessage[iLen], charsmax(szMessage) - iLen, " (%L: %s)", iPlayer, "REASON", szReason);
		}

		show_activity_id(iPlayer, id, szName, szMessage);
	}

	console_print(id, "[AMXX] %l", "CLIENT_BANNED", szName2);
	return PLUGIN_HANDLED;
}

public cmdSlay(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szPlayer[32];	
	read_argv(1, szPlayer, charsmax(szPlayer));
	
	new iPlayer = cmd_target(id, szPlayer, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF | CMDTARGET_ONLY_ALIVE);
	
	if (!iPlayer)
	{
		return PLUGIN_HANDLED;
	}
	
	user_kill(iPlayer);

	log_amx("Cmd: ^"%N^" slay ^"%N^"", id, iPlayer);
	show_activity_key("ADMIN_SLAY_1", "ADMIN_SLAY_2", fmt("%n", id), fmt("%n", iPlayer));
	console_print(id, "[AMXX] %l", "CLIENT_SLAYED", iPlayer);

	return PLUGIN_HANDLED;
}

public cmdSlap(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new szPlayer[32];
	read_argv(1, szPlayer, charsmax(szPlayer));

	new iPlayer = cmd_target(id, szPlayer, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF | CMDTARGET_ONLY_ALIVE);
	
	if (!iPlayer)
	{
		return PLUGIN_HANDLED;
	}

	new iDamage = clamp(read_argv_int(2), 0);
	user_slap(iPlayer, iDamage);
	
	log_amx("Cmd: ^"%N^" slap ^"%N^" (%d damage)", id, iPlayer, iDamage);
	show_activity_key("ADMIN_SLAP_1", "ADMIN_SLAP_2", fmt("%n", id), fmt("%n", iPlayer), iDamage);
	console_print(id, "[AMXX] %l", "CLIENT_SLAPPED", iPlayer, iDamage);
	
	return PLUGIN_HANDLED;
}

public cmdMap(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new szMap[32], iMapLen = read_argv(1, szMap, charsmax(szMap));
	
	if (!is_map_valid(szMap))
	{
		console_print(id, "[AMXX] %l", "MAP_NOT_FOUND");
		return PLUGIN_HANDLED;
	}

	show_activity_key("ADMIN_MAP_1", "ADMIN_MAP_2", fmt("%n", id), szMap);
	log_amx("Cmd: ^"%N^" change map to ^"%s^"", id, szMap);
	
	static szMod[10];

	if(!szMod[0])
	{
		get_modname(szMod, charsmax(szMod));
	}
	
	if (!equal(szMod, "zp"))
	{
		message_begin(MSG_ALL, SVC_INTERMISSION);
		message_end();
	}
	
	set_task(2.0, "ChangeMap", 0, szMap, iMapLen + 1);
	return PLUGIN_HANDLED;
}

public cmdExtendMap(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new iMinutes = read_argv_int(1);
	
	if (iMinutes <= 0)
	{
		return PLUGIN_HANDLED;
	}
	
	static szMap[32];

	if(!szMap[0])
	{
		get_mapname(szMap, charsmax(szMap));
	}

	set_pcvar_num(g_pTimelimit, get_pcvar_num(g_pTimelimit) + iMinutes);

	show_activity_key("ADMIN_EXTEND_1", "ADMIN_EXTEND_2", fmt("%n", id), iMinutes);
	log_amx("ExtendMap: ^"%N^" extend map ^"%s^" for %d minutes", id, szMap, iMinutes);
	console_print(id, "%L", id, "MAP_EXTENDED", szMap, iMinutes);
	
	return PLUGIN_HANDLED;
}

public cmdCvar(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szCvar[32], szValue[64];
	read_argv(1, szCvar, charsmax(szCvar));
	read_argv(2, szValue, charsmax(szValue));
	
	new pCvar;
	
	if (equal(szCvar, "add") && (get_user_flags(id) & ADMIN_RCON))
	{
		if ((pCvar = get_cvar_pointer(szValue))!=0)
		{
			new flags = get_pcvar_flags(pCvar);
			
			if (!(flags & FCVAR_PROTECTED))
			{
				set_pcvar_flags(pCvar, flags | FCVAR_PROTECTED);
			}
		}

		return PLUGIN_HANDLED;
	}
	
	trim(szCvar);
	
	if ((pCvar = get_cvar_pointer(szCvar)) == 0)
	{
		console_print(id, "[AMXX] %l", "UNKNOWN_CVAR", szCvar);
		return PLUGIN_HANDLED;
	}
	
	if (onlyRcon(szCvar) && !(get_user_flags(id) & ADMIN_RCON))
	{
		// Exception for the new onlyRcon rules:
		// sv_password is allowed to be modified by ADMIN_PASSWORD
		if (!(equali(szCvar,"sv_password") && (get_user_flags(id) & ADMIN_PASSWORD)))
		{
			console_print(id, "[AMXX] %l", "CVAR_NO_ACC");
			return PLUGIN_HANDLED;
		}
	}
	
	if (read_argc() < 3)
	{
		get_pcvar_string(pCvar, szValue, charsmax(szValue));
		console_print(id, "[AMXX] %l", "CVAR_IS", szCvar, szValue);
		return PLUGIN_HANDLED;
	}
	
	if (equali(szCvar, "servercfgfile") || equali(szCvar, "lservercfgfile"))
	{
		new iPos = contain(szValue, ";");

		if (iPos != -1)
		{
			szValue[iPos] = '^0';
		}
	}

	log_amx("Cmd: ^"%N^" set cvar (name ^"%s^") (value ^"%s^")", id, szCvar, szValue);
	set_pcvar_string(pCvar, szValue);
	
	// display the message to all clients

	new szCvarValue[64], iPlayers[MAX_PLAYERS], iPnum;
	get_players_ex(iPlayers, iPnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new iPlayer, i; i < iPnum; i++)
	{
		iPlayer = iPlayers[i];

		if (get_pcvar_flags(pCvar) & FCVAR_PROTECTED || equali(szCvar, "rcon_password"))
		{
			formatex(szCvarValue, charsmax(szCvarValue), "*** %L ***", iPlayer, "PROTECTED");
		}
		else
		{
			copy(szCvarValue, charsmax(szCvarValue), szValue);
		}

		show_activity_id(iPlayer, id, fmt("%n", id), "%L", iPlayer, "SET_CVAR_TO", "", szCvar, szCvarValue);
	}

	console_print(id, "[AMXX] %l", "CVAR_CHANGED", szCvar, szValue);
	return PLUGIN_HANDLED;
}

public cmdXvar(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}

	new szCommand[15], szXvar[32], szValue[32];
	read_argv(0, szCommand, charsmax(szCommand));
	read_argv(1, szXvar, charsmax(szXvar));
	trim(szXvar);

	if (read_argc() > 2)
	{
		read_argv(2, szValue, charsmax(szValue));
		trim(szValue);

		if (equali(szXvar, "add"))
		{
			if (get_user_flags(id) & ADMIN_RCON && xvar_exists(szValue))
			{
				if (!g_tXvarsFlags)
				{
					g_tXvarsFlags = TrieCreate();
				}

				TrieSetCell(g_tXvarsFlags, szValue, 1);
			}

			return PLUGIN_HANDLED;
		}
	}

	new bool:bFloat = equali(szCommand, "amx_xvar_float") != 0;
	new iXvar = get_xvar_id(szXvar);

	if (iXvar == -1)
	{
		console_print(id, "[AMXX] %l", "UNKNOWN_XVAR", szXvar);
		return PLUGIN_HANDLED;
	}

	new any:value;

	if (!szValue[0]) // get value
	{
		value = get_xvar_num(iXvar);

		if (bFloat)
		{
			float_to_str(value, szValue, charsmax(szValue));
		}
		else
		{
			num_to_str(value, szValue, charsmax(szValue));
		}

		console_print(id, "[AMXX] %l", "XVAR_IS", szXvar, szValue);
		return PLUGIN_HANDLED;
	}

	// set value
	if (g_tXvarsFlags && TrieKeyExists(g_tXvarsFlags, szXvar) && ~get_user_flags(id) & ADMIN_RCON)
	{
		console_print(id, "[AMXX] %l", "XVAR_NO_ACC");
		return PLUGIN_HANDLED;
	}

	new iEndPos;

	if (bFloat)
	{
		value = strtof(szValue, iEndPos);

		if (!iEndPos)
		{
			return PLUGIN_HANDLED;
		}
	}
	else
	{
		value = strtol(szValue, iEndPos);

		if (!iEndPos)
		{
			return PLUGIN_HANDLED;
		}
	}

	set_xvar_num(iXvar, value);

	// convert back value to string so admin can know value has been set correctly
	if (bFloat)
	{
		float_to_str(value, szValue, charsmax(szValue));
	}
	else
	{
		num_to_str(value, szValue, charsmax(szValue));
	}

	log_amx("Cmd: ^"%N^" set xvar (name ^"%s^") (value ^"%s^")", id, szXvar, szValue);
	
	// display the message to all clients
	new iPlayers[MAX_PLAYERS], iPnum;
	get_players_ex(iPlayers, iPnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new iPlayer, i; i < iPnum; i++)
	{
		iPlayer = iPlayers[i];
		show_activity_id(iPlayer, id, fmt("%n", id), "%L", iPlayer, "SET_XVAR_TO", "", szXvar, szValue);
	}
	
	console_print(id, "[AMXX] %l", "XVAR_CHANGED", szXvar, szValue);
	return PLUGIN_HANDLED;
}

public cmdPlugins(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
	{
		return PLUGIN_HANDLED;
	}
		
	if (!id) // if server executes redirect this to "amxx plugins" for more in-depth output
	{
		server_cmd("amxx plugins");
		server_exec();
		return PLUGIN_HANDLED;
	}

	new iPluginsNum = get_pluginsnum();
	new szTemp[96], szName[32], szVersion[32], szAuthor[32], szFileName[32], szStatus[32], iStartPlugin, iEndPlugin, iPluginsRunning;
	
	if (read_argc() > 1)
	{
		iStartPlugin = read_argv_int(1) - 1; // zero-based
	}

	iEndPlugin = min(iStartPlugin + 10, iPluginsNum);
	
	console_print(id, "----- %l -----", "LOADED_PLUGINS");
	console_print(id, "%-18.17s %-11.10s %-17.16s %-16.15s %-9.8s", fmt("%l", "NAME"), fmt("%l", "VERSION"), fmt("%l", "AUTHOR"), fmt("%l", "FILE"), fmt("%l", "STATUS"));

	new i = iStartPlugin;

	while (i < iEndPlugin)
	{
		get_plugin(i++, szFileName, charsmax(szFileName), szName, charsmax(szName), szVersion, charsmax(szVersion), szAuthor, charsmax(szAuthor), szStatus, charsmax(szStatus));
		console_print(id, "%-18.17s %-11.10s %-17.16s %-16.15s %-9.8s", szName, szVersion, szAuthor, szFileName, szStatus);
		
		if (szStatus[0] == 'd' || szStatus[0] == 'r') // "debug" or "running"
		{
			iPluginsRunning++;
		}
	}

	console_print(id, "%l", "PLUGINS_RUN", iEndPlugin - iStartPlugin, iPluginsRunning);
	console_print(id, "----- %l -----", "HELP_ENTRIES", iStartPlugin + 1, iEndPlugin, iPluginsNum);
	
	if (iEndPlugin < iPluginsNum)
	{
		formatex(szTemp, charsmax(szTemp), "----- %L -----", id, "HELP_USE_MORE", "amx_help", iEndPlugin + 1);
		replace_string(szTemp, charsmax(szTemp), "amx_help", "amx_plugins");
		console_print(id, szTemp);
	}
	else
	{
		formatex(szTemp, charsmax(szTemp), "----- %L -----", id, "HELP_USE_BEGIN", "amx_help");
		replace_string(szTemp, charsmax(szTemp), "amx_help", "amx_plugins");
		console_print(id, szTemp);
	}

	return PLUGIN_HANDLED;
}

public cmdModules(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
	{
		return PLUGIN_HANDLED;
	}

	new iModulesNum = get_modulesnum();

	console_print(id, "%l:", "LOADED_MODULES");
	console_print(id, "%-23.22s %-11.10s %-20.19s %-11.10s", fmt("%l", "NAME"), fmt("%l", "VERSION"), fmt("%l", "AUTHOR"), fmt("%l", "STATUS"));
	
	for (new szName[32], szVersion[32], szAuthor[32], szStatus[16], iStatus, i; i < iModulesNum; i++)
	{
		get_module(i, szName, charsmax(szName), szAuthor, charsmax(szAuthor), szVersion, charsmax(szVersion), iStatus);
		
		switch (iStatus)
		{
			case module_loaded:
			{
				formatex(szStatus, charsmax(szStatus), "%L", id, "MODULE_RUNNING");
			}
			default:
			{
				formatex(szStatus, charsmax(szStatus), "%L", id, "MODULE_BAD_LOAD");
				formatex(szName, charsmax(szName), "%L", id, "MODULE_UNKNOWN");
				formatex(szAuthor, charsmax(szAuthor), "%L", id, "MODULE_UNKNOWN");
				formatex(szVersion, charsmax(szVersion), "%L", id, "MODULE_UNKNOWN");
			}
		}
		
		console_print(id, "%-23.22s %-11.10s %-20.19s %-11.10s", szName, szVersion, szAuthor, szStatus);
	}

	console_print(id, "%l", "NUM_MODULES", iModulesNum);
	return PLUGIN_HANDLED;
}

public cmdCfg(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szFile[128];
	read_argv(1, szFile, charsmax(szFile));
	
	if (!file_exists(szFile))
	{
		console_print(id, "[AMXX] %l", "FILE_NOT_FOUND", szFile);
		return PLUGIN_HANDLED;
	}
	
	log_amx("Cmd: ^"%N^" execute cfg file ^"%s^"", id, szFile);
	console_print(id, "[AMXX] %l", id, szFile);
	show_activity_key("ADMIN_CONF_1", "ADMIN_CONF_2", fmt("%n", id), szFile);

	server_cmd("exec ^"%s^"", szFile);
	return PLUGIN_HANDLED;
}

public cmdLBack()
{
	if (!g_bPauseAllowed)
	{
		return PLUGIN_CONTINUE;
	}

	set_pcvar_float(g_pPausable, g_fPausable);
	console_print(g_iPauseCon, "[AMXX] %l", g_bPaused ? "ADMIN_UNPAUSE_CON" : "ADMIN_PAUSE_CON");

	g_bPauseAllowed = false;
	g_bPaused = !g_bPaused;
	return PLUGIN_HANDLED;
}

public cmdPause(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	new iPlayer = id;
	
	if (g_pPausable)
	{
		g_fPausable = get_pcvar_float(g_pPausable);
	}
	
	if (!iPlayer)
	{
		iPlayer = find_player_ex(FindPlayer_ExcludeBots);
	}
	
	if (!iPlayer)
	{ 
		console_print(id, "[AMXX] %l", "UNABLE_PAUSE");
		return PLUGIN_HANDLED;
	}

	g_bPauseAllowed = true;
	set_pcvar_float(g_pPausable, 1.0);
	client_cmd(iPlayer, "pause;pauseAck");
	
	log_amx("Cmd: ^"%N^" %s server", id, g_bPaused ? "unpause" : "pause");
	console_print(id, "[AMXX] %l", g_bPaused ? "UNPAUSING" : "PAUSING");

	// display the message to all clients

	new iPlayers[MAX_PLAYERS], iPnum;
	get_players_ex(iPlayers, iPnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);

	for (new i; i < iPnum; i++)
	{
		show_activity_id(iPlayers[i], id, fmt("%n", id), "%L server", i, g_bPaused ? "UNPAUSE" : "PAUSE");
	}

	g_iPauseCon = id;	
	return PLUGIN_HANDLED;
} 

public cmdShowRcon(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
		
	new szPassword[64];	
	get_pcvar_string(g_pRconPassword, szPassword, charsmax(szPassword));
	
	if (!szPassword[0])
	{
		cmdRcon(id, iLevel, iCid);
	} 
	else 
	{
		new szArgs[128];		
		read_args(szArgs, charsmax(szArgs));
		client_cmd(id, "rcon_password %s", szPassword);
		client_cmd(id, "rcon %s", szArgs);
	}
	
	return PLUGIN_HANDLED;
}

public cmdRcon(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szCommand[128];
	read_args(szCommand, charsmax(szCommand));

	log_amx("Cmd: ^"%N^" server console (cmdline ^"%s^")", id, szCommand);
	console_print(id, "[AMXX] %l", "COM_SENT_SERVER", szCommand);
	server_cmd("%s", szCommand);

	return PLUGIN_HANDLED;
}

public cmdWho(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
	{
		return PLUGIN_HANDLED;
	}

	new szYes[16], szNo[16], iPlayers[MAX_PLAYERS], iPnum, szAuth[32], szName[MAX_NAME_LENGTH];
	formatex(szYes, charsmax(szYes), "%L", id, "YES");
	formatex(szNo, charsmax(szNo), "%L", id, "NO");
	
	get_players_ex(iPlayers, iPnum, GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV);
	console_print(id, "^n%s:^n #  %-16.15s %-20s %-8s %-4.3s %-4.3s %s", fmt("%l", "CLIENTS_ON_SERVER"), "nick", "authid", "userid", fmt("%l", "IMMU"), fmt("%l", "RESERV"), fmt("%l", "ACCESS"));
	
	for (new szFlags[32], iPlayer, iFlags, i; i < iPnum; i++)
	{
		iPlayer = iPlayers[i];
		iFlags = get_user_flags(iPlayer);

		get_user_authid(iPlayer, szAuth, charsmax(szAuth));
		get_user_name(iPlayer, szName, charsmax(szName));		
		get_flags(iFlags, szFlags, charsmax(szFlags));

		console_print(id, "%2d  %-16.15s %-20s %-8d %-6.5s %-6.5s %s", iPlayer, szName, szAuth,
		get_user_userid(iPlayer), iFlags & ADMIN_IMMUNITY ? szYes : szNo, iFlags & ADMIN_RESERVATION ? szYes : szNo, szFlags);
	}
	
	console_print(id, "%l", "TOTAL_NUM", iPnum);
	log_amx("Cmd: ^"%N^" ask for players list", id);
	return PLUGIN_HANDLED;
}

public cmdLeave(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szTags[4][32], iTagsNum;
	
	for (new iArgNum = read_argc(), i = 1; i < 5; i++)
	{
		if (i < iArgNum)
		{
			read_argv(i, szTags[iTagsNum++], charsmax(szTags[]));
		}
		else
		{
			szTags[iTagsNum++][0] = 0;
		}
	}
	
	new szName[MAX_NAME_LENGTH], iPlayers[MAX_PLAYERS], iPnum, iCount;
	get_players_ex(iPlayers, iPnum, GetPlayers_IncludeConnecting);
	
	for (new iPlayer, iRes, i; i < iPnum; i++)
	{
		iPlayer = iPlayers[i];
		iRes = hasTag(szName, szTags, iTagsNum);
		get_user_name(iPlayer, szName, charsmax(szName));
		
		if (iRes != -1)
		{
			console_print(id, "[AMXX] %l", "SKIP_MATCH", szName, szTags[iRes]);
			continue;
		}
		
		if (get_user_flags(iPlayer) & ADMIN_IMMUNITY)
		{
			console_print(id, "[AMXX] %l", "SKIP_IMM", szName);
			continue;
		}
		
		console_print(id, "[AMXX] %l", "KICK_PL", szName);
		
		if (is_user_bot(iPlayer))
		{
			server_cmd("kick #%d", get_user_userid(iPlayer));
		}
		else
		{
			server_cmd("kick #%d ^"%L^"", get_user_userid(iPlayer), iPlayer, "YOU_DROPPED");
		}

		iCount++;
	}
	
	console_print(id, "[AMXX] %l", "KICKED_CLIENTS", iCount);
	log_amx("Kick: ^"%N^" leave some group (tag1 ^"%s^") (tag2 ^"%s^") (tag3 ^"%s^") (tag4 ^"%s^")", id, szTags[0], szTags[1], szTags[2], szTags[3]);
	show_activity_key("ADMIN_LEAVE_1", "ADMIN_LEAVE_2", fmt("%n", id), szTags[0], szTags[1], szTags[2], szTags[3]);

	return PLUGIN_HANDLED;
}

public cmdNick(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 3))
	{
		return PLUGIN_HANDLED;
	}

	new szPlayer[32], szNick[32], szName2[MAX_NAME_LENGTH];
	read_argv(1, szPlayer, charsmax(szPlayer));
	read_argv(2, szNick, charsmax(szNick));

	new iPlayer = cmd_target(id, szPlayer, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF);
	
	if (!iPlayer)
	{
		return PLUGIN_HANDLED;
	}

	get_user_name(iPlayer, szName2, charsmax(szName2));
	set_user_info(iPlayer, "name", szNick);

	log_amx("Cmd: ^"%N^" change nick to ^"%s^" ^"%N^"", id, szNick, iPlayer);
	show_activity_key("ADMIN_NICK_1", "ADMIN_NICK_2", fmt("%n", id), szName2, szNick);
	console_print(id, "[AMXX] %l", "CHANGED_NICK", szName2, szNick);

	return PLUGIN_HANDLED;
}

public cmdLast(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	// This alignment is a bit weird (it should grow if the name is larger)
	// but otherwise for the more common shorter name, it'll wrap in server console
	// Steam client display is all skewed anyway because of the non fixed font.
	console_print(id, "%19s %20s %15s %s", "name", "authid", "ip", "access");
	
	for (new szName[MAX_NAME_LENGTH], szAuth[32], szFlags[32], szIP[16], iAccess, i; i < g_iSize; i++)
	{
		GetInfo(i, szName, charsmax(szName), szAuth, charsmax(szAuth), szIP, charsmax(szIP), iAccess);
		get_flags(iAccess, szFlags, charsmax(szFlags));
		console_print(id, "%19s %20s %15s %s", szName, szAuth, szIP, szFlags);
	}
	
	console_print(id, "[AMXX] %l", "ADMIN_OLD_CONNS_SAVED", g_iSize);
	return PLUGIN_HANDLED;
}

InsertInfo(id)
{
	// Scan to see if this entry is the last entry in the list
	// If it is, then update the name and access
	// If it is not, then insert it again.

	if (g_iSize > 0)
	{
		new szAuth[34], szIP[16], iLast;
		get_user_authid(id, szAuth, charsmax(szAuth));
		get_user_ip(id, szIP, charsmax(szIP), 1);

		if (g_iSize < sizeof(g_szSteamIDs))
		{
			iLast = g_iSize - 1;
		}
		else
		{
			iLast = g_iTracker - 1;
			
			if (iLast < 0)
			{
				iLast = g_iSize - 1;
			}
		}
		
		if (equal(szAuth, g_szSteamIDs[iLast]) && equal(szIP, g_szIPs[iLast])) // need to check ip too, or all the nosteams will while it doesn't work with their illegitimate server
		{
			get_user_name(id, g_szNames[iLast], charsmax(g_szNames[]));
			g_iAccess[iLast] = get_user_flags(id);
			return;
		}
	}
	
	// Need to insert the entry
	
	new iTarget;  // the slot to save the info at

	// Queue is not yet full
	if (g_iSize < sizeof(g_szSteamIDs))
	{
		iTarget = g_iSize;
		++g_iSize;
	}
	else
	{
		iTarget = g_iTracker;
		++g_iTracker;

		// If we reached the end of the array, then move to the front
		if (g_iTracker == sizeof(g_szSteamIDs))
		{
			g_iTracker = 0;
		}
	}
	
	get_user_authid(id, g_szSteamIDs[iTarget], charsmax(g_szSteamIDs[]));
	get_user_name(id, g_szNames[iTarget], charsmax(g_szNames[]));
	get_user_ip(id, g_szIPs[iTarget], charsmax(g_szIPs[]), 1);
	g_iAccess[iTarget] = get_user_flags(id);
}

GetInfo(i, szName[], iNameLen, szAuth[] = "", iAuthLen = 0, szIP[] = "", iIPLen = 0, &iAccess = 0)
{
	if (i >= g_iSize)
	{
		abort(AMX_ERR_NATIVE, "GetInfo: Out of bounds (%d:%d)", i, g_iSize);
	}
	
	new iTarget = (g_iTracker + i) % sizeof(g_szSteamIDs);
	
	copy(szName, iNameLen, g_szNames[iTarget]);
	copy(szAuth, iAuthLen, g_szSteamIDs[iTarget]);
	copy(szIP,   iIPLen,   g_szIPs[iTarget]);
	iAccess = g_iAccess[iTarget];
	
}

/**
 * ';' and '\n' are command delimiters. If a command arg contains these 2
 * it is not safe to be passed to server_cmd() as it may be trying to execute
 * a command.
 */
isCommandArgSafe(const szArg[])
{
	return contain(szArg, ";") == -1 && contain(szArg, "\n") == -1;
}

public ChangeMap(szMap[])
{
	engine_changelevel(szMap);
}

bool:onlyRcon(const szName[])
{
	new iPointer = get_cvar_pointer(szName);
	return iPointer && get_pcvar_flags(iPointer) & FCVAR_PROTECTED;
}

hasTag(szName[], szTags[4][32], iTagsNum)
{
	for (new i = 0; i < iTagsNum; i++)
	{
		if (contain(szName, szTags[i]) != -1)
		{
			return i;
		}
	}

	return -1;
}
