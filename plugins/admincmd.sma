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

public client_disconnected(id)
{
	if (!is_user_bot(id))
	{
		InsertInfo(id);
	}
}

public plugin_init()
{
	register_plugin("Admin Commands", AMXX_VERSION_STR, "AMXX Dev Team");

	register_dictionary("admincmd.txt");
	register_dictionary("adminhelp.txt");
	register_dictionary("common.txt");

	register_concmd("amx_kick",			"cmdKick",		ADMIN_KICK,					"AMX_KICK_SYNTAX",		.info_ml = true);
	register_concmd("amx_ban",			"cmdBan",		ADMIN_BAN|ADMIN_BAN_TEMP,	"AMX_BAN_SYNTAX",		.info_ml = true);
	register_concmd("amx_banip",		"cmdBanIP",		ADMIN_BAN|ADMIN_BAN_TEMP,	"AMX_BANIP_SYNTAX",		.info_ml = true);
	register_concmd("amx_addban",		"cmdAddBan",	ADMIN_BAN,					"AMX_ADDBAN_SYNTAX",	.info_ml = true);
	register_concmd("amx_unban",		"cmdUnban",		ADMIN_BAN|ADMIN_BAN_TEMP,	"AMX_UNBAN_SYNTAX",		.info_ml = true);
	register_concmd("amx_slay",			"cmdSlay",		ADMIN_SLAY,					"AMX_SLAY_SYNTAX",		.info_ml = true);
	register_concmd("amx_slap",			"cmdSlap",		ADMIN_SLAY,					"AMX_SLAP_SYNTAX",		.info_ml = true);
	register_concmd("amx_leave",		"cmdLeave",		ADMIN_KICK,					"AMX_LEAVE_SYNTAX",		.info_ml = true);
	register_concmd("amx_pause",		"cmdPause",		ADMIN_CVAR,					"AMX_PAUSE_SYNTAX",		.info_ml = true);
	register_concmd("amx_who",			"cmdWho",		ADMIN_ADMIN,				"AMX_WHO_SYNTAX",		.info_ml = true);
	register_concmd("amx_cvar",			"cmdCvar",		ADMIN_CVAR,					"AMX_CVAR_SYNTAX",		.info_ml = true);
	register_concmd("amx_xvar_float",	"cmdXvar",		ADMIN_CVAR,					"AMX_XVAR_SYNTAX",		.info_ml = true);
	register_concmd("amx_xvar_int",		"cmdXvar",		ADMIN_CVAR,					"AMX_XVAR_SYNTAX",		.info_ml = true);
	register_concmd("amx_plugins",		"cmdPlugins",	ADMIN_ADMIN,				"AMX_PLUGINS_SYNTAX",	.info_ml = true);
	register_concmd("amx_modules",		"cmdModules",	ADMIN_ADMIN,				"AMX_MODULES_SYNTAX",	.info_ml = true);
	register_concmd("amx_map",			"cmdMap",		ADMIN_MAP,					"AMX_MAP_SYNTAX",		.info_ml = true);
	register_concmd("amx_extendmap",	"cmdExtendMap",	ADMIN_MAP,					"AMX_EXTENDMAP_SYNTAX",	.info_ml = true);
	register_concmd("amx_cfg",			"cmdCfg",		ADMIN_CFG,					"AMX_CFG_SYNTAX",		.info_ml = true);
	register_concmd("amx_nick",			"cmdNick",		ADMIN_SLAY,					"AMX_NICK_SYNTAX",		.info_ml = true);
	register_concmd("amx_last",			"cmdLast",		ADMIN_BAN,					"AMX_LAST_SYNTAX",		.info_ml = true);
	register_clcmd("amx_rcon",			"cmdRcon",		ADMIN_RCON,					"AMX_RCON_SYNTAX",		.info_ml = true);
	register_clcmd("amx_showrcon",		"cmdShowRcon",	ADMIN_RCON,					"AMX_RCON_SYNTAX",		.info_ml = true);
	register_clcmd("pauseAc",			"cmdLBack");

	g_pRconPassword = get_cvar_pointer("rcon_password");
	g_pPausable = get_cvar_pointer("pausable");
	g_pTimelimit = get_cvar_pointer("mp_timelimit");
	g_pTempBanMaxTime = register_cvar("amx_tempban_maxtime", "4320", FCVAR_PROTECTED);
	g_tTempBans = TrieCreate();

	new iFlags = get_pcvar_flags(g_pRconPassword);

	if (!(iFlags & FCVAR_PROTECTED))
	{
		set_pcvar_flags(g_pRconPassword, iFlags | FCVAR_PROTECTED);
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
	return PLUGIN_HANDLED
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

public cmdUnban(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
	{
		return PLUGIN_HANDLED;
	}
	
	new szArg[32], szAuth[32];
	read_argv(1, szArg, charsmax(szArg));
	get_user_authid(id, szAuth, charsmax(szAuth));

	if (!(get_user_flags(id) & ( ADMIN_BAN | ADMIN_RCON )))
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

	return PLUGIN_HANDLED
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

	if ( iMinutes < 0 ) // since negative values result in permanent bans
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
	get_players(iPlayers, iPnum, "ch");

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
	
	console_print(id, "[AMXX] %l", "CLIENT_BANNED", szName2)
	return PLUGIN_HANDLED
}

public cmdBanIP(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 3))
		return PLUGIN_HANDLED
	
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

	if ( iMinutes < 0 ) // since negative values result in permanent bans
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
	get_players(iPlayers, iPnum, "ch")

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

	console_print(id, "[AMXX] %l", "CLIENT_BANNED", szName2)
	return PLUGIN_HANDLED
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

	return PLUGIN_HANDLED
}

public cmdSlap(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED

	new arg[32]
	
	read_argv(1, arg, charsmax(arg))
	new player = cmd_target(id, arg, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF | CMDTARGET_ONLY_ALIVE)
	
	if (!player)
		return PLUGIN_HANDLED

	new spower[32], authid[32], name2[MAX_NAME_LENGTH], authid2[32], name[MAX_NAME_LENGTH]
	
	read_argv(2, spower, charsmax(spower))
	
	new damage = clamp( str_to_num(spower), 0)
	
	user_slap(player, damage)
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	get_user_authid(player, authid2, charsmax(authid2))
	get_user_name(player, name2, charsmax(name2))
	
	log_amx("Cmd: ^"%s<%d><%s><>^" slap with %d damage ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, damage, name2, get_user_userid(player), authid2)

	show_activity_key("ADMIN_SLAP_1", "ADMIN_SLAP_2", name, name2, damage);

	console_print(id, "[AMXX] %L", id, "CLIENT_SLAPED", name2, damage)
	
	return PLUGIN_HANDLED
}

public chMap(map[])
{
	engine_changelevel(map);
}

public cmdMap(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED

	new arg[32]
	new arglen = read_argv(1, arg, charsmax(arg))
	
	if (!is_map_valid(arg))
	{
		console_print(id, "[AMXX] %L", id, "MAP_NOT_FOUND")
		return PLUGIN_HANDLED
	}

	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	
	show_activity_key("ADMIN_MAP_1", "ADMIN_MAP_2", name, arg);
	
	log_amx("Cmd: ^"%s<%d><%s><>^" changelevel ^"%s^"", name, get_user_userid(id), authid, arg)
	
	new _modName[10]
	get_modname(_modName, charsmax(_modName))
	
	if (!equal(_modName, "zp"))
	{
		message_begin(MSG_ALL, SVC_INTERMISSION)
		message_end()
	}
	
	set_task(2.0, "chMap", 0, arg, arglen + 1)
	
	return PLUGIN_HANDLED
}

public cmdExtendMap(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED
	
	new arg[32]
	read_argv(1, arg, charsmax(arg))
	new mns = str_to_num(arg)
	
	if (mns <= 0)
		return PLUGIN_HANDLED
	
	new mapname[32]
	get_mapname(mapname, charsmax(mapname))
	set_pcvar_num( g_pTimelimit , get_pcvar_num( g_pTimelimit ) + mns)
	
	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	
	show_activity_key("ADMIN_EXTEND_1", "ADMIN_EXTEND_2", name, mns)
	
	log_amx("ExtendMap: ^"%s<%d><%s><>^" extended map ^"%s^" for %d minutes.", name, get_user_userid(id), authid, mapname, mns)
	console_print(id, "%L", id, "MAP_EXTENDED", mapname, mns)
	
	return PLUGIN_HANDLED
}

stock bool:onlyRcon(const name[])
{
	new ptr=get_cvar_pointer(name);
	if (ptr && get_pcvar_flags(ptr) & FCVAR_PROTECTED)
	{
		return true;
	}
	return false;
}

public cmdCvar(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED
	
	new arg[32], arg2[64]
	
	read_argv(1, arg, charsmax(arg))
	read_argv(2, arg2, charsmax(arg2))
	
	new pointer;
	
	if (equal(arg, "add") && (get_user_flags(id) & ADMIN_RCON))
	{
		if ((pointer=get_cvar_pointer(arg2))!=0)
		{
			new flags=get_pcvar_flags(pointer);
			
			if (!(flags & FCVAR_PROTECTED))
			{
				set_pcvar_flags(pointer,flags | FCVAR_PROTECTED);
			}
		}
		return PLUGIN_HANDLED
	}
	
	trim(arg);
	
	if ((pointer=get_cvar_pointer(arg))==0)
	{
		console_print(id, "[AMXX] %L", id, "UNKNOWN_CVAR", arg)
		return PLUGIN_HANDLED
	}
	
	if (onlyRcon(arg) && !(get_user_flags(id) & ADMIN_RCON))
	{
		// Exception for the new onlyRcon rules:
		//   sv_password is allowed to be modified by ADMIN_PASSWORD
		if (!(equali(arg,"sv_password") && (get_user_flags(id) & ADMIN_PASSWORD)))
		{
			console_print(id, "[AMXX] %L", id, "CVAR_NO_ACC")
			return PLUGIN_HANDLED
		}
	}
	
	if (read_argc() < 3)
	{
		get_pcvar_string(pointer, arg2, charsmax(arg2))
		console_print(id, "[AMXX] %L", id, "CVAR_IS", arg, arg2)
		return PLUGIN_HANDLED
	}
	
	if (equali(arg, "servercfgfile") || equali(arg, "lservercfgfile"))
	{
		new pos = contain(arg2, ";")
		if (pos != -1)
		{
			arg2[pos] = '^0'
		}
	}

	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	
	log_amx("Cmd: ^"%s<%d><%s><>^" set cvar (name ^"%s^") (value ^"%s^")", name, get_user_userid(id), authid, arg, arg2)
	set_pcvar_string(pointer, arg2)
	
	
	// Display the message to all clients

	new cvar_val[64];
	new players[MAX_PLAYERS], pnum, plr
	get_players(players, pnum, "ch")
	for (new i; i<pnum; i++)
	{
		plr = players[i]
		if (get_pcvar_flags(pointer) & FCVAR_PROTECTED || equali(arg, "g_pRconPassword"))
		{
			formatex(cvar_val, charsmax(cvar_val), "*** %L ***", plr, "PROTECTED");
		}
		else
		{
			copy(cvar_val, charsmax(cvar_val), arg2);
		}
		show_activity_id(plr, id, name, "%L", plr, "SET_CVAR_TO", "", arg, cvar_val);
	}

	console_print(id, "[AMXX] %L", id, "CVAR_CHANGED", arg, arg2)
	
	return PLUGIN_HANDLED
}

public cmdXvar(id, iLevel, iCid)
{
	if ( !cmd_access(id, iLevel, iCid, 2) )
	{
		return PLUGIN_HANDLED;
	}

	new cmd[15], arg1[32], arg2[32];
	
	read_argv(0, cmd, charsmax(cmd));
	read_argv(1, arg1, charsmax(arg1));
	trim(arg1);
	if ( read_argc() > 2 )
	{
		read_argv(2, arg2, charsmax(arg2));
		trim(arg2);

		if ( equali(arg1, "add") )
		{
			if ( get_user_flags(id) & ADMIN_RCON && xvar_exists(arg2) )
			{
				if ( !g_tXvarsFlags )
				{
					g_tXvarsFlags = TrieCreate();
				}
				TrieSetCell(g_tXvarsFlags, arg2, 1);
			}
			return PLUGIN_HANDLED;
		}
	}

	new bFloat = equali(cmd, "amx_xvar_float");

	new xvar = get_xvar_id( arg1 );

	if ( xvar == -1 )
	{
		console_print(id, "[AMXX] %L", id, "UNKNOWN_XVAR", arg1)
		return PLUGIN_HANDLED
	}

	new any:value;

	if ( !arg2[0] ) // get value
	{
		value = get_xvar_num(xvar);
		if ( bFloat )
		{
			float_to_str(value, arg2, charsmax(arg2));
		}
		else
		{
			num_to_str(value, arg2, charsmax(arg2));
		}
		console_print(id, "[AMXX] %L", id, "XVAR_IS", arg1, arg2);
		return PLUGIN_HANDLED;
	}

	// set value
	if ( g_tXvarsFlags && TrieKeyExists(g_tXvarsFlags, arg1) && ~get_user_flags(id) & ADMIN_RCON )
	{
		console_print(id, "[AMXX] %L", id, "XVAR_NO_ACC");
		return PLUGIN_HANDLED;
	}

	new endPos;
	if ( bFloat )
	{
		value = strtof(arg2, endPos);
		if ( !endPos )
		{
			return PLUGIN_HANDLED;
		}
	}
	else
	{
		value = strtol(arg2, endPos);
		if ( !endPos )
		{
			return PLUGIN_HANDLED;
		}
	}

	set_xvar_num(xvar, value);

	// convert back value to string so admin can know value has been set correctly
	if ( bFloat )
	{
		float_to_str(value, arg2, charsmax(arg2));
	}
	else
	{
		num_to_str(value, arg2, charsmax(arg2));
	}

	new authid[32], name[MAX_NAME_LENGTH];
	
	get_user_authid(id, authid, charsmax(authid));
	get_user_name(id, name, charsmax(name));
	
	log_amx("Cmd: ^"%s<%d><%s><>^" set xvar (name ^"%s^") (value ^"%s^")", name, get_user_userid(id), authid, arg1, arg2);
	
	// Display the message to all clients
	new players[MAX_PLAYERS], pnum, plr;
	get_players(players, pnum, "ch");
	for (new i; i<pnum; i++)
	{
		plr = players[i];
		show_activity_id(plr, id, name, "%L", plr, "SET_XVAR_TO", "", arg1, arg2);
	}
	
	console_print(id, "[AMXX] %L", id, "XVAR_CHANGED", arg1, arg2);

	return PLUGIN_HANDLED;
}

public cmdPlugins(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
		return PLUGIN_HANDLED
		
	if (id==0) // If server executes redirect this to "amxx plugins" for more in depth output
	{
		server_cmd("amxx plugins");
		server_exec();
		return PLUGIN_HANDLED;
	}

	new name[MAX_NAME_LENGTH], version[32], author[32], filename[32], status[32]
	new lName[32], lVersion[32], lAuthor[32], lFile[32], lStatus[32]

	format(lName, charsmax(lName), "%L", id, "NAME")
	format(lVersion, charsmax(lVersion), "%L", id, "VERSION")
	format(lAuthor, charsmax(lAuthor), "%L", id, "AUTHOR")
	format(lFile, charsmax(lFile), "%L", id, "FILE")
	format(lStatus, charsmax(lStatus), "%L", id, "STATUS")

	new StartPLID=0;
	new EndPLID;

	new Temp[96]

	new num = get_pluginsnum()
	
	if (read_argc() > 1)
	{
		read_argv(1,Temp,charsmax(Temp));
		StartPLID=str_to_num(Temp)-1; // zero-based
	}

	EndPLID=min(StartPLID + 10, num);
	
	new running = 0
	
	console_print(id, "----- %L -----", id, "LOADED_PLUGINS")
	console_print(id, "%-18.17s %-11.10s %-17.16s %-16.15s %-9.8s", lName, lVersion, lAuthor, lFile, lStatus)

	new i=StartPLID;
	while (i <EndPLID)
	{
		get_plugin(i++, filename, charsmax(filename), name, charsmax(name), version, charsmax(version), author, charsmax(author), status, charsmax(status))
		console_print(id, "%-18.17s %-11.10s %-17.16s %-16.15s %-9.8s", name, version, author, filename, status)
		
		if (status[0]=='d' || status[0]=='r') // "debug" or "running"
			running++
	}
	console_print(id, "%L", id, "PLUGINS_RUN", EndPLID-StartPLID, running)
	console_print(id, "----- %L -----",id,"HELP_ENTRIES",StartPLID + 1,EndPLID,num);
	
	if (EndPLID < num)
	{
		formatex(Temp,charsmax(Temp),"----- %L -----",id,"HELP_USE_MORE", "amx_help", EndPLID + 1);
		replace_all(Temp,charsmax(Temp),"amx_help","amx_plugins");
		console_print(id,"%s",Temp);
	}
	else
	{
		formatex(Temp,charsmax(Temp),"----- %L -----",id,"HELP_USE_BEGIN", "amx_help");
		replace_all(Temp,charsmax(Temp),"amx_help","amx_plugins");
		console_print(id,"%s",Temp);
	}

	return PLUGIN_HANDLED
}

public cmdModules(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
		return PLUGIN_HANDLED

	new name[32], version[32], author[32], status, sStatus[16]
	new lName[32], lVersion[32], lAuthor[32], lStatus[32];

	format(lName, charsmax(lName), "%L", id, "NAME")
	format(lVersion, charsmax(lVersion), "%L", id, "VERSION")
	format(lAuthor, charsmax(lAuthor), "%L", id, "AUTHOR")
	format(lStatus, charsmax(lStatus), "%L", id, "STATUS")

	new num = get_modulesnum()
	
	console_print(id, "%L:", id, "LOADED_MODULES")
	console_print(id, "%-23.22s %-11.10s %-20.19s %-11.10s", lName, lVersion, lAuthor, lStatus)
	
	for (new i = 0; i < num; i++)
	{
		get_module(i, name, charsmax(name), author, charsmax(author), version, charsmax(version), status)
		
		switch (status)
		{
			case module_loaded: copy(sStatus, charsmax(sStatus), "running")
			default: 
			{
				copy(sStatus, charsmax(sStatus), "bad load");
				copy(name, charsmax(name), "unknown");
				copy(author, charsmax(author), "unknown");
				copy(version, charsmax(version), "unknown");
			}
		}
		
		console_print(id, "%-23.22s %-11.10s %-20.19s %-11.10s", name, version, author, sStatus)
	}
	console_print(id, "%L", id, "NUM_MODULES", num)

	return PLUGIN_HANDLED
}

public cmdCfg(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED
	
	new arg[128]
	read_argv(1, arg, charsmax(arg))
	
	if (!file_exists(arg))
	{
		console_print(id, "[AMXX] %L", id, "FILE_NOT_FOUND", arg)
		return PLUGIN_HANDLED
	}
	
	new authid[32], name[MAX_NAME_LENGTH]
	
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	
	log_amx("Cmd: ^"%s<%d><%s><>^" execute cfg (file ^"%s^")", name, get_user_userid(id), authid, arg)
	
	console_print(id, "[AMXX] Executing file ^"%s^"", arg)
	server_cmd("exec ^"%s^"", arg)

	show_activity_key("ADMIN_CONF_1", "ADMIN_CONF_2", name, arg);

	return PLUGIN_HANDLED
}

public cmdLBack()
{
	if (!g_bPauseAllowed)
		return PLUGIN_CONTINUE	

	new paused[25]
	
	format(paused, 24, "%L", g_iPauseCon, g_bPaused ? "UNPAUSED" : "PAUSED")
	set_pcvar_float(g_pPausable, g_fPausable)
	console_print(g_iPauseCon, "[AMXX] Server %s", paused)
	g_bPauseAllowed = false
	
	if (g_bPaused)
		g_bPaused = false
	else 
		g_bPaused = true
	
	return PLUGIN_HANDLED
}

public cmdPause(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
		return PLUGIN_HANDLED 
	
	new authid[32], name[MAX_NAME_LENGTH], slayer = id
	
	get_user_authid(id, authid, charsmax(authid)) 
	get_user_name(id, name, charsmax(name)) 
	if (g_pPausable!=0)
	{
		g_fPausable = get_pcvar_float(g_pPausable)
	}
	
	if (!slayer)
		slayer = find_player("h") 
	
	if (!slayer)
	{ 
		console_print(id, "[AMXX] %L", id, "UNABLE_PAUSE") 
		return PLUGIN_HANDLED
	}

	set_pcvar_float(g_pPausable, 1.0)
	g_bPauseAllowed = true
	client_cmd(slayer, "pause;pauseAck")
	
	log_amx("Cmd: ^"%s<%d><%s><>^" %s server", name, get_user_userid(id), authid, g_bPaused ? "unpause" : "pause")
	
	console_print(id, "[AMXX] %L", id, g_bPaused ? "UNPAUSING" : "PAUSING")

	// Display the message to all clients

	new players[MAX_PLAYERS], pnum
	get_players(players, pnum, "ch")
	for (new i; i<pnum; i++)
	{
		show_activity_id(players[i], id, name, "%L server", i, g_bPaused ? "UNPAUSE" : "PAUSE");
	}

	g_iPauseCon = id
	
	return PLUGIN_HANDLED
} 

public cmdShowRcon(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED
		
	new password[64]
	
	get_pcvar_string(g_pRconPassword, password, charsmax(password))
	
	if (!password[0])
	{
		cmdRcon(id, iLevel, iCid)
	} 
	else 
	{
		new args[128]
		
		read_args(args, charsmax(args))
		client_cmd(id, "g_pRconPassword %s", password)
		client_cmd(id, "rcon %s", args)
	}
	
	return PLUGIN_HANDLED
}

public cmdRcon(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED
	
	new arg[128], authid[32], name[MAX_NAME_LENGTH]
	
	read_args(arg, charsmax(arg))
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	
	log_amx("Cmd: ^"%s<%d><%s><>^" server console (cmdline ^"%s^")", name, get_user_userid(id), authid, arg)
	
	console_print(id, "[AMXX] %L", id, "COM_SENT_SERVER", arg)
	server_cmd("%s", arg)
	
	return PLUGIN_HANDLED
}

public cmdWho(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
		return PLUGIN_HANDLED

	new players[MAX_PLAYERS], inum, cl_on_server[64], authid[32], name[MAX_NAME_LENGTH], flags, sflags[32], plr
	new lImm[16], lRes[16], lAccess[16], lYes[16], lNo[16]
	
	formatex(lImm, charsmax(lImm), "%L", id, "IMMU")
	formatex(lRes, charsmax(lRes), "%L", id, "RESERV")
	formatex(lAccess, charsmax(lAccess), "%L", id, "ACCESS")
	formatex(lYes, charsmax(lYes), "%L", id, "YES")
	formatex(lNo, charsmax(lNo), "%L", id, "NO")
	
	get_players(players, inum)
	format(cl_on_server, charsmax(cl_on_server), "%L", id, "CLIENTS_ON_SERVER")
	console_print(id, "^n%s:^n #  %-16.15s %-20s %-8s %-4.3s %-4.3s %s", cl_on_server, "nick", "authid", "userid", lImm, lRes, lAccess)
	
	for (new a = 0; a < inum; ++a)
	{
		plr = players[a]
		get_user_authid(plr, authid, charsmax(authid))
		get_user_name(plr, name, charsmax(name))
		flags = get_user_flags(plr)
		get_flags(flags, sflags, charsmax(sflags))
		console_print(id, "%2d  %-16.15s %-20s %-8d %-6.5s %-6.5s %s", plr, name, authid, 
		get_user_userid(plr), (flags&ADMIN_IMMUNITY) ? lYes : lNo, (flags&ADMIN_RESERVATION) ? lYes : lNo, sflags)
	}
	
	console_print(id, "%L", id, "TOTAL_NUM", inum)
	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	log_amx("Cmd: ^"%s<%d><%s><>^" ask for players list", name, get_user_userid(id), authid) 
	
	return PLUGIN_HANDLED
}

hasTag(name[], tags[4][32], tagsNum)
{
	for (new a = 0; a < tagsNum; ++a)
		if (contain(name, tags[a]) != -1)
			return a
	return -1
}

public cmdLeave(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 2))
		return PLUGIN_HANDLED
	
	new argnum = read_argc()
	new ltags[4][32]
	new ltagsnum = 0
	
	for (new a = 1; a < 5; ++a)
	{
		if (a < argnum)
			read_argv(a, ltags[ltagsnum++], charsmax(ltags[]))
		else
			ltags[ltagsnum++][0] = 0
	}
	
	new nick[MAX_NAME_LENGTH], ires, pnum = MaxClients, count = 0, lReason[128]
	
	for (new b = 1; b <= pnum; ++b)
	{
		if (!is_user_connected(b) && !is_user_connecting(b)) continue

		get_user_name(b, nick, charsmax(nick))
		ires = hasTag(nick, ltags, ltagsnum)
		
		if (ires != -1)
		{
			console_print(id, "[AMXX] %L", id, "SKIP_MATCH", nick, ltags[ires])
			continue
		}
		
		if (get_user_flags(b) & ADMIN_IMMUNITY)
		{
			console_print(id, "[AMXX] %L", id, "SKIP_IMM", nick)
			continue
		}
		
		console_print(id, "[AMXX] %L", id, "KICK_PL", nick)
		
		if (is_user_bot(b))
			server_cmd("kick #%d", get_user_userid(b))
		else
		{
			formatex(lReason, charsmax(lReason), "%L", b, "YOU_DROPPED")
			server_cmd("kick #%d ^"%s^"", get_user_userid(b), lReason)
		}
		count++
	}
	
	console_print(id, "[AMXX] %L", id, "KICKED_CLIENTS", count)
	
	new authid[32], name[MAX_NAME_LENGTH]

	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	log_amx("Kick: ^"%s<%d><%s><>^" leave some group (tag1 ^"%s^") (tag2 ^"%s^") (tag3 ^"%s^") (tag4 ^"%s^")", name, get_user_userid(id), authid, ltags[0], ltags[1], ltags[2], ltags[3])

	show_activity_key("ADMIN_LEAVE_1", "ADMIN_LEAVE_2", name, ltags[0], ltags[1], ltags[2], ltags[3]);

	return PLUGIN_HANDLED
}

public cmdNick(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 3))
		return PLUGIN_HANDLED

	new arg1[32], arg2[32], authid[32], name[32], authid2[32], name2[32]

	read_argv(1, arg1, charsmax(arg1))
	read_argv(2, arg2, charsmax(arg2))

	new player = cmd_target(id, arg1, CMDTARGET_OBEY_IMMUNITY | CMDTARGET_ALLOW_SELF)
	
	if (!player)
		return PLUGIN_HANDLED

	get_user_authid(id, authid, charsmax(authid))
	get_user_name(id, name, charsmax(name))
	get_user_authid(player, authid2, charsmax(authid2))
	get_user_name(player, name2, charsmax(name2))

	set_user_info(player, "name", arg2)

	log_amx("Cmd: ^"%s<%d><%s><>^" change nick to ^"%s^" ^"%s<%d><%s><>^"", name, get_user_userid(id), authid, arg2, name2, get_user_userid(player), authid2)

	show_activity_key("ADMIN_NICK_1", "ADMIN_NICK_2", name, name2, arg2);

	console_print(id, "[AMXX] %L", id, "CHANGED_NICK", name2, arg2)

	return PLUGIN_HANDLED
}

public cmdLast(id, iLevel, iCid)
{
	if (!cmd_access(id, iLevel, iCid, 1))
	{
		return PLUGIN_HANDLED;
	}
	
	new name[MAX_NAME_LENGTH];
	new authid[32];
	new ip[32];
	new flags[32];
	new access;
	
	
	// This alignment is a bit weird (it should grow if the name is larger)
	// but otherwise for the more common shorter name, it'll wrap in server console
	// Steam client display is all skewed anyway because of the non fixed font.
	console_print(id, "%19s %20s %15s %s", "name", "authid", "ip", "access");
	
	for (new i = 0; i < g_iSize; i++)
	{
		GetInfo(i, name, charsmax(name), authid, charsmax(authid), ip, charsmax(ip), access);
		
		get_flags(access, flags, charsmax(flags));
		
		console_print(id, "%19s %20s %15s %s", name, authid, ip, flags);
	}
	
	console_print(id, "%d old connections saved.", g_iSize);
	
	return PLUGIN_HANDLED;
}

public plugin_end()
{
	TrieDestroy(g_tTempBans);
	TrieDestroy(g_tXvarsFlags);
}
