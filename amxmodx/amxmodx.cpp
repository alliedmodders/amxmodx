// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include <time.h>
#include <amtl/am-utility.h>
#include "amxmodx.h"
#include "CMenu.h"
#include "newmenus.h"
#include "natives.h"
#include "debugger.h"
#include "binlog.h"
#include "libraries.h"
#include "CFlagManager.h"
#include "nongpl_matches.h"
#include "format.h"

extern CFlagManager FlagMan;
ke::Vector<CAdminData *> DynamicAdmins;

const char *g_sInaccessibleXVars[] =
{
	"MaxClients",
	"MapName",
	"PluginName",
	"PluginVersion",
	"PluginAuthor",
	"PluginURL",
	"NULL_STRING",
	"NULL_VECTOR"
};

static cell AMX_NATIVE_CALL get_xvar_id(AMX *amx, cell *params)
{
	int len;
	char* sName = get_amxstring(amx, params[1], 0, len);
	cell ptr;

	for (auto name : g_sInaccessibleXVars)
	{
		if (!strcmp(sName, name))
		{
			return -1;
		}
	}

	for (CPluginMngr::iterator a = g_plugins.begin(); a ; ++a)
	{
		if ((*a).isValid() && amx_FindPubVar((*a).getAMX(), sName, &ptr) == AMX_ERR_NONE)
			return g_xvars.put((*a).getAMX(), get_amxaddr((*a).getAMX(), ptr));
	}

	return -1;
}

static cell AMX_NATIVE_CALL get_xvar_num(AMX *amx, cell *params)
{
	return g_xvars.getValue(params[1]);
}

static cell AMX_NATIVE_CALL set_xvar_num(AMX *amx, cell *params)
{
	if (g_xvars.setValue(params[1], params[2]))
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid xvar id");
		return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL xvar_exists(AMX *amx, cell *params)
{
	return (get_xvar_id(amx, params) != -1) ? 1 : 0;
}

static cell AMX_NATIVE_CALL emit_sound(AMX *amx, cell *params) /* 7 param */
{
	int len;
	char* szSample = get_amxstring(amx, params[3], 0, len);
	REAL vol = amx_ctof(params[4]);
	REAL att = amx_ctof(params[5]);
	int channel = params[2];
	int pitch = params[7];
	int flags = params[6];

	if (params[1] == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients ; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame)
				EMIT_SOUND_DYN2(pPlayer->pEdict, channel, szSample, vol, att, flags, pitch);
		}
	} else {
		edict_t* pEdict = TypeConversion.id_to_edict(params[1]);

		if (!FNullEnt(pEdict))
			EMIT_SOUND_DYN2(pEdict, channel, szSample, vol, att, flags, pitch);
	}

	return 1;
}

static cell AMX_NATIVE_CALL server_print(AMX *amx, cell *params) /* 1 param */
{
	int len;
	g_langMngr.SetDefLang(LANG_SERVER);			// Default language = server
	char* message = format_amxstring(amx, params, 1, len);

	if (len > 254)
		len = 254;

	message[len++] = '\n';
	message[len] = 0;
	SERVER_PRINT(message);

	return len;
}

static cell AMX_NATIVE_CALL engclient_print(AMX *amx, cell *params) /* 3 param */
{
	int len = 0;
	char *msg;
	PRINT_TYPE type = (PRINT_TYPE)params[2];

	if (params[1] == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

			if ((type == print_console  && pPlayer->initialized) || pPlayer->ingame)
			{
				g_langMngr.SetDefLang(i);
				msg = format_amxstring(amx, params, 3, len);
				msg[len++] = '\n';
				msg[len] = 0;
				CLIENT_PRINT(pPlayer->pEdict, type, msg);
			}
		}
	} else {
		int index = params[1];

		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if ((type == print_console  && pPlayer->initialized) || pPlayer->ingame)
		{
			g_langMngr.SetDefLang(index);
			msg = format_amxstring(amx, params, 3, len);
			msg[len++] = '\n';
			msg[len] = 0;
			CLIENT_PRINT(pPlayer->pEdict, type, msg);
		}
	}

	return len;
}

static cell AMX_NATIVE_CALL console_cmd(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];
	g_langMngr.SetDefLang(index);
	int len;
	char* cmd = format_amxstring(amx, params, 2, len);

	cmd[len++] = '\n';
	cmd[len] = 0;

	if (index < 1 || index > gpGlobals->maxClients)
	{
		SERVER_COMMAND(cmd);
	} else {
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (!pPlayer->IsBot() && pPlayer->initialized)
			CLIENT_COMMAND(pPlayer->pEdict, "%s", cmd);
	}

	return len;
}

// The server console is limited to 255 bytes, including the newline.
// The client console is limited to 127 bytes, including the newline.
static cell AMX_NATIVE_CALL console_print(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		g_langMngr.SetDefLang(LANG_SERVER);
	}
	else
	{
		g_langMngr.SetDefLang(index);
	}

	int len;
	char* message = format_amxstring(amx, params, 2, len);

	if (index < 1 || index > gpGlobals->maxClients)	// Server console
	{
		if (len > 254) // Server console truncates after byte 255. (254 + \n = 255)
		{
			len = 254;
			if ((message[len - 1] & 1 << 7))
			{
				len -= UTIL_CheckValidChar(message + len - 1); // Don't truncate a multi-byte character
			}
		}
		message[len++] = '\n';
		message[len] = 0;

		SERVER_PRINT(message);
	}
	else	// A specific player's console
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame && !pPlayer->IsBot())
		{
			if (len > 125)	// Client console truncates after byte 127. (125 + \n\n = 127)
			{
				len = 125;
				if ((message[len - 1] & 1 << 7))
				{
					len -= UTIL_CheckValidChar(message + len - 1); // Don't truncate a multi-byte character
				}
			}
			message[len++] = '\n';

			const auto canUseFormatString = g_official_mod && !g_bmod_dod; // Temporary exclusion for DoD until officially supported

			if (canUseFormatString)
			{
				message[len++] = '\n';    //  Double newline is required when pre-formatted string in TextMsg is passed as argument.
			}
			
			message[len] = 0;

			UTIL_ClientPrint(pPlayer->pEdict, HUD_PRINTCONSOLE, message);
		}
	}

	return len;
}

// print_notify and print_console are limited to 127 bytes, including the newline.
// print_chat and print_center are not limited by *this* function.
static cell AMX_NATIVE_CALL client_print(AMX *amx, cell *params) /* 3 param */
{
	int len = 0;
	char *msg;

	const auto canUseFormatString = g_official_mod && !g_bmod_dod; // Temporary exclusion for DoD until officially supported

	if (params[1] == 0)	// 0 = All players
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame && !pPlayer->IsBot())
			{
				g_langMngr.SetDefLang(i);
				msg = format_amxstring(amx, params, 3, len);

				// Client console truncates after byte 127.
				// If format string is used, limit includes double new lines (125 + \n\n), otherwise one new line (126 + \n).
				const auto bytesLimit = canUseFormatString ? 125 : 126;

				if (g_bmod_cstrike && params[2] == HUD_PRINTCENTER) // Likely a temporary fix.
				{
					for (int j = 0; j < len; ++j)
					{
						if (msg[j] == '\n')
						{
							msg[j] = '\r';
						}
					}
				}
				else if (((params[2] == HUD_PRINTNOTIFY) || (params[2] == HUD_PRINTCONSOLE)) && (len > bytesLimit))	
				{
					len = bytesLimit;
					if ((msg[len - 1] & 1 << 7))
					{
						len -= UTIL_CheckValidChar(msg + len - 1); // Don't truncate a multi-byte character
					}
				}
				msg[len++] = '\n';
				
				if (canUseFormatString)
				{
					if (!g_bmod_cstrike || params[2] == HUD_PRINTNOTIFY || params[2] == HUD_PRINTCONSOLE)
					{
						msg[len++] = '\n';  // Double newline is required when pre-formatted string in TextMsg is passed as argument.
					}
				}
				
				msg[len] = 0;

				UTIL_ClientPrint(pPlayer->pEdict, params[2], msg);
			}
		}
	}
	else	// A specific player
	{
		int index = params[1];

		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame && !pPlayer->IsBot())
		{
			g_langMngr.SetDefLang(index);

			msg = format_amxstring(amx, params, 3, len);

			// Client console truncates after byte 127.
			// If format string is used, limit includes double new lines (125 + \n\n), otherwise one new line (126 + \n).
			const auto bytesLimit = canUseFormatString ? 125 : 126;
			
			if (g_bmod_cstrike && params[2] == HUD_PRINTCENTER) // Likely a temporary fix.
			{
				for (int j = 0; j < len; ++j)
				{
					if (msg[j] == '\n')
					{
						msg[j] = '\r';
					}
				}
			}
			else if (((params[2] == HUD_PRINTNOTIFY) || (params[2] == HUD_PRINTCONSOLE)) && (len > bytesLimit))	// Client console truncates after byte 127. (125 + \n\n = 127)
			{
				len = bytesLimit;
				if ((msg[len - 1] & 1 << 7))
				{
					len -= UTIL_CheckValidChar(msg + len - 1); // Don't truncate a multi-byte character
				}
			}
			msg[len++] = '\n';

			if (canUseFormatString)
			{
				if (!g_bmod_cstrike || params[2] == HUD_PRINTNOTIFY || params[2] == HUD_PRINTCONSOLE)
				{
					msg[len++] = '\n';  // Double newline is required when pre-formatted string in TextMsg is passed as argument.
				}
			}
			
			msg[len] = 0;

			UTIL_ClientPrint(pPlayer->pEdict, params[2], msg);
		}
	}

	return len;
}

static cell AMX_NATIVE_CALL client_print_color(AMX *amx, cell *params) /* 3 param */
{
	if (!g_bmod_cstrike)
	{
		params[2] = print_chat;
		return client_print(amx, params);
	}

	int len = 0;
	char *msg;
	int index = params[1];
	int sender = params[2];

	if (sender < print_team_blue || sender > gpGlobals->maxClients)
	{
		sender = print_team_default;
	}
	else if (sender < print_team_default)
	{
		sender = abs(sender) + 32; // align indexes to the TeamInfo ones.
	}

	if (!index)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame && !pPlayer->IsBot())
			{
				g_langMngr.SetDefLang(i);
				msg = format_amxstring(amx, params, 3, len);

				if (static_cast<byte>(*msg) > 4) // Insert default color code at the start if not present, otherwise message will not be colored.
				{
					memmove(msg + 1, msg, ke::Min(len++, 191));
					*msg = 1;
				}

				if (len > 187)	// Max available bytes: 188
				{
					len = 187;
					if ((msg[len - 1] & 1 << 7))
					{
						len -= UTIL_CheckValidChar(msg + len - 1); // Don't truncate a multi-byte character
					}
				}
				
				msg[len] = 0;

				UTIL_ClientSayText(pPlayer->pEdict, sender ? sender : i, msg);
			}
		}
	}
	else
	{
		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame && !pPlayer->IsBot())
		{
			g_langMngr.SetDefLang(index);

			msg = format_amxstring(amx, params, 3, len);

			if (static_cast<byte>(*msg) > 4) // Insert default color code at the start if not present, otherwise message will not be colored.
			{
				memmove(msg + 1, msg, ke::Min(len++, 191));
				*msg = 1;
			}

			if (len > 187)	// Max available bytes: 188
			{
				len = 187;
				if ((msg[len - 1] & 1 << 7))
				{
					len -= UTIL_CheckValidChar(msg + len - 1); // Don't truncate a multi-byte character
				}
			}

			msg[len] = 0;

			UTIL_ClientSayText(pPlayer->pEdict, sender ? sender : index, msg);
		}
	}

	return len;
}

static cell AMX_NATIVE_CALL show_motd(AMX *amx, cell *params) /* 3 param */
{
	int ilen;
	const char* szHead = get_amxstring(amx, params[3], 0, ilen);

	if (!ilen)
		szHead = hostname->string;

	char* szBody = get_amxstring(amx, params[2], 1, ilen);
	int iFile = 0;
	char* sToShow = NULL; // = szBody;

	if (ilen < 128)
		sToShow = (char*)LOAD_FILE_FOR_ME(szBody, &iFile);

	if (!iFile)
		sToShow = szBody;
	else
		ilen = iFile;

	if (params[1] == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame && !pPlayer->IsBot())
				UTIL_ShowMOTD(pPlayer->pEdict, sToShow, ilen, szHead);
		}
	} else {
		int index = params[1];

		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			if (iFile)
				FREE_FILE(sToShow);

			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame && !pPlayer->IsBot())
			UTIL_ShowMOTD(pPlayer->pEdict, sToShow, ilen, szHead);
	}

	if (iFile)
		FREE_FILE(sToShow);

	return 1;
}

static cell AMX_NATIVE_CALL next_hudchannel(AMX *amx, cell *params)
{
	int index = params[1];
	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", index);
		return 0;
	}

	CPlayer *pPlayer = GET_PLAYER_POINTER_I(index);
	if (!pPlayer->ingame)
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d not in game", index);
		return 0;
	}

	return pPlayer->NextHUDChannel();
}

static cell AMX_NATIVE_CALL set_hudmessage(AMX *amx, cell *params) /* 11 param */
{
	cell num_params = params[0] / sizeof(cell);

	if (num_params >= 13)
	{
		cell *color2 = get_amxaddr(amx, params[13]);

		g_hudset.a1 = static_cast<byte>(params[12]);
		g_hudset.a2 = static_cast<byte>(color2[3]);
		g_hudset.r2 = static_cast<byte>(color2[0]);
		g_hudset.g2 = static_cast<byte>(color2[1]);
		g_hudset.b2 = static_cast<byte>(color2[2]);
	}
	else
	{
		g_hudset.a1 = 0;
		g_hudset.a2 = 0;
		g_hudset.r2 = 255;
		g_hudset.g2 = 255;
		g_hudset.b2 = 250;
	}

	g_hudset.r1 = static_cast<byte>(params[1]);
	g_hudset.g1 = static_cast<byte>(params[2]);
	g_hudset.b1 = static_cast<byte>(params[3]);
	g_hudset.x = amx_ctof(params[4]);
	g_hudset.y = amx_ctof(params[5]);
	g_hudset.effect = params[6];
	g_hudset.fxTime = amx_ctof(params[7]);
	g_hudset.holdTime = amx_ctof(params[8]);
	g_hudset.fadeinTime = amx_ctof(params[9]);
	g_hudset.fadeoutTime = amx_ctof(params[10]);
	g_hudset.channel = params[11];

	return 1;
}

static cell AMX_NATIVE_CALL show_hudmessage(AMX *amx, cell *params) /* 2 param */
{
	int len = 0;
	g_langMngr.SetDefLang(params[1]);
	char* message = NULL;

	/**
	 * Earlier versions would ignore invalid bounds.
	 * Now, bounds are only checked for internal operations.
	 *  "channel" stores the valid channel that core uses.
	 *  "g_hudset.channel" stores the direct channel passed to the engine.
	 */

	bool aut = (g_hudset.channel == -1) ? true : false;
	int channel = -1;
	if (!aut)
	{
		/**
		 * guarantee this to be between 0-4
		 * if it's not auto, we don't care
		 */
		channel = abs(g_hudset.channel % 5);
	}
	if (params[1] == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame && !pPlayer->IsBot())
			{
				g_langMngr.SetDefLang(i);
				message = UTIL_SplitHudMessage(format_amxstring(amx, params, 2, len));
				if (aut)
				{
					channel = pPlayer->NextHUDChannel();
					pPlayer->channels[channel] = gpGlobals->time;
					g_hudset.channel = channel;
				}
				//don't need to set g_hudset!
				pPlayer->hudmap[channel] = 0;
				UTIL_HudMessage(pPlayer->pEdict, g_hudset, message);
			}
		}
	} else {
		message = UTIL_SplitHudMessage(format_amxstring(amx, params, 2, len));
		int index = params[1];

		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame && !pPlayer->IsBot())
		{
			if (aut)
			{
				channel = pPlayer->NextHUDChannel();
				pPlayer->channels[channel] = gpGlobals->time;
				g_hudset.channel = channel;
			}
			pPlayer->hudmap[channel] = 0;
			UTIL_HudMessage(pPlayer->pEdict, g_hudset, message);
		}
	}

	return len;
}

static cell AMX_NATIVE_CALL set_dhudmessage(AMX *amx, cell *params) /* 10 param */
{
	g_hudset.a1 = 0;
	g_hudset.a2 = 0;
	g_hudset.r2 = 255;
	g_hudset.g2 = 255;
	g_hudset.b2 = 250;
	g_hudset.r1 = static_cast<byte>(params[1]);
	g_hudset.g1 = static_cast<byte>(params[2]);
	g_hudset.b1 = static_cast<byte>(params[3]);
	g_hudset.x = amx_ctof(params[4]);
	g_hudset.y = amx_ctof(params[5]);
	g_hudset.effect = params[6];
	g_hudset.fxTime = amx_ctof(params[7]);
	g_hudset.holdTime = amx_ctof(params[8]);
	g_hudset.fadeinTime = amx_ctof(params[9]);
	g_hudset.fadeoutTime = amx_ctof(params[10]);
	g_hudset.channel = -1;

	return 1;
}

static cell AMX_NATIVE_CALL show_dhudmessage(AMX *amx, cell *params) /* 2 param */
{
	int len = 0;
	int index = params[1];
	char *message;

	if (!index)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame && !pPlayer->IsBot())
			{
				g_langMngr.SetDefLang(i);
				message = format_amxstring(amx, params, 2, len);

				if (len > 127)	// Client truncates after byte 127.
				{
					len = 127;

					// Don't truncate a double-byte character
					if (((message[len - 1] & 0xFF) >= 0xC2) && ((message[len - 1] & 0xFF) <= 0xEF))
					{
						len--;
					}

					message[len] = 0;
				}

				UTIL_DHudMessage(pPlayer->pEdict, g_hudset, message, len);
			}
		}
	}
	else
	{
		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame && !pPlayer->IsBot())
		{
			g_langMngr.SetDefLang(index);
			message = format_amxstring(amx, params, 2, len);

			if (len > 127)	// Client truncates after byte 127.
			{
				len = 127;

				// Don't truncate a double-byte character
				if (((message[len - 1] & 0xFF) >= 0xC2) && ((message[len - 1] & 0xFF) <= 0xEF))
				{
					--len;
				}

				message[len] = 0;
			}

			UTIL_DHudMessage(pPlayer->pEdict, g_hudset, message, len);
		}
	}

	return len;
}

static cell AMX_NATIVE_CALL get_user_name(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];
	int maxlen = params[3];

	if (index < 1 || index > gpGlobals->maxClients)
		return set_amxstring_utf8(amx, params[2], hostname->string, strlen(hostname->string), maxlen);
	else
		return set_amxstring_utf8(amx, params[2], g_players[index].name.chars(), g_players[index].name.length(), maxlen);
}

static cell AMX_NATIVE_CALL get_user_index(AMX *amx, cell *params) /* 1 param */
{
	int i;
	char* sptemp = get_amxstring(amx, params[1], 0, i);

	for (i = 1; i <= gpGlobals->maxClients; ++i)
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

		if (strcmp(pPlayer->name.chars(), sptemp) == 0)
			return i;
	}

	return 0;
}

static cell AMX_NATIVE_CALL is_dedicated_server(AMX *amx, cell *params)
{
	return (IS_DEDICATED_SERVER() ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_linux_server(AMX *amx, cell *params)
{
#if defined(__linux__) || defined(__APPLE__)
	return 1;
#else
	return 0;
#endif
}

static cell AMX_NATIVE_CALL is_amd64_server(AMX *amx, cell *params)
{
#if PAWN_CELL_SIZE==64
	return 1;
#else
	return 0;
#endif
}

static cell AMX_NATIVE_CALL is_jit_enabled(AMX *amx, cell *params)		// PM: Useless ;P
{
#ifdef JIT
	return 1;
#else
	return 0;
#endif
}

static cell AMX_NATIVE_CALL is_map_valid(AMX *amx, cell *params) /* 1 param */
{
	int ilen;
	return (IS_MAP_VALID(get_amxstring(amx, params[1], 0, ilen)) ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_user_connected(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return (pPlayer->ingame ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_user_connecting(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return (!pPlayer->ingame && pPlayer->initialized && (GETPLAYERUSERID(pPlayer->pEdict) > 0)) ? 1 : 0;
}

static cell AMX_NATIVE_CALL is_user_bot(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	return (GET_PLAYER_POINTER_I(index)->IsBot() ? 1 : 0);
}

static cell AMX_NATIVE_CALL is_user_hltv(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer *pPlayer = GET_PLAYER_POINTER_I(index);

	if (!pPlayer->initialized)
		return 0;

	if (pPlayer->pEdict->v.flags & FL_PROXY)
		return 1;

	const char *authid = GETPLAYERAUTHID(pPlayer->pEdict);

	if (authid && stricmp(authid, "HLTV") == 0)
		return 1;

	return 0;
}

extern bool g_bmod_tfc;
static cell AMX_NATIVE_CALL is_user_alive(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		return FALSE;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (!pPlayer->ingame)
	{
		return FALSE;
	}

	if (g_bmod_tfc)
	{
		edict_t *e = pPlayer->pEdict;
		if (e->v.flags & FL_SPECTATOR ||
			(!e->v.team || !e->v.playerclass))
		{
			return FALSE;
		}
	}

	return pPlayer->IsAlive() ? TRUE : FALSE;
}

static cell AMX_NATIVE_CALL get_amxx_verstring(AMX *amx, cell *params) /* 2 params */
{
	return set_amxstring(amx, params[1], AMXX_VERSION, params[2]);
}

static cell AMX_NATIVE_CALL get_user_frags(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return (cell)(pPlayer->ingame ? pPlayer->pEdict->v.frags : 0);
}

static cell AMX_NATIVE_CALL get_user_deaths(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return (cell)(pPlayer->ingame ? pPlayer->deaths : 0);
}

static cell AMX_NATIVE_CALL get_user_armor(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return (cell)(pPlayer->ingame ? pPlayer->pEdict->v.armorvalue : 0);
}

static cell AMX_NATIVE_CALL get_user_health(AMX *amx, cell *params) /* param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return (cell)(pPlayer->ingame ? pPlayer->pEdict->v.health : 0);
}

static cell AMX_NATIVE_CALL get_user_userid(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	return pPlayer->initialized ? GETPLAYERUSERID(pPlayer->pEdict) : -1;
}

static cell AMX_NATIVE_CALL get_user_authid(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];
	const char* authid = 0;

	if (index > 0 && index <= gpGlobals->maxClients)
		authid = GETPLAYERAUTHID(g_players[index].pEdict);

	return set_amxstring(amx, params[2], authid ? authid : "", params[3]);
}

static cell AMX_NATIVE_CALL is_user_authorized(AMX *amx, cell *params)
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	return GET_PLAYER_POINTER_I(index)->authorized;
}

static cell AMX_NATIVE_CALL get_weaponname(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index >= MAX_WEAPONS)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", index);
		return 0;
	}

	return set_amxstring(amx, params[2], g_weaponsData[index].fullName.chars(), params[3]);
}

static cell AMX_NATIVE_CALL get_weaponid(AMX *amx, cell *params)
{
	int ilen;
	const char *name = get_amxstring(amx, params[1], 0, ilen);

	for (int i = 1; i < MAX_WEAPONS; i++)
	{
		if (!strcmp(g_weaponsData[i].fullName.chars(), name))
			return g_weaponsData[i].iId;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_weapons(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		cell *cpNum = get_amxaddr(amx, params[3]);
		cell *cpIds = get_amxaddr(amx, params[2]);
		*cpIds = 0;

		int weapons = pPlayer->pEdict->v.weapons & ~(1<<31); // don't count last element

		if (g_bmod_dod)
		{
			// Don't ignore that last element for dod
			weapons = pPlayer->pEdict->v.weapons;
		}

		for (int i = 1; i < MAX_WEAPONS; ++i)
		{
			if (weapons & (1<<i))
			{
				*(cpIds+(*cpNum)) = i;
				(*cpNum)++;
			}
		}
		return weapons;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_origin(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		int mode = params[3];
		cell *cpOrigin = get_amxaddr(amx, params[2]);

		if (mode == 4)
		{
			cpOrigin[0] = (long int)pPlayer->lastHit.x;
			cpOrigin[1] = (long int)pPlayer->lastHit.y;
			cpOrigin[2] = (long int)pPlayer->lastHit.z;
			return 1;
		}

		edict_t* edict = pPlayer->pEdict;
		Vector pos = edict->v.origin;

		if (mode && mode != 2)
			pos = pos + edict->v.view_ofs;

		if (mode > 1)
		{
			Vector vec;
			Vector v_angle = edict->v.v_angle;
			float v_vec[3];

			v_vec[0] = v_angle.x;
			v_vec[1] = v_angle.y;
			v_vec[2] = v_angle.z;

			ANGLEVECTORS(v_vec, vec, NULL, NULL);
			TraceResult trEnd;
			Vector v_dest = pos + vec * 9999;

			float f_pos[3];
			f_pos[0] = pos.x;
			f_pos[1] = pos.y;
			f_pos[2] = pos.z;

			float f_dest[3];
			f_dest[0] = v_dest.x;
			f_dest[1] = v_dest.y;
			f_dest[2] = v_dest.z;

			TRACE_LINE(f_pos, f_dest, 0, edict, &trEnd);
			pos = (trEnd.flFraction < 1.0) ? trEnd.vecEndPos : Vector(0, 0, 0);
		}
		cpOrigin[0] = (long int)pos.x;
		cpOrigin[1] = (long int)pos.y;
		cpOrigin[2] = (long int)pos.z;

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_ip(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];
	char *ptr;
	char szIp[32];
	strcpy(szIp, (index < 1 || index > gpGlobals->maxClients) ? CVAR_GET_STRING("net_address") : g_players[index].ip.chars());

	if (params[4] && (ptr = strstr(szIp, ":")) != 0)
		*ptr = '\0';

	return set_amxstring(amx, params[2], szIp, params[3]);
}

static cell AMX_NATIVE_CALL get_user_attacker(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	edict_t *enemy = NULL;

	if (pPlayer->ingame)
	{
		enemy = pPlayer->pEdict->v.dmg_inflictor;
		if (!FNullEnt(enemy))
		{
			int weapon = 0;

			if (enemy->v.flags & (FL_CLIENT | FL_FAKECLIENT))
			{
				pPlayer = GET_PLAYER_POINTER(enemy);
				weapon = pPlayer->current;
			} else if (g_grenades.find(enemy, &pPlayer, weapon)) {
				enemy = pPlayer->pEdict;
			} else {
				enemy = enemy->v.owner;
				if (!FNullEnt(enemy) && (enemy->v.flags & (FL_CLIENT | FL_FAKECLIENT)))
				{
					pPlayer = GET_PLAYER_POINTER(enemy);
					weapon = pPlayer->current;
				} else {
					switch (*params / sizeof(cell))
					{
						case 3: *get_amxaddr(amx, params[3]) = 0;
						case 2: *get_amxaddr(amx, params[2]) = 0;
					}
					return ENTINDEX(pPlayer->pEdict->v.dmg_inflictor);
				}
			}

			if (enemy)
			{
				switch (*params / sizeof(cell))
				{
					case 3: *get_amxaddr(amx, params[3]) = pPlayer->aiming;
					case 2: *get_amxaddr(amx, params[2]) = weapon;
				}
			}
		}
	}

	return (enemy ? pPlayer->index : 0);
}

static cell AMX_NATIVE_CALL user_has_weapon(AMX *amx, cell *params)
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (!pPlayer->ingame)
	{
		return 0;
	}

	edict_t *pEntity = pPlayer->pEdict;

	if (params[3] == -1)
	{
		if ((pEntity->v.weapons & (1<<params[2])) > 0)
		{
			return 1;
		}
	} else {
		if ((pEntity->v.weapons & (1<<params[2])) > 0)
		{
			if (params[3] == 0)
			{
				pEntity->v.weapons &= ~(1<<params[2]);
				return 1;
			}

			return 0;
		} else {
			if (params[3] == 1)
			{
				pEntity->v.weapons |= (1<<params[2]);
				return 1;
			}
		}

		return 0;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_weapon(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		int wpn = pPlayer->current;

		cell *cpTemp = get_amxaddr(amx, params[2]);
		*cpTemp = pPlayer->weapons[wpn].clip;
		cpTemp = get_amxaddr(amx, params[3]);
		*cpTemp = pPlayer->weapons[wpn].ammo;

		return wpn;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_ammo(AMX *amx, cell *params) /* 4 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		int wpn = params[2];

		if (wpn < 1 || wpn >= MAX_WEAPONS)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", wpn);
			return 0;
		}

		cell *cpTemp = get_amxaddr(amx, params[3]);
		*cpTemp = pPlayer->weapons[wpn].clip;
		cpTemp = get_amxaddr(amx, params[4]);
		*cpTemp = pPlayer->weapons[wpn].ammo;

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_team(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return -1;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		// SidLuke, DoD fix
		if (g_bmod_dod)
		{
			int iTeam = pPlayer->pEdict->v.team;

			if (params[3])
			{
				const char *szTeam = "";

				switch (iTeam)
				{
					case 1:
						szTeam = "Allies";
						break;
					case 2:
						szTeam = "Axis";
						break;
				}

				set_amxstring(amx, params[2], szTeam, params[3]);
			}
			return iTeam;
		}
		//
		if (params[3])
		{
			set_amxstring(amx, params[2], pPlayer->team.chars(), params[3]);
		}

		return pPlayer->teamId;
	}

	return -1;
}

static cell AMX_NATIVE_CALL show_menu(AMX *amx, cell *params) /* 3 param */
{
	auto closeMenu = [amx](int index) -> int
	{
		auto pPlayer = GET_PLAYER_POINTER_I(index);

		if (!pPlayer->ingame)
		{
			return 1;
		}

		pPlayer->keys = 0;
		pPlayer->menu = 0;

		// Fire newmenu callback so closing it can be handled by the plugin
		if (!CloseNewMenus(pPlayer))
		{
			return 2;
		}

		if (g_bmod_cstrike)
		{
			enum JoinState { Joined = 0 };
			enum MenuState { Menu_OFF = 0, Menu_ChooseTeam = 1, Menu_ChooseAppearance = 3 };

			GET_OFFSET("CBasePlayer", m_iJoiningState);
			GET_OFFSET("CBasePlayer", m_iMenu);

			if (get_pdata<int>(pPlayer->pEdict, m_iJoiningState) == Joined || (get_pdata<int>(pPlayer->pEdict, m_iMenu) != Menu_ChooseTeam && get_pdata<int>(pPlayer->pEdict, m_iMenu) != Menu_ChooseAppearance))
			{
				set_pdata<int>(pPlayer->pEdict, m_iMenu, Menu_OFF);
			}
		}

		return 0;
	};

	int index = params[1];

	// If show_menu is called from within a newmenu callback upon receiving MENU_EXIT
	// it is possible for this native to recurse. We need to close newmenus right away
	// because the recursive call would otherwise modify/corrupt the static get_amxstring
	// buffer mid execution. This will either display incorrect text or result in UTIL_ShowMenu
	// running into an infinite loop.
	if (index == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			if (closeMenu(i) == 2)
			{
				return 0;
			}
		}
	}
	else
	{
		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		if (closeMenu(index) == 2)
		{
			return 0;
		}
	}

	int ilen = 0, ilen2 = 0;
	char *sMenu = get_amxstring(amx, params[3], 0, ilen);
	char *lMenu = get_amxstring(amx, params[5], 1, ilen2);
	int menuid = 0;

	if (ilen2 && lMenu)
	{
		menuid = g_menucmds.findMenuId(lMenu, amx);
	} else {
		menuid = g_menucmds.findMenuId(sMenu, amx);
	}

	int keys = params[2];
	int time = params[4];

	if (index == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame)
			{
				pPlayer->keys = keys;
				pPlayer->menu = menuid;
				pPlayer->vgui = false;

				if (time == -1)
					pPlayer->menuexpire = static_cast<float>(INFINITE);
				else
					pPlayer->menuexpire = gpGlobals->time + static_cast<float>(time);

				pPlayer->page = 0;
				UTIL_ShowMenu(pPlayer->pEdict, keys, time, sMenu, ilen);
			}
		}
	} else {
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame)
		{
			pPlayer->keys = keys;
			pPlayer->menu = menuid;
			pPlayer->vgui = false;

			if (time == -1)
				pPlayer->menuexpire = static_cast<float>(INFINITE);
			else
				pPlayer->menuexpire = gpGlobals->time + static_cast<float>(time);

			pPlayer->page = 0;
			UTIL_ShowMenu(pPlayer->pEdict, keys, time, sMenu, ilen);
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL register_plugin(AMX *amx, cell *params) /* 5 param */
{
	enum { arg_count, arg_title, arg_version, arg_author, arg_url, arg_description };

	CPluginMngr::CPlugin* a = g_plugins.findPluginFast(amx);
	int i;


	a->setTitle(get_amxstring(amx, params[arg_title], 0, i));
	a->setVersion(get_amxstring(amx, params[arg_version], 0, i));
	a->setAuthor(get_amxstring(amx, params[arg_author], 0, i));

#if defined BINLOG_ENABLED
	g_BinLog.WriteOp(BinLog_Registered, a->getId(), a->getTitle(), a->getVersion());
#endif

	if (params[arg_count] / sizeof(cell) > arg_author)
	{
		a->setUrl(get_amxstring(amx, params[arg_url], 0, i));
		a->setDescription(get_amxstring(amx, params[arg_description], 0, i));
	}

	/* Check if we need to add fail counters */
	i = 0;
	unsigned int counter = 0;
	while (NONGPL_PLUGIN_LIST[i].author != NULL)
	{
		if (strcmp(NONGPL_PLUGIN_LIST[i].author, a->getAuthor()) == 0)
		{
			counter++;
		}
		if (stricmp(NONGPL_PLUGIN_LIST[i].filename, a->getName()) == 0)
		{
			counter++;
		}
		if (stricmp(NONGPL_PLUGIN_LIST[i].title, a->getTitle()) == 0)
		{
			counter++;
		}
		if (counter)
		{
			a->AddToFailCounter(counter);
			break;
		}
		i++;
	}

	return a->getId();
}

static cell AMX_NATIVE_CALL register_menucmd(AMX *amx, cell *params) /* 3 param */
{
	CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast(amx);
	int ilen, idx;
	char* sptemp = get_amxstring(amx, params[3], 0, ilen);

	idx = registerSPForwardByName(amx, sptemp, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (idx == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Function \"%s\" was not found", sptemp);
		return 0;
	}

	g_menucmds.registerMenuCmd(plugin, params[1], params[2], idx);

	return 1;
}

static cell AMX_NATIVE_CALL get_plugin(AMX *amx, cell *params) /* 15 param */
{
	enum
	{ 
		arg_count, arg_plugin, arg_name, arg_namelen, arg_title, arg_titlelen, 
		arg_version, arg_versionlen, arg_author, arg_authorlen, arg_status, arg_statuslen,
		arg_url, arg_urllen, arg_description, arg_descriptionlen 
	};

	CPluginMngr::CPlugin* a;

	if (params[arg_plugin] < 0)
		a = g_plugins.findPluginFast(amx);
	else
		a = g_plugins.findPlugin((int)params[arg_plugin]);

	if (a)
	{
		set_amxstring(amx, params[arg_name], a->getName(), params[arg_namelen]);
		set_amxstring(amx, params[arg_title], a->getTitle(), params[arg_titlelen]);
		set_amxstring(amx, params[arg_version], a->getVersion(), params[arg_versionlen]);
		set_amxstring(amx, params[arg_author], a->getAuthor(), params[arg_authorlen]);
		set_amxstring(amx, params[arg_status], a->getStatus(), params[arg_statuslen]);

		if (params[arg_count] / sizeof(cell) > arg_url)
		{
			set_amxstring(amx, params[arg_url], a->getUrl(), params[arg_urllen]);
			set_amxstring(amx, params[arg_description], a->getDescription(), params[arg_descriptionlen]);
		}

		return a->getId();
	}

	return -1;
}

static cell AMX_NATIVE_CALL amx_md5(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);
	const char *hash = hashString((const char *)str, len, Hash_Md5);

	return set_amxstring(amx, params[2], hash, 32);
}

static cell AMX_NATIVE_CALL amx_md5_file(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);
	char file[PLATFORM_MAX_PATH];

	build_pathname_r(file, sizeof(file), "%s", str);

	const char *hash = hashFile((const char *)file, Hash_Md5);
	if (!hash)
	{
		LogError(amx, AMX_ERR_NATIVE, "Cant open file \"%s\"", file);
		return 0;
	}

	return set_amxstring(amx, params[2], hash, 32);
}

static cell AMX_NATIVE_CALL amx_hash_string(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);
	HashType type = (HashType)params[2];

	const char *hash = hashString((const char *)str, len, type);
	if (!hash)
	{
		LogError(amx, AMX_ERR_NATIVE, "Cant hash string \"%s\"", str);
		return 0;
	}

	return set_amxstring(amx, params[3], hash, params[4]);
}

static cell AMX_NATIVE_CALL amx_hash_file(AMX *amx, cell *params)
{
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);
	char file[PLATFORM_MAX_PATH];
	build_pathname_r(file, sizeof(file), "%s", str);

	HashType type = (HashType)params[2];

	const char *hash = hashFile((const char *)file, type);
	if (!hash)
	{
		LogError(amx, AMX_ERR_NATIVE, "Cant open file \"%s\"", file);
		return 0;
	}

	return set_amxstring(amx, params[3], hash, params[4]);
}

static cell AMX_NATIVE_CALL get_pluginsnum(AMX *amx, cell *params)
{
	return g_plugins.getPluginsNum();
}

// native register_concmd(const cmd[], const function[], flags = -1, const info[] = "", FlagManager = -1, bool:info_ml = false);
static cell AMX_NATIVE_CALL register_concmd(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast(amx);
	int i, idx = 0;
	char* temp = get_amxstring(amx, params[2], 0, i);

	idx = registerSPForwardByName(amx, temp, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (idx == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Function \"%s\" was not found", temp);
		return 0;
	}

	temp = get_amxstring(amx, params[1], 0, i);
	char* info = get_amxstring(amx, params[4], 1, i);
	CmdMngr::Command* cmd;
	int access = params[3];
	bool listable = true;
	bool info_ml = *params / sizeof(cell) >= 6 && params[6] != 0 && i;

	if (access < 0)		// is access is -1 then hide from listing
	{
		access = 0;
		listable = false;
	}

	if (FlagMan.ShouldIAddThisCommand(amx,params,temp)==1)
	{
		FlagMan.LookupOrAdd(temp,access,amx);
	}

	if ((cmd = g_commands.registerCommand(plugin, idx, temp, info, access, listable, info_ml)) == NULL)
		return 0;

	if (CheckBadConList(temp, 1))
	{
		plugin->AddToFailCounter(1);
	}

	cmd->setCmdType(CMD_ConsoleCommand);
	REG_SVR_COMMAND((char*)cmd->getCommand(), plugin_srvcmd);

	return cmd->getId();
}

// native register_clcmd(const client_cmd[], const function[], flags = -1, const info[] = "", FlagManager = -1, bool:info_ml = false);
static cell AMX_NATIVE_CALL register_clcmd(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast(amx);
	int i, idx = 0;
	char* temp = get_amxstring(amx, params[2], 0, i);

	idx = registerSPForwardByName(amx, temp, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (idx == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Function \"%s\" was not found", temp);
		return 0;
	}

	temp = get_amxstring(amx, params[1], 0, i);
	const char* info = get_amxstring(amx, params[4], 1, i);
	CmdMngr::Command* cmd;
	int access = params[3];
	bool listable = true;
	bool info_ml = *params / sizeof(cell) >= 6 && params[6] != 0 && i;

	if (access < 0)		// is access is -1 then hide from listing
	{
		access = 0;
		listable = false;
	}

	if (FlagMan.ShouldIAddThisCommand(amx,params,temp)==1)
	{
		FlagMan.LookupOrAdd(temp,access,amx);
	}

	if ((cmd = g_commands.registerCommand(plugin, idx, temp, info, access, listable, info_ml)) == NULL)
		return 0;

	cmd->setCmdType(CMD_ClientCommand);

	return cmd->getId();
}

// native register_srvcmd(const server_cmd[], const function[], flags = -1, const info[] = "", bool:info_ml = false);
static cell AMX_NATIVE_CALL register_srvcmd(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast(amx);
	int i, idx = 0;
	char* temp = get_amxstring(amx, params[2], 0, i);

	idx = registerSPForwardByName(amx, temp, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (idx == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Function \"%s\" was not found", temp);
		return 0;
	}

	temp = get_amxstring(amx, params[1], 0, i);
	const char* info = get_amxstring(amx, params[4], 1, i);
	CmdMngr::Command* cmd;
	int access = params[3];
	bool listable = true;
	bool info_ml = *params / sizeof(cell) >= 5 && params[5] != 0 && i;

	if (access < 0)		// is access is -1 then hide from listing
	{
		access = 0;
		listable = false;
	}

	if ((cmd = g_commands.registerCommand(plugin, idx, temp, info, access, listable, info_ml)) == NULL)
		return 0;

	cmd->setCmdType(CMD_ServerCommand);
	REG_SVR_COMMAND((char*)cmd->getCommand(), plugin_srvcmd);

	return cmd->getId();
}

// native get_concmd(index, cmd[], len1, &flags, info[], len2, flag, id = -1, &bool:info_ml = false);
static cell AMX_NATIVE_CALL get_concmd(AMX *amx, cell *params)
{
	int who = params[8];

	if (who > 0)		// id of player - client command
		who = CMD_ClientCommand;
	else if (who == 0)	// server
		who = CMD_ServerCommand;
	else				// -1 parameter - all commands
		who = CMD_ConsoleCommand;

	CmdMngr::Command* cmd = g_commands.getCmd(params[1], who, params[7]);

	if (cmd == 0)
		return 0;

	set_amxstring_utf8(amx, params[2], cmd->getCmdLine(), strlen(cmd->getCmdLine()), params[3]);
	set_amxstring_utf8(amx, params[5], cmd->getCmdInfo(), strlen(cmd->getCmdInfo()), params[6]);
	cell *cpFlags = get_amxaddr(amx, params[4]);
	*cpFlags = cmd->getFlags();

	if (*params / sizeof(cell) >= 9)
	{
		*get_amxaddr(amx, params[9]) = cmd->isInfoML();
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_concmd_plid(AMX *amx, cell *params)
{
	int who = params[3];
	if (who > 0)
	{
		who = CMD_ClientCommand;
	} else if (who == 0) {
		who = CMD_ServerCommand;
	} else {
		who = CMD_ConsoleCommand;
	}

	CmdMngr::Command *cmd = g_commands.getCmd(params[1], who, params[2]);

	if (cmd == NULL)
	{
		return -1;
	}

	return cmd->getPlugin()->getId();
}

// native get_clcmd(index, command[], len1, &flags, info[], len2, flag, &bool:info_ml = false);
static cell AMX_NATIVE_CALL get_clcmd(AMX *amx, cell *params)
{
	CmdMngr::Command* cmd = g_commands.getCmd(params[1], CMD_ClientCommand, params[7]);

	if (cmd == 0)
		return 0;

	set_amxstring_utf8(amx, params[2], cmd->getCmdLine(), strlen(cmd->getCmdLine()), params[3]);
	set_amxstring_utf8(amx, params[5], cmd->getCmdInfo(), strlen(cmd->getCmdInfo()), params[6]);

	cell *cpFlags = get_amxaddr(amx, params[4]);
	*cpFlags = cmd->getFlags();

	if (*params / sizeof(cell) >= 8)
	{
		*get_amxaddr(amx, params[8]) = cmd->isInfoML();
	}

	return 1;
}

// native get_srvcmd(index, server_cmd[], len1, &flags, info[], len2, flag, &bool:info_ml = false);
static cell AMX_NATIVE_CALL get_srvcmd(AMX *amx, cell *params)
{
	CmdMngr::Command* cmd = g_commands.getCmd(params[1], CMD_ServerCommand, params[7]);

	if (cmd == 0)
		return 0;

	set_amxstring_utf8(amx, params[2], cmd->getCmdLine(), strlen(cmd->getCmdLine()), params[3]);
	set_amxstring_utf8(amx, params[5], cmd->getCmdInfo(), strlen(cmd->getCmdInfo()), params[6]);
	cell *cpFlags = get_amxaddr(amx, params[4]);
	*cpFlags = cmd->getFlags();

	if (*params / sizeof(cell) >= 8)
	{
		*get_amxaddr(amx, params[8]) = cmd->isInfoML();
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_srvcmdsnum(AMX *amx, cell *params)
{
	return g_commands.getCmdNum(CMD_ServerCommand, params[1]);
}

static cell AMX_NATIVE_CALL get_clcmdsnum(AMX *amx, cell *params) /* 1 param */
{
	return g_commands.getCmdNum(CMD_ClientCommand, params[1]);
}

static cell AMX_NATIVE_CALL get_concmdsnum(AMX *amx, cell *params) /* 1 param */
{
	int who = params[2];

	if (who > 0)
		return g_commands.getCmdNum(CMD_ClientCommand, params[1]);
	if (who == 0)
		return g_commands.getCmdNum(CMD_ServerCommand, params[1]);

	return g_commands.getCmdNum(CMD_ConsoleCommand, params[1]);
}

// native register_event(const event[], const function[], const flags[], const cond[] = "", ...);
static cell AMX_NATIVE_CALL register_event(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin* plugin = g_plugins.findPluginFast(amx);

	int len, eventId, forwardId;

	const char* eventName = get_amxstring(amx, params[1], 0, len);

	if ((eventId = g_events.getEventId(eventName)) == 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid event (name \"%s\") (plugin \"%s\")", eventName, plugin->getName());
		return 0;
	}

	const char* callback = get_amxstring(amx, params[2], 0, len);

	forwardId = registerSPForwardByName(amx, callback, FP_CELL, FP_DONE);

	if (forwardId == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Function \"%s\" was not found", callback);
		return 0;
	}

	int numparam = *params / sizeof(cell);
	int flags = 0;

	if (numparam > 2)
	{
		flags = UTIL_ReadFlags(get_amxstring(amx, params[3], 0, len));
	}

	int handle = g_events.registerEvent(plugin, forwardId, flags, eventId);

	if (!handle)
	{
		return 0;
	}

	auto event = EventHandles.lookup(handle)->m_event;

	for (int i = 4; i <= numparam; ++i)
	{
		event->registerFilter(get_amxstring(amx, params[i], 0, len));
	}

	return handle;
}

// native register_event_ex(const event[], const function[], RegisterEventFlags:flags, const cond[] = "", ...);
static cell AMX_NATIVE_CALL register_event_ex(AMX *amx, cell *params)
{
	cell amx_addr;
	cell *phys_addr;
	char strFlags[8];

	amx_Allot(amx, ARRAY_LENGTH(strFlags), &amx_addr, &phys_addr);
	UTIL_GetFlags(strFlags, params[3]);
	set_amxstring(amx, amx_addr, strFlags, ARRAY_LENGTH(strFlags) - 1);

	params[3] = amx_addr;
	cell ret = register_event(amx, params);
	amx_Release(amx, amx_addr);

	return ret;
}

static cell AMX_NATIVE_CALL enable_event(AMX *amx, cell *params)
{
	auto handle = EventHandles.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid event handle %d", params[1]);
		return 0;
	}

	handle->m_event->setForwardState(FSTATE_ACTIVE);

	return 1;
}

static cell AMX_NATIVE_CALL disable_event(AMX *amx, cell *params)
{
	auto handle = EventHandles.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid event handle: %d", params[1]);
		return 0;
	}

	handle->m_event->setForwardState(FSTATE_STOP);

	return 1;
}

static cell AMX_NATIVE_CALL user_kill(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame && pPlayer->IsAlive())
	{
		float bef = pPlayer->pEdict->v.frags;
		MDLL_ClientKill(pPlayer->pEdict);

		if (params[2])
			pPlayer->pEdict->v.frags = bef;

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL user_slap(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	int power = (int)params[2];

	if (power < 0)
		power = 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame && pPlayer->IsAlive())
	{
		if (pPlayer->pEdict->v.health <= power)
		{
			float bef = pPlayer->pEdict->v.frags;
			MDLL_ClientKill(pPlayer->pEdict);
			pPlayer->pEdict->v.frags = bef;
		} else {
			edict_t *pEdict = pPlayer->pEdict;
			int numparam = *params / sizeof(cell);

			if (numparam < 3 || params[3])
			{
				pEdict->v.velocity.x += RANDOM_LONG(-600, 600);
				pEdict->v.velocity.y += RANDOM_LONG(-180, 180);
				pEdict->v.velocity.z += RANDOM_LONG(100, 200);
			} else {
				Vector v_forward, v_right;
				Vector vang = pEdict->v.angles;
				float fang[3];
				fang[0] = vang.x;
				fang[1] = vang.y;
				fang[2] = vang.z;
				ANGLEVECTORS(fang, v_forward, v_right, NULL);
				pEdict->v.velocity = pEdict->v.velocity + v_forward * 220 + Vector(0, 0, 200);
			}

			pEdict->v.punchangle.x = static_cast<vec_t>(RANDOM_LONG(-10, 10));
			pEdict->v.punchangle.y = static_cast<vec_t>(RANDOM_LONG(-10, 10));
			pEdict->v.health -= power;

			int armor = (int)pEdict->v.armorvalue;
			armor -= power;

			if (armor < 0)
				armor = 0;

			pEdict->v.armorvalue = static_cast<float>(armor);
			pEdict->v.dmg_inflictor = pEdict;

			if (g_bmod_cstrike)
			{
				static const char *cs_sound[4] =
				{
					"player/bhit_flesh-3.wav",
					"player/bhit_flesh-2.wav",
					"player/pl_die1.wav",
					"player/pl_pain6.wav"
				};
				EMIT_SOUND_DYN2(pEdict, CHAN_VOICE, cs_sound[RANDOM_LONG(0, 3)], 1.0, ATTN_NORM, 0, PITCH_NORM);
			} else{
				static const char *bit_sound[3] =
				{
					"weapons/cbar_hitbod1.wav",
					"weapons/cbar_hitbod2.wav",
					"weapons/cbar_hitbod3.wav"
				};
				EMIT_SOUND_DYN2(pEdict, CHAN_VOICE, bit_sound[RANDOM_LONG(0, 2)], 1.0, ATTN_NORM, 0, PITCH_NORM);
			}
		}

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL server_cmd(AMX *amx, cell *params) /* 1 param */
{
	int len;
	g_langMngr.SetDefLang(LANG_SERVER);
	char* cmd = format_amxstring(amx, params, 1, len);

	cmd[len++] = '\n';
	cmd[len] = 0;

	SERVER_COMMAND(cmd);

	CoreCfg.CheckLegacyBufferedCommand(cmd);

	return len;
}

static cell AMX_NATIVE_CALL client_cmd(AMX *amx, cell *params) /* 2 param */
{
	int len;
	char* cmd = format_amxstring(amx, params, 2, len);
	cmd[len++] = '\n';
	cmd[len] = 0;

	if (params[1] == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
			if (!pPlayer->IsBot() && pPlayer->initialized /*&& pPlayer->ingame*/)
				CLIENT_COMMAND(pPlayer->pEdict, "%s", cmd);
		}
	} else {
		int index = params[1];

		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (!pPlayer->IsBot() && pPlayer->initialized /*&& pPlayer->ingame*/)
			CLIENT_COMMAND(pPlayer->pEdict, "%s", cmd);
	}

	return len;
}

static cell AMX_NATIVE_CALL log_message(AMX *amx, cell *params) /* 1 param */
{
	int len;
	g_langMngr.SetDefLang(LANG_SERVER);
	char* message = format_amxstring(amx, params, 1, len);

	message[len++] = '\n';
	message[len] = 0;

	ALERT(at_logged, "%s", message);

	return len;
}

static cell AMX_NATIVE_CALL elog_message(AMX *amx, cell *params) /* 1 param */
{
	int len;
	g_langMngr.SetDefLang(LANG_SERVER);
	char* message = format_amxstring(amx, params, 1, len);

	message[len++] = '\n';
	message[len] = 0;

	g_pEngTable->pfnAlertMessage(at_logged, "%s", message);
	return len;
}

static cell AMX_NATIVE_CALL log_to_file(AMX *amx, cell *params) /* 1 param */
{
	int ilen;
	char* szFile = get_amxstring(amx, params[1], 0, ilen);
	FILE*fp;
	char file[PLATFORM_MAX_PATH];

	if (strchr(szFile, '/') || strchr(szFile, '\\'))
	{
		build_pathname_r(file, sizeof(file), "%s", szFile);
	} else {
		build_pathname_r(file, sizeof(file), "%s/%s", g_log_dir.chars(), szFile);
	}

	bool first_time = true;

	if ((fp = fopen(file, "r")) != NULL)
	{
		first_time = false;
		fclose(fp);
	}

	if ((fp = fopen(file, "a")) == NULL)
	{
		//amx_RaiseError(amx, AMX_ERR_NATIVE);
		//would cause too much troubles in old plugins
		return 0;
	}

	char date[32];
	time_t td; time(&td);
	strftime(date, 31, "%m/%d/%Y - %H:%M:%S", localtime(&td));
	int len;
	g_langMngr.SetDefLang(LANG_SERVER);
	char* message = format_amxstring(amx, params, 2, len);

	message[len++] = '\n';
	message[len] = 0;

	if (first_time)
	{
		fprintf(fp, "L %s: Log file started (file \"%s\") (game \"%s\") (amx \"%s\")\n", date, file, g_mod_name.chars(), Plugin_info.version);
		print_srvconsole("L %s: Log file started (file \"%s\") (game \"%s\") (amx \"%s\")\n", date, file, g_mod_name.chars(), Plugin_info.version);
	}

	fprintf(fp, "L %s: %s", date, message);
	print_srvconsole("L %s: %s", date, message);
	fclose(fp);

	return 1;
}

static cell AMX_NATIVE_CALL num_to_word(AMX *amx, cell *params) /* 3 param */
{
	char sptemp[512];
	UTIL_IntToString(params[1], sptemp);

	return set_amxstring(amx, params[2], sptemp, params[3]);
}

static cell AMX_NATIVE_CALL get_timeleft(AMX *amx, cell *params)
{
	float flCvarTimeLimit = mp_timelimit->value;

	if (flCvarTimeLimit)
	{
		int iReturn = (int)((g_game_timeleft + flCvarTimeLimit * 60.0) - gpGlobals->time);
		return (iReturn < 0) ? 0 : iReturn;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_time(AMX *amx, cell *params) /* 3 param */
{
	int ilen;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);
	time_t td = time(NULL);
	tm* lt = localtime(&td);

	char szDate[512];
	strftime(szDate, 511, sptemp, lt);

	return set_amxstring(amx, params[2], szDate, params[3]);
}

static cell AMX_NATIVE_CALL format_time(AMX *amx, cell *params) /* 3 param */
{
	int ilen;
	char* sptemp = get_amxstring(amx, params[3], 0, ilen);
	time_t tim = params[4];
	time_t td = (tim != -1) ? tim : time(NULL);
	tm* lt = localtime(&td);

	if (lt == 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Couldn't get localtime");
		return 0;
	}

	char szDate[512];
	ilen = strftime(szDate, 511, sptemp, lt);

	return set_amxstring_utf8(amx, params[1], szDate, ilen, params[2]);

}

static cell AMX_NATIVE_CALL parse_time(AMX *amx, cell *params) /* 3 param */
{
	int ilen;
	char* sTime = get_amxstring(amx, params[1], 1, ilen);
	char* sFormat = get_amxstring(amx, params[2], 0, ilen);
	tm* mytime;
	time_t td;

	if (params[3] == -1)
	{
		td = time(NULL);
		mytime = localtime(&td);

		if (mytime == 0)
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't get localtime");
			return 0;
		}

		strptime(sTime, sFormat, mytime, 0);
	} else {
		td = params[3];
		mytime = localtime(&td);

		if (mytime == 0)
		{
			LogError(amx, AMX_ERR_NATIVE, "Couldn't get localtime");
			return 0;
		}

		strptime(sTime, sFormat, mytime, 1);
	}

	return mktime(mytime);
}

static cell AMX_NATIVE_CALL get_systime(AMX *amx, cell *params) /* 3 param */
{
	time_t td = time(NULL);
	td += params[1];

	return td;
}

static cell AMX_NATIVE_CALL read_datanum(AMX *amx, cell *params) /* 0 param */
{
	return g_events.getArgNum();
}

static cell AMX_NATIVE_CALL read_data(AMX *amx, cell *params) /* 3 param */
{
	if (params[0] == 0)
	{
		return g_events.getCurrentMsgType();
	}

	switch (*params / sizeof(cell))
	{
		case 1:
			return g_events.getArgInteger(params[1]);
		case 3:
			return set_amxstring_utf8(amx, params[2], g_events.getArgString(params[1]),
				strlen(g_events.getArgString(params[1])),*get_amxaddr(amx, params[3]));
		default:
			cell *fCell = get_amxaddr(amx, params[2]);
			REAL fparam = (REAL)g_events.getArgFloat(params[1]);
			fCell[0] = amx_ftoc(fparam);
			return (int)fparam;
	}
}

static cell AMX_NATIVE_CALL read_datatype(AMX *amx, cell *params) /* 0 param */
{
	return g_events.getCurrentMsgType();
}

static cell AMX_NATIVE_CALL get_playersnum(AMX *amx, cell *params)
{
	if (!params[1])
		return g_players_num;

	int a = 0;

	for (int i = 1; i <= gpGlobals->maxClients; ++i)
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

		if (pPlayer->initialized && (GETPLAYERUSERID(pPlayer->pEdict) > 0))
			++a;
	}

	return a;
}

static cell AMX_NATIVE_CALL get_players(AMX *amx, cell *params) /* 4 param */
{
	int iNum = 0;
	int ilen;
	char* sptemp = get_amxstring(amx, params[3], 0, ilen);
	int flags = UTIL_ReadFlags(sptemp);

	cell *aPlayers = get_amxaddr(amx, params[1]);
	cell *iMax = get_amxaddr(amx, params[2]);

	int team = 0;

	if (flags & 48)
	{
		sptemp = get_amxstring(amx, params[4], 0, ilen);

		if (flags & 16)
		{
			if (flags & 64)
				team = g_teamsIds.findTeamId(sptemp);
			else
				team = g_teamsIds.findTeamIdCase(sptemp);
		}
	}

	for (int i = 1; i <= gpGlobals->maxClients; ++i)
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);
		if (pPlayer->ingame || ((flags & 256) && pPlayer->initialized))
		{
			if (pPlayer->IsAlive() ? (flags & 2) : (flags & 1))
				continue;
			if (pPlayer->IsBot() ? (flags & 4) : (flags & 8))
				continue;
			if ((flags & 16) && (pPlayer->teamId != team))
				continue;
			if ((flags & 128) && (pPlayer->pEdict->v.flags & FL_PROXY))
				continue;
			if (flags & 32)
			{
				if (flags & 64)
				{
					if (utf8stristr(pPlayer->name.chars(), sptemp) == NULL)
						continue;
				}
				else if (strstr(pPlayer->name.chars(), sptemp) == NULL)
					continue;
			}
			aPlayers[iNum++] = i;
		}
	}

	*iMax = iNum;

	return 1;
}

static cell AMX_NATIVE_CALL find_player(AMX *amx, cell *params) /* 1 param */
{
	typedef int (*STRCOMPARE)(const char*, const char*);
	STRCOMPARE func;

	int ilen, userid = 0;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);
	int flags = UTIL_ReadFlags(sptemp);

	if (flags & 31)
		sptemp = get_amxstring(amx, params[2], 0, ilen);
	else if (flags & 1024)
		userid = *get_amxaddr(amx, params[2]);

	// a b c d e f g h i j k l
	int result = 0;

	// Switch for the l flag
	if (flags & 2048)
		func = utf8strcasecmp;
	else
		func = strcmp;

	for (int i = 1; i <= gpGlobals->maxClients; ++i)
	{
		CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

		if (pPlayer->ingame || ((flags & 4096) && pPlayer->initialized))
		{
			if (pPlayer->IsAlive() ? (flags & 64) : (flags & 32))
				continue;

			if (pPlayer->IsBot() ? (flags & 128) : (flags & 256))
				continue;

			if (flags & 1)
			{
				if ((func)(pPlayer->name.chars(), sptemp))
					continue;
			}

			if (flags & 2)
			{
				if (flags & 2048)
				{
					if (utf8stristr(pPlayer->name.chars(), sptemp) == NULL)
						continue;
				}
				else if (strstr(pPlayer->name.chars(), sptemp) == NULL)
					continue;
			}

			if (flags & 4)
			{
				const char* authid = GETPLAYERAUTHID(pPlayer->pEdict);

				if (!authid || (func)(authid, sptemp))
					continue;
			}

			if (flags & 1024)
			{
				if (userid != GETPLAYERUSERID(pPlayer->pEdict))
					continue;
			}

			if (flags & 8)
			{
				if (strncmp(pPlayer->ip.chars(), sptemp, ilen))
					continue;
			}

			if (flags & 16)
			{
				if ((func)(pPlayer->team.chars(), sptemp))
					continue;
			}

			result = i;

			if ((flags & 512) == 0)
				break;
		}
	}

	return result;
}

// native find_player_ex(FindPlayerFlags:flags, ...);
static cell AMX_NATIVE_CALL find_player_ex(AMX *amx, cell *params)
{
	cell amx_addr;
	cell *phys_addr;
	char strFlags[14];

	amx_Allot(amx, ARRAY_LENGTH(strFlags), &amx_addr, &phys_addr);
	UTIL_GetFlags(strFlags, params[1]);
	set_amxstring(amx, amx_addr, strFlags, ARRAY_LENGTH(strFlags) - 1);

	params[1] = amx_addr;
	cell ret = find_player(amx, params);
	amx_Release(amx, amx_addr);

	return ret;
}

static cell AMX_NATIVE_CALL get_maxplayers(AMX *amx, cell *params)
{
	return gpGlobals->maxClients;
}

static cell AMX_NATIVE_CALL get_gametime(AMX *amx, cell *params)
{
	REAL pFloat = (REAL)gpGlobals->time;
	return amx_ftoc(pFloat);
}

static cell AMX_NATIVE_CALL get_mapname(AMX *amx, cell *params) /* 2 param */
{
	return set_amxstring(amx, params[1], STRING(gpGlobals->mapname), params[2]);
}

static cell AMX_NATIVE_CALL get_modname(AMX *amx, cell *params) /* 2 param */
{
	return set_amxstring(amx, params[1], g_mod_name.chars(), params[2]);
}

static cell AMX_NATIVE_CALL get_localinfo(AMX *amx, cell *params) /* 3 param */
{
	int ilen;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);

	char *value = LOCALINFO(sptemp);
	return set_amxstring_utf8(amx, params[2], value, strlen(value), params[3]);
}

static cell AMX_NATIVE_CALL set_localinfo(AMX *amx, cell *params) /* 2 param */
{
	int ilen;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);
	char* szValue = get_amxstring(amx, params[2], 1, ilen);

	SET_LOCALINFO(sptemp, szValue);

	return 1;
}

static cell AMX_NATIVE_CALL get_user_info(AMX *amx, cell *params) /* 4 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (!pPlayer->pEdict)
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d is not connected", index);
		return 0;
	}

	int ilen;
	char* sptemp = get_amxstring(amx, params[2], 0, ilen);

	return set_amxstring(amx, params[3], ENTITY_KEYVALUE(pPlayer->pEdict, sptemp), params[4]);
	return 1;
}

static cell AMX_NATIVE_CALL set_user_info(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (!pPlayer->pEdict)
	{
		LogError(amx, AMX_ERR_NATIVE, "Player %d is not connected", index);
		return 0;
	}

	int ilen;
	char* sptemp = get_amxstring(amx, params[2], 0, ilen);
	char* szValue = get_amxstring(amx, params[3], 1, ilen);

	ENTITY_SET_KEYVALUE(pPlayer->pEdict, sptemp, szValue);

	return 1;
}

static cell AMX_NATIVE_CALL read_argc(AMX *amx, cell *params)
{
	return g_fakecmd.notify ? g_fakecmd.argc : CMD_ARGC();
}

static cell AMX_NATIVE_CALL read_argv(AMX *amx, cell *params) /* 3 param */
{
	int argc = params[1];

	const char *value = g_fakecmd.notify ? ((argc >= 0 && argc < 3 && g_fakecmd.argv[argc] != nullptr) ? g_fakecmd.argv[argc] : "") : CMD_ARGV(argc);
	return set_amxstring_utf8(amx, params[2], value, strlen(value), params[3]);
}

static cell AMX_NATIVE_CALL read_argv_int(AMX *amx, cell *params) /* 1 param */
{
	int argc = params[1];

	if (argc <= 0)
	{
		return 0;
	}

	const char *value = g_fakecmd.notify ? ((argc >= 1 && argc < 3 && g_fakecmd.argv[argc] != nullptr) ? g_fakecmd.argv[argc] : "") : CMD_ARGV(argc);

	return atoi(value);
}

static cell AMX_NATIVE_CALL read_argv_float(AMX *amx, cell *params) /* 1 param */
{
	int argc = params[1];

	if (argc <= 0)
	{
		return 0;
	}

	const char *value = g_fakecmd.notify ? ((argc >= 1 && argc < 3 && g_fakecmd.argv[argc] != nullptr) ? g_fakecmd.argv[argc] : "") : CMD_ARGV(argc);
	float flValue = atof(value);

	return amx_ftoc(flValue);
}

static cell AMX_NATIVE_CALL read_args(AMX *amx, cell *params) /* 2 param */
{
	const char* sValue = g_fakecmd.notify ? (g_fakecmd.argc > 1 ? g_fakecmd.args : "") : CMD_ARGS();
	return set_amxstring_utf8(amx, params[1], sValue ? sValue : "", sValue ? strlen(sValue) : 0, params[2]);
}

static cell AMX_NATIVE_CALL get_user_msgid(AMX *amx, cell *params) /* 1 param */
{
	int ilen;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);

	return GET_USER_MSG_ID(PLID, sptemp, NULL);
}

static cell AMX_NATIVE_CALL get_user_msgname(AMX *amx, cell *params) /* get_user_msgname(msg, str[], len) = 3 params */
{
	const char* STRING = GET_USER_MSG_NAME(PLID, params[1], NULL);
	if (STRING)
		return set_amxstring(amx, params[2], STRING, params[3]);

	// Comes here if GET_USER_MSG_NAME failed (ie, invalid msg id)
	return 0;
}

static cell AMX_NATIVE_CALL set_task(AMX *amx, cell *params) /* 2 param */
{

	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);

	int a, iFunc;

	char* stemp = get_amxstring(amx, params[2], 1, a);

	if (params[5])
	{
		iFunc = registerSPForwardByName(amx, stemp, FP_ARRAY, FP_CELL, FP_DONE);
	} else {
		iFunc = registerSPForwardByName(amx, stemp, FP_CELL, FP_DONE);
	}

	if (iFunc == -1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function is not present (function \"%s\") (plugin \"%s\")", stemp, plugin->getName());
		return 0;
	}

	float base = amx_ctof(params[1]);

	if (base < 0.1f)
		base = 0.1f;

	char* temp = get_amxstring(amx, params[6], 0, a);

	g_tasksMngr.registerTask(plugin, iFunc, UTIL_ReadFlags(temp), params[3], base, params[5], get_amxaddr(amx, params[4]), params[7]);

	return 1;
}

static cell AMX_NATIVE_CALL remove_task(AMX *amx, cell *params) /* 1 param */
{
	return g_tasksMngr.removeTasks(params[1], params[2] ? 0 : amx);
}

static cell AMX_NATIVE_CALL change_task(AMX *amx, cell *params)
{
	REAL flNewTime = amx_ctof(params[2]);
	return g_tasksMngr.changeTasks(params[1], params[3] ? 0 : amx, flNewTime);
}

static cell AMX_NATIVE_CALL engine_changelevel(AMX *amx, cell *params)
{
	int length;
	ke::AString new_map(get_amxstring(amx, params[1], 0, length));

	// Same as calling "changelevel" command but will trigger "server_changelevel" AMXX forward as well.
	// Filling second param will call "changelevel2" command, but this is not usable in multiplayer game.
	g_pEngTable->pfnChangeLevel(new_map.chars(), NULL);

	return 1;
}

static cell AMX_NATIVE_CALL task_exists(AMX *amx, cell *params) /* 1 param */
{
	return g_tasksMngr.taskExists(params[1], params[2] ? 0 : amx);
}

static cell AMX_NATIVE_CALL get_user_ping(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		cell *cpPing = get_amxaddr(amx, params[2]);
		cell *cpLoss = get_amxaddr(amx, params[3]);
		int ping, loss;
		PLAYER_CNX_STATS(pPlayer->pEdict, &ping, &loss);
		*cpPing = ping;
		*cpLoss = loss;

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_time(AMX *amx, cell *params) /* 1 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
		return 0;

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		int time = (int)(gpGlobals->time - (params[2] ? pPlayer->playtime : pPlayer->time));
		return time;
	}

	return 0;
}

static cell AMX_NATIVE_CALL server_exec(AMX *amx, cell *params)
{
	SERVER_EXECUTE();
	return 1;
}

int sendFakeCommand(AMX *amx, cell *params, bool send_forward = false)
{
	enum args { arg_count, arg_index, arg_command, arg_argument1, arg_argument2 };

	char command[128 * 2];
	auto command_length = strncopy(command, get_amxaddr(amx, params[arg_command]), sizeof(command));

	if (!command_length)
	{
		return 0;
	}

	char argument1[128];
	char argument2[128];
	auto argument1_length = strncopy(argument1, get_amxaddr(amx, params[arg_argument1]), sizeof(argument1));
	auto argument2_length = strncopy(argument2, get_amxaddr(amx, params[arg_argument2]), sizeof(argument2));

	const char *pArgument1 = argument1_length ? argument1 : nullptr;
	const char *pArgument2 = argument2_length ? argument2 : nullptr;

	int index = params[arg_index];

	if (index == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer* pPlayer = GET_PLAYER_POINTER_I(i);

			if (pPlayer->ingame /*&& pPlayer->initialized */)
				UTIL_FakeClientCommand(pPlayer->pEdict, command, pArgument1, pArgument2, send_forward);
		}
	}
	else
	{
		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (/*pPlayer->initialized && */pPlayer->ingame)
			UTIL_FakeClientCommand(pPlayer->pEdict, command, pArgument1, pArgument2, send_forward);
	}

	return 1;
}

// native engclient_cmd(index, const command[], const arg1[] = "", const arg2[] = "");
static cell AMX_NATIVE_CALL engclient_cmd(AMX *amx, cell *params)
{
	return sendFakeCommand(amx, params);
}

// native amxclient_cmd(index, const command[], const arg1[] = "", const arg2[] = "");
static cell AMX_NATIVE_CALL amxclient_cmd(AMX *amx, cell *params)
{
	return sendFakeCommand(amx, params, true);
}

static cell AMX_NATIVE_CALL pause(AMX *amx, cell *params) /* 3 param */
{
	int ilen;
	char* temp = get_amxstring(amx, params[1], 0, ilen);
	int flags = UTIL_ReadFlags(temp);

	CPluginMngr::CPlugin *plugin = 0;

	if (flags & 2)		// pause function
	{
		LogError(amx, AMX_ERR_NATIVE, "This usage of the native pause() has been deprecated!");
		return 1;
	}
	else if (flags & 4)
	{
		temp = get_amxstring(amx, params[2], 0, ilen);
		plugin = g_plugins.findPlugin(temp);
	}
	else
		plugin = g_plugins.findPluginFast(amx);

	if (plugin && plugin->isValid())
	{
		if (flags & 8)
			plugin->setStatus(ps_stopped);
		/*else if (flags & 16)
			plugin->setStatus(ps_locked);*/
		else
			plugin->pausePlugin();

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL unpause(AMX *amx, cell *params) /* 3 param */
{

	int ilen;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);
	int flags = UTIL_ReadFlags(sptemp);
	CPluginMngr::CPlugin *plugin = 0;

	if (flags & 2)
	{
		LogError(amx, AMX_ERR_NATIVE, "This usage of the native pause() has been deprecated!");
		return 1;
	}
	else if (flags & 4)
	{
		sptemp = get_amxstring(amx, params[2], 0, ilen);
		plugin = g_plugins.findPlugin(sptemp);
	}
	else
		plugin = g_plugins.findPluginFast(amx);

	if (plugin && plugin->isValid() && plugin->isPaused() && !plugin->isStopped())
	{
		plugin->unpausePlugin();
		return 1;
	}

	return 0;

}

static cell AMX_NATIVE_CALL read_flags(AMX *amx, cell *params) /* 1 param */
{
	int ilen;
	char* sptemp = get_amxstring(amx, params[1], 0, ilen);

	return UTIL_ReadFlags(sptemp);
}

static cell AMX_NATIVE_CALL get_flags(AMX *amx, cell *params) /* 1 param */
{
	char flags[32];
	UTIL_GetFlags(flags, params[1]);

	return set_amxstring(amx, params[2], flags, params[3]);
}

static cell AMX_NATIVE_CALL get_user_flags(AMX *amx, cell *params) /* 2 param */
{
	int index = params[1];

	if (index < 0 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	int id = params[2];

	if (id < 0)
		id = 0;

	if (id > 31)
		id = 31;

	return GET_PLAYER_POINTER_I(index)->flags[id];
}

static cell AMX_NATIVE_CALL set_user_flags(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 0 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	int flag = params[2];
	int id = params[3];

	if (id < 0)
		id = 0;

	if (id > 31)
		id = 31;

	pPlayer->flags[id] |= flag;

	return 1;
}

static cell AMX_NATIVE_CALL remove_user_flags(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 0 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	int flag = params[2];
	int id = params[3];

	if (id < 0)
		id = 0;

	if (id > 31)
		id = 31;

	pPlayer->flags[id] &= ~flag;

	return 1;
}

static cell AMX_NATIVE_CALL register_menuid(AMX *amx, cell *params) /* 1 param */
{
	int i;
	char* temp = get_amxstring(amx, params[1], 0, i);
	AMX* a = (*params / sizeof(cell) < 2 || params[2]) ? 0 : amx;

	return g_menucmds.registerMenuId(temp, a);
}

static cell AMX_NATIVE_CALL get_user_menu(AMX *amx, cell *params) /* 3 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	cell *cpMenu = get_amxaddr(amx, params[2]);
	cell *cpKeys = get_amxaddr(amx, params[3]);
	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

	if (pPlayer->ingame)
	{
		if (gpGlobals->time > pPlayer->menuexpire)
		{
			if (Menu *pMenu = get_menu_by_id(pPlayer->newmenu))
				pMenu->Close(pPlayer->index);
			else
				pPlayer->menu = 0;

			*cpMenu = 0;
			*cpKeys = 0;

			return 0;
		}

		*cpMenu = pPlayer->menu;
		*cpKeys = pPlayer->keys;

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL precache_sound(AMX *amx, cell *params) /* 1 param */
{
	if (g_dontprecache)
	{
		LogError(amx, AMX_ERR_NATIVE, "Precaching not allowed");
		return 0;
	}

	int len;
	char* sptemp = get_amxstring(amx, params[1], 0, len);

	return PRECACHE_SOUND((char*)STRING(ALLOC_STRING(sptemp)));
}

static cell AMX_NATIVE_CALL precache_model(AMX *amx, cell *params) /* 1 param */
{
	if (g_dontprecache)
	{
		LogError(amx, AMX_ERR_NATIVE, "Precaching not allowed");
		return 0;
	}

	int len;
	char* sptemp = get_amxstring(amx, params[1], 0, len);

	return PRECACHE_MODEL((char*)STRING(ALLOC_STRING(sptemp)));
}

static cell AMX_NATIVE_CALL precache_generic(AMX *amx, cell *params)
{
	if (g_dontprecache)
	{
		LogError(amx, AMX_ERR_NATIVE, "Precaching not allowed");
		return 0;
	}

	int len;
	char* sptemp = get_amxstring(amx, params[1], 0, len);

	return PRECACHE_GENERIC((char*)STRING(ALLOC_STRING(sptemp)));
}

static cell AMX_NATIVE_CALL precache_event(AMX *amx, cell *params)
{
	int len;
	char *sptemp = format_amxstring(amx, params, 2, len);

	return PRECACHE_EVENT(params[1], (char*)STRING(ALLOC_STRING(sptemp)));
}

static cell AMX_NATIVE_CALL random_float(AMX *amx, cell *params) /* 2 param */
{
	float one = amx_ctof(params[1]);
	float two = amx_ctof(params[2]);
	REAL fRnd = RANDOM_FLOAT(one, two);

	return amx_ftoc(fRnd);
}

static cell AMX_NATIVE_CALL random_num(AMX *amx, cell *params) /* 2 param */
{
	return RANDOM_LONG(params[1], params[2]);
}

static cell AMX_NATIVE_CALL remove_quotes(AMX *amx, cell *params) /* 1 param */
{
	cell *text = get_amxaddr(amx, params[1]);

	if (*text == '\"')
	{
		register cell *temp = text;
		int len = 0;

		while (*temp++)
			++len; // get length

		cell *src = text;

		if (src[len-1] == '\r')
			src[--len] = 0;

		if (src[--len] == '\"')
		{
			src[len] = 0;
			temp = src + 1;
			while ((*src++ = *temp++));

			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_user_aiming(AMX *amx, cell *params) /* 4 param */
{
	int index = params[1];

	if (index < 1 || index > gpGlobals->maxClients)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
		return 0;
	}

	CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);
	cell *cpId = get_amxaddr(amx, params[2]);
	cell *cpBody = get_amxaddr(amx, params[3]);

	REAL pfloat = 0.0f;

	if (pPlayer->ingame)
	{
		edict_t* edict = pPlayer->pEdict;

		Vector v_forward;
		Vector v_src = edict->v.origin + edict->v.view_ofs;

		ANGLEVECTORS(edict->v.v_angle, v_forward, NULL, NULL);
		TraceResult trEnd;
		Vector v_dest = v_src + v_forward * static_cast<float>(params[4]);
		TRACE_LINE(v_src, v_dest, 0, edict, &trEnd);

		*cpId = FNullEnt(trEnd.pHit) ? 0 : ENTINDEX(trEnd.pHit);
		*cpBody = trEnd.iHitgroup;

		if (trEnd.flFraction < 1.0)
		{
			pfloat = (trEnd.vecEndPos - v_src).Length();
		}
	} else {
		*cpId = 0;
		*cpBody = 0;
	}

	return amx_ftoc(pfloat);
}

static cell AMX_NATIVE_CALL force_unmodified(AMX *amx, cell *params)
{
	int a;

	cell *cpVec1 = get_amxaddr(amx, params[2]);
	cell *cpVec2 = get_amxaddr(amx, params[3]);

	Vector vec1 = Vector((float)cpVec1[0], (float)cpVec1[1], (float)cpVec1[2]);
	Vector vec2 = Vector((float)cpVec2[0], (float)cpVec2[1], (float)cpVec2[2]);

	char* filename = get_amxstring(amx, params[4], 0, a);

	auto object = ke::AutoPtr<ForceObject>(new ForceObject(filename, (FORCE_TYPE)((int)(params[1])), vec1, vec2, amx));

	if (object)
	{
		auto forceObjVec = &g_forcegeneric;

		if (stristr(filename, ".wav"))
			forceObjVec = &g_forcesounds;
		else if (stristr(filename, ".mdl"))
			forceObjVec = &g_forcemodels;

		forceObjVec->append(ke::Move(object));

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL read_logdata(AMX *amx, cell *params)
{
	const char *value = g_logevents.getLogString();
	return set_amxstring_utf8(amx, params[1], value, strlen(value), params[2]);
}

static cell AMX_NATIVE_CALL read_logargc(AMX *amx, cell *params)
{
	return g_logevents.getLogArgNum();
}

static cell AMX_NATIVE_CALL read_logargv(AMX *amx, cell *params)
{
	const char *value = g_logevents.getLogArg(params[1]);
	return set_amxstring_utf8(amx, params[2], value, strlen(value), params[3]);
}

static cell AMX_NATIVE_CALL parse_loguser(AMX *amx, cell *params)
{
	int len;
	char *text = get_amxstring(amx, params[1], 0, len);

	if (len < 6)	// no user to parse!?
	{
		LogError(amx, AMX_ERR_NATIVE, "No user name specified");
		return 0;
	}

	/******** GET TEAM **********/
	char* end = text + --len;
	*end = 0;

	while (*end != '<' && len--)
		--end;

	++end;
	cell *cPtr = get_amxaddr(amx, params[7]);
	int max = params[8]; // get TEAM
	// print_srvconsole("Got team: %s (Len %d)\n", end, len);

	while (max-- && *end)
		*cPtr++ = *end++;

	*cPtr = 0;

	/******** GET AUTHID **********/
	if (len <= 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "No Authid found");
		return 0;
	}

	end = text + --len;
	*end = 0;

	while (*end != '<' && len--)
		--end;

	++end;
	cPtr = get_amxaddr(amx, params[5]);
	max = params[6]; // get AUTHID
	// print_srvconsole("Got auth: %s (Len %d)\n", end, len);

	while (max-- && *end)
		*cPtr++ = *end++;

	*cPtr = 0;

	/******** GET USERID **********/
	if (len <= 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "No Userid found");
		return 0;
	}

	end = text + --len;
	*end = 0;

	while (*end != '<' && len--)
		--end;

	// print_srvconsole("Got userid: %s (Len %d)\n", end + 1, len);
	if (*(cPtr = get_amxaddr(amx, params[4])) != -2)
		*cPtr = atoi(end + 1);

	/******** GET NAME **********/
	*end = 0;
	cPtr = get_amxaddr(amx, params[2]);
	max = params[3]; // get NAME
	// print_srvconsole("Got name: %s (Len %d)\n", text, len);

	while (max-- && *text)
		*cPtr++ = *text++;

	*cPtr = 0;

	return 1;
}

// native register_logevent(const function[], argsnum, ...);
static cell AMX_NATIVE_CALL register_logevent(AMX *amx, cell *params)
{
	int length;
	auto callback = get_amxstring(amx, params[1], 0, length);

	auto forwardId = registerSPForwardByName(amx, callback, FP_DONE);

	if (forwardId == -1)
	{
		LogError(amx, AMX_ERR_NOTFOUND, "Function \"%s\" was not found", callback);
		return 0;
	}

	auto handle = g_logevents.registerLogEvent(g_plugins.findPluginFast(amx), forwardId, params[2]);

	if (!handle)
	{
		return 0;
	}

	auto logevent = LogEventHandles.lookup(handle)->m_logevent;
	auto numparam = *params / sizeof(cell);

	for (auto i = 3U; i <= numparam; ++i)
	{
		logevent->registerFilter(get_amxstring(amx, params[i], 0, length));
	}

	return handle;
}

// native enable_logevent(handle);
static cell AMX_NATIVE_CALL enable_logevent(AMX *amx, cell *params)
{
	auto handle = LogEventHandles.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid log event handle %d", params[1]);
		return 0;
	}

	handle->m_logevent->setForwardState(FSTATE_ACTIVE);

	return 1;
}

// native disable_logevent(handle);
static cell AMX_NATIVE_CALL disable_logevent(AMX *amx, cell *params)
{
	auto handle = LogEventHandles.lookup(params[1]);

	if (!handle)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid log event handle: %d", params[1]);
		return 0;
	}

	handle->m_logevent->setForwardState(FSTATE_STOP);

	return 1;
}

// native is_module_loaded(const name[]);
static cell AMX_NATIVE_CALL is_module_loaded(AMX *amx, cell *params)
{
	// param1: name
	int len;
	char *name = get_amxstring(amx, params[1], 0, len);
	int id = 0;

	for (auto module : g_modules)
	{
		if (!stricmp(module->getName(), name))
			return id;

		++id;
	}

	return -1;
}

// native is_plugin_loaded(const name[]);
// 1.8 changed to: is_plugin_loaded(const name[], bool:usefilename=false);
static cell AMX_NATIVE_CALL is_plugin_loaded(AMX *amx, cell *params)
{
	// param1: name
	int len;
	char *name = get_amxstring(amx, params[1], 0, len);
	int id = 0;

	if (params[0] / sizeof(cell) == 1 || // compiled pre-1.8 - assume plugin's registered name
		params[2] == 0) // compiled post 1.8 - wants plugin's registered name
	{
		// searching for registered plugin name
		for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
		{
			if (stricmp((*iter).getTitle(), name) == 0)
				return id;

			++id;
		}
	}
	else
	{
		// searching for filename
		// filename search is case sensitive
		for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
		{
			if (strcmp((*iter).getName(), name) == 0)
				return id;

			++id;
		}
	}

	return -1;
}

// native get_modulesnum();
static cell AMX_NATIVE_CALL get_modulesnum(AMX *amx, cell *params)
{
	return (cell)countModules(CountModules_All);
}

// native get_module(id, name[], nameLen, author[], authorLen, version[], versionLen, &status);
static cell AMX_NATIVE_CALL get_module(AMX *amx, cell *params)
{
	// find the module
	int i = params[1];

	for (auto module : g_modules)
	{
		if (i--)
		{
			continue;
		}

		// set name, author, version
		const amxx_module_info_s *info = module->getInfoNew();
		const char *name = info && info->name ? info->name : "unk";
		const char *author = info && info->author ? info->author : "unk";
		const char *version = info && info->version ? info->version : "unk";

		set_amxstring_utf8(amx, params[2], name, strlen(name), params[3]);
		set_amxstring_utf8(amx, params[4], author, strlen(author), params[5]);
		set_amxstring_utf8(amx, params[6], version, strlen(version), params[7]);

		// compatibility problem possible
		int numParams = params[0] / sizeof(cell);

		if (numParams < 8)
		{
			LogError(amx, AMX_ERR_NATIVE, "Call to incompatible version");
			return 0;
		}

		// set status
		cell *addr;
		if (amx_GetAddr(amx, params[8], &addr) != AMX_ERR_NONE)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid reference plugin");
			return 0;
		}

		*addr = (cell)module->getStatusValue();
		return params[1];
	}
	return -1;
}

// native log_amx(const msg[], ...);
static cell AMX_NATIVE_CALL log_amx(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *plugin = g_plugins.findPluginFast(amx);
	int len;

	g_langMngr.SetDefLang(LANG_SERVER);
	AMXXLOG_Log("[%s] %s", plugin->getName(), format_amxstring(amx, params, 1, len));

	return 0;
}

/*********************************************************************/

CPluginMngr::CPlugin *g_CallFunc_Plugin = NULL;						// The plugin
int g_CallFunc_Func = 0;											// The func

struct CallFunc_ParamInfo
{
	unsigned char flags;											// flags
	cell byrefAddr;													// byref address in caller plugin
	cell size;														// byref size
	cell *alloc;													// allocated block
	bool copyback;													// copy back?
};

#if !defined CALLFUNC_MAXPARAMS
#define CALLFUNC_MAXPARAMS 64										/* Maximal params number */
#endif

cell g_CallFunc_Params[CALLFUNC_MAXPARAMS] = {0};					// Params
CallFunc_ParamInfo g_CallFunc_ParamInfo[CALLFUNC_MAXPARAMS] = {{0}};	// Flags
int g_CallFunc_CurParam = 0;										// Current param id

#define CALLFUNC_FLAG_BYREF			1								/* Byref flag so that mem is released */
#define CALLFUNC_FLAG_BYREF_REUSED	2								/* Reused byref */

// native callfunc_begin(const func[], const plugin[]="");
static cell AMX_NATIVE_CALL callfunc_begin(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);

	if (g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_begin called without callfunc_end");
		return 0;
	}

	int len;
	char *pluginStr = get_amxstring(amx, params[2], 0, len);
	char *funcStr = get_amxstring(amx, params[1], 1, len);
	CPluginMngr::CPlugin *plugin = NULL;

	if (!pluginStr || !*pluginStr)
		plugin = curPlugin;
	else
		plugin = g_plugins.findPlugin(pluginStr);

	if (!plugin)
	{
		return -1;		// plugin not found: -1
	}

	int func;

	if (amx_FindPublic(plugin->getAMX(), funcStr, &func) != AMX_ERR_NONE)
	{
		return -2;		// func not found: -2
	}

	// set globals
	g_CallFunc_Plugin = plugin;
	g_CallFunc_Func = func;
	g_CallFunc_CurParam = 0;

	return 1;			// success: 1
}

// native callfunc_begin_i(funcId, pluginId = -1)
static cell AMX_NATIVE_CALL callfunc_begin_i(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *plugin;

	if (params[2] < 0)
		plugin = g_plugins.findPluginFast(amx);
	else
		plugin = g_plugins.findPlugin(params[2]);

	if (!plugin)
		return -1;

	if (g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_begin called without callfunc_end");
		return 0;
	}

	if (params[1] < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Public function %d is invalid", params[1]);
		return -1;
	}

	if (!plugin->isExecutable(params[1]))
		return -2;

	g_CallFunc_Plugin = plugin;
	g_CallFunc_Func = params[1];
	g_CallFunc_CurParam = 0;

	return 1;
}

// native get_func_id(funcName[], pluginId = -1)
static cell AMX_NATIVE_CALL get_func_id(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *plugin;

	if (params[2] < 0)
	{
		plugin = g_plugins.findPluginFast(amx);
	} else {
		plugin = g_plugins.findPlugin(params[2]);
	}

	if (!plugin)
	{
		return -1;
	}

	if (!plugin->isValid())
	{
		return -1;
	}

	int len;
	const char *funcName = get_amxstring(amx, params[1], 0, len);
	int index, err;

	if ((err = amx_FindPublic(plugin->getAMX(), funcName, &index)) != AMX_ERR_NONE)
	{
		index = -1;
	}

	return index;
}

// native callfunc_end();
static cell AMX_NATIVE_CALL callfunc_end(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);

	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_end called without callfunc_begin");
		return 0;
	}

	// call the func
	cell retVal;
	int err;

	// copy the globs so the called func can also use callfunc
	cell gparams[CALLFUNC_MAXPARAMS];
	CallFunc_ParamInfo gparamInfo[CALLFUNC_MAXPARAMS];

	CPluginMngr::CPlugin *plugin = g_CallFunc_Plugin;
	int func = g_CallFunc_Func;
	int curParam = g_CallFunc_CurParam;

	memcpy(gparams, g_CallFunc_Params, sizeof(cell) * curParam);
	memcpy(gparamInfo, g_CallFunc_ParamInfo, sizeof(CallFunc_ParamInfo) * curParam);

	// cleanup
	g_CallFunc_Plugin = NULL;
	g_CallFunc_CurParam = 0;

	AMX *pAmx = plugin->getAMX();

	Debugger *pDebugger = (Debugger *)pAmx->userdata[UD_DEBUGGER];

	if (pDebugger)
	{
		pDebugger->BeginExec();
	}

	// first pass over byref things
	for (int i = curParam - 1; i >= 0; i--)
	{
		if (gparamInfo[i].flags & CALLFUNC_FLAG_BYREF)
		{
			cell amx_addr, *phys_addr;
			amx_Allot(pAmx, gparamInfo[i].size, &amx_addr, &phys_addr);
			memcpy(phys_addr, gparamInfo[i].alloc, gparamInfo[i].size * sizeof(cell));
			gparams[i] = amx_addr;
			delete [] gparamInfo[i].alloc;
			gparamInfo[i].alloc = NULL;
		}
	}

	// second pass, link in reused byrefs
	for (int i = curParam - 1; i >= 0; i--)
	{
		if (gparamInfo[i].flags & CALLFUNC_FLAG_BYREF_REUSED)
		{
			gparams[i] = gparams[gparams[i]];
		}
	}

	// actual call
	// Pawn - push parameters in reverse order
	for (int i = curParam - 1; i >= 0; i--)
	{
		amx_Push(pAmx, gparams[i]);
	}

	err = amx_ExecPerf(pAmx, &retVal, func);

	if (err != AMX_ERR_NONE)
	{
		if (pDebugger && pDebugger->ErrorExists())
		{
			//already handled
		} else {
			LogError(amx, err, NULL);
		}
	}

	if (pDebugger)
	{
		pDebugger->EndExec();
	}

	// process byref params (not byref_reused)
	for (int i = 0; i < curParam; ++i)
	{
		if (gparamInfo[i].flags & CALLFUNC_FLAG_BYREF)
		{
			// copy back so that references work
			AMX *amxCalled = plugin->getAMX();

			if (gparamInfo[i].copyback)
			{
				AMX *amxCaller = curPlugin->getAMX();
				AMX_HEADER *hdrCaller = (AMX_HEADER *)amxCaller->base;
				AMX_HEADER *hdrCalled = (AMX_HEADER *)amxCalled->base;
					memcpy(	/** DEST ADDR **/
					(amxCaller->data ? amxCaller->data : (amxCaller->base + hdrCaller->dat)) + gparamInfo[i].byrefAddr,
					/** SOURCE ADDR **/
					(amxCalled->data ? amxCalled->data : (amxCalled->base + hdrCalled->dat)) + gparams[i],
					/** SIZE **/
					gparamInfo[i].size * sizeof(cell));
			}

			// free memory used for params passed by reference
			amx_Release(amxCalled, gparams[i]);
		}
	}

	return retVal;
}

// native callfunc_push_int(value);
// native callfunc_push_float(Float: value);
static cell AMX_NATIVE_CALL callfunc_push_byval(AMX *amx, cell *params)
{
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx called without callfunc_begin");
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		LogError(amx, AMX_ERR_NATIVE, "Callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		return 0;
	}

	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = 0;
	g_CallFunc_Params[g_CallFunc_CurParam++] = params[1];

	return 0;
}

// native callfunc_push_intref(&value);
// native callfunc_push_floatref(Float: &value);
static cell AMX_NATIVE_CALL callfunc_push_byref(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *curPlugin = g_plugins.findPluginFast(amx);

	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx called without callfunc_begin");
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		return 0;
	}

	// search for the address; if it is found, dont create a new copy
	for (int i = 0; i < g_CallFunc_CurParam; ++i)
	{
		if ((g_CallFunc_ParamInfo[i].flags & CALLFUNC_FLAG_BYREF) && (g_CallFunc_ParamInfo[i].byrefAddr == params[1]))
		{
			// the byrefAddr and size params should not be used; set them anyways...
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF_REUSED;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].alloc = NULL;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = true;
			g_CallFunc_Params[g_CallFunc_CurParam++] = i;		/* referenced parameter */
			return 0;
		}
	}

	cell *phys_addr = new cell[1];

	// copy the value to the allocated memory
	cell *phys_addr2;
	amx_GetAddr(curPlugin->getAMX(), params[1], &phys_addr2);
	*phys_addr = *phys_addr2;

	// push the address and set the reference flag so that memory is released after function call.
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].alloc = phys_addr;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = true;
	g_CallFunc_Params[g_CallFunc_CurParam++] = 0;

	return 0;
}

// native callfunc_push_array(array[], size, [copyback])
static cell AMX_NATIVE_CALL callfunc_push_array(AMX *amx, cell *params)
{
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx called without callfunc_begin");
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		return 0;
	}

	// search for the address; if it is found, dont create a new copy
	for (int i = 0; i < g_CallFunc_CurParam; ++i)
	{
		if ((g_CallFunc_ParamInfo[i].flags & CALLFUNC_FLAG_BYREF) && (g_CallFunc_ParamInfo[i].byrefAddr == params[1]))
		{
			// the byrefAddr and size params should not be used; set them anyways...
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF_REUSED;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].alloc = NULL;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = g_CallFunc_ParamInfo[i].copyback;
			g_CallFunc_Params[g_CallFunc_CurParam++] = i;		/* referenced parameter */
			return 0;
		}
	}

	// not found; create an own copy
	// get the string and its length
	cell *pArray = get_amxaddr(amx, params[1]);
	cell array_size = params[2];

	// allocate enough memory for the array
	cell *phys_addr = new cell[array_size];

	memcpy(phys_addr, pArray, array_size * sizeof(cell));

	// push the address and set the reference flag so that memory is released after function call.
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = array_size;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].alloc = phys_addr;

	if (params[0] / sizeof(cell) >= 3)
	{
		g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = params[3] ? true : false;
	} else {
		g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = true;
	}

	g_CallFunc_Params[g_CallFunc_CurParam++] = 0;

	return 0;
}

// native callfunc_push_str(value[]);
static cell AMX_NATIVE_CALL callfunc_push_str(AMX *amx, cell *params)
{
	if (!g_CallFunc_Plugin)
	{
		// scripter's fault
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx called without callfunc_begin");
		return 0;
	}

	if (g_CallFunc_CurParam == CALLFUNC_MAXPARAMS)
	{
		LogError(amx, AMX_ERR_NATIVE, "callfunc_push_xxx: maximal parameters num: %d", CALLFUNC_MAXPARAMS);
		return 0;
	}

	// search for the address; if it is found, dont create a new copy
	for (int i = 0; i < g_CallFunc_CurParam; ++i)
	{
		if ((g_CallFunc_ParamInfo[i].flags & CALLFUNC_FLAG_BYREF) && (g_CallFunc_ParamInfo[i].byrefAddr == params[1]))
		{
			// the byrefAddr and size params should not be used; set them anyways...
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF_REUSED;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = 1;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].alloc = NULL;
			g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = g_CallFunc_ParamInfo[i].copyback;
			g_CallFunc_Params[g_CallFunc_CurParam++] = i;
			// we are done
			return 0;
		}
	}

	// not found; create an own copy
	// get the string and its length
	int len;
	char *str = get_amxstring(amx, params[1], 0, len);

	// allocate enough memory for the string
	cell *phys_addr = new cell[len+1];

	// copy it to the allocated memory
	// we assume it's unpacked
	// :NOTE: 4th parameter use_wchar since Small Abstract Machine 2.5.0
	amx_SetStringOld(phys_addr, str, 0, 0);

	// push the address and set the reference flag so that memory is released after function call.
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].flags = CALLFUNC_FLAG_BYREF;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].byrefAddr = params[1];
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].size = len + 1;
	g_CallFunc_ParamInfo[g_CallFunc_CurParam].alloc = phys_addr;

	if (params[0] / sizeof(cell) >= 3)
	{
		g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = params[3] ? true : false;
	} else {
		g_CallFunc_ParamInfo[g_CallFunc_CurParam].copyback = true;
	}

	g_CallFunc_Params[g_CallFunc_CurParam++] = 0;

	return 0;
}

// get_langsnum();
static cell AMX_NATIVE_CALL get_langsnum(AMX *amx, cell *params)
{
	return g_langMngr.GetLangsNum();
}

// get_lang(id, name[(at least 3)]);
static cell AMX_NATIVE_CALL get_lang(AMX *amx, cell *params)
{
	set_amxstring(amx, params[2], g_langMngr.GetLangName(params[1]), 2);
	return 0;
}

// register_dictionary(const filename[]);
static cell AMX_NATIVE_CALL register_dictionary(AMX *amx, cell *params)
{
	int len;
	static char file[PLATFORM_MAX_PATH];
	int result = g_langMngr.MergeDefinitionFile(build_pathname_r(file, sizeof(file), "%s/lang/%s", get_localinfo("amxx_datadir", "addons/amxmodx/data"), get_amxstring(amx, params[1], 1, len)));

	return result;
}

static cell AMX_NATIVE_CALL plugin_flags(AMX *amx, cell *params)
{
	if ((params[0] / sizeof(cell)) == 1 || // compiled with old include file
		 params[2] < 0) // specifically want calling plugin's flags
	{
		if (params[1])
		{
			AMX_HEADER *hdr;
			hdr = (AMX_HEADER *)amx->base;
			return hdr->flags;
		}

		return amx->flags;
	}
	else
	{
		CPluginMngr::CPlugin* a = g_plugins.findPlugin((int)params[2]);

		if (a == NULL)
		{
			return 0;
		}
		if (params[1])
		{
			AMX_HEADER *hdr;
			hdr = (AMX_HEADER *)a->getAMX()->base;
			return hdr->flags;
		}

		return a->getAMX()->flags;
	}
}

// lang_exists(const name[]);
static cell AMX_NATIVE_CALL lang_exists(AMX *amx, cell *params)
{
	int len = 0;
	return g_langMngr.LangExists(get_amxstring(amx, params[1], 1, len)) ? 1 : 0;
}

static cell AMX_NATIVE_CALL require_module(AMX *amx, cell *params)
{
	return 1;
}

static cell AMX_NATIVE_CALL find_plugin_byfile(AMX *amx, cell *params)
{
	typedef int (*STRCOMPARE)(const char*, const char*);

	STRCOMPARE func;

	if (params[2])
	{
		func = strcasecmp;
	} else {
		func = strcmp;
	}

	int len, i = 0;
	char *file = get_amxstring(amx, params[1], 0, len);

	for (CPluginMngr::iterator iter = g_plugins.begin(); iter; ++iter)
	{
		if ((func)((*iter).getName(), file) == 0)
			return i;
		i++;
	}

	return -1;
}

static cell AMX_NATIVE_CALL int3(AMX *amx, cell *params)
{
#if defined _DEBUG || defined DEBUG
#if defined WIN32
	__asm
	{
		int 3;
	};
#else
	asm("int $3");
#endif //WIN32
#endif //DEBUG

	return 0;
}

/*********************************************************************/

static cell AMX_NATIVE_CALL amx_abort(AMX *amx, cell *params)
{
	int err = params[1];

	int len;
	char *fmt = format_amxstring(amx, params, 2, len);

	if (fmt[0] == '\0')
		fmt = NULL;

	const char *filename = "";
	CPluginMngr::CPlugin *pPlugin = g_plugins.findPluginFast(amx);

	if (pPlugin)
		filename = pPlugin->getName();

	//we were in a callfunc?
	if (g_CallFunc_Plugin == pPlugin)
		g_CallFunc_Plugin = NULL;

	if (fmt)
		LogError(amx, err, "[%s] %s", filename, fmt);
	else
		LogError(amx, err, NULL);

	return 1;
}

static cell AMX_NATIVE_CALL module_exists(AMX *amx, cell *params)
{
	int len;
	char *module = get_amxstring(amx, params[1], 0, len);

	if (!FindLibrary(module, LibType_Library))
		return FindLibrary(module, LibType_Class);

	return true;
}

static cell AMX_NATIVE_CALL LibraryExists(AMX *amx, cell *params)
{
	int len;
	char *library = get_amxstring(amx, params[1], 0, len);

	return FindLibrary(library, static_cast<LibType>(params[2]));
}

static cell AMX_NATIVE_CALL set_fail_state(AMX *amx, cell *params)
{
	int len;
	char* str;

	g_langMngr.SetDefLang(LANG_SERVER);	// Default language = server

	if (params[0] / sizeof(cell) > 1)
		str = format_amxstring(amx, params, 1, len);
	else
		str = get_amxstring(amx, params[1], 0, len);

	CPluginMngr::CPlugin *pPlugin = g_plugins.findPluginFast(amx);

	pPlugin->setStatus(ps_error);
	pPlugin->setError(str);

	AMXXLOG_Error("[AMXX] Plugin (\"%s\") is setting itself as failed.", pPlugin->getName());
	AMXXLOG_Error("[AMXX] Plugin says: %s", str);

	LogError(amx, AMX_ERR_EXIT, NULL);

	//plugin dies once amx_Exec concludes
	return 0;
}

static cell AMX_NATIVE_CALL get_var_addr(AMX *amx, cell *params)
{
	if (params[0] / sizeof(cell) > 0)
	{
		return params[1];
	}

	return 0;
}

static cell AMX_NATIVE_CALL get_addr_val(AMX *amx, cell *params)
{
	cell *addr;
	int err;

	if ( (err=amx_GetAddr(amx, params[1], &addr)) != AMX_ERR_NONE )
	{
		LogError(amx, err, "Bad reference %d supplied", params[1]);
		return 0;
	}

	return addr ? *addr : 0;
}

static cell AMX_NATIVE_CALL set_addr_val(AMX *amx, cell *params)
{
	cell *addr;
	int err;

	if ( (err=amx_GetAddr(amx, params[1], &addr)) != AMX_ERR_NONE )
	{
		LogError(amx, err, "Bad reference %d supplied", params[1]);
		return 0;
	}

	if (addr)
		*addr = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL CreateMultiForward(AMX *amx, cell *params)
{
	int len;
	char *funcname = get_amxstring(amx, params[1], 0, len);

	cell ps[FORWARD_MAX_PARAMS];
	cell count = params[0] / sizeof(cell);
	for (cell i=3; i<=count; i++)
	{
		ps[i-3] = *get_amxaddr(amx, params[i]);
	}

	return registerForwardC(funcname, static_cast<ForwardExecType>(params[2]), ps, count-2);
}

static cell AMX_NATIVE_CALL CreateOneForward(AMX *amx, cell *params)
{
	CPluginMngr::CPlugin *p = g_plugins.findPlugin(params[1]);

	if (!p)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid plugin id: %d", params[1]);
		return -1;
	} else if (!p->isExecutable(0)) {
		return -1;
	}

	int len;
	char *funcname = get_amxstring(amx, params[2], 0, len);

	cell ps[FORWARD_MAX_PARAMS];
	cell count = params[0] / sizeof(cell);
	for (cell i=3; i<=count; i++)
	{
		ps[i-3] = *get_amxaddr(amx, params[i]);
	}

	return registerSPForwardByNameC(p->getAMX(), funcname, ps, count-2);
}

static cell AMX_NATIVE_CALL PrepareArray(AMX *amx, cell *params)
{
	cell *addr = get_amxaddr(amx, params[1]);
	unsigned int len = static_cast<unsigned int>(params[2]);
	bool copyback = params[3] ? true : false;

	return prepareCellArray(addr, len, copyback);
}

static cell AMX_NATIVE_CALL ExecuteForward(AMX *amx, cell *params)
{
	int id = static_cast<int>(params[1]);
	int len, err;
	cell *addr = get_amxaddr(amx, params[2]);

	if (!g_forwards.isIdValid(id))
		return 0;

	struct allot_info
	{
		cell amx_addr;
		cell *phys_addr;
	};

	cell ps[FORWARD_MAX_PARAMS];
	allot_info allots[FORWARD_MAX_PARAMS];
	cell count = params[0] / sizeof(cell);
	if (count - 2 != g_forwards.getParamsNum(id))
	{
		LogError(amx, AMX_ERR_NATIVE, "Expected %d parameters, got %d", g_forwards.getParamsNum(id), count-2);
		return 0;
	}

	ForwardParam param_type;

	for (cell i=3; i<=count; i++)
	{
		param_type = g_forwards.getParamType(id, i-3);
		if (param_type == FP_STRING)
		{
			char *tmp = get_amxstring(amx, params[i], 0, len);
			cell num = len / sizeof(cell) + 1;
			if ((err=amx_Allot(amx, num, &allots[i-3].amx_addr, &allots[i-3].phys_addr)) != AMX_ERR_NONE)
			{
				LogError(amx, err, NULL);
				return 0;
			}
			strcpy((char *)allots[i-3].phys_addr, tmp);
			ps[i-3] = (cell)allots[i-3].phys_addr;
		}
		else if (param_type == FP_CELL_BYREF)
		{
			cell *temp = get_amxaddr(amx, params[i]);
			ps[i-3] = reinterpret_cast<cell>(temp);
		}
		else
		{
			ps[i-3] = *get_amxaddr(amx, params[i]);
		}
	}

	*addr = g_forwards.executeForwards(id, ps);

	for (cell i=3; i<=count; i++)
	{
		if (g_forwards.getParamType(id, i-3) == FP_STRING)
		{
			amx_Release(amx, allots[i-3].amx_addr);
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL DestroyForward(AMX *amx, cell *params)
{
	int id = static_cast<int>(params[1]);

	/* only implemented for single forwards */
	if (g_forwards.isIdValid(id) && g_forwards.isSPForward(id))
		g_forwards.unregisterSPForward(id);

	return 1;
}

ke::Vector<cell *> g_hudsync;

static cell AMX_NATIVE_CALL CreateHudSyncObj(AMX *amx, cell *params)
{
	cell *p = new cell[gpGlobals->maxClients+1];
	memset(p, 0, sizeof(cell) * (gpGlobals->maxClients + 1));
	g_hudsync.append(p);

	return static_cast<cell>(g_hudsync.length());
}

void CheckAndClearPlayerHUD(CPlayer *player, int &channel, unsigned int sync_obj)
{
	/**
	 * player and channel should be guaranteed to be good to go.
	 */
	//get the sync object's hud list
	cell *plist = g_hudsync[sync_obj];
	//get the last channel this message class was displayed on.
	cell last_channel = plist[player->index];
	//check if the last sync on this channel was this sync obj
	if ((unsigned int)player->hudmap[last_channel] == sync_obj + 1)
	{
		//if so, we can safely REUSE it
		channel = (int)last_channel;
	}

	//set the new states
	plist[player->index] = channel;
	player->hudmap[channel] = sync_obj + 1;
}

static cell AMX_NATIVE_CALL ClearSyncHud(AMX *amx, cell *params)
{
	int len = 0;
	int index = params[1];
	unsigned int sync_obj = static_cast<unsigned int>(params[2]) - 1;

	if (sync_obj >= g_hudsync.length())
	{
		LogError(amx, AMX_ERR_NATIVE, "HudSyncObject %d is invalid", sync_obj);
		return 0;
	}

	g_langMngr.SetDefLang(params[1]);

	if (index == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);

			int channel;
			if (pPlayer->ingame)
			{
				g_langMngr.SetDefLang(i);
				channel = pPlayer->NextHUDChannel();
				CheckAndClearPlayerHUD(pPlayer, channel, sync_obj);
				pPlayer->channels[channel] = gpGlobals->time;
				g_hudset.channel = channel;
				UTIL_HudMessage(pPlayer->pEdict, g_hudset, "");
			}
		}
	} else {
		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame)
		{
			int channel = pPlayer->NextHUDChannel();
			CheckAndClearPlayerHUD(pPlayer, channel, sync_obj);
			pPlayer->channels[channel] = gpGlobals->time;
			g_hudset.channel = channel;
			UTIL_HudMessage(pPlayer->pEdict, g_hudset, "");
		}
	}

	return len;
}

//params[1] - target
//params[2] - HudSyncObj
//params[3] - hud message
static cell AMX_NATIVE_CALL ShowSyncHudMsg(AMX *amx, cell *params)
{
	int len = 0;
	char* message = NULL;
	int index = params[1];
	unsigned int sync_obj = static_cast<unsigned int>(params[2]) - 1;

	if (sync_obj >= g_hudsync.length())
	{
		LogError(amx, AMX_ERR_NATIVE, "HudSyncObject %d is invalid", sync_obj);
		return 0;
	}

	g_langMngr.SetDefLang(params[1]);

	if (index == 0)
	{
		for (int i = 1; i <= gpGlobals->maxClients; ++i)
		{
			CPlayer *pPlayer = GET_PLAYER_POINTER_I(i);

			int channel;
			if (pPlayer->ingame)
			{
				g_langMngr.SetDefLang(i);
				channel = pPlayer->NextHUDChannel();
				CheckAndClearPlayerHUD(pPlayer, channel, sync_obj);
				pPlayer->channels[channel] = gpGlobals->time;
				g_hudset.channel = channel;
				message = UTIL_SplitHudMessage(format_amxstring(amx, params, 3, len));
				UTIL_HudMessage(pPlayer->pEdict, g_hudset, message);
			}
		}
	} else {
		if (index < 1 || index > gpGlobals->maxClients)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player id %d", index);
			return 0;
		}

		CPlayer* pPlayer = GET_PLAYER_POINTER_I(index);

		if (pPlayer->ingame)
		{
			int channel = pPlayer->NextHUDChannel();
			CheckAndClearPlayerHUD(pPlayer, channel, sync_obj);
			pPlayer->channels[channel] = gpGlobals->time;
			g_hudset.channel = channel;
			message = UTIL_SplitHudMessage(format_amxstring(amx, params, 3, len));
			UTIL_HudMessage(pPlayer->pEdict, g_hudset, message);
		}
	}

	return len;
}

static cell AMX_NATIVE_CALL arrayset(AMX *amx, cell *params)
{
	cell value = params[2];

	if (!value)
	{
		memset(get_amxaddr(amx, params[1]), 0, params[3] * sizeof(cell));
	} else {
		int size = params[3];
		cell *addr = get_amxaddr(amx, params[1]);
		for (int i=0; i<size; i++)
		{
			addr[i] = value;
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL CreateLangKey(AMX *amx, cell *params)
{
	int len;
	const char *key = get_amxstring(amx, params[1], 0, len);
	int key_index = g_langMngr.GetKeyEntry(key);

	if (key_index != -1)
	{
		return key_index;
	}

	return g_langMngr.AddKeyEntry(key);
}

static cell AMX_NATIVE_CALL AddTranslation(AMX *amx, cell *params)
{
	int len;
	const char *lang = get_amxstring(amx, params[1], 0, len);
	int key_index = params[2];
	const char *phrase = get_amxstring(amx, params[3], 1, len);

	ke::Vector<sKeyDef> queue;
	sKeyDef def;

	def.definition = new ke::AutoString(phrase);
	def.key = key_index;

	queue.append(def);

	g_langMngr.MergeDefinitions(lang, queue);

	return 1;
}

static cell AMX_NATIVE_CALL GetLangTransKey(AMX *amx, cell *params)
{
	int len;
	const char *key = get_amxstring(amx, params[1], 0, len);

	return g_langMngr.GetKeyEntry(key);
}

static cell AMX_NATIVE_CALL admins_push(AMX *amx, cell *params)
{
	// admins_push("SteamID","password",access,flags);
	CAdminData *TempData=new CAdminData;

	TempData->SetAuthID(get_amxaddr(amx,params[1]));
	TempData->SetPass(get_amxaddr(amx,params[2]));
	TempData->SetAccess(params[3]);
	TempData->SetFlags(params[4]);

	DynamicAdmins.append(TempData);

	return 0;
};
static cell AMX_NATIVE_CALL admins_flush(AMX *amx, cell *params)
{
	// admins_flush();

	size_t iter=DynamicAdmins.length();

	while (iter--)
	{
		delete DynamicAdmins[iter];
	}

	DynamicAdmins.clear();

	return 0;

};
static cell AMX_NATIVE_CALL admins_num(AMX *amx, cell *params)
{
	// admins_num();

	return static_cast<cell>(DynamicAdmins.length());
};
static cell AMX_NATIVE_CALL admins_lookup(AMX *amx, cell *params)
{
	// admins_lookup(Num, Property, Buffer[]={0}, BufferSize=-1);

	if (params[1]>=static_cast<int>(DynamicAdmins.length()))
	{
		LogError(amx,AMX_ERR_NATIVE,"Invalid admins num");
		return 1;
	};

	int BufferSize;
	cell *Buffer;
	const cell *Input;

	switch(params[2])
	{
	case Admin_Auth:
		BufferSize=params[4];
		Buffer=get_amxaddr(amx, params[3]);
		Input=DynamicAdmins[params[1]]->GetAuthID();

		while (BufferSize-->0)
		{
			if ((*Buffer++=*Input++)==0)
			{
				return 0;
			}
		}
		// hit max buffer size, terminate string
		*Buffer=0;
		return 0;
		break;
	case Admin_Password:
		BufferSize=params[4];
		Buffer=get_amxaddr(amx, params[3]);
		Input=DynamicAdmins[params[1]]->GetPass();

		while (BufferSize-->0)
		{
			if ((*Buffer++=*Input++)==0)
			{
				return 0;
			}
		}
		// hit max buffer size, terminate string
		*Buffer=0;
		return 0;
		break;
	case Admin_Access:
		return DynamicAdmins[params[1]]->GetAccess();
		break;
	case Admin_Flags:
		return DynamicAdmins[params[1]]->GetFlags();
		break;
	};

	// unknown property
	return 0;
};
// LookupLangKey(Output[], OutputSize, const Key[], const &id)
static cell AMX_NATIVE_CALL LookupLangKey(AMX *amx, cell *params)
{
	int len;
	char *key=get_amxstring(amx,params[3],0,len);
	const char *def=translate(amx, playerlang(*get_amxaddr(amx, params[4])),key);

	if (def==NULL)
	{
		return 0;
	}

	set_amxstring(amx,params[1],def,params[2]);
	return 1;
};

// SetGlobalTransTarget(client)
static cell AMX_NATIVE_CALL SetGlobalTransTarget(AMX *amx, cell *params)
{
	g_langMngr.SetDefLang(params[1]);

	return 1;
};

// has_map_ent_class(const classname[])
static cell AMX_NATIVE_CALL has_map_ent_class(AMX *amx, cell *params)
{
	int len;
	char *name = get_amxstring(amx, params[1], 0, len);

	return len && !FNullEnt(FIND_ENTITY_BY_STRING(NULL, "classname", name));
};

static cell AMX_NATIVE_CALL AutoExecConfig(AMX *amx, cell *params)
{
	int length;
	bool autocreate = params[1] != 0;
	const char *name   = get_amxstring(amx, params[2], 0, length);
	const char *folder = get_amxstring(amx, params[3], 1, length);

	auto plugin = g_plugins.findPluginFast(amx);

	if (*name == '\0')
	{
		char pluginName[PLATFORM_MAX_PATH];
		strncopy(pluginName, plugin->getName(), sizeof(pluginName));

		char *ptr;

		if ((ptr = strstr(pluginName, ".amxx")))
		{
			*ptr = '\0';
		}

		static char newName[PLATFORM_MAX_PATH];
		ke::SafeSprintf(newName, sizeof(newName), "plugin-%s", pluginName);

		name = newName;
	}

	plugin->AddConfig(autocreate, name, folder);

	return 1;
}

//native RequestFrame(const callback[], any:data);
static cell AMX_NATIVE_CALL RequestFrame(AMX *amx, cell *params)
{
	int len;
	const char *funcName = get_amxstring(amx, params[1], 0, len);

	int func = registerSPForwardByName(amx, funcName, FP_CELL, FP_DONE);
	if (func < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Function \"%s\" was not found", funcName);
		return 0;
	}

	g_frameActionMngr.AddFrameAction(func, params[2]);

	return 1;
}

AMX_NATIVE_INFO amxmodx_Natives[] =
{
	{"abort",					amx_abort},
	{"admins_flush",			admins_flush},
	{"admins_lookup",			admins_lookup},
	{"admins_num",				admins_num},
	{"admins_push",				admins_push},
	{"amxclient_cmd",			amxclient_cmd},
	{"arrayset",				arrayset},
	{"get_addr_val",			get_addr_val},
	{"get_var_addr",			get_var_addr},
	{"set_addr_val",			set_addr_val},
	{"callfunc_begin",			callfunc_begin},
	{"callfunc_begin_i",		callfunc_begin_i},
	{"callfunc_end",			callfunc_end},
	{"callfunc_push_int",		callfunc_push_byval},
	{"callfunc_push_float",		callfunc_push_byval},
	{"callfunc_push_intrf",		callfunc_push_byref},
	{"callfunc_push_floatrf",	callfunc_push_byref},
	{"callfunc_push_str",		callfunc_push_str},
	{"callfunc_push_array",		callfunc_push_array},
	{"change_task",				change_task},
	{"engine_changelevel",		engine_changelevel},
	{"client_cmd",				client_cmd},
	{"client_print",			client_print},
	{"client_print_color",		client_print_color},
	{"console_cmd",				console_cmd},
	{"console_print",			console_print},
	{"emit_sound",				emit_sound},
	{"engclient_cmd",			engclient_cmd},
	{"engclient_print",			engclient_print},
	{"find_player",				find_player},
	{"find_player_ex",			find_player_ex},
	{"find_plugin_byfile",		find_plugin_byfile},
	{"force_unmodified",		force_unmodified},
	{"format_time",				format_time},
	{"get_clcmd",				get_clcmd},
	{"get_clcmdsnum",			get_clcmdsnum},
	{"get_concmd",				get_concmd},
	{"get_concmdsnum",			get_concmdsnum},
	{"get_concmd_plid",			get_concmd_plid},
	{"get_flags",				get_flags},
	{"get_func_id",				get_func_id},
	{"get_gametime",			get_gametime},
	{"get_lang",				get_lang},
	{"get_langsnum",			get_langsnum},
	{"get_localinfo",			get_localinfo},
	{"get_mapname",				get_mapname},
	{"get_maxplayers",			get_maxplayers},
	{"get_modname",				get_modname},
	{"get_module",				get_module},
	{"get_modulesnum",			get_modulesnum},
	{"get_players",				get_players},
	{"get_playersnum",			get_playersnum},
	{"get_plugin",				get_plugin},
	{"get_pluginsnum",			get_pluginsnum},
	{"get_srvcmd",				get_srvcmd},
	{"get_srvcmdsnum",			get_srvcmdsnum},
	{"get_systime",				get_systime},
	{"get_time",				get_time},
	{"get_timeleft",			get_timeleft},
	{"get_amxx_verstring",		get_amxx_verstring},
	{"get_user_aiming",			get_user_aiming},
	{"get_user_ammo",			get_user_ammo},
	{"get_user_armor",			get_user_armor},
	{"get_user_attacker",		get_user_attacker},
	{"get_user_authid",			get_user_authid},
	{"get_user_flags",			get_user_flags},
	{"get_user_frags",			get_user_frags},
	{"get_user_deaths",			get_user_deaths},
	{"get_user_health",			get_user_health},
	{"get_user_index",			get_user_index},
	{"get_user_info",			get_user_info},
	{"get_user_ip",				get_user_ip},
	{"get_user_menu",			get_user_menu},
	{"get_user_msgid",			get_user_msgid},
	{"get_user_msgname",		get_user_msgname},
	{"get_user_name",			get_user_name},
	{"get_user_origin",			get_user_origin},
	{"get_user_ping",			get_user_ping},
	{"get_user_team",			get_user_team},
	{"get_user_time",			get_user_time},
	{"get_user_userid",			get_user_userid},
	{"get_user_weapon",			get_user_weapon},
	{"get_user_weapons",		get_user_weapons},
	{"get_weaponid",			get_weaponid},
	{"get_weaponname",			get_weaponname},
	{"get_xvar_float",			get_xvar_num},
	{"get_xvar_id",				get_xvar_id},
	{"get_xvar_num",			get_xvar_num},
	{"has_map_ent_class",		has_map_ent_class},
	{"int3",					int3},
	{"is_amd64_server",			is_amd64_server},
	{"is_dedicated_server",		is_dedicated_server},
	{"is_jit_enabled",			is_jit_enabled},
	{"is_linux_server",			is_linux_server},
	{"is_map_valid",			is_map_valid},
	{"is_module_loaded",		is_module_loaded},
	{"is_plugin_loaded",		is_plugin_loaded},
	{"is_user_alive",			is_user_alive},
	{"is_user_authorized",		is_user_authorized},
	{"is_user_bot",				is_user_bot},
	{"is_user_connected",		is_user_connected},
	{"is_user_connecting",		is_user_connecting},
	{"is_user_hltv",			is_user_hltv},
	{"lang_exists",				lang_exists},
	{"log_amx",					log_amx},
	{"log_message",				log_message},
	{"elog_message",			elog_message},
	{"log_to_file",				log_to_file},
	{"md5",						amx_md5},
	{"md5_file",				amx_md5_file},
	{"hash_string",				amx_hash_string},
	{"hash_file",				amx_hash_file},
	{"module_exists",			module_exists},
	{"next_hudchannel",			next_hudchannel},
	{"num_to_word",				num_to_word},
	{"parse_loguser",			parse_loguser},
	{"parse_time",				parse_time},
	{"pause",					pause},
	{"plugin_flags",			plugin_flags},
	{"precache_model",			precache_model},
	{"precache_sound",			precache_sound},
	{"precache_generic",			precache_generic},
	{"precache_event",			precache_event},
	{"random_float",			random_float},
	{"random_num",				random_num},
	{"read_argc",				read_argc},
	{"read_args",				read_args},
	{"read_argv",				read_argv},
	{"read_argv_int",			read_argv_int},
	{"read_argv_float",			read_argv_float},
	{"read_data",				read_data},
	{"read_datanum",			read_datanum},
	{"read_datatype",			read_datatype},
	{"read_flags",				read_flags},
	{"read_logargc",			read_logargc},
	{"read_logargv",			read_logargv},
	{"read_logdata",			read_logdata},
	{"register_clcmd",			register_clcmd},
	{"register_concmd",			register_concmd},
	{"register_dictionary",		register_dictionary},
	{"register_event",			register_event},
	{"register_event_ex",		register_event_ex},
	{"enable_event",			enable_event},
	{"disable_event",			disable_event},
	{"register_logevent",		register_logevent},
	{"enable_logevent",			enable_logevent},
	{"disable_logevent",		disable_logevent},
	{"register_menucmd",		register_menucmd},
	{"register_menuid",			register_menuid},
	{"register_plugin",			register_plugin},
	{"register_srvcmd",			register_srvcmd},
	{"require_module",			require_module},
	{"remove_quotes",			remove_quotes},
	{"remove_task",				remove_task},
	{"remove_user_flags",		remove_user_flags},
	{"server_cmd",				server_cmd},
	{"server_exec",				server_exec},
	{"server_print",			server_print},
	{"set_fail_state",			set_fail_state},
	{"set_dhudmessage",			set_dhudmessage},
	{"set_hudmessage",			set_hudmessage},
	{"set_localinfo",			set_localinfo},
	{"set_task",				set_task},
	{"set_user_flags",			set_user_flags},
	{"set_user_info",			set_user_info},
	{"set_xvar_float",			set_xvar_num},
	{"set_xvar_num",			set_xvar_num},
	{"show_dhudmessage",		show_dhudmessage},
	{"show_hudmessage",			show_hudmessage},
	{"show_menu",				show_menu},
	{"show_motd",				show_motd},
	{"task_exists",				task_exists},
	{"unpause",					unpause},
	{"user_has_weapon",			user_has_weapon},
	{"user_kill",				user_kill},
	{"user_slap",				user_slap},
	{"xvar_exists",				xvar_exists},
	{"AddTranslation",			AddTranslation},
	{"ClearSyncHud",			ClearSyncHud},
	{"CreateHudSyncObj",		CreateHudSyncObj},
	{"CreateLangKey",			CreateLangKey},
	{"CreateMultiForward",		CreateMultiForward},
	{"CreateOneForward",		CreateOneForward},
	{"DestroyForward",			DestroyForward},
	{"ExecuteForward",			ExecuteForward},
	{"GetLangTransKey",			GetLangTransKey},
	{"LibraryExists",			LibraryExists},
	{"LookupLangKey",			LookupLangKey},
	{"SetGlobalTransTarget",	SetGlobalTransTarget},
	{"PrepareArray",			PrepareArray},
	{"ShowSyncHudMsg",			ShowSyncHudMsg},
	{"AutoExecConfig",			AutoExecConfig},
	{"RequestFrame",			RequestFrame},
	{NULL,						NULL}
};
