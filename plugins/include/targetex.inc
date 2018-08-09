// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// cmd_targetex stock
//

#if defined _targetex_included
	#endinput
#endif
#define _targetex_included

#include <amxmisc>
#include <engine>

static const TargetEx_GroupArguments[][] = { "all", "alive", "bots", "dead", "humans", "view" }
static TargetEx_ArgLens[sizeof(TargetEx_GroupArguments)];

enum TargetexFlags (<<= 1)
{
	TARGETEX_NONE = 0,
	TARGETEX_OBEY_IMM_SINGLE = 1,
	TARGETEX_OBEY_IMM_GROUP,
	TARGETEX_NO_SELF,
	TARGETEX_NO_GROUPS,
	TARGETEX_NO_BOTS,
	TARGETEX_NO_ALIVE,
	TARGETEX_NO_DEAD
}

enum TargetexTeams
{
	TARGETEX_E_CODE[5],
	TARGETEX_E_NAME[10],
	TARGETEX_E_SUFFIX[6],
	TARGETEX_E_LEN,
	TARGETEX_E_LEN_PLUS_ONE,
}

static const Targetex_Teams[][TargetexTeams] =
{
	{ "t", "TERRORIST", "_T" },
	{ "ct", "CT", "_CT" },
	{ "spec", "SPECTATOR", "_SPEC" }
}

/**
 * Processes a generic target pattern and tries to match it to a client or a
 * group of clients based on filtering flags and the usage of special arguments.
 *
 * @note If no clients were matched an appropriate message is displayed
 *       to the admin.
 * @note If no group matching symbol is used, the function will use the
 *       cmd_target() stock instead.
 * @note In order to use the special arguments provided by this function,
 *       the admin must start the <player> argument with the "@" symbol.
 * @note This is a list of all the available arguments that admins can use:
 *         @aim - targets the player that the admin is aiming at
 *         @all - targets all players
 *         @alive - targets alive players
 *         @bots - targets all bots
 *         @dead - targets dead players
 *         @humans - targets all humans
 *         @me - targets the admin himself
 *         @spectating - targets the client that the admin is spectating
 *         @view - targets all clients in the admin's field of view
 * @note For arguments that are used to target more than one client,
 *       the admin can also specify a team name after the argument itself,
 *       e.g. @alivect will target all alive counter-terrorists. All the
 *       valid team names and argument codes can be found in the Targetex_Teams
 *       constant in targetex.inc.
 * @note When using an argument that targets a group of clients, the admin
 *       can also exclude himself from the command by adding a "!" symbol
 *       right after the "@" one, e.g. @!humans will target all humans
 *       except the admin that used the command.
 * @note The "name" argument is used to store the matched client's name
 *       or one of the translations found in the targetex.txt dictionary.
 *
 * @param id        Client index of admin performing an action
 * @param arg       Target pattern
 * @param players   Array to store the matched clients in
 * @param name      Buffer to store the client name or argument translation
 * @param len       Maximum buffer length
 * @param flags     Optional filtering flags (enum TargetexFlags); valid flags are:
 *                    TARGETEX_NONE - no filter (default)
 *                    TARGETEX_OBEY_IMM_SINGLE - immunity will be obeyed when using arguments that target a single client
 *                    TARGETEX_OBEY_IMM_GROUP - immunity will be obeyed when using arguments that target a group of clients
 *                    TARGETEX_NO_SELF - doesn't allow the admin to target himself
 *                    TARGETEX_NO_GROUPS - doesn't allow usage of arguments that target a group of clients
 *                    TARGETEX_NO_BOTS - doesn't allow targeting bots
 *                    TARGETEX_NO_ALIVE - doesn't allow targeting alive clients
 *                    TARGETEX_NO_DEAD - doesn't allowt targeting dead clients
 *
 * @return          Number of clients matched
 */
stock cmd_targetex(id, const arg[], players[MAX_PLAYERS], name[], len, TargetexFlags:flags = TARGETEX_OBEY_IMM_SINGLE)
{
	static bool:bDontInit;

	if(!bDontInit)
	{
		bDontInit = true;

		register_dictionary("targetex.txt")

		for(new i; i < sizeof(Targetex_Teams); i++)
		{
			Targetex_Teams[i][TARGETEX_E_LEN] = strlen(Targetex_Teams[i][TARGETEX_E_CODE]);
			Targetex_Teams[i][TARGETEX_E_LEN_PLUS_ONE] = Targetex_Teams[i][TARGETEX_E_LEN] + 1;
		}

		for(new i; i < sizeof(TargetEx_GroupArguments); i++)
			TargetEx_ArgLens[i] = strlen(TargetEx_GroupArguments[i]);
	}

	if(arg[0] == '@')
	{
		new bool:bExceptMe = arg[1] == '!', iStartArg = bExceptMe ? 2 : 1;
		new bool:bIsGroupArg = targetex_is_group_argument(arg[iStartArg]);

		if(bIsGroupArg)
		{
			if(flags & TARGETEX_NO_GROUPS)
			{
				console_print(id, "%L", id, "TARGETEX_NO_GROUPS");
				return 0;
			}
		}
		else if(bExceptMe)
			return 0;

		new iPlayers[MAX_PLAYERS], iPnum;
		new szMatchingString[10], GetPlayersFlags:iMatchingFlags, iMatchedPlayers;

		if(flags & TARGETEX_NO_ALIVE)
			iMatchingFlags |= GetPlayers_ExcludeAlive;
		else if(flags & TARGETEX_NO_DEAD)
			iMatchingFlags |= GetPlayers_ExcludeDead;

		if(flags & TARGETEX_NO_BOTS)
			iMatchingFlags |= GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV;

		new szLangKey[32], szSuffix[6], iArgLen = strlen(arg)
		new iArgLen2 = iArgLen - (bExceptMe ? 1 : 0);

		for(new i; i < sizeof(Targetex_Teams); i++)
		{
			if(equal(arg[iArgLen - Targetex_Teams[i][TARGETEX_E_LEN]], Targetex_Teams[i][TARGETEX_E_CODE]))
			{
				iMatchingFlags |= GetPlayers_MatchTeam;
				copy(szSuffix, charsmax(szSuffix), Targetex_Teams[i][TARGETEX_E_SUFFIX]);
				copy(szMatchingString, charsmax(szMatchingString), Targetex_Teams[i][TARGETEX_E_NAME]);

				if(iArgLen2 == Targetex_Teams[i][TARGETEX_E_LEN_PLUS_ONE])
				{
					copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_ALL");
					get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
				}
			}
		}

		if(szLangKey[0])
			goto @AFTER_ARGS;

		if(equal(arg[iStartArg], "aim", 3))
		{
			new iTarget, iBody;
			get_user_aiming(id, iTarget, iBody);

			if(iTarget)
				iPlayers[iPnum++] = iTarget;
		}
		else if(equal(arg[iStartArg], "all", 3))
		{
			copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_ALL");
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], "alive", 5))
		{
			if(flags & TARGETEX_NO_ALIVE)
			{
				console_print(id, "%L", id, "TARGETEX_NO_ALIVE")
				return 0;
			}

			iMatchingFlags |= GetPlayers_ExcludeDead;
			copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_ALIVE");
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], "bots", 4))
		{
			if(flags & TARGETEX_NO_BOTS)
			{
				console_print(id, "%L", id, "TARGETEX_NO_BOTS")
				return 0;
			}

			iMatchingFlags |= GetPlayers_ExcludeHuman;
			copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_BOTS");
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], "dead", 4))
		{
			if(flags & TARGETEX_NO_DEAD)
			{
				console_print(id, "%L", id, "TARGETEX_NO_DEAD")
				return 0;
			}

			iMatchingFlags |= GetPlayers_ExcludeAlive;
			copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_DEAD");
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], "humans", 4))
		{
			iMatchingFlags |= GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV;
			copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_HUMANS");
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], "me", 2))
			iPlayers[iPnum++] = id;
		else if(equal(arg[iStartArg], "spectating", 10))
		{
			new iTarget = entity_get_int(id, EV_INT_iuser2);

			if(iTarget)
				iPlayers[iPnum++] = iTarget;
		}
		else if(equal(arg[iStartArg], "view", 4))
		{
			new iViewPlayers[MAX_PLAYERS], iViewPnum;
			get_players_ex(iViewPlayers, iViewPnum, iMatchingFlags, szMatchingString);

			for(new Float:fOrigin[3], iPlayer, i; i < iViewPnum; i++)
			{
				iPlayer = iViewPlayers[i];
				entity_get_vector(iPlayer, EV_VEC_origin, fOrigin);

				if(is_in_viewcone(id, fOrigin, 1))
					iPlayers[iPnum++] = iPlayer;
			}

			copy(szLangKey, charsmax(szLangKey), "TARGETEX_ARG_VIEW");
		}

		@AFTER_ARGS:

		for(new iPlayer, i; i < iPnum; i++)
		{
			iPlayer = iPlayers[i];
			
			if(id == iPlayer)
			{
				if(bExceptMe || flags & TARGETEX_NO_SELF)
					continue;
			}
			else if(get_user_flags(iPlayer) & ADMIN_IMMUNITY)
			{
				if(flags & TARGETEX_OBEY_IMM_GROUP && bIsGroupArg)
					continue;

				if(flags & TARGETEX_OBEY_IMM_SINGLE && !bIsGroupArg)
					continue;
			}

			players[iMatchedPlayers++] = iPlayer;
		}

		switch(iMatchedPlayers)
		{
			case 0: console_print(id, "%L", id, "TARGETEX_NO_MATCHES")
			case 1: get_user_name(players[0], name, len);
			default:
			{
				if(szSuffix[0])
					add(szLangKey, charsmax(szLangKey), szSuffix);

				if(bExceptMe)
					formatex(name, len, "%L %L", LANG_PLAYER, szLangKey, LANG_PLAYER, "TARGETEX_EXCEPT_HIMSELF");
				else formatex(name, len, "%L", LANG_PLAYER, szLangKey);
			}
		}

		return iMatchedPlayers;
	}
	else
	{
		new iSingleFlags;

		if(flags & TARGETEX_OBEY_IMM_SINGLE)
			iSingleFlags |= CMDTARGET_OBEY_IMMUNITY;

		if(~flags & TARGETEX_NO_SELF)
			iSingleFlags |= CMDTARGET_ALLOW_SELF;

		if(flags & TARGETEX_NO_DEAD)
			iSingleFlags |= CMDTARGET_ONLY_ALIVE;

		if(flags & TARGETEX_NO_BOTS)
			iSingleFlags |= CMDTARGET_NO_BOTS;

		players[0] = cmd_target(id, arg, iSingleFlags);

		if(players[0])
		{
			get_user_name(players[0], name, len);
			return 1;
		}
	}

	return 0;
}

static stock bool:targetex_is_group_argument(const arg[])
{
	for(new i; i < sizeof(TargetEx_GroupArguments); i++)
	{
		if(equal(arg, TargetEx_GroupArguments[i], TargetEx_ArgLens[i]))
			return true;
	}

	for(new i; i < sizeof(Targetex_Teams); i++)
	{
		if(equal(arg, Targetex_Teams[i][TARGETEX_E_CODE], Targetex_Teams[i][TARGETEX_E_LEN]))
			return true;
	}

	return false;
}