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

/**
 * Selectors used with cmd_targetex()
 */
#define TARGETEX_ARG_AIM 			"aim"
#define TARGETEX_ARG_ALL 			"all"
#define TARGETEX_ARG_ALIVE 			"alive"
#define TARGETEX_ARG_BOTS 			"bots"
#define TARGETEX_ARG_DEAD 			"dead"
#define TARGETEX_ARG_HUMANS 		"humans"
#define TARGETEX_ARG_ME 			"me"
#define TARGETEX_ARG_SPECTATING	 	"spectating"
#define TARGETEX_ARG_VIEW 			"view"

static const TargetEx_GroupArguments[][] = { TARGETEX_ARG_ALL, TARGETEX_ARG_ALIVE, TARGETEX_ARG_BOTS, TARGETEX_ARG_DEAD, TARGETEX_ARG_HUMANS, TARGETEX_ARG_VIEW };
static const TargetEx_NoExcludeArguments[][] = { TARGETEX_ARG_AIM, TARGETEX_ARG_BOTS, TARGETEX_ARG_ME, TARGETEX_ARG_SPECTATING, TARGETEX_ARG_VIEW };
static TargetEx_GroupArgsLens[sizeof(TargetEx_GroupArguments)];
static TargetEx_NoExcludeArgsLens[sizeof(TargetEx_NoExcludeArguments)];

/**
 * Flags used with cmd_targetex()
 */
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

enum TargetExTeamStructure
{
	TARGETEX_TEAM_CODE[5],
	TARGETEX_TEAM_NAME[10],
	TARGETEX_TEAM_LEN
}

/**
 * List of teams used with cmd_targetex()
 */
static TargetEx_Teams[][TargetExTeamStructure] =
{
	{ "ct", "CT" },
	{ "t", "TERRORIST" },
	{ "spec", "SPECTATOR" }
}

static TargetEx_TeamStart, TargetEx_TeamEnd;

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
 *       valid team names and argument codes can be found in the TargetEx_Teams
 *       constant in targetex.inc.
 * @note When using an argument that targets a group of clients, the admin
 *       can also exclude himself from the command by adding a "!" symbol
 *       right after the "@" one, e.g. @!humans will target all humans
 *       except the admin that used the command.
 * @note If only one client is matched, the client's name will be copied
 *       in the "name" buffer, otherwise a translated string using one
 *       of the language keys found in cmdtarget.txt will be copied in the
 *       same buffer.
 *
 * @param id        Client index of admin performing the action
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
 *                    TARGETEX_NO_DEAD - doesn't allow targeting dead clients
 *
 * @return          Number of clients matched
 */
stock cmd_targetex(id, const arg[], players[MAX_PLAYERS], name[], len, TargetexFlags:flags = TARGETEX_OBEY_IMM_SINGLE)
{
	static bool:bDontInit;

	if(!bDontInit)
	{
		bDontInit = true;

		new szModName[32];
		get_modname(szModName, charsmax(szModName));

		if(equal(szModName, "cstrike"))
		{
			TargetEx_TeamStart = 0;
			TargetEx_TeamEnd = 2;
		}

		TargetEx_TeamEnd++;

		for(new i = TargetEx_TeamStart; i < TargetEx_TeamEnd; i++)
			TargetEx_Teams[i][TARGETEX_TEAM_LEN] = strlen(TargetEx_Teams[i][TARGETEX_TEAM_CODE]);

		for(new i; i < sizeof(TargetEx_GroupArguments); i++)
			TargetEx_GroupArgsLens[i] = strlen(TargetEx_GroupArguments[i]);

		for(new i; i < sizeof(TargetEx_NoExcludeArgsLens); i++)
			TargetEx_NoExcludeArgsLens[i] = strlen(TargetEx_NoExcludeArgsLens[i]);
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
		
		if(bExceptMe && targetex_is_no_exclude_argument(arg[iStartArg]))
		{
			targetex_cant_exclude(id, arg);
			return 0;
		}

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

		for(new i = TargetEx_TeamStart; i < TargetEx_TeamEnd; i++)
		{
			if(equal(arg[iArgLen - TargetEx_Teams[i][TARGETEX_TEAM_LEN]], TargetEx_Teams[i][TARGETEX_TEAM_CODE]))
			{
				szSuffix = "_TEAM";
				iMatchingFlags |= GetPlayers_MatchTeam;
				copy(szMatchingString, charsmax(szMatchingString), TargetEx_Teams[i][TARGETEX_TEAM_NAME]);

				if(iArgLen2 == TargetEx_Teams[i][TARGETEX_TEAM_LEN] + 1)
				{
					szLangKey = "TARGETEX_ARG_ALL";
					get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
					goto @AFTER_ARGS;
				}

				break;
			}
		}

		if(equal(arg[iStartArg], TARGETEX_ARG_AIM, 3))
		{
			new iTarget, iBody;
			get_user_aiming(id, iTarget, iBody);

			if(iTarget)
				iPlayers[iPnum++] = iTarget;
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_ALL, 3))
		{
			szLangKey = "TARGETEX_ARG_ALL";
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_ALIVE, 5))
		{
			if(flags & TARGETEX_NO_ALIVE)
			{
				console_print(id, "%L", id, "TARGETEX_NO_ALIVE")
				return 0;
			}

			szLangKey = "TARGETEX_ARG_ALIVE";
			iMatchingFlags |= GetPlayers_ExcludeDead;
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_BOTS, 4))
		{
			if(flags & TARGETEX_NO_BOTS)
			{
				console_print(id, "%L", id, "TARGETEX_NO_BOTS")
				return 0;
			}

			szLangKey = "TARGETEX_ARG_BOTS";
			iMatchingFlags |= GetPlayers_ExcludeHuman;
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_DEAD, 4))
		{
			if(flags & TARGETEX_NO_DEAD)
			{
				console_print(id, "%L", id, "TARGETEX_NO_DEAD")
				return 0;
			}

			szLangKey = "TARGETEX_ARG_DEAD";
			iMatchingFlags |= GetPlayers_ExcludeAlive;
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_HUMANS, 6))
		{
			szLangKey = "TARGETEX_ARG_HUMANS";
			iMatchingFlags |= GetPlayers_ExcludeBots|GetPlayers_ExcludeHLTV;
			get_players_ex(iPlayers, iPnum, iMatchingFlags, szMatchingString);
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_ME, 2))
			iPlayers[iPnum++] = id;
		else if(equal(arg[iStartArg], TARGETEX_ARG_SPECTATING, 10))
		{
			new iTarget = entity_get_int(id, EV_INT_iuser2);

			if(iTarget)
				iPlayers[iPnum++] = iTarget;
		}
		else if(equal(arg[iStartArg], TARGETEX_ARG_VIEW, 4))
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

			szLangKey = "TARGETEX_ARG_VIEW";
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
			case 0: console_print(id, "%L", id, "TARGETEX_NO_MATCHES");
			case 1: get_user_name(players[0], name, len);
			default:
			{
				if(szSuffix[0])
				{
					add(szLangKey, charsmax(szLangKey), szSuffix);
					formatex(name, len, "%L", LANG_PLAYER, szLangKey, szMatchingString);
				}
				else formatex(name, len, "%L", LANG_PLAYER, szLangKey);

				if(bExceptMe)
					add(name, len, fmt(" %L", LANG_PLAYER, "TARGETEX_EXCEPT_HIMSELF"));
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
			if(flags & TARGETEX_NO_ALIVE && is_user_alive(players[0]))
			{
				console_print(id, "%L", id, "TARGETEX_NO_ALIVE");
				return 0;
			}

			get_user_name(players[0], name, len);
			return 1;
		}
	}

	return 0;
}

static stock targetex_cant_exclude(id, const arg[])
	console_print(id, "%L", id, "TARGETEX_NO_EXCLUDE", arg[2]);

/**
 * Checks whether a specified argument is a team argument.
 *
 * @param arg       Target pattern
 *
 * @return          True if it is, false otherwise
 */
bool:targetex_is_team_argument(const arg[])
{
	for(new i = TargetEx_TeamStart; i < TargetEx_TeamEnd; i++)
	{
		if(equal(arg, TargetEx_Teams[i][TARGETEX_TEAM_CODE], TargetEx_Teams[i][TARGETEX_TEAM_LEN]))
			return true;
	}

	return false;
}

/**
 * Checks whether a specified argument is a group argument.
 *
 * @param arg       Target pattern
 *
 * @return          True if it is, false otherwise
 */
bool:targetex_is_group_argument(const arg[])
{
	for(new i; i < sizeof(TargetEx_GroupArguments); i++)
	{
		if(equal(arg, TargetEx_GroupArguments[i], TargetEx_GroupArgsLens[i]))
			return true;
	}

	return targetex_is_team_argument(arg);
}

/**
 * Checks whether a specified argument is a type of argument that doesn't
 * allow the admin to exclude himself.
 *
 * @param arg       Target pattern
 *
 * @return          True if it is, false otherwise
 */
bool:targetex_is_no_exclude_argument(const arg[])
{
	for(new i; i < sizeof(TargetEx_NoExcludeArguments); i++)
	{
		if(equal(arg, TargetEx_NoExcludeArguments[i], TargetEx_NoExcludeArgsLens[i]))
			return true;
	}

	return false;
}