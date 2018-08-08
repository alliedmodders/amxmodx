// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Message Stocks
//
 
#if defined _message_stocks_included
	#endinput
#endif
#define _message_stocks_included

/**
 * Sends a death message.
 *
 * @param killer        Killer id
 * @param victim        Victim id
 * @param weaponNUM     Weapon index
 *
 * @noreturn
 */
stock dod_make_deathmsg(killer, victim, weaponNUM)
{
	static msgid = 0;
	if (!msgid)
	{
		msgid = get_user_msgid("DeathMsg");
	}
	message_begin(MSG_ALL, msgid, {0,0,0}, 0);
	write_byte(killer);
	write_byte(victim);
	write_byte(weaponNUM);
	message_end();

	return 1;
}

/**
 * Kills a user without a message.
 *
 * @param index         Client index
 * @param flag          If nonzero, the death will not affect the client's score
 *
 * @noreturn
 */
stock user_silentkill(index, flag = 1)
{
	static msgid = 0;
	new msgblock;
	if (!msgid)
	{
		msgid = get_user_msgid("DeathMsg");
	}
	msgblock = get_msg_block(msgid);
	set_msg_block(msgid, BLOCK_ONCE);
	user_kill(index, flag);
	set_msg_block(msgid, msgblock);

	return 1;
}

/**
 * Creates a death message.
 *
 * @param killer        Killer id
 * @param victim        Victim id
 * @param headshot      Headshot
 * @param weapon        Weapon
 *
 * @noreturn
 */
stock make_deathmsg(killer, victim, headshot, const weapon[])
{
	static msgid = 0;
	if (!msgid)
	{
		msgid = get_user_msgid("DeathMsg");
	}
	message_begin(MSG_ALL, msgid, {0,0,0}, 0);
	write_byte(killer);
	write_byte(victim);

	new mod_name[32];
	get_modname(mod_name, 31);
	if (equal(mod_name, "cstrike") || equal(mod_name, "czero") || equal(mod_name, "csv15") || equal(mod_name, "cs13"))
		write_byte(headshot);
	write_string(weapon);
	message_end();

	return 1;
}

/**
 * Sends a predefined text message to player.
 * Predefined texts are default game messages which will be translated
 * to player's game language, e.g. #Game_join_ct.
 *
 * @note Set index to 0 to send text globally.
 *
 * @note There does not necessarily have to be a total of 6 arguments.
 *       It will depend if message takes arguments, e.g.:
 *         client_printex(id, print_chat, "#Game_join_ct", "Pimp Daddy")
 *         client_printex(id, print_chat, "1", "#Game_radio", "Pimp Daddy", "Hello world!")
 *
 * @param index         Index of the player, use 0 to send to all players.
 * @param type          The message destination. See print_* constants.
 * @param msg_name      The custom or predefined message to send.
 * @param msg_param1    Optional message argument.
 * @param msg_param2    Optional message argument.
 * @param msg_param3    Optional message argument.
 * @param msg_param4    Optional message argument.
 *
 * @noreturn
 */
stock client_printex(index, type, const msg_name[], const msg_param1[] = "", const msg_param2[] = "", const msg_param3[] = "", const msg_param4[] = "")
{
	new ch = msg_name[0];

	// If not a predefined message, we don't care about it and forward directly to client_print.
	// Special case for radio message. msg_name is an index, msg_param1 #Game_radio*, etc. Checking index should be enough.
	if (ch != '#' && (type != print_radio || !strtol(msg_name)))
	{
		return client_print(index, type, msg_name, msg_param1, msg_param2, msg_param3, msg_param4);
	}

	// Even if message starts with '#', we should check its length for safety.
	new length = strlen(msg_name);

	// If string is larger than expected, we forward to client_print which will cut message properly.
	// This means also this can't be a predefined game message.
	//   Max console length: 128 = \n (126) + \0 (127)
	//   Max SayText length: 192 = \n (190) + \0 (191)
	if ((length > 126 && (print_notify <= type <= print_console)) 
	|| ( length > 190 && (print_chat   <= type <= print_radio)))
	{
		return client_print(index, type, msg_name, msg_param1, msg_param2, msg_param3, msg_param4);
	}
	
	static msgTextMsg; 
	if (!msgTextMsg) 
	{ 
		msgTextMsg = get_user_msgid("TextMsg"); 
	}

	message_begin(index > 0 ? MSG_ONE_UNRELIABLE : MSG_BROADCAST, msgTextMsg, {0,0,0}, index);
	write_byte(type);
	write_string(msg_name);
	if (msg_param1[0]) { write_string(msg_param1); }
	if (msg_param2[0]) { write_string(msg_param2); }
	if (msg_param3[0]) { write_string(msg_param3); }
	if (msg_param4[0]) { write_string(msg_param4); }
	message_end();
	
	return 1;
}
