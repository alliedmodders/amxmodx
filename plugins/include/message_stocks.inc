/* Message Stocks
*
* by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*
*/
 
#if defined _message_stocks_included
	#endinput
#endif
#define _message_stocks_included

/* Creates a death message. */
stock dod_make_deathmsg(killer, victim, weaponNUM)
{
	message_begin(MSG_ALL, get_user_msgid("DeathMsg"), {0,0,0}, 0);
	write_byte(killer);
	write_byte(victim);
	write_byte(weaponNUM);
	message_end();

	return 1;
}

/* Kills a user without a message. */
stock user_silentkill(index)
{
	static msgid = 0;
	new msgblock;
	if (!msgid)
	{
		msgid = get_user_msgid("DeathMsg");
	}
	msgblock = get_msg_block(msgid);
	set_msg_block(msgid, BLOCK_ONCE);	
	user_kill(index, 1);
	set_msg_block(msgid, msgblock);

	return 1;
}

/* Creates a death message. */
stock make_deathmsg(killer, victim, headshot, const weapon[])
{
	message_begin(MSG_ALL, get_user_msgid("DeathMsg"), {0,0,0}, 0);
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
 * Sends a custom or predefined text message to player.
 * Predefined texts are default game messages which will be translated
 * to player's game language, e.g. #Game_join_ct.
 *
 * @note Set index to 0 to send text globally.
 *
 * @note There does not necessarily have to be a total of 6 arguments.
 *       It will depend if message takes arguments, e.g.:
 *         Predefined message: client_printex(id, print_chat, "#Game_join_ct", "Pimp Daddy")
 *         Custom message    : client_printex(id, print_chat, "%s is joining the Counter-Terrorist force", "Pimp Daddy")
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
	static msgTextMsg;

	if (!msgTextMsg)
	{
		msgTextMsg = get_user_msgid("TextMsg");
	}

	message_begin(index > 0 ? MSG_ONE : MSG_ALL, msgTextMsg, .player = index);
	write_byte(type);
	write_string(msg_name);

	if (msg_param1[0])
		write_string(msg_param1);

	if (msg_param2[0])
		write_string(msg_param2);

	if (msg_param3[0])
		write_string(msg_param3);

	if (msg_param4[0])
		write_string(msg_param4);

	message_end();
}
