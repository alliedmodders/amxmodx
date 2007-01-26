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
