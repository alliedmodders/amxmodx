/* Gasnades */

#include <amxmodx>
#include <fun>
#include <engine>
#include <csx>

new g_Gas
//----------------------------------------------------------------------------------------------
public plugin_init()
{
	register_plugin("Gasgrenades", "1.3 (CSX)", "RichoDemus/AssKicR/T(+)rget")

	register_cvar("amx_gasnades", "1")
	register_cvar("amx_gasdmg", "10")
	register_cvar("amx_gascheck", "3")
	register_cvar("amx_gasradius", "200")
	register_cvar("amx_smokegasp", "1")
	register_cvar("amx_gasobeyFF", "1")

	g_Gas = custom_weapon_add("gasnade",0,"gasnade")
}
//----------------------------------------------------------------------------------------------
public plugin_precache()
{ 
   precache_sound("player/gasp1.wav")
   precache_sound("player/gasp2.wav")
}

public gas(par[])
{ 
	new grenadeid = par[1]
	new id = par[0]

	if ( !is_valid_ent(grenadeid) )
		return

	new player[3], inum, Float:temp_grenade[3], grenade[3], players[32]
	if(get_cvar_num("amx_gasobeyFF") != 1)
		get_players(players, inum) // Get number of players
	else{
		new FFOn = get_cvar_num("mp_friendlyfire")
		if(FFOn == 0){
			new team[33]
			get_user_team(id, team, 32)
			if(equali(team, "CT"))
				get_players(players, inum, "ae", "TERRORIST")
			else
				get_players(players, inum, "ae", "CT")
		}
		else
			get_players(players, inum) // Get number of players
	}
	entity_get_vector(grenadeid, EV_VEC_origin, temp_grenade) // Get the position of the grenade
	grenade[0] = floatround(temp_grenade[0])
	grenade[1] = floatround(temp_grenade[1])
	grenade[2] = floatround(temp_grenade[2])
	for(new i = 0; i < inum; ++i){ // Loop through all players
		get_user_origin(players[i],player,0)
		new distance = get_distance(grenade, player)

		if((distance < get_cvar_num("amx_gasradius"))){ // Check who is standing close
			if(get_cvar_num("amx_smokegasp") != 0){
				if(get_cvar_num("amx_gascheck") >= 1){
					new number = random_num(1, 2)
					if(is_user_alive(players[i])){
						switch (number){
							case 1:emit_sound(players[i], CHAN_VOICE, "player/gasp1.wav", 1.0, ATTN_NORM, 0, PITCH_NORM)
							case 2:emit_sound(players[i], CHAN_VOICE, "player/gasp2.wav", 1.0, ATTN_NORM, 0, PITCH_NORM)
						}
					}
				}
			}
			message_begin(MSG_ONE, get_user_msgid("Damage"), {0,0,0}, players[i])
			write_byte(30) // dmg_save
			write_byte(30) // dmg_take
			write_long((1 << 16)) // visibleDamageBits
			write_coord(grenade[0]) // damageOrigin.x
			write_coord(grenade[1]) // damageOrigin.y
			write_coord(grenade[2]) // damageOrigin.z
			message_end()
			if(is_user_alive(id))
				ExtraDamage(players[i], id, get_cvar_num("amx_gasdmg"), "gasgrenade")
		}
	}
	set_task(get_cvar_float("amx_gascheck"), "gas",grenadeid,par,2) // If the grenade still exists we do a new check in get_cvar_num("amx_gascheck") second
}
//----------------------------------------------------------------------------------------------
public grenade_throw(index,greindex,wId){
	if ( task_exists(greindex) )
		remove_task(greindex)
	if( get_cvar_num("amx_gasnades") !=1 || wId != CSW_SMOKEGRENADE )
		return PLUGIN_CONTINUE
	if (g_Gas) custom_weapon_shot(g_Gas,index)
	new par[2]
	par[0] = index
	par[1] = greindex
	set_task(1.5, "gas", greindex,par,2)
	return PLUGIN_CONTINUE
}
//----------------------------------------------------------------------------------------------
stock ExtraDamage(id, attacker, damage, weaponDescription[])
{
	if(is_user_alive(id)) 
	{
		new userHealth = get_user_health(id)
		set_user_health(id, userHealth - damage)
		if (g_Gas)
			custom_weapon_dmg(g_Gas,attacker,id,damage)

		if(userHealth - damage <= 0)
		{
			logKill(attacker, id, weaponDescription)
			if(get_user_team(id) != get_user_team(attacker))
			{
				// The person dying shouldn't get negative credit for this kill (so add it back)
				set_user_frags(id, get_user_frags(id) + 1)
				// The killing should get credit for the frag
				set_user_frags(attacker, get_user_frags(attacker) + 1)
			}
			else
			{
				// The person dying shouldn't get negative credit for this kill (so add it back)
				set_user_frags(id, get_user_frags(id) + 1)
				// The killer killed a teammember or self
				// Engine gives credit for the kill so let's take away that + 1
				set_user_frags(attacker, get_user_frags(attacker) - 2)
			}
		}else{
			if(get_cvar_num("mp_logdetail") == 3){
				logDmg(attacker, id, weaponDescription,"body",damage,userHealth-damage)
			}
		}
	}
}
//----------------------------------------------------------------------------------------------
/* Log a kill using standard HL-logging format */
stock logKill(attacker, victim, weaponDescription[])
{
	// Save Hummiliation
	new namea[24], namev[24], authida[20], authidv[20], teama[10], teamv[10]

	// Info On Attacker
	get_user_name(attacker, namea, 23)
	get_user_team(attacker, teama, 9)
	get_user_authid(attacker, authida, 19)
	// Info On Victim
	get_user_name(victim, namev, 23)
	get_user_team(victim, teamv, 9)
	get_user_authid(victim, authidv, 19)
	// Log This Kill
	log_message("^"%s<%d><%s><%s>^" killed ^"%s<%d><%s><%s>^" with ^"%s^"",
	namea, get_user_userid(attacker), authida, teama, namev, get_user_userid(victim), authidv, teamv, weaponDescription)
}

/* Log damage using standard HL-logging format */
stock logDmg(attacker, victim, weaponDescription[],hit[],damage,health)
{
	// Save Hummiliation
	new namea[24], namev[24], authida[20], authidv[20], teama[10], teamv[10]

	// Info On Attacker
	get_user_name(attacker, namea, 23)
	get_user_team(attacker, teama, 9)
	get_user_authid(attacker, authida, 19)
	// Info On Victim
	get_user_name(victim, namev, 23)
	get_user_team(victim, teamv, 9)
	get_user_authid(victim, authidv, 19)
	// Log This Damage
	log_message("^"%s<%d><%s><%s>^" attacked ^"%s<%d><%s><%s>^" with ^"%s^" (hit ^"%s^") (damage ^"%d^") (health ^"%d^")",
	namea,get_user_userid(attacker),authida,teama,namev,get_user_userid(victim),authidv,teamv,weaponDescription,hit,damage,health)
}



//----------------------------------------------------------------------------------------------