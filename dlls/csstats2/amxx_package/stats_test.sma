#include <amxmodx>

public plugin_init() {
	register_plugin("Stats Test","0.1","SidLuke")
}

public client_damage( attacker,victim,damage,wpnindex,hitplace,TA ){
	client_print(0 /*attacker*/,print_console,"DAMAGE: att:%d vic:%d dmg:%d wpn:%d aim:%d TA:%d",
			attacker,victim,damage,wpnindex,hitplace,TA)
	return PLUGIN_CONTINUE
}

public client_death( killer,victim,wpnindex,hitplace,TK ){
	client_print(0 /*killer*/,print_console,"DEATH: att:%d vic:%d wpn:%d aim:%d TK:%d",
			killer,victim,wpnindex,hitplace,TK)
	return PLUGIN_CONTINUE
}