/* AMX Mod script.
*
* (c) 2002-2004, OLO
*  modified by the AMX Mod X Development Team
*
* This file is provided as is (no warranties).
*/

#include <amxmod>
#include <amxmisc>

// Comment if you don't want to hide not used reserved slots
#define HIDE_RESERVED_SLOTS

new g_cmdLoopback[16]

public plugin_init()
{
	register_plugin("Slots Reservation","0.1","default")
	register_cvar("amx_reservation","1")
	
  	format( g_cmdLoopback, 15, "amxres%c%c%c%c" , 
  		random_num('A','Z') , random_num('A','Z') ,random_num('A','Z'),random_num('A','Z')  )
  	
  	register_clcmd( g_cmdLoopback, "ackSignal" )
}

#if !defined NO_STEAM

public ackSignal(id)
	server_cmd("kick #%d", get_user_userid(id)  )

public client_authorized(id)
#else
public client_connect(id)
#endif
{
	new maxplayers = get_maxplayers( )
	new players = get_playersnum( 1 )
	new limit = maxplayers - get_cvar_num( "amx_reservation" )
  
	if ( (get_user_flags(id) & ADMIN_RESERVATION) || (players <= limit) )
	{
#if defined HIDE_RESERVED_SLOTS
		setVisibleSlots( players , maxplayers, limit )
#endif
		return PLUGIN_CONTINUE
	}

#if !defined NO_STEAM
	client_cmd(id,"echo ^"Dropped due to slot reservation^";%s" , g_cmdLoopback)
#else
	if ( is_user_bot(id) ) 
		server_cmd("kick #%d", get_user_userid(id)  )
	else 
		client_cmd(id,"echo ^"Dropped due to slot reservation^";disconnect")
#endif  
  
	return PLUGIN_HANDLED
}

#if defined HIDE_RESERVED_SLOTS
public client_disconnect(id)
{
	new maxplayers = get_maxplayers( )
	setVisibleSlots( get_playersnum(1) - 1 , maxplayers , 
		maxplayers - get_cvar_num( "amx_reservation" )  )
	return PLUGIN_CONTINUE
}

setVisibleSlots( players , maxplayers , limit )
{
	new num = players + 1

	if ( players == maxplayers )
		num = maxplayers
	else if ( players < limit )
		num = limit
		
	set_cvar_num( "sv_visiblemaxplayers" , num )
}
#endif

