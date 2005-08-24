/***********************************************

[ Corona-Bytes.NET ] EvolutionX Core Plugin

	(c) Corona - Bytes .NET coders :: coders@corona-bytes.net
	
		> 2005 Corona Bytes :: http://www.corona-bytes.net

***********************************************/

#include <amxmodx>
#include <fakemeta>
#include <engine>

public plugin_natives ()
{
	register_library( "EvolutionXCore" );

// >> [ PowerLevel ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	register_native( "setClientSPL", "__setClientSPL" );

	register_native( "setClientPL", "__setClientPL" );
	register_native( "getClientPL", "__getClientPL" );

	register_native( "setClientACPL", "__setClientACPL" );
	register_native( "getClientACPL", "__getClientACPL" );

	register_native( "setClientADPL", "__setClientADPL" );
	register_native( "getClientADPL", "__getClientADPL" );

	register_native( "setPLtoADPL", "__setPLtoADPL" );
// >> [ KineticEnergy ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "getClientKI", "__getClientKI" );
	register_native( "setClientKI", "__setClientKI" );

// >> [ Health ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "setClientHP", "__setClientHP" );
	register_native( "getClientHP", "__getClientHP" );

	register_native( "setClientMHP", "__setClientMHP" );
	register_native( "getClientMHP", "__getClientMHP" );

// >> [ Speed ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "setClientSPEED", "__setClientSPEED" );
	register_native( "getClientSPEED", "__getClientSPEED" );

	register_native( "setClientSWOOPSPEED", "__setClientSWOOPSPEED" );
	register_native( "getClientSWOOPSPEED", "__getClientSWOOPSPEED" );

// >> [ Protect ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


	register_native( "setClientPROTECT", "__setClientPROTECT" );
	register_native( "getClientPROTECT", "__getClientPROTECT" );

// >> [ Frozen ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "setClientFROZEN", "__setClientFROZEN" );
	register_native( "getClientFROZEN", "__getClientFROZEN" );

// >> [ GoD ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "setClientGOD", "__setClientGOD" );
	register_native( "getClientGOD", "__getClientGOD" );


// >> [ Fly ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "getClientFLY", "__getClientFLY" );

// >> [ Turbo ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "setClientHiddenTurbo", "__setClientHiddenTURBO" );
	register_native( "getClientTURBO", "__getClientTURBO" );

// >> [ Block ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "getClientBLOCK", "__getClientBLOCK" );

// >> [ Powerup ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "setClientHiddenPOWERUP", "__setClientHiddenPOWERUP" );
	register_native( "getClientPOWERUP", "__getClientPOWERUP" );

// >> [ ... ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

	register_native( "getClientSWOOPING", "__getClientSWOOPING" );

	register_native( "getClientATKSHOOT", "__getClientATKSHOOT" );

	register_native( "getClientATKCHARGE", "__getClientATKCHARGE" );

// >> Client in AdvancedMelee
	register_native( "getClientMELEE", "__getClientMELEE" );

// >> Client is in thrown away
	register_native( "getClientTHROWAWAY", "__getClientTHROWAWAY" );

// >> Client is in Throw
	register_native( "getClientTHROW", "__getClientTHROW" );

// >> Client has been thrown into Wall / on Ground
	register_native( "getClientWALLGND","__getClientWALLGND" );


// >> Client is in freefall like after jumping of ground
	register_native( "getClientINFREEFALL", "__getClientINFREEFALL" );

// >> Client is in beamjump
	register_native( "getClientBEAMJUMP", "__getClientBEAMJUMP" );

// [ MISC ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

// kills a player without death or score messages
	register_native( "silentClientKILL", "__skill" );

}

public plugin_init ( )
{
	register_plugin( "<< CORE >>", "3.0", "<< Corona-Bytes.NET >>" );
}

// >> [ PowerLevel ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientSPL( )
{
	set_pdata_int( get_param( 1 ) , 460, get_param( 2 ) );
	set_pdata_int( get_param( 1 ) , 461, get_param( 2 ) );
}

public __setClientPL()
	set_pdata_int( get_param( 1 ) , 460, get_param( 2 ) );

public __getClientPL()
	return get_pdata_int( get_param( 1 ) , 460 );

public __setClientACPL()
	set_pdata_int( get_param( 1 ) , 463, get_param( 2 ) );

public __getClientACPL()
	return get_pdata_int( get_param( 1 ), 463 );

public __setClientADPL()
	set_pdata_int( get_param( 1 ) , 461, get_param( 2 ) );

public __getClientADPL()
	return get_pdata_int( get_param( 1 ), 461 );

public __setPLtoADPL()
{
	set_pdata_int( get_param( 1 ) , 460, get_pdata_int( get_param( 1 ), 461 ));
	return 1;
}

// >> [ KineticEnergy ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientKI()
{
	entity_set_float( get_param( 1 ), EV_FL_fuser4, float( get_param( 2 ) ) );

}
public __getClientKI()
	return floatround( entity_get_float( get_param( 1 ), EV_FL_fuser4 ) );

// >> [ Health ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientHP()
{
	entity_set_float( get_param( 1 ), EV_FL_health, float( get_param( 2 ) ) );

	message_begin( MSG_ONE, get_user_msgid("Health"), {0,0,0} , get_param( 1 ) );
	write_byte( get_param( 2 ) );
	message_end( );
}

public __getClientHP()
	return floatround( entity_get_float( get_param( 1 ), EV_FL_health ) );

public __setClientMHP( )
{
	set_pdata_int( get_param( 1 ) , 142, get_param( 2 ) );

	message_begin( MSG_ONE, get_user_msgid( "MaxHealth" ), { 0, 0, 0 }, get_param( 1 ) );
	write_byte( get_param( 2 ) );
	message_end( );
}

public __getClientMHP( )
	return get_pdata_int( get_param( 1 ) , 142 );

// >> [ Speed ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientSPEED()
	set_pdata_int( get_param( 1 ) , 462, get_param( 2 ) );

public __getClientSPEED()
	return get_pdata_int( get_param( 1 ) , 462 );


public __setClientSWOOPSPEED()
{
	entity_set_float( get_param( 1 ), EV_FL_fuser1, float( get_param( 2 ) ) );

}
public __getClientSWOOPSPEED()
	return floatround( entity_get_float( get_param( 1 ), EV_FL_fuser1 ) );
// >> [ Protect ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientPROTECT()
{
	if ( get_param( 2 ) )
	{
		set_pdata_int( get_param( 1 ) , 301, 1, -89 );
		//entity_set_float( get_param( 1 ), EV_FL_takedamage, 0.0 );

	}

	else
	{
		set_pdata_int( get_param( 1 ) , 301, 0, -89 );
		//entity_set_float( get_param( 1 ), EV_FL_takedamage, 2.0 );
	}
}

public __getClientPROTECT()
	return get_pdata_int( get_param( 1 ) , 301 , -89 );

// >> [ Frozen ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientFROZEN()
{
	if ( get_param( 2 ) )
		entity_set_int( get_param( 1 ), EV_INT_flags, entity_get_int( get_param( 1 ), EV_INT_flags ) | FL_FROZEN );
	else
		entity_set_int( get_param( 1 ), EV_INT_flags, entity_get_int( get_param( 1 ), EV_INT_flags ) & ~FL_FROZEN );
}

public __getClientFROZEN()
	return entity_get_int( get_param( 1 ), EV_INT_flags ) & FL_FROZEN;

// >> [ GoD ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientGOD()
{
	if ( get_param( 2 ) )
		entity_set_float( get_param( 1 ), EV_FL_takedamage, 0.0 );
	else
		entity_set_float( get_param( 1 ), EV_FL_takedamage, 2.0 );
}

public __getClientGOD()
{
	if ( entity_get_float( get_param( 1 ), EV_FL_takedamage ) == 0.0 )
		return 1;
	return 0;
}

// >> [ Fly ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __getClientFLY()
	return get_pdata_int( get_param( 1 ) , 195 );


// >> [ Turbo ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


public __setClientHiddenTURBO()
{
	set_pdata_int(get_param( 1 ),196,get_param( 2 ));
	return 1;
}

public __getClientTURBO()
	return get_pdata_int( get_param( 1 ) , 196 );
	
// >> [ Block ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __getClientBLOCK()
	return get_pdata_int( get_param( 1 ) , 198 );	// << -
// >> [ Powerup ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __setClientHiddenPOWERUP()
{
	set_pdata_int(get_param( 1 ),199,get_param( 2 ));
	return 1;
}

public __getClientPOWERUP( )
	return get_pdata_int( get_param( 1 ) , 199 );
// >> [ ... ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

public __getClientSWOOPING( )
	return get_pdata_int( get_param( 1 ) , 317 );

public __getClientATKSHOOT(  )
	return get_pdata_int( get_param( 1 ) , 464 ) == 0 ? 1 : 0;

public __getClientATKCHARGE( )
	return get_pdata_int( get_param( 1 ) , 200 );

// >> Client in AdvancedMelee
public __getClientMELEE(  )
	return get_pdata_int( get_param( 1 ) , 298  );

// >> Client is in thrown away
public __getClientTHROWAWAY( )
	return get_pdata_int( get_param( 1 ) , 230 );

// >> Client is in Throw
public __getClientTHROW( )
	return get_pdata_int( get_param( 1 ) , 300 );

// >> Client has been thrown into Wall / on Ground
public __getClientWALLGND(  )
	return get_pdata_int( get_param( 1 ) , 230 );


// >> Client is in freefall like after jumping of ground
public __getClientINFREEFALL(  )
	return get_pdata_int( get_param( 1 ) , 27, 4 );

// >> Client is in beamjump
public __getClientBEAMJUMP( )
	return entity_get_int( get_param( 1 ), EV_INT_movetype ) == 15;

// [ MISC ] >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

// kills a player without death or score messages
public __skill(  )
{
	new gmsgDeathMsg	= get_user_msgid( "DeathMsg"	);
	new gmsgScoreInfo	= get_user_msgid( "ScoreInfo"	);
	set_msg_block( gmsgDeathMsg, BLOCK_ONCE );
	set_msg_block( gmsgScoreInfo, BLOCK_ONCE );

	set_pdata_int( get_param( 1 ) , 301, 0, -89 );
	entity_set_float( get_param( 1 ), EV_FL_takedamage, 2.0 );

	// hack for the blood in evm
	entity_set_float( get_param( 1 ), EV_FL_health, 1.0 );
	message_begin( MSG_ONE, get_user_msgid("Health"), {0,0,0} , get_param( 1 ) );
	write_byte( 1 );
	message_end( );
	// hack end
	
	user_kill( get_param( 1 ), 1 );

	entity_set_float( get_param( 1 ), EV_FL_frags, entity_get_float( get_param( 1 ), EV_FL_frags ) );

	return 1;
}