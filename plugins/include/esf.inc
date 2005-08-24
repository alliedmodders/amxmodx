/***********************************************

[ Corona-Bytes.NET ] EvolutionX Core Plugin

	(c) Corona - Bytes .NET coders :: coders@corona-bytes.net
	
		> 2005 Corona Bytes :: http://www.corona-bytes.net

***********************************************/

#if defined __EVOLUTION_CORE__
  #endinput
#endif
#define __EVOLUTION_CORE__

#pragma library EvolutionXCore

native setClientPL 				( Client, PowerLevel );
native getClientPL 				( Client );
native setClientACPL 			( Client, ActualPowerLevel );
native getClientACPL 			( Client );
native setClientADPL 			( Client, AfterDeathPowerLevel );
native getClientADPL 			( Client );
native setClientSPL 			( Client, PowerLevel );
native setClientPLtoADPL 		( Client );

native setClientKI 				( Client, Ki );
native getClientKI 				( Client );

native setClientHP 				( Client, Health );
native getClientHP 				( Client );
native setClientMHP	 			( Client, MaximumHealth );
native getClientMHP 			( Client );

native setClientSPEED 			( Client, Speed );
native getClientSPEED 			( Client );
native setClientSWOOPSPEED 		( Client, SwoopSpeed );
native getClientSWOOPSPEED 		( Client );

native setClientPROTECT 		( Client, bool:Enable = true );
native getClientPROTECT 		( Client );

native setClientFROZEN 			( Client, bool:Enable = true );
native getClientFROZEN 			( Client );

native setClientGOD 			( Client, bool:Enable = true );
native getClientGOD 			( Client );

native getClientFLY 			( Client );

native setClientHiddenTURBO 	( Client, bool:Enable = true );
native getClientTURBO 			( Client );

native getClientBLOCK 			( Client );

native setClientHiddenPOWERUP 	( Client, bool:Enable = true );
native getClientPOWERUP 		( Client );

native getClientSWOOPING 		( Client );
native getClientATKSHOOT 		( Client );
native getClientATKCHARGE 		( Client );
native getClientMELEE 			( Client );
native getClientTHROWAWAY 		( Client );
native getClientTHROW	 		( Client );
native getClientWALLGND			( Client );
native getClientINFREEFALL		( Client );
native getClientBEAMJUMP		( Client );

// kills a player without score/death msg
native silentClientKILL			( Client );