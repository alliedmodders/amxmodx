/*
* Plugin for sounds precache
*/

#include <amxmodx>

public plugin_precache(){
	precache_sound( "misc/impressive.wav")
	precache_sound( "misc/headshot.wav")
	precache_sound( "misc/multikill.wav")
	precache_sound( "misc/doublekill.wav")
	precache_sound( "misc/godlike.wav")
	precache_sound( "misc/ultrakill.wav")
	precache_sound( "misc/killingspree.wav")
	precache_sound( "misc/rampage.wav")
	precache_sound( "misc/unstoppable.wav")
	precache_sound( "misc/monsterkill.wav")
	precache_sound( "misc/humiliation.wav")

	precache_sound( "misc/takenlead.wav" ) 
	precache_sound( "misc/tiedlead.wav" ) 
	precache_sound( "misc/lostlead.wav" ) 

	return PLUGIN_CONTINUE
}

public plugin_init() {
  register_plugin("TFC Sounds Precache",AMXX_VERSION_STR,"AMXX Dev Team")
}