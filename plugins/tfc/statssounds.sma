// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// TFC Sounds Precache Plugin
//

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
