new const PLUGINNAME[] = "Weapon Arena"
new const VERSION[] = "1.6"
new const AUTHOR[] = "jghg"

/*
Copyleft 2003
AMX Mod X versions:
http://www.amxmodx.org/forums/viewtopic.php?p=4173

AMX Mod versions:
http://amxmod.net/forums/viewtopic.php?t=15549


WEAPON ARENA
============
Lets admin start an "<insert your favourite weapon here> arena" which means that everyone will instantly be given specified weapon, and infinite (optional) ammo for it. By default no other weapons will be visible on ground in game. As long as Weapon Arena is enabled you can't change to any other weapon than the given one, grenades, knife or C4. Upon new round you will be given a new weapon, if you lost your previous... Upon default you will not even have to reload any clips - you will always have bullets to fire! This can be changed with a cvar.


INSTALLATION
============
1. If your server is not running STEAM, uncomment NO_STEAM in amxconst.inc before you compile!
2. Compile sma to weaponarena.amx
3. Move weaponarena.amx into amxx/plugins
4. Add a line at the end of amxx/plugins.ini to say:
   weaponarena.amx
5. Done


USAGE
=====
weaponarena_menu
----------------
(client command) Menu driven weapon selection. Only for connected clients, that have access ADMIN_CFG. Use it to start and stop Weapon Arena in an easy way. There is a subset of menus to select which weapons will be selected in random mode.

weaponarena_randomlist
----------------------
(client/server command) Displays what weapons are available for random mode. When run from command line it also displays the letters used to specify these when changing the random setting from command line.

It may look like this on STEAM, though you'd want to double check it to make sure the letters are the same:

Weapon          status  letter
p228            off     a
scout           off     b
xm1014          off     c
mac10           off     d
aug             off     e
elite           off     f
fiveseven       off     g
ump45           off     h
sg550           off     i
galil           off     j
famas           off     k
usp             off     l
glock18         off     m
awp             off     n
mp5navy         off     o
m249            off     p
m3              off     q
m4a1            off     r
tmp             off     s
g3sg1           off     t
deagle          off     u
sg552           off     v
ak47            off     w
knife           off     x
p90             off     y

And this list is for WON:

Weapon          status  letter
p228            off     a
scout           off     b
xm1014          off     c
mac10           off     d
aug             off     e
elite           off     f
fiveseven       off     g
ump45           off     h
sg550           off     i
usp             off     j
glock18         off     k
awp             off     l
mp5navy         off     m
m249            off     n
m3              off     o
m4a1            off     p
tmp             off     q
g3sg1           off     r
deagle          off     s
sg552           off     t
ak47            off     u
knife           off     v
p90             off     w

weaponarena_random <abc...> [save]
----------------------------------
(client/server command) Specify what weapons will be available in random mode through command line/config files. To see which weapon has which letter, do a "weaponarena_randomlist",
as the letters vary if you run STEAM/WON.
If "save" is specified as 2nd parameter, this command also stores the current setting to AMXX vault which means that it will be loaded again when next map starts,
so no need of executing this command again if you want to keep the same setting.
If no record exists in vault this will default to all weapons enabled. If no weapons are enabled when random mode executes, it will always select Knife Arena.
A complete line for steam: "weaponarena_random abcdefghijklmnopqrstuvwxy" and for won: "weaponarena_random abcdefghijklmnopqrstuvw".

weaponarena <weaponnumber|random|list|off>
------------------------------------------
(client/server command) Specify a valid weaponnumber as argument, to turn it on, or change current weapon type if Weapon Arena is already on. Specify "random" to automatically change
current Weapon Arena weapon on every new round. Will enable Weapon Arena if it is disabled. Specify "list" to have a list of valid weapon numbers in a "message of the day" window
(or in server console). Specify "off" to turn Weapon Arena off.

weaponarena_unlimitedammo
-------------------------
(cvar) Values:
0 - to disable unlimited ammo. You will receive full ammo at round start.
1 - to enable unlimited ammo, AND you will NOT have to reload any clips, so you can fire forever without reloading! This is default.
2 - to enable unlimited ammo, but you will have to reload inbetween clips.

weaponarena_invisibleweapons
----------------------------
(cvar) Values:
0 - to disable all weapons but current Arena weapon from going invisible. You might want to use this if playing with bots: some bots still "see" the weapons (they are still there, just invisible and you can't touch them), and get stuck when they can't pick them up.
1 - to enable all weapons but current Arena weapon to go invisible. This is default.

weaponarena_delay
-----------------
(cvar) Standard delay of seconds from the time a player spawns until he receives the weapon.
Don't set to anything other than 0 (default) if you aren't having any problems. You might want to set this to 1.5 or something for maps consisting of player_weaponstrip entities. These maps, like ka_matrix and ka_legoland, usually make it so you lose your glock/usp at round starts, and sometimes you are given other weapons. This interferes with Weapon Arena. Use the plugin "Custom Map Configs" to have custom configs for different maps. Find it here: http://amxmod.net/amx-plugins.php?cat=7#25130

Examples:
weaponarena off
(Turns it off)

weaponarena 10
(Starts Weapon Arena with Berettas)

weaponarena list
(Displays valid weapon numbers)

weaponarena random
(Starts Weapon Arena, making a random selection of gun
for every new round to give to all players.)


VERSIONS
========
Released	Version		Comment
040331		1.6			Update to weaponarena_random cmd: Must now add "save" as last parameter for it to save that setting to vault.
040325		1.5			Weaponarena_randomlist didn't work from client. Now it only displays weapon letters when run from console.
						Command weaponarena_random now stores current setting to vault. The setting will be retrieved when map loads. If no setting is in vault, it defaults to all weapons enabled.

040324		1.4			Added a new menu set for random arena. Use it to select which weapons will be selected in random mode.
						Made a command to set weapons for random mode from console/config files.
						Fixed up some code: works better with bots and when changing weapons. Guns are now never dropped, instead it ultimately forces you to hold the knife.
						All weapons but current Arena weapon are made invisible and untouchable on ground.
						Cvar added to enable/disable invisibility of illegal weapons.

040322		1.3			Fixed unlimited ammo bug in previous version.
						Removed use of jghg2 module, now requires cstrike module.

040321		1.2			Remade for AMX Mod X.
						Setting 1 of weaponarena_unlimitedammo now works better, you can buy ammo but will never have to reload...
						Removed possibility of shield coming up when using random weapons, as the shield part isn't really working well.

			1.1			Added shield.
						Some bugfixing.
						Changed STEAM define to look for NO_STEAM define in amxconst.inc. Make sure you set it to the right setting before compile!

			1.01		First update for a time. Trying this script out under CS 1.6/Steam/Metamod 1.17/AMX 0.9.7j,
						the engclient_cmd() seems to not work really as expected. It did crash the server when forcing
						drops of weapons, and it couldn't force a player to hold the current WA weapon, resulting in
						really odd behaviour. Change to client_cmd() fixes this.

						#define STEAM is now default, so if you run CS 1.5 you must comment it.

			1.0			Added "weaponarena_unlimitedammo 2"

			0.91		Added support for Galil and Famas. Uncomment #define STEAM and recompile of you want this.

			0.9			Removed weaponarena_stripdelay, and so also the need
						for xtrafun module is removed.

			0.8.3b		Bug fixing, unlimited ammo off didn't work properly.

			0.8.3a		Bug fixing

			0.8.3		Maps with "player_weaponstrip" entities, like
						ka_matrix where you only get a knife at start,
						wouldn't work very well with Weapon Arena.
						This version postpones the giving of weapon
						1.5 seconds (default) from when player spawns.
						Seems to fix the problem. If you have problems
						still, try changing cvar weaponarena_stripdelay
						to hold how many seconds the giving of weapons
						should be postponed.

			0.8.2		Minor tweak.

			0.8.1		Minor tweak.

			0.8			Unlimited ammo added.

			0.7			Found (thanks to boundary checking) a bug/typo
						since newest RC (8) of AMX 0.9.4.
						So this version is fixed to work with newest
						AMX Mod. Thanks to DoubleTap for help with this.

						weaponarena_delay is another new cvar that works
						like weapon_arenastripdelay, only that it
						is used on all maps that don't have these
						"player_weaponstrip"s. Defaults to 0.

			0.6			Seemed previous version crashes,
						this version has some stuff that might help.

			0.5			Knife added

			0.4			Added random new weapon for every new round

			0.3			Added menu for selecting weapons

			0.2			Touch up...

			0.1			First version


TO DO
=====
- Make it so ammo displayed is updated instantly when your clip is replenished...
- Shield doesn't work really well at all... (don't use it yet ;-) )

	  - Johnny got his gun
*/

#include <amxmodx>
#include <fun>
#include <engine>
#include <amxmisc>
#include <cstrike>

#if !defined CSW_SHIELD
	#define CSW_SHIELD		2
#endif
#if defined (CSW_GALIL)
#else
	#define CSW_GALIL		14
#endif

#if defined (CSW_FAMAS)
#else
	#define CSW_FAMAS		15
#endif

#define NROFITEMS			34
#define MAXGIVEWEAPONS		1
#define MENUBUTTON1			(1<<0)
#define MENUBUTTON2			(1<<1)
#define MENUBUTTON3			(1<<2)
#define MENUBUTTON4			(1<<3)
#define MENUBUTTON5			(1<<4)
#define MENUBUTTON6			(1<<5)
#define MENUBUTTON7			(1<<6)
#define MENUBUTTON8			(1<<7)
#define MENUBUTTON9			(1<<8)
#define MENUBUTTON0			(1<<9)
#define MENUSELECT1			0
#define MENUSELECT2			1
#define MENUSELECT3			2
#define MENUSELECT4			3
#define MENUSELECT5			4
#define MENUSELECT6			5
#define MENUSELECT7			6
#define MENUSELECT8			7
#define MENUSELECT9			8
#define MENUSELECT0			9

// Global vars below
new bool:weaponArena
new bool:randomWeapon = false
new g_randomflags = 33554431 // 33554431 means all flags enabled on STEAM, should work for WON also...
new waWeapon
new weaponString[50]
new ammoString[50]
#if defined (NO_STEAM)
	new weaponNumbers[] = "\
		P228		   1^n\
		SCOUT          3^n\
		XM1014         5^n\
		MAC10          7^n\
		AUG            8^n\
		ELITE         10^n\
		FIVESEVEN     11^n\
		UMP45         12^n\
		SG550         13^n\
		USP           16^n\
		GLOCK18       17^n\
		AWP           18^n\
		MP5NAVY       19^n\
		M249          20^n\
		M3            21^n\
		M4A1          22^n\
		TMP           23^n\
		G3SG1         24^n\
		DEAGLE        26^n\
		SG552         27^n\
		AK47          28^n\
		KNIFE         29^n\
		P90           30"
#else
	new weaponNumbers[] = "\
		P228		   1^n\
		SHIELD		   2^n\
		SCOUT          3^n\
		XM1014         5^n\
		MAC10          7^n\
		AUG            8^n\
		ELITE         10^n\
		FIVESEVEN     11^n\
		UMP45         12^n\
		SG550         13^n\
		GALI    	  14^n\
		FAMAS         15^n\
		USP           16^n\
		GLOCK18       17^n\
		AWP           18^n\
		MP5NAVY       19^n\
		M249          20^n\
		M3            21^n\
		M4A1          22^n\
		TMP           23^n\
		G3SG1         24^n\
		DEAGLE        26^n\
		SG552         27^n\
		AK47          28^n\
		KNIFE         29^n\
		P90           30"
#endif
new g_hadThisRound[33] = {0,...}
new bool:randomizedThisRound = false
new bool:hidThisRound = false
new const DEFAULTDELAY[] = "0"
new const weaponstrings[NROFITEMS][] = {
	"",						// 0 First should be empty
	"weapon_p228",
	"weapon_shield",
	"weapon_scout",
	"weapon_hegrenade",
	"weapon_xm1014",
	"",						// 6 C4 CSW_C4
	"weapon_mac10",
	"weapon_aug",
	"weapon_smokegrenade",
	"weapon_elite",
	"weapon_fiveseven", // = "models/w_fiveseven.mdl"
	"weapon_ump45",
	"weapon_sg550",
#if defined (NO_STEAM)
	"",						// 14 empty in CS 1.5
	"",						// 15 empty in CS 1.5
#else
	"weapon_galil",			// 14
	"weapon_famas",			// 15
#endif
	"weapon_usp",
	"weapon_glock18",
	"weapon_awp",
	"weapon_mp5navy",
	"weapon_m249",
	"weapon_m3",
	"weapon_m4a1",
	"weapon_tmp",
	"weapon_g3sg1",
	"weapon_flashbang",
	"weapon_deagle",
	"weapon_sg552",
	"weapon_ak47",
	"weapon_knife",						// 27 knife CSW_KNIFE
	"weapon_p90",
	"item_kevlar",
	"item_assaultsuit",
	"item_thighpack"
}
new const MAXAMMOINCLIP[] = {
	0,						// 0 First should be empty
	13,		// weapon_p228
	0,		// weapon_shield
	10,		// weapon_scout
	0,		// weapon_hegrenade (max is 1)
	7,		// weapon_xm1014
	0,		// 6 C4 CSW_C4
	30,		// weapon_mac10
	30,		// weapon_aug
	0,		// weapon_smokegrenade (max is 1)
	30,		// weapon_elite
	20,		// weapon_fiveseven
	25,		// weapon_ump45
	30,		// weapon_sg550
#if defined (NO_STEAM)
	0,		// 14 empty in CS 1.5
	0,		// 15 empty in CS 1.5
#else
	35,		// weapon_galil
	25,		// weapon_famas
#endif
	12,		// weapon_usp
	20,		// weapon_glock18
	10,		// weapon_awp
	30,		// weapon_mp5navy
	100,	// weapon_m249
	8,		// weapon_m3
	30,		// weapon_m4a1
	30,		// weapon_tmp
	20,		// weapon_g3sg1
	0,		// weapon_flashbang (max is 2)
	7,		// weapon_deagle
	30,		// weapon_sg552
	30,		// weapon_ak47
	0,		// 27 knife CSW_KNIFE
	50		// weapon_p90
/*	"",		// item_kevlar
	"",		// item_assaultsuit
	""		// item_thighpack
*/
}
new const BLED[2][] = {"off", "on"}
new const VAULTKEY_RANDOM[] = "weaponarena_random"
/*
new weapondescriptions[31][] = {
	"ERROR: Invalid weapon (0)","Sig Sauer P228","Shield","Steyr Scout",
	"High explosive grenade","Benelli XM1014","C4 bomb","Ingram Mac-10",
	"Steyr Aug","Smoke grenade","Dual Beretta 96G Elites","Fabrique Nationale Five-seven",
	"Heckler & Koch UMP45","Sig SG-550 Commando","Galil","Famas",
	"Heckler & Koch USP45","Glock 18","Accuracy Int. Arctic Warfare/Magnum (AW/M)","Heckler & Koch MP5/Navy",
	"FN M249 Para","Benelli M3 Super 90","Colt M4A1","Steyr Tactical Machine Pistol",
	"Heckler & Koch G3/SG-1","Flashbang","IMI Desert Eagle","Sig SG-552 Commando",
	"Avtomat Kalashnikov AK-47","Combat knife","Fabrique Nationale P90"}
*/
// Global vars above

public newround_event(id) {
	// Is WA on and is player alive?
	if (!weaponArena || !is_user_alive(id))
		return PLUGIN_CONTINUE

	if (get_cvar_num("weaponarena_invisibleweapons") == 1 && !hidThisRound && !randomWeapon) {
		HideWeapons() // only do this once
		hidThisRound = true
	}

	if (randomWeapon && !randomizedThisRound) {
		randomizedThisRound = true
		randomize(0)
	}

	InitGives()

	// Give WA weapon, if id already doesn't have it.
	new idd[1]
	idd[0] = id

	set_task(get_cvar_float("weaponarena_delay"),"newround_event2",3526735 + id,idd,1)

	return PLUGIN_CONTINUE
}

public newround_event2(idd[]) {
	new id = idd[0]
	if (giveIfHasnt(id)) {
		//client_print(id, print_chat, "DEBUG: Gave you wpn because of new round")
	}
	//else
		//client_print(id, print_chat, "DEBUG: Didn't give you wpn because of new round")
	if (get_cvar_num("weaponarena_unlimitedammo") == 0)
		giveAmmo(id)

	new clip, ammo
	if (get_user_weapon(id,clip,ammo) != waWeapon)
		holdwaweapon(id)
}

public holdwaweapon(id) {
	if (is_user_connected(id) && is_user_alive(id))
		engclient_cmd(id, weaponString)
}

randomize(giveNewWeapon) {
	new newWeapon
	/*
	#if defined NO_STEAM
	do {
		newWeapon = random_num(1,30)
	}
	while (newWeapon < 1 || newWeapon > 30
	|| newWeapon == CSW_SHIELD || newWeapon == CSW_GALIL || newWeapon == CSW_FAMAS
	|| newWeapon == CSW_HEGRENADE || newWeapon == CSW_C4 || newWeapon == CSW_SMOKEGRENADE || newWeapon == CSW_FLASHBANG)
	#else
	*/
	// Count nr of random weapons enabled and put them into array
	new nrOfRandomWeapons, randomlist[30]

	for (new i = 1, j = 0; i <= 30; i++) {
		if (i == CSW_C4 || i == CSW_SHIELD || i == CSW_FLASHBANG || i == CSW_SMOKEGRENADE || i == CSW_HEGRENADE)
			continue
#if defined NO_STEAM
		if (i == CSW_GALIL || i == CSW_FAMAS)
			continue
#endif

		if (g_randomflags & (1<<j))
			randomlist[nrOfRandomWeapons++] = i
		j++
	}

	if (nrOfRandomWeapons == 0)
		newWeapon = CSW_KNIFE // error, set knife :-)
	else {
		newWeapon = randomlist[random_num(1, nrOfRandomWeapons) - 1]
	}

	if (!changeWeapon(0,newWeapon)) {
		client_print(0,print_chat,"WEAPON ARENA ERROR: Couldn't set new random weapon :-(")
		console_print(0,"WEAPON ARENA ERROR: Couldn't set new random weapon :-(")
	}

	new parm[2]
	parm[0] = 0
	parm[1] = giveNewWeapon
	set_task(0.5,"RunChanged",235262,parm,2)
}

public RunChanged(parm[]) {
	changed(parm[0],parm[1])
}

public giveIfHasnt(id) {
	// Don't give a knife!
	if (waWeapon == CSW_KNIFE)
		return 0

	// DEBUG EXTRA SAFETY THING ADDED
	if (!is_user_connected(id) || !is_user_alive(id))
		return 0
	// DEBUG EXTRA SAFETY THING ADDED

	new gave = 0
	new wpnList[32]
	new number

	new tempName[33]
	get_user_name(id,tempName,32)

	get_user_weapons(id,wpnList,number)
	for (new i = 0;i < number;i++) {
		if (wpnList[i] == waWeapon)
		{
			//client_print(0,print_chat,"%s already has the right weapon, won't give",tempName);
			return gave
		}
	}

	if (g_hadThisRound[id] < MAXGIVEWEAPONS) {
		g_hadThisRound[id]++
		give_item(id,weaponString)

		gave = 1
	}
	//else
		//client_print(0,print_chat,"Naaah %s already has received %d weapons this round... max is %d",tempName,g_hadThisRound[id],MAXGIVEWEAPONS)


	return gave
}

public setWpnString() {
	switch (waWeapon) {
		case CSW_P228           : weaponString = "weapon_p228"
		case CSW_SHIELD			: weaponString = "weapon_shield"
		case CSW_SCOUT          : weaponString = "weapon_scout"
		case CSW_XM1014         : weaponString = "weapon_xm1014"
		case CSW_MAC10          : weaponString = "weapon_mac10"
		case CSW_AUG            : weaponString = "weapon_aug"
		case CSW_ELITE			: weaponString = "weapon_elite"
		case CSW_FIVESEVEN		: weaponString = "weapon_fiveseven"
		case CSW_UMP45			: weaponString = "weapon_ump45"
		case CSW_SG550			: weaponString = "weapon_sg550"
		#if !defined (NO_STEAM)
			case CSW_GALIL			: weaponString = "weapon_galil"
			case CSW_FAMAS			: weaponString = "weapon_famas"
		#endif
		case CSW_USP			: weaponString = "weapon_usp"
		case CSW_GLOCK18		: weaponString = "weapon_glock18"
		case CSW_AWP			: weaponString = "weapon_awp"
		case CSW_MP5NAVY		: weaponString = "weapon_mp5navy"
		case CSW_M249			: weaponString = "weapon_m249"
		case CSW_M3				: weaponString = "weapon_m3"
		case CSW_M4A1			: weaponString = "weapon_m4a1"
		case CSW_TMP			: weaponString = "weapon_tmp"
		case CSW_G3SG1			: weaponString = "weapon_g3sg1"
		case CSW_DEAGLE			: weaponString = "weapon_deagle"
		case CSW_SG552			: weaponString = "weapon_sg552"
		case CSW_AK47			: weaponString = "weapon_ak47"
		case CSW_KNIFE			: weaponString = "weapon_knife"
		case CSW_P90			: weaponString = "weapon_p90"
		default					: {
			// bad weapon type, mega error, quit quit!
		}
	}
	switch (waWeapon) {
		case CSW_P228           : ammoString = "ammo_357sig"
		case CSW_SCOUT          : ammoString = "ammo_762nato"
		case CSW_XM1014         : ammoString = "ammo_buckshot"
		case CSW_MAC10          : ammoString = "ammo_45acp"
		case CSW_AUG            : ammoString = "ammo_556nato"
		case CSW_ELITE			: ammoString = "ammo_9mm"
		case CSW_FIVESEVEN		: ammoString = "ammo_57mm"
		case CSW_UMP45			: ammoString = "ammo_45acp"
		case CSW_SG550			: ammoString = "ammo_556nato"
		#if !defined (NO_STEAM)
			case CSW_GALIL			: ammoString = "ammo_556nato"
			case CSW_FAMAS			: ammoString = "ammo_556nato"
		#endif
		case CSW_USP			: ammoString = "ammo_45acp"
		case CSW_GLOCK18		: ammoString = "ammo_9mm"
		case CSW_AWP			: ammoString = "ammo_338magnum"
		case CSW_MP5NAVY		: ammoString = "ammo_9mm"
		case CSW_M249			: ammoString = "ammo_556natobox"
		case CSW_M3				: ammoString = "ammo_buckshot"
		case CSW_M4A1			: ammoString = "ammo_556nato"
		case CSW_TMP			: ammoString = "ammo_9mm"
		case CSW_G3SG1			: ammoString = "ammo_762nato"
		case CSW_DEAGLE			: ammoString = "ammo_50ae"
		case CSW_SG552			: ammoString = "ammo_556nato"
		case CSW_AK47			: ammoString = "ammo_762nato"
		case CSW_P90			: ammoString = "ammo_57mm"
		default					: {
			// bad weapon type, mega error, quit quit! (or it doesn't have ammo)
		}
	}
}

public toggle(id,level,cid) {
	if (!cmd_access(id,level,cid,2)) {
		return PLUGIN_HANDLED
	}

	new bool:originalState = weaponArena
	new oldWeapon = waWeapon

	if (read_argc() != 2) {
		console_print(id,"[AMX] ERROR: Use - amx_weaponarena <weaponnumber|list|off|random>")
		return PLUGIN_HANDLED
	}

	new argument[7]
	read_argv(1,argument,6)
	strtolower(argument)
	//console_print(id,"%s",argument)
	if (equal(argument,"off")) {
		if (!weaponArena) {
			console_print(id,"[AMX] ERROR: Weapon Arena is already off.")
			randomWeapon = false
			return PLUGIN_HANDLED
		}
		else {
			randomWeapon = false
			weaponArena = false
		}
	}
	else if (equal(argument,"list")) {
		if (id >= 1 && id <= 32)
			show_motd(id,weaponNumbers,"Weapon Arena")
		else
			console_print(id,weaponNumbers)
	}
	else if (equal(argument,"random")) {
		SwitchRandom(id)
	}
	else {
		new wpnNumber = str_to_num(argument)
		if (changeWeapon(id,wpnNumber))
			weaponArena = true
		else {
			console_print(id,"[AMX] ERROR: Use - amx_weaponarena <weaponnumber|list|off|random>",argument)
		}
	}

	if (originalState != weaponArena) {
		if (weaponArena) {
			// Activate
			activate(id)
		}
		else {
			disabled(id)
		}
	}
	else if (oldWeapon != waWeapon) {
		changed(id,1)
	}

	return PLUGIN_HANDLED
}

activate(id) {
	set_hudmessage(0, 100, 0, -1.0, 0.65, 2, 0.02, 10.0, 0.01, 0.1, 2)
	show_hudmessage(0,"%c%s Arena is enabled!",weaponString[7] - 32,weaponString[8])
	client_print(id,print_console,"[AMX] %c%s Arena was enabled!",weaponString[7] - 32,weaponString[8])
	server_print("[AMX] %c%s Arena was enabled!",weaponString[7] - 32,weaponString[8])

	giveAllIfHavent()

	// Hide all other weapons
	HideWeapons()
}

public HideWeapons() {
	// Search weaponboxes and check their models. Real weaponmodelstring should be "models/w_fiveseven.mdl" if classname is weapon_fiveseven
	new matchmodelstring[128]
	format(matchmodelstring, 127, "models/w_%s.mdl", weaponString[7])
	//server_print("Should hide those whose models aren't %s.", matchmodelstring)

	new ent = 0, model[128], boxes = 0, armouries = 0
	while ((ent = find_ent_by_class(ent, "weaponbox"))) {
		entity_get_string(ent, EV_SZ_model, model, 127)
		//server_print("weaponbox %d Read model: ^"%s^"", ent, model)
		if (!equal(matchmodelstring, model)) {
			//server_print("%s doesn't equal what it should be: %s", model, matchmodelstring)
			set_entity_visibility(ent, 0)
			entity_set_int(ent, EV_INT_solid, SOLID_NOT)
			boxes++
		}
	}
	while ((ent = find_ent_by_class(ent, "armoury_entity"))) {
		entity_get_string(ent, EV_SZ_model, model, 127)
		//server_print("armoury_entity %d Read model: ^"%s^"", ent, model)
		if (!equal(matchmodelstring, model)) {
			//server_print("%s doesn't equal what it should be: %s", model, matchmodelstring)
			set_entity_visibility(ent, 0)
			entity_set_int(ent, EV_INT_solid, SOLID_NOT)
			armouries++
		}
	}
	//client_print(0, print_chat, "Hided %d weaponboxes and %d armoury_entities.", boxes, armouries)
	//server_print("Hided %d weaponboxes and %d armoury_entities.", boxes, armouries)
}

public HideOneWeapon(ent[1]) {
	//new weaponString2[32]
	//get_weaponname(wti[0], weaponString2, 31)
	//return find_ent_by_owner(0, weaponString, ownerId)

	new const owner = entity_get_edict(ent[0], EV_ENT_owner)
	if (owner >= 1 && owner <= get_global_int(GL_maxClients))
		set_task(0.1, "HideOneWeapon", ent[0], ent, 1) // loop until owner has become a weaponbox
	else {
		// this should be a weaponbox, double check this..?
		new ownerclassname[64]
		entity_get_string(owner, EV_SZ_classname, ownerclassname, 63)
		if (!equal(ownerclassname, "weaponbox")) {
			client_print(0, print_chat, "Error, the dropped weapon's owner is not a weaponbox (maybe someone picked it up) owner is a: %s", ownerclassname)
			return
		}
		// Now hide this weaponbox
		set_entity_visibility(owner, 0)
		entity_set_int(owner, EV_INT_solid, SOLID_NOT)
	}
}

ShowWeapons() {
	new ent = 0
	while ((ent = find_ent_by_class(ent, "weaponbox"))) {
		set_entity_visibility(ent)
		entity_set_int(ent, EV_INT_solid, SOLID_TRIGGER)
	}

	while ((ent = find_ent_by_class(ent, "armoury_entity"))) {
		set_entity_visibility(ent)
		entity_set_int(ent, EV_INT_solid, SOLID_TRIGGER)
	}
}

InitGives()
{
	for (new i = 1;i <= 32;i++)
		g_hadThisRound[i] = 0

	//client_print(0,print_center,"inited gives")
}

public giveAllIfHavent() {
	// Init amount of weapons given to people this round...
	InitGives()

	// Collect all alive players and give'm a weapon if they
	// already not have one.
	new playersList[32]
	new playersFound
	get_players(playersList,playersFound,"a")
	//console_print(0,"Found %d players",playersFound)
	new clip, ammo
	for (new i = 0;i < playersFound;i++) {
		if (giveIfHasnt(playersList[i]) && get_cvar_num("weaponarena_unlimitedammo") == 0)
			giveAmmo(playersList[i])
		if (get_user_weapon(playersList[i],clip,ammo) != waWeapon)
			holdwaweapon(playersList[i])
	}
}

public changed(id,giveNewWeapon) {
	InitGives()

	set_hudmessage(0, 100, 0, -1.0, 0.65, 2, 0.02, 10.0, 0.01, 0.1, 2)

	show_hudmessage(0,"Changed to %c%s Arena!",weaponString[7] - 32,weaponString[8])
	client_print(id,print_console,"[AMX] Changed to %c%s Arena!",weaponString[7] - 32,weaponString[8])
	server_print("[AMX] Changed to %c%s Arena!",weaponString[7] - 32,weaponString[8])

	if (get_cvar_num("weaponarena_invisibleweapons") == 1) {
		ShowWeapons() // First show all weapons
		HideWeapons() // Then hide all that aren't the new weapon
	}

	if (giveNewWeapon) {
		// Collect all alive players and give'm a weapon if they
		// already not have one.
		new playersList[32]
		new playersFound
		get_players(playersList,playersFound,"a")
		for (new i = 0;i < playersFound;i++) {
			if (giveIfHasnt(playersList[i]) && get_cvar_num("weaponarena_unlimitedammo") == 0)
				giveAmmo(playersList[i])

			new clip, ammo
			if (get_user_weapon(playersList[i],clip,ammo) != waWeapon)
				holdwaweapon(playersList[i])
		}
	}
}

disabled(id) {
	set_hudmessage(0, 100, 0, -1.0, 0.65, 2, 0.02, 10.0, 0.01, 0.1, 2)
	show_hudmessage(0,"%c%s Arena is disabled!",weaponString[7] - 32,weaponString[8])
	client_print(id,print_console,"[AMX] %c%s Arena was disabled!",weaponString[7] - 32,weaponString[8])
	server_print("[AMX] %c%s Arena was disabled!",weaponString[7] - 32,weaponString[8])

	ShowWeapons()
}

public giveAmmo(id) {
	// DEBUG EXTRA SAFETY THING ADDED
	if (!is_user_connected(id) || !is_user_alive(id))
		return
	// DEBUG EXTRA SAFETY THING ADDED

	new clips = 0
	switch (waWeapon) {
		case CSW_P228           : clips = 4
		case CSW_SCOUT          : clips = 3
		case CSW_XM1014         : clips = 4
		case CSW_MAC10          : clips = 9
		case CSW_AUG            : clips = 3
		case CSW_ELITE			: clips = 4
		case CSW_FIVESEVEN		: clips = 2
		case CSW_UMP45			: clips = 9
		case CSW_SG550			: clips = 3
		#if !defined (NO_STEAM)
			case CSW_GALIL			: clips = 3
			case CSW_FAMAS			: clips = 3
		#endif
		case CSW_USP			: clips = 9
		case CSW_GLOCK18		: clips = 4
		case CSW_AWP			: clips = 3
		case CSW_MP5NAVY		: clips = 4
		case CSW_M249			: clips = 7
		case CSW_M3				: clips = 4
		case CSW_M4A1			: clips = 3
		case CSW_TMP			: clips = 4
		case CSW_G3SG1			: clips = 3
		case CSW_DEAGLE			: clips = 5
		case CSW_SG552			: clips = 3
		case CSW_AK47			: clips = 3
		case CSW_P90			: clips = 2
	}

	for (new i = 0;i < clips;i++)
		give_item(id,ammoString)
}

public giveAmmoWTI(id, weaponTypeId) {
	// DEBUG EXTRA SAFETY THING ADDED
	if (!is_user_connected(id) || !is_user_alive(id))
		return
	// DEBUG EXTRA SAFETY THING ADDED

	new clips = 0
	switch (weaponTypeId) {
		case CSW_P228           : clips = 4
		case CSW_SCOUT          : clips = 3
		case CSW_XM1014         : clips = 4
		case CSW_MAC10          : clips = 9
		case CSW_AUG            : clips = 3
		case CSW_ELITE			: clips = 4
		case CSW_FIVESEVEN		: clips = 2
		case CSW_UMP45			: clips = 9
		case CSW_SG550			: clips = 3
		#if !defined (NO_STEAM)
		case CSW_GALIL			: clips = 3
		case CSW_FAMAS			: clips = 3
		#endif
		case CSW_USP			: clips = 9
		case CSW_GLOCK18		: clips = 4
		case CSW_AWP			: clips = 3
		case CSW_MP5NAVY		: clips = 4
		case CSW_M249			: clips = 7
		case CSW_M3				: clips = 4
		case CSW_M4A1			: clips = 3
		case CSW_TMP			: clips = 4
		case CSW_G3SG1			: clips = 3
		case CSW_DEAGLE			: clips = 5
		case CSW_SG552			: clips = 3
		case CSW_AK47			: clips = 3
		case CSW_P90			: clips = 2
		default					: return
	}

	new ammostrings[64]

	switch (weaponTypeId) {
		case CSW_P228           : ammostrings = "ammo_357sig"
		case CSW_SCOUT          : ammostrings = "ammo_762nato"
		case CSW_XM1014         : ammostrings = "ammo_buckshot"
		case CSW_MAC10          : ammostrings = "ammo_45acp"
		case CSW_AUG            : ammostrings = "ammo_556nato"
		case CSW_ELITE			: ammostrings = "ammo_9mm"
		case CSW_FIVESEVEN		: ammostrings = "ammo_57mm"
		case CSW_UMP45			: ammostrings = "ammo_45acp"
		case CSW_SG550			: ammostrings = "ammo_556nato"
		#if !defined (NO_STEAM)
		case CSW_GALIL			: ammostrings = "ammo_556nato"
		case CSW_FAMAS			: ammostrings = "ammo_556nato"
		#endif
		case CSW_USP			: ammostrings = "ammo_45acp"
		case CSW_GLOCK18		: ammostrings = "ammo_9mm"
		case CSW_AWP			: ammostrings = "ammo_338magnum"
		case CSW_MP5NAVY		: ammostrings = "ammo_9mm"
		case CSW_M249			: ammostrings = "ammo_556natobox"
		case CSW_M3				: ammostrings = "ammo_buckshot"
		case CSW_M4A1			: ammostrings = "ammo_556nato"
		case CSW_TMP			: ammostrings = "ammo_9mm"
		case CSW_G3SG1			: ammostrings = "ammo_762nato"
		case CSW_DEAGLE			: ammostrings = "ammo_50ae"
		case CSW_SG552			: ammostrings = "ammo_556nato"
		case CSW_AK47			: ammostrings = "ammo_762nato"
		case CSW_P90			: ammostrings = "ammo_57mm"
		default					: return
	}

	for (new i = 0;i < clips;i++)
		give_item(id, ammostrings)
}

public dropweapon(id) {
    // Since update to CS 1.6/Steam/AMX 0.9.7j/Metamod 1.17 (dunno what causes it really)
    // engclient_cmd(id,"drop") doesn't work anymore. So we change it to client_cmd().
	if (is_user_connected(id) && is_user_alive(id)) {
		new idd[1]
		idd[0] = id

		set_task(0.5, "seconddropcheck", 0, idd, 1)
	}
}

public seconddropcheck(idd[]) {
	new id = idd[0], _clip, _ammo
	new weaponTypeId = get_user_weapon(id, _clip, _ammo)
	if (weaponTypeId == waWeapon) {
		//client_print(id, print_chat, "(sdc) You hold %s, which %s the current WA weapon. (no drop)", weapontypetemptext[weaponTypeId], (weaponTypeId == waWeapon) ? "is" : "is not")
		switch (get_cvar_num("weaponarena_unlimitedammo")) {
			case 1:
			{
				new clip, ammo
				get_user_ammo(id,weaponTypeId,clip,ammo)
				if (clip < 1) {
					if (is_user_connected(id) && is_user_alive(id)) {
						give_item(id,weaponString)
						holdwaweapon(id)
					}
				}
			}
			case 2:
			{
				new clip, ammo
				get_user_ammo(id,weaponTypeId,clip,ammo)
				if (clip < 1 && ammo == 0) {
					if (is_user_connected(id) && is_user_alive(id)) {
						giveAmmo(id)
						holdwaweapon(id)
					}
				}
			}
		}
	}
	else if (weaponTypeId == CSW_HEGRENADE
	|| weaponTypeId == CSW_SMOKEGRENADE
	|| weaponTypeId == CSW_FLASHBANG
	|| weaponTypeId == CSW_KNIFE
	|| weaponTypeId == CSW_C4) {
		//client_print(id, print_chat, "You hold %s, which %s the current WA weapon. (no drop)", weapontypetemptext[weaponTypeId], (weaponTypeId == waWeapon) ? "is" : "is not")
		return PLUGIN_CONTINUE
	}
	else if (waWeapon == CSW_SHIELD
	&& (weaponTypeId == CSW_USP
	|| weaponTypeId == CSW_GLOCK18
	|| weaponTypeId == CSW_P228
	|| weaponTypeId == CSW_FIVESEVEN
	|| weaponTypeId == CSW_DEAGLE)) {
		// Ok to hold a pistol/knife if shield arena (except berettas).
		// Check if reload is needed.
		checkreload(id, weaponTypeId)
		return PLUGIN_CONTINUE
	}
	else {
		//client_print(id, print_chat, "[AMX] Don't hold that weapon! It's currently %c%s Arena! (forcing you to drop it)", weaponString[7] - 32,weaponString[8])
		client_print(id, print_chat, "[AMX] Don't hold that weapon! It's currently %c%s Arena! (forcing you to hold the knife)", weaponString[7] - 32,weaponString[8])
		//client_print(id, print_chat, "(sdc) You hold %s, which %s the current WA weapon. (drop)", weapondescriptions[weaponTypeId], (weaponTypeId == waWeapon) ? "is" : "is not")
		//handledrop(id)
		//engclient_cmd(id, "drop", weaponstrings[weaponTypeId])
		//holdwaweapon(id)
		engclient_cmd(id, "weapon_knife")
	}

	return PLUGIN_CONTINUE
}

// find_wpn_ent_fast() returns 0 if failed (that is, if no wpn ent could be found
// among all "weapontext" weapons in the game.
// Description: Returns the weapon entity number, where weapon type is weapontext[] and ownerId is the owner.
stock find_wpnent_fast(ownerId, weaponTypeId) {
	new weaponString2[20]
	get_weaponname(weaponTypeId, weaponString2, 19)

	return find_ent_by_owner(0, weaponString2, ownerId)
}

public holdwpn_event(id) {
	if (!weaponArena)
		return PLUGIN_CONTINUE

	new weaponTypeId = read_data(2)

	// Only valid weapons to hold are waWeapon, C4, knife, and grenades.
	if (weaponTypeId == waWeapon) {
		//client_print(id, print_chat, "You hold %s, which %s the current WA weapon. (no drop)", weapontypetemptext[weaponTypeId], (weaponTypeId == waWeapon) ? "is" : "is not")
		switch (get_cvar_num("weaponarena_unlimitedammo")) {
			case 1:
			{
				new clip, ammo
				get_user_ammo(id,weaponTypeId,clip,ammo)
				/*if (clip < 1) {
					if (is_user_connected(id) && is_user_alive(id)) {
						give_item(id,weaponString)
						holdwaweapon(id)
					}
				}*/
				if (clip < 3) {
					new weaponEntity = find_wpnent_fast(id, weaponTypeId)
					if (weaponEntity != -1 && is_user_connected(id) && is_user_alive(id)) {
						//if (!is_linux_server())
							//set_offset(weaponEntity, OFFSET_CLIPAMMO, MAXAMMOINCLIP[weaponTypeId])
						//else
						//server_print("Reload: ent=%d, offset=%d, maxammo=%d wti=%d", weaponEntity, OFFSET_CLIPAMMO_LINUX, MAXAMMOINCLIP[weaponTypeId], weaponTypeId)
						//client_print(id, print_chat, "Reload: ent=%d, offset=%d, maxammo=%d wti=%d", weaponEntity, OFFSET_CLIPAMMO_LINUX, MAXAMMOINCLIP[weaponTypeId], weaponTypeId)
						//set_offset(weaponEntity, OFFSET_CLIPAMMO_LINUX, MAXAMMOINCLIP[weaponTypeId])
						cs_set_weapon_ammo(weaponEntity, MAXAMMOINCLIP[weaponTypeId] + 1)

						//client_print(id, print_chat, "Restocked ammo in clip")
						return PLUGIN_HANDLED
					}
				}
			}
			case 2:
			{
				new clip, ammo
				get_user_ammo(id,weaponTypeId,clip,ammo)
				if (clip < 1 && ammo == 0) {
					if (is_user_connected(id) && is_user_alive(id)) {
						giveAmmo(id)
						holdwaweapon(id)
					}
				}
			}
		}
	}
	else if (weaponTypeId == CSW_HEGRENADE
	|| weaponTypeId == CSW_SMOKEGRENADE
	|| weaponTypeId == CSW_FLASHBANG
	|| weaponTypeId == CSW_KNIFE
	|| weaponTypeId == CSW_C4) {
		//client_print(id, print_chat, "You hold %s, which %s the current WA weapon. (no drop)", weapontypetemptext[weaponTypeId], (weaponTypeId == waWeapon) ? "is" : "is not")
		return PLUGIN_CONTINUE
	}
	else if (waWeapon == CSW_SHIELD
	&& (weaponTypeId == CSW_USP
	|| weaponTypeId == CSW_GLOCK18
	|| weaponTypeId == CSW_P228
	|| weaponTypeId == CSW_FIVESEVEN
	|| weaponTypeId == CSW_DEAGLE)) {
		// Ok to hold a pistol/knife if shield arena (except berettas).
		// Check if reload is needed.
		checkreload(id, weaponTypeId)
		return PLUGIN_CONTINUE
	}
	else {
		//client_print(id, print_chat, "[AMX] Don't hold that weapon! It's currently %c%s Arena!", weaponString[7] - 32,weaponString[8])
		//client_print(id, print_chat, "You hold %s, which %s the current WA weapon. (recheck in 0.5)", weapondescriptions[weaponTypeId], (weaponTypeId == waWeapon) ? "is" : "is not")
		dropweapon(id)
		holdwaweapon(id)
	}

	return PLUGIN_CONTINUE
}

stock checkreload(id, weaponTypeId) {
	switch (get_cvar_num("weaponarena_unlimitedammo")) {
		case 1:
		{
			new clip, ammo
			get_user_ammo(id,weaponTypeId,clip,ammo)
			if (clip < 1) {
				if (is_user_connected(id) && is_user_alive(id)) {
					give_item(id, weaponstrings[weaponTypeId])
					holdwaweapon(id)
				}
			}
		}
		case 2:
		{
			new clip, ammo
			get_user_ammo(id,weaponTypeId,clip,ammo)
			if (clip < 1 && ammo == 0) {
				if (is_user_connected(id) && is_user_alive(id)) {
					giveAmmoWTI(id, weaponTypeId)
					holdwaweapon(id)
				}
			}
		}
	}
}

// Returns 0 on failure, 1 on success
public changeWeapon(id, newWeapon) {
	if (newWeapon < 1 || newWeapon > 30
	//|| newWeapon == 2 (removed to support CSW_SHIELD)
	|| newWeapon == CSW_C4
	|| newWeapon == CSW_HEGRENADE
	|| newWeapon == CSW_SMOKEGRENADE
	|| newWeapon == CSW_FLASHBANG) {
		// Bad weapon type, does not change.
		//console_print(ifd,"[AMX] ERROR: Bad weapon type (%d is out of bounds)",newWeapon)
		return 0
	}
	else {
		waWeapon = newWeapon
		setWpnString()

		return 1
	}

	return 0
}

public endround_event() {
	randomizedThisRound = false
	hidThisRound = false
}

public SwitchRandom(id) {
	(randomWeapon = !randomWeapon)

	if (randomWeapon) {
		set_hudmessage(0, 100, 0, -1.0, 0.35, 2, 0.02, 10.0, 0.01, 0.1, 3)
		show_hudmessage(0,"Weapon Arena will select random weapon on every new round!")
		client_print(id,print_console,"[AMX] Weapon Arena will select random weapon on every new round!")
		server_print("[AMX] Weapon Arena will select random weapon on every new round!")

		if (!weaponArena) {
			weaponArena = true
		}

		randomize(0)

		giveAllIfHavent()
	}
	else {
		set_hudmessage(0, 100, 0, -1.0, 0.35, 2, 0.02, 10.0, 0.01, 0.1, 3)
		show_hudmessage(0,"Disabled Weapon Arena.")
		client_print(id,print_console,"[AMX] Disabled Weapon Arena.")
		server_print("[AMX] Disabled Weapon Arena.")
		/*
		set_hudmessage(0, 100, 0, -1.0, 0.35, 2, 0.02, 10.0, 0.01, 0.1, 3)
		show_hudmessage(0,"Disabled random weapon on every new round!")
		client_print(id,print_console,"[AMX] Disabled random weapon on every new round!")
		server_print("[AMX] Disabled random weapon on every new round!")
		*/
	}
}

public wamenu1(id) {
	if (!(get_user_flags(id)&ADMIN_CFG)){
		//client_print(id,print_console,"You have no access to that command")
		return PLUGIN_HANDLED
	}
	new disableLine[64]
	if (weaponArena)
		format(disableLine, 63, "8. Disable %c%s Arena^n", weaponString[7] - 32,weaponString[8])

	new menuBody[512]
	new len = format(menuBody, 511, "\yWeapon Arena\R1/4^n^n\w") // \y \R \w
	len += format(menuBody[len],511-len,"\
	1. P228^n\
	2. Scout^n\
	3. XM1014^n\
	4. Mac10^n\
	5. Aug^n\
	6. Elite^n^n\
	7. Random menu...^n\
	%s\
	9. More weapons...^n\
	0. Exit", weaponArena ? disableLine : "")
	new flags = MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON5|MENUBUTTON6|MENUBUTTON7|MENUBUTTON9|MENUBUTTON0

	if (weaponArena)
		flags |= MENUBUTTON8

	show_menu(id, flags, menuBody)

	return PLUGIN_HANDLED
}

public wamenu2(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\yWeapon Arena\R2/4^n^n\w")
#if !defined NO_STEAM
	len += format(menuBody[len],511-len,"\
	1. Fiveseven^n\
	2. UMP45^n\
	3. SG550^n\
	4. Galil^n\
	5. Famas^n\
	6. USP^n\
	7. Glock18^n\
	8. AWM^n\
	^n\
	9. More weapons...^n\
	0. Back")
	new flags = MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON5|MENUBUTTON6|MENUBUTTON7|MENUBUTTON9|MENUBUTTON0
#else
	len += format(menuBody[len],511-len,"\
	1. Fiveseven^n\
	2. UMP45^n\
	3. SG550^n\
	6. USP^n\
	7. Glock18^n\
	8. AWM^n\
	^n\
	9. More weapons...^n\
	0. Back")
	new flags = MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON6|MENUBUTTON7|MENUBUTTON9|MENUBUTTON0
#endif
	show_menu(id, flags, menuBody)

	return PLUGIN_HANDLED
}

public wamenu3(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\yWeapon Arena\R3/4^n^n\w")
	len += format(menuBody[len],511-len,"1. MP5Navy^n2. M249^n3. M3^n4. M4A1^n5. TMP^n6. G3SG1^n7. Desert eagle^n8. SG552^n^n9. More weapons...^n0. Back")
	show_menu(id,((1<<0)|(1<<1)|(1<<2)|(1<<3)|(1<<4)|(1<<5)|(1<<6)|(1<<7)|(1<<8)|(1<<9)),menuBody)

	return PLUGIN_HANDLED
}

public wamenu4(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\yWeapon Arena\R4/4^n^n\w")

#if !defined NO_STEAM
	len += format(menuBody[len],511-len,"\
	1. AK47^n\
	2. Combat knife^n\
	3. P90^n\
	4. Shield^n\
	^n\
	0. Back")
	new flags = MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON0
#else
	len += format(menuBody[len],511-len,"\
	1. AK47^n\
	2. Combat knife^n\
	3. P90^n\
	^n\
	0. Back")
	new flags = MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON0
#endif
	show_menu(id, flags, menuBody)

	return PLUGIN_HANDLED
}

public menu_wa1(id,key)
{
	new bool:chose = true
	switch(key) {
		case MENUSELECT1: changeWeapon(id,CSW_P228)
		case MENUSELECT2: changeWeapon(id,CSW_SCOUT)
		case MENUSELECT3: changeWeapon(id,CSW_XM1014)
		case MENUSELECT4: changeWeapon(id,CSW_MAC10)
		case MENUSELECT5: changeWeapon(id,CSW_AUG)
		case MENUSELECT6: changeWeapon(id,CSW_ELITE)
		case MENUSELECT7: {
			/*
			new bool:weaponArenaState = weaponArena
			SwitchRandom(id)
			if (weaponArena && !weaponArenaState) {
				activate(id)
			}
			chose = false
			*/
			chose = false
			randommain(id)
		}
		case MENUSELECT8: {
			weaponArena = false
			randomWeapon = false
			disabled(id)
			chose = false
		}
		case MENUSELECT9: {
			wamenu2(id)
			chose = false
		}
		case MENUSELECT0: chose = false // Exit, don't do anything
	}

	if (chose) {
		randomWeapon = false
		if (weaponArena)
			changed(id,1)
		else {
			weaponArena = true
			activate(id)
		}
	}

	return PLUGIN_HANDLED
}

public menu_wa2(id,key)
{
	new bool:chose = true
	switch(key) {
		case MENUSELECT1: changeWeapon(id,CSW_FIVESEVEN)
		case MENUSELECT2: changeWeapon(id,CSW_UMP45)
		case MENUSELECT3: changeWeapon(id,CSW_SG550)
#if !defined NO_STEAM
		case MENUSELECT4: changeWeapon(id,CSW_GALIL)
		case MENUSELECT5: changeWeapon(id,CSW_FAMAS)
#endif
		case MENUSELECT6: changeWeapon(id,CSW_USP)
		case MENUSELECT7: changeWeapon(id,CSW_GLOCK18)
		case MENUSELECT8: changeWeapon(id,CSW_AWP)
		case MENUSELECT9: {
			wamenu3(id)
			chose = false
		}
		case MENUSELECT0: {
			wamenu1(id)
			chose = false
		}
	}

	if (chose) {
		randomWeapon = false
		if (weaponArena)
			changed(id,1)
		else {
			weaponArena = true
			activate(id)
		}
	}

	return PLUGIN_HANDLED
}

public menu_wa3(id,key)
{
	new bool:chose = true
	switch(key) {
		case 0: changeWeapon(id,CSW_MP5NAVY)
		case 1: changeWeapon(id,CSW_M249)
		case 2: changeWeapon(id,CSW_M3)
		case 3: changeWeapon(id,CSW_M4A1)
		case 4: changeWeapon(id,CSW_TMP)
		case 5: changeWeapon(id,CSW_G3SG1)
		case 6: changeWeapon(id,CSW_DEAGLE)
		case 7: changeWeapon(id,CSW_SG552)
		case 8: {
			wamenu4(id)
			chose = false
		}
		case 9: {
			wamenu2(id)
			chose = false
		}
	}

	if (chose) {
		randomWeapon = false
		if (weaponArena)
			changed(id,1)
		else {
			weaponArena = true
			activate(id)
		}
	}

	return PLUGIN_HANDLED
}

public menu_wa4(id,key)
{
	new bool:chose = true
	switch(key) {
		case 0: changeWeapon(id,CSW_AK47)
		case 1: changeWeapon(id,CSW_KNIFE)
		case 2: changeWeapon(id,CSW_P90)
		case 3: changeWeapon(id,CSW_SHIELD)
		case 9: {
			wamenu3(id)
			chose = false
		}
	}

	if (chose) {
		randomWeapon = false
		if (weaponArena)
			changed(id,1)
		else {
			weaponArena = true
			activate(id)
		}

		if (waWeapon == CSW_SHIELD) {
			// Check each player. If he has any allowed weapon, make him hold that, else give him a random allowed weapon before we give shield.
			new players[32], playersFound
			get_players(players, playersFound, "a") // No deads
			new clip, ammo, weaponTypeId, allowedShieldWeapons[5] = {CSW_USP, CSW_GLOCK18, CSW_P228, CSW_FIVESEVEN, CSW_DEAGLE}
			for (new i = 0;i < playersFound;i++) {
				weaponTypeId = get_user_weapon(players[i], clip, ammo)
				switch (weaponTypeId) {
					case CSW_USP: client_cmd(players[i], "%s", weaponstrings[weaponTypeId])
					case CSW_GLOCK18: client_cmd(players[i], "%s", weaponstrings[weaponTypeId])
					case CSW_P228: client_cmd(players[i], "%s", weaponstrings[weaponTypeId])
					case CSW_FIVESEVEN: client_cmd(players[i], "%s", weaponstrings[weaponTypeId])
					case CSW_DEAGLE: client_cmd(players[i], "%s", weaponstrings[weaponTypeId])
					default: {
						new randomAllowedWeapon = allowedShieldWeapons[random_num(0, 4)] // Randomize a pistol
						give_item(players[i], weaponstrings[randomAllowedWeapon]) // Give it
						client_cmd(players[i], "%s", weaponstrings[randomAllowedWeapon]) // Make sure to hold it...
					}
				}

				// Check if reload is needed.
				//checkreload(players[i], weaponTypeId)
			}

			// Give in a little bit...
			set_task(1.8, "delayedGiveAll")
		}
	}

	return PLUGIN_HANDLED
}

public delayedGiveAll() {
	client_print(0, print_chat, "(DGA)")
	giveAllIfHavent()
}

show_console(message[], len) {
	replaceall(message, len, "<TR>", "")
	replaceall(message, len, "</TR>", "^n")
	replaceall(message, len, "<TD>", "")
	replaceall(message, len, "</TD>", "^t")
	replaceall(message, len, "<BR>", "^n")
	replaceall(message, len, "<B>", "")
	replaceall(message, len, "</B>", "")
	replaceall(message, len, "<P>", "")
	replaceall(message, len, "</P>", "^n")
	replaceall(message, len, "<HTML>", "")
	replaceall(message, len, "</HTML>", "")
	replaceall(message, len, "<BODY>", "")
	replaceall(message, len, "</BODY>", "")
	replaceall(message, len, "<TABLE>", "")
	replaceall(message, len, "</TABLE>", "")
	replaceall(message, len, "<CENTER>", "")
	replaceall(message, len, "</CENTER>", "")

	// Show max 254? bytes a message...
	const BUFFERSIZE = 200
	new buffer[BUFFERSIZE - 1]
	new pos = 0

	while (strlen(message) > 0) {
		if (contain(message, "^n") == -1) {
			server_print(message)
			break
		}
		copyc(buffer, BUFFERSIZE, message, '^n')
		replace(message, len, buffer, "")
		trim(message, len, 1)
		pos = strlen(buffer)
		buffer[pos] = '^0'
		server_print(buffer)
		//server_print("Length of message is %d", strlen(message))
	}
}

replaceall(text[], const LEN, const WHAT[], const WITH[]) {
	while (contain(text, WHAT) != -1)
		replace(text, LEN, WHAT, WITH)
}

trim(stringtotrim[], const LEN, charstotrim, const bool:FROMLEFT = true) {
	if (charstotrim <= 0)
		return

	if (FROMLEFT) {
		new maxlen = strlen(stringtotrim)
		if (charstotrim > maxlen)
			charstotrim = maxlen

		format(stringtotrim, LEN, "%s", stringtotrim[charstotrim])
	}
	else {
		new maxlen = strlen(stringtotrim) - charstotrim
		if (maxlen < 0)
			maxlen = 0

		format(stringtotrim, maxlen, "%s", stringtotrim)
	}
}

/*public randommenustarter(id) {
	randommain(id)

	return PLUGIN_HANDLED
}*/

getwpnflag(const WEAPON) {
	for (new i = 1, j = 0; i <= 30; i++) {
		if (i == CSW_C4 || i == CSW_SHIELD || i == CSW_FLASHBANG || i == CSW_SMOKEGRENADE || i == CSW_HEGRENADE)
			continue
#if defined NO_STEAM
		if (i == CSW_GALIL || i == CSW_FAMAS)
			continue
#endif
		if (i == WEAPON)
			return (1<<j)
		j++
	}

	return -1
}

getactive(const WEAPON) {
	if (g_randomflags & getwpnflag(WEAPON))
		return 1

	return 0
}

public randommain(id) {
	new menuBody[512]
	new len = format(menuBody, 511, "\ySelect weapons to be enabled in random mode^n^n\w")

	new bool:saveneeded
	if (!vaultdata_exists(VAULTKEY_RANDOM) || g_randomflags != get_vaultdata(VAULTKEY_RANDOM))
		saveneeded = true
	else
		saveneeded = false

	new const SAVECHANGES[] = "7. Save changes to vault"

	len += format(menuBody[len], 511-len, "\
	1. Pistols^n\
	2. Shotguns and machine guns^n\
	3. Sub machine guns^n\
	4. Rifles^n\
	5. Snipers^n\
	6. Knife\R%s^n\
	%s^n\
	8. Back to main menu...^n\
	%s\
	0. Exit", BLED[getactive(CSW_KNIFE)], saveneeded == true ? SAVECHANGES : "\d7. Save changes to vault (not needed)\w", !randomWeapon ? "9. Activate random mode^n" : "")

	new flags = MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON5|MENUBUTTON6|MENUBUTTON8|MENUBUTTON0

	if (!randomWeapon)
		flags |= MENUBUTTON9

	if (saveneeded)
		flags |= MENUBUTTON7

	show_menu(id, flags, menuBody)

	return PLUGIN_HANDLED
}

public menu_randommain(id, key) {
	new bool:stayInMenu = false
	switch(key) {
		case MENUSELECT1: menu_pistols(id)
		case MENUSELECT2: menu_shotmg(id)
		case MENUSELECT3: menu_smg(id)
		case MENUSELECT4: menu_rifles(id)
		case MENUSELECT5: menu_snipers(id)
		case MENUSELECT6: {
			alterobject(id, CSW_KNIFE)
			stayInMenu = true
		}
		case MENUSELECT7: {
			updatevault(id, true)
			stayInMenu = true
		}
		case MENUSELECT8: wamenu1(id)
		case MENUSELECT9: {
			new bool:weaponArenaState = weaponArena
			SwitchRandom(id)
			if (weaponArena && !weaponArenaState) {
				activate(id)
			}
		}
	}
	if (stayInMenu)
		randommain(id)

	return PLUGIN_HANDLED
}

menu_pistols(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\ySelect pistols^n^n\w")
	len += format(menuBody[len],511-len,"\
	1. Sig Sauer P228\R%s^n\
	2. Dual Beretta 96G Elites\R%s^n\
	3. Fabrique Nationale Five-seven\R%s^n\
	4. Heckler & Koch USP45\R%s^n\
	5. Glock 18\R%s^n\
	6. IMI Desert Eagle\R%s^n\
	^n\
	0. Back to main menu", BLED[getactive(CSW_P228)], BLED[getactive(CSW_ELITE)], BLED[getactive(CSW_FIVESEVEN)], BLED[getactive(CSW_USP)], BLED[getactive(CSW_GLOCK18)], BLED[getactive(CSW_DEAGLE)])
	new flags = (MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON5|MENUBUTTON6|MENUBUTTON0)
//	0. Back to main menu", randomweapons[CSW_P228], randomweapons[CSW_ELITE], randomweapons[CSW_FIVESEVEN], randomweapons[CSW_USP], randomweapons[CSW_GLOCK18], randomweapons[CSW_DEAGLE])

	return show_menu(id, flags, menuBody) // Just odd, shouldn't need to return a value, but the #¤!#%@ compiler is complaining...
}

alterobject(id, const WEAPON) {
	id++ // not used
	new const WPNFLAG = getwpnflag(WEAPON)
	if (g_randomflags & WPNFLAG)
		g_randomflags &= ~WPNFLAG
	else
		g_randomflags |= WPNFLAG
}

updatevault(id, bool:fromMenu) {
	new message[96]

	new flagsstring[64]
	num_to_str(g_randomflags, flagsstring, 63)
	if (set_vaultdata(VAULTKEY_RANDOM, flagsstring))
		format(message, 95, "%s: Successfully saved current random list to vault.", PLUGINNAME)
	else
		format(message, 95, "%s: ERROR - Failed while trying to save current random list to vault!", PLUGINNAME)

	if (fromMenu)
		client_print(id, print_chat, message)
	else
		console_print(id, message)
}

public menu_ws_pistols(id,key)
{
	new bool:stayInMenu = true

	switch (key) {
		case MENUSELECT1: alterobject(id, CSW_P228)
		case MENUSELECT2: alterobject(id, CSW_ELITE)
		case MENUSELECT3: alterobject(id, CSW_FIVESEVEN)
		case MENUSELECT4: alterobject(id, CSW_USP)
		case MENUSELECT5: alterobject(id, CSW_GLOCK18)
		case MENUSELECT6: alterobject(id, CSW_DEAGLE)
		case MENUSELECT0: {
			stayInMenu = false
			randommain(id)
		}
	}

	if (stayInMenu)
		menu_pistols(id)

	return PLUGIN_HANDLED
}

menu_shotmg(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\ySelect shotguns and machine guns^n^n\w")
	len += format(menuBody[len],511-len,"\
	1. Benelli M3 Super 90\R%s^n\
	2. Benelli XM1014\R%s^n\
	3. FN M249 Para\R%s^n\
	^n\
	0. Back to main menu", BLED[getactive(CSW_M3)], BLED[getactive(CSW_XM1014)], BLED[getactive(CSW_M249)])
	new flags = (MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON0)
	//0. Back to main menu", randomweapons[CSW_M3], randomweapons[CSW_XM1014], randomweapons[CSW_M249])

	show_menu(id, flags, menuBody)
}

public menu_ws_shotmg(id,key)
{
	new bool:stayInMenu = true

	switch(key) {
		case MENUSELECT1: alterobject(id, CSW_M3)
		case MENUSELECT2: alterobject(id, CSW_XM1014)
		case MENUSELECT3: alterobject(id, CSW_M249)
		case MENUSELECT0: {
			stayInMenu = false
			randommain(id)
		}
	}

	if (stayInMenu)
		menu_shotmg(id)

	return PLUGIN_HANDLED
}

menu_smg(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\ySelect sub machine guns^n^n\w")
	len += format(menuBody[len],511-len,"\
	1. Heckler & Koch UMP45\R%s^n\
	2. Heckler & Koch MP5/Navy\R%s^n\
	3. Steyr Tactical Machine Pistol\R%s^n\
	4. Fabrique Nationale P90\R%s^n\
	5. Ingram Mac-10\R%s^n\
	^n\
	0. Back to main menu", BLED[getactive(CSW_UMP45)], BLED[getactive(CSW_MP5NAVY)], BLED[getactive(CSW_TMP)], BLED[getactive(CSW_P90)], BLED[getactive(CSW_MAC10)])
	//0. Back to main menu", randomweapons[CSW_UMP45], randomweapons[CSW_MP5NAVY], randomweapons[CSW_TMP], randomweapons[CSW_P90], randomweapons[CSW_MAC10])
	new flags = (MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON5|MENUBUTTON0)

	show_menu(id, flags, menuBody)
}

public menu_ws_smg(id,key)
{
	new bool:stayInMenu = true

	switch(key) {
		case MENUSELECT1: alterobject(id, CSW_UMP45)
		case MENUSELECT2: alterobject(id, CSW_MP5NAVY)
		case MENUSELECT3: alterobject(id, CSW_TMP)
		case MENUSELECT4: alterobject(id, CSW_P90)
		case MENUSELECT5: alterobject(id, CSW_MAC10)
		case MENUSELECT0: {
			stayInMenu = false
			randommain(id)
		}
	}

	if (stayInMenu)
		menu_smg(id)

	return PLUGIN_HANDLED
}

menu_rifles(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\ySelect rifles^n^n\w")

#if !defined NO_STEAM
	len += format(menuBody[len],511-len,"\
	1. Galil\R%s^n\
	2. Famas\R%s^n\
	3. Colt M4A1\R%s^n\
	4. Avtomat Kalashnikov AK-47\R%s^n\
	5. Sig SG-552 Commando\R%s^n\
	6. Steyr Aug\R%s^n\
	^n\
	0. Back to main menu", BLED[getactive(CSW_GALIL)], BLED[getactive(CSW_FAMAS)], BLED[getactive(CSW_M4A1)], BLED[getactive(CSW_AK47)], BLED[getactive(CSW_SG552)], BLED[getactive(CSW_AUG)])
	//0. Back to main menu", randomweapons[CSW_GALIL], randomweapons[CSW_FAMAS], randomweapons[CSW_M4A1], randomweapons[CSW_AK47], randomweapons[CSW_SG552], randomweapons[CSW_AUG])

#else
	len += format(menuBody[len],511-len,"\
	3. Colt M4A1\R%s^n\
	4. Avtomat Kalashnikov AK-47\R%s^n\
	5. Sig SG-552 Commando\R%s^n\
	6. Steyr Aug\R%s^n\
	^n\
	0. Back to main menu", BLED[getactive(CSW_M4A1)], BLED[getactive(CSW_AK47)], BLED[getactive(CSW_SG552)], BLED[getactive(CSW_AUG)])
	//0. Back to main menu", randomweapons[CSW_M4A1], randomweapons[CSW_AK47], randomweapons[CSW_SG552], randomweapons[CSW_AUG])
#endif

	new flags = (MENUBUTTON3|MENUBUTTON4|MENUBUTTON5|MENUBUTTON6|MENUBUTTON0)
#if !defined NO_STEAM
	flags |= MENUBUTTON1|MENUBUTTON2
#endif
	show_menu(id, flags, menuBody)
}

public menu_ws_rifles(id,key)
{
	new bool:stayInMenu = true

	switch(key) {
		case MENUSELECT1: alterobject(id, CSW_GALIL)
		case MENUSELECT2: alterobject(id, CSW_FAMAS)
		case MENUSELECT3: alterobject(id, CSW_M4A1)
		case MENUSELECT4: alterobject(id, CSW_AK47)
		case MENUSELECT5: alterobject(id, CSW_SG552)
		case MENUSELECT6: alterobject(id, CSW_AUG)
		case MENUSELECT0: {
			stayInMenu = false
			randommain(id)
		}
	}

	if (stayInMenu)
		menu_rifles(id)

	return PLUGIN_HANDLED
}

menu_snipers(id) {
	new menuBody[512]
	new len = format(menuBody,511,"\ySelect snipers^n^n\w")
	len += format(menuBody[len],511-len,"\
	1. Steyr Scout\R%s^n\
	2. Accuracy Int. Arctic Warfare/Magnum (AW/M)\R%s^n\
	3. Heckler & Koch G3/SG-1\R%s^n\
	4. Sig SG-550 Commando\R%s^n\
	^n\
	0. Back to main menu", BLED[getactive(CSW_SCOUT)], BLED[getactive(CSW_AWP)], BLED[getactive(CSW_G3SG1)], BLED[getactive(CSW_SG550)])
	//0. Back to main menu", randomweapons[CSW_SCOUT], randomweapons[CSW_AWP], randomweapons[CSW_G3SG1], randomweapons[CSW_SG550])

	new flags = (MENUBUTTON1|MENUBUTTON2|MENUBUTTON3|MENUBUTTON4|MENUBUTTON0)

	show_menu(id, flags, menuBody)
}

public menu_ws_snipers(id,key)
{
	new bool:stayInMenu = true

	switch(key) {
		case MENUSELECT1: alterobject(id, CSW_SCOUT)
		case MENUSELECT2: alterobject(id, CSW_AWP)
		case MENUSELECT3: alterobject(id, CSW_G3SG1)
		case MENUSELECT4: alterobject(id, CSW_SG550)
		case MENUSELECT0: {
			stayInMenu = false
			randommain(id)
		}
	}

	if (stayInMenu)
		menu_snipers(id)

	return PLUGIN_HANDLED
}

public handledrop(id) {
	if (!weaponArena || get_cvar_num("weaponarena_invisibleweapons") == 0)
		return PLUGIN_CONTINUE

	new clip, ammo
	new droppingweapontype = get_user_weapon(id,clip,ammo)
	// Block all weapons that really can't be dropped here.
	// C4 can be dropped, but we don't want to process anything for it since it can't level. :-)
	if (droppingweapontype != waWeapon
	&& droppingweapontype != CSW_C4) {
		new ent[1]
		ent[0] = find_wpnent_fast(id, droppingweapontype)
		if (!ent[0])
			client_print(id, print_chat, "Error, couldn't find the weapon (type %d) you dropped to hide it...", droppingweapontype)
		else
			set_task(0.1, "HideOneWeapon", ent[0], ent, 1)
	}

	return PLUGIN_CONTINUE
}

public death_event() {
	// Delay hide...
	if (weaponArena && get_cvar_num("weaponarena_invisibleweapons") == 1)
		set_task(0.1, "HideWeapons")

	return PLUGIN_CONTINUE
}

pad(outstring[], const MAXLENGTH, const FROMSTRING[], const PROPERLENGTH, const PADCHARACTER, const bool:AFTER = true) {
	if (PROPERLENGTH > MAXLENGTH)
		return
	format(outstring, MAXLENGTH, "%s", FROMSTRING)
	new len = strlen(outstring)
	if (len == PROPERLENGTH)
		return

	if (AFTER)
		while (format(outstring, MAXLENGTH, "%s%c", outstring, PADCHARACTER) < PROPERLENGTH) {} // left justify
	else
		while (format(outstring, MAXLENGTH, "%c%s", PADCHARACTER, outstring) < PROPERLENGTH) {} // right justify
}

public listrandomweapons(id) {
	new buffer[2048]
	new pos = 0
	new const MAXSIZE = (sizeof buffer) - 1
	pos += format(buffer[pos], MAXSIZE - pos, "<HTML><BODY><P><B>Weapons activated for random mode:</B></P>")
	new wpnstring[20]
	pad(wpnstring, 19, "Weapon", 9, ' ', true) // false to right-justify
	if (id == 0)
		pos += format(buffer[pos], MAXSIZE - pos, "<P><TABLE><TR><TD>%s</TD><TD>status</TD><TD>letter</TD></TR>", wpnstring)
	else
		pos += format(buffer[pos], MAXSIZE - pos, "<P><TABLE><TR><TD>%s</TD><TD>status</TD></TR>", wpnstring)
	for (new i = 1, j = 0; i <= 30; i++) {
		if (i == CSW_C4 || i == CSW_SHIELD || i == CSW_FLASHBANG || i == CSW_SMOKEGRENADE || i == CSW_HEGRENADE)
			continue
#if defined NO_STEAM
		if (i == CSW_GALIL || i == CSW_FAMAS)
			continue
#endif

		// fiveseven = 9 chars
		pad(wpnstring, 19, weaponstrings[i][7], 9, ' ', true) // false to right-justify

		//format(buffer, 1023, "%s<B>%s</B>^t=^t%s<BR>", buffer, weaponstrings[i], randomweapons[i] ? "on" : "off")
		if (id == 0)
			pos += format(buffer[pos], MAXSIZE - pos, "<TR><TD>%s</TD><TD>%s</TD><TD>%c</TD></TR>", wpnstring, g_randomflags & 1<<j ? BLED[1] : BLED[0], j + 97)
		else
			pos += format(buffer[pos], MAXSIZE - pos, "<TR><TD>%s</TD><TD>%s</TD></TR>", wpnstring, g_randomflags & 1<<j ? BLED[1] : BLED[0])
		j++
	}
	pos += format(buffer[pos], MAXSIZE - pos, "</TABLE></P></BODY></HTML>")

	if (id == 0)
		show_console(buffer, MAXSIZE)
	else {
		show_motd(id, buffer, "Weapon Arena")
	}

	return PLUGIN_HANDLED
}

public setrandoms(id, level, cid) {
	if (!cmd_access(id, level, cid, 2)) {
		return PLUGIN_HANDLED
	}

	new randoms[128]
	read_argv(1, randoms, 127)
	g_randomflags = read_flags(randoms)
	console_print(id, "%s: Updated random list.", PLUGINNAME)
	//new gotflags[128]
	//get_flags(g_randomflags, gotflags, 127)

	//if (vaultdata_exists
	//server_print("randoms: %s", randoms)
	//server_print("gotflags: %s", gotflags)
	//server_print("Randomflags is: %d", g_randomflags)

	// Store this new value in vault if 2nd parameter is "save"
	new argcount = read_argc()
	if (argcount < 3)
		return PLUGIN_HANDLED

	new arg[5]
	read_argv(2, arg, 4)
	if (equali(arg, "save"))
		updatevault(id, false)

	return PLUGIN_HANDLED
}

public plugin_init() {
	register_plugin(PLUGINNAME, VERSION, AUTHOR)

	register_logevent("death_event", 5, "1=killed", "3=with")

	register_event("ResetHUD","newround_event","b")
	register_event("CurWeapon","holdwpn_event","be","1=1")
	register_event("SendAudio","endround_event","a","2&%!MRAD_terwin","2&%!MRAD_ctwin","2&%!MRAD_rounddraw")
	register_event("TextMsg","endround_event","a","2&#Game_C","2&#Game_w")
	register_event("TextMsg","endround_event","a","2&#Game_will_restart_in")

	register_concmd("weaponarena","toggle", ADMIN_CFG, "<weaponnumber|off|list|random>")
	register_concmd("weaponarena_random", "setrandoms", ADMIN_CFG, "<abcd...> [save] - Set what weapons will be available in random mode (this will override any previous setting, add save as last parameter to optionally save this)")
	register_concmd("weaponarena_randomlist", "listrandomweapons", 0, "- lists all weapons enabled for random mode")

	register_clcmd("weaponarena_menu","wamenu1", ADMIN_CFG,"- displays Weapon Arena's menu")
	//register_clcmd("weaponarena_randommenu", "randommenustarter", ADMIN_CFG, "- add/remove weapons enabled in random mode")
	register_clcmd("drop", "handledrop")

//#if defined NO_STEAM
	/*register_menucmd(register_menuid("Weapon Arena 1/3"),1023,"menu_wa1")
	register_menucmd(register_menuid("Weapon Arena 2/3"),1023,"menu_wa2")
	register_menucmd(register_menuid("Weapon Arena 3/3"),1023,"menu_wa3")*/
//#else
	register_menucmd(register_menuid("Weapon Arena\R1/4"), 1023, "menu_wa1")
	register_menucmd(register_menuid("Weapon Arena\R2/4"), 1023, "menu_wa2")
	register_menucmd(register_menuid("Weapon Arena\R3/4"), 1023, "menu_wa3")
	register_menucmd(register_menuid("Weapon Arena\R4/4"), 1023, "menu_wa4")
//#endif
	register_menucmd(register_menuid("Select weapons to be enabled in random mode"), 1023, "menu_randommain")
	register_menucmd(register_menuid("Select pistols"), 1023, "menu_ws_pistols")
	register_menucmd(register_menuid("Select shotguns and machine guns"), 1023, "menu_ws_shotmg")
	register_menucmd(register_menuid("Select sub machine guns"), 1023, "menu_ws_smg")
	register_menucmd(register_menuid("Select rifles"), 1023, "menu_ws_rifles")
	register_menucmd(register_menuid("Select snipers"), 1023, "menu_ws_snipers")

	register_cvar("weaponarena_version",VERSION,FCVAR_SERVER|FCVAR_SPONLY)
	register_cvar("weaponarena_unlimitedammo", "1")
	register_cvar("weaponarena_invisibleweapons", "1")
	register_cvar("weaponarena_delay",DEFAULTDELAY)

	weaponArena = false
	changeWeapon(0, CSW_FIVESEVEN) // Default weapon...

	if (vaultdata_exists(VAULTKEY_RANDOM))
		g_randomflags = get_vaultdata(VAULTKEY_RANDOM)
}
