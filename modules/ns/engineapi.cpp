// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Natural Selection Module
//

/* Calls going from the game dll to the engine are handled here */

#include "amxxmodule.h"
#include "ns.h"
#include "utilfunctions.h"
#include "GameManager.h"
#include "ParticleManager.h"
#include "CPlayer.h"


// Parse log messages here for any desired information (structure_built, etc.)
// The following logs are needed:
// "sawce<1><STEAM_0:1:4560311><alien1team>" triggered "structure_built" (type "defensechamber")`
void AlertMessage_Post(ALERT_TYPE atype, const char *szFmt, ...)
{
	if (atype != at_logged)
	{
		RETURN_META(MRES_IGNORED);
	}

	char *MessageStart; // original pointer to start of the message
	char *TypePointer;  // pointer to the structure type
	char *CIDPointer;   // pointer to the name text

	int CID;            // connection ID of the player

	va_list LogArgs;

	va_start(LogArgs,szFmt);
	MessageStart=va_arg(LogArgs,char *);
	va_end(LogArgs);

	if (MessageStart==NULL) // Somehow got a null pointer, get out of here now
	{
		RETURN_META(MRES_IGNORED);
	}


	if ((TypePointer=strstr(MessageStart,"\"structure_built\""))==NULL) // was not found
	{
		RETURN_META(MRES_IGNORED);
	}

	if (*(TypePointer - 2) != 'd') // If this is not from a 'triggered "structure_built"', then ignore the message
	{
		RETURN_META(MRES_IGNORED);
	}

	// If we got here, then for all we can tell this message is good

	// Move up a few spaces
	TypePointer+=25; // strlen("\"structure_built\" (type \"")=25

	// Now get the player's CID
	CIDPointer=MessageStart+1; // skip over the very first quotation mark

	while (*CIDPointer++ != '"') /*do nothing*/;

	--CIDPointer;

	// Move back past three <

	while (*CIDPointer-- != '<') /* do nothing*/;
	while (*CIDPointer-- != '<') /* do nothing*/;
	while (*CIDPointer-- != '<') /* do nothing*/;

	++CIDPointer;

	// now skip past the <
	++CIDPointer;

	// We now point to the CID string, atoi will stop at the > so just atoi it for the CID
	CID=atoi(CIDPointer);

	CPlayer *Player;

	if ((Player=UTIL_PlayerByCID(CID))==NULL)
	{
		RETURN_META(MRES_IGNORED);
	}

	// list of what impulses represent what type of structure building
	// use 
	// 0 for unknown (shouldn't be used ever)
	// 1 for marine
	// 2 for alien

	// I'm marking the upgrades as marine structures just incase
	// they should never be used though!
	static int StructureTypes[128] = 
	{
		
		0, // 0  = unknown
		0, // 1  = next weapon
		0, // 2  = reload
		0, // 3  = drop weapon
		0, // 4  = unknown
		0, // 5  = unknown
		0, // 6  = unknown
		0, // 7  = radio comm
		0, // 8  = radio comm
		0, // 9  = radio comm

		0, // 10 = radio comm
		0, // 11 = radio comm
		0, // 12 = radio comm
		0, // 13 = radio comm
		0, // 14 = radio comm
		0, // 15 = radio comm
		0, // 16 = unknown
		0, // 17 = unknown
		0, // 18 = unknown
		0, // 19 = unknown
		
		1, // 20 = armor 1
		1, // 21 = armor 2
		1, // 22 = armor 3
		1, // 23 = weapons 1
		1, // 24 = weapons 2
		1, // 25 = weapons 3
		1, // 26 = siege upgrade
		1, // 27 = drop catalyst
		1, // 28 = research jp
		1, // 29 = research ha

		1, // 30 = distress beacon
		1, // 31 = resupply (combat)
		0, // 32 = unknown
		1, // 33 = motion tracking
		1, // 34 = phase gates upgrade
		0, // 35 = unknown
		1, // 36 = electricity upgrade
		1, // 37 = handgrenades upgrade
		1, // 38 = drop jetpack
		1, // 39 = drop heavy armor

		1, // 40 = infantry portal
		1, // 41 = marine RT
		0, // 42 = unused
		1, // 43 = turret factory
		0, // 44 = unused
		1, // 45 = arms lab
		1, // 46 = proto lab
		1, // 47 = upgrade
		1, // 48 = armory
		1, // 49 = advanced armory

		0, // 50 = unknown
		1, // 51 = observatory
		0, // 52 = unknown
		1, // 53 = scanner sweep
		0, // 54 = unknown
		1, // 55 = build phase gate
		1, // 56 = build turret
		1, // 57 = build siege turret
		1, // 58 = build command chair
		1, // 59 = drop health pack

		1, // 60 = drop ammo pack
		1, // 61 = drop mine pack
		1, // 62 = drop welder
		0, // 63 = unknown
		1, // 64 = drop shotgun
		1, // 65 = drop heavymachinegun
		1, // 66 = drop grenadelauncher
		0, // 67 = unknown
		0, // 68 = unknown
		0, // 69 = unknown

		0, // 70 = unknown
		0, // 71 = unknown
		0, // 72 = unknown
		0, // 73 = unknown
		0, // 74 = unknown
		0, // 75 = unknown
		0, // 76 = unknown
		0, // 77 = unknown
		0, // 78 = unknown
		0, // 79 = unknown

		0, // 80 = radio comm
		0, // 81 = radio comm
		1, // 82 = commander message
		0, // 83 = commander message
		0, // 84 = commander message
		0, // 85 = unknown
		0, // 86 = unknown
		0, // 87 = unknown
		0, // 88 = unknown
		0, // 89 = unknown

		2, // 90 = alienresourcetower
		2, // 91 = offensechamber
		2, // 92 = defensechamber
		2, // 93 = sensorychamber
		2, // 94 = movementchamber
		2, // 95 = team_hive
		0, // 96 = unknown
		0, // 97 = unknown
		0, // 98 = unknown
		0, // 99 = unknown

		0, // 100 = unknown
		2, // 101 = carapace
		2, // 102 = regeneration
		2, // 103 = redemption
		0, // 104 = unknown
		1, // 105 = select all marines
		0, // 106 = unknown
		2, // 107 = celerity
		2, // 108 = adrenaline
		2, // 109 = silence

		2, // 110 = cloaking
		2, // 111 = focus
		2, // 112 = scent of fear
		2, // 113 = skulk
		2, // 114 = gorge
		2, // 115 = lerk
		2, // 116 = fade
		2, // 117 = onos
		2, // 118 = unlock next ability (combat)
		0, // 119 = unknown

		0, // 120 = unknown
		0, // 121 = unknown
		0, // 122 = unknown
		0, // 123 = unknown
		0, // 124 = unknown
		0, // 125 = unknown
		2, // 126 = unlock next ability (combat)
		0  // 127 = unknown
	};

	int impulse=Player->GetPev()->impulse;
	if (impulse < 0 || impulse > 127)
	{
		RETURN_META(MRES_IGNORED);
	}

	if (impulse==95/*hive*/)
	{
		GameMan.ExecuteClientBuilt(Player->index(), UTIL_FindBuildingHive(), StructureTypes[impulse], impulse);
	}
	else
	{
		GameMan.ExecuteClientBuilt(Player->index(), ENTINDEX_NEW(GameMan.GetTemporaryEdict()), StructureTypes[impulse], impulse);
	}

	RETURN_META(MRES_IGNORED);
}

// We hook newly created entities here.
// This is where we check for client_built created entities.
edict_t* CreateNamedEntity_Post(int className)
{
	if (GameMan.IsCombat()) // this shouldn't be called during co, just incase
	{
		RETURN_META_VALUE(MRES_IGNORED,0);
	}

	// Incase another plugin supercedes/overrides, use their returned value here.
	// (Untested).
	if (gpMetaGlobals->status >= MRES_OVERRIDE)
	{
		GameMan.SetTemporaryEdict(META_RESULT_OVERRIDE_RET(edict_t *));
	}
	else
	{
		GameMan.SetTemporaryEdict(META_RESULT_ORIG_RET(edict_t *));
	}
	RETURN_META_VALUE(MRES_IGNORED,0);
}

unsigned short PrecacheEvent_Post(int type, const char *psz)
{
	ParticleMan.PrecacheEvent(psz);
	RETURN_META_VALUE(MRES_IGNORED,0);
}
