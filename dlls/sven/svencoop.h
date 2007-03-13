/* AMX Mod X
   *   Sven Co-op Module
   *
   * by the AMX Mod X Development Team
   *
   * This file is part of AMX Mod X.
   *
   *
   *  This program is free software; you can redistribute it and/or modify it
   *  under the terms of the GNU General Public License as published by the
   *  Free Software Foundation; either version 2 of the License, or (at
   *  your option) any later version.
   *
   *  This program is distributed in the hope that it will be useful, but
   *  WITHOUT ANY WARRANTY; without even the implied warranty of
   *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   *  General Public License for more details.
   *
   *  You should have received a copy of the GNU General Public License
   *  along with this program; if not, write to the Free Software Foundation,
   *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
   *
   *  In addition, as a special exception, the author gives permission to
   *  link the code of this program with the Half-Life Game Engine ("HL
   *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
   *  L.L.C ("Valve"). You must obey the GNU General Public License in all
   *  respects for all of the code used other than the HL Engine and MODs
   *  from Valve. If you modify this file, you may extend this exception
   *  to your version of the file, but you are not obligated to do so. If
   *  you do not wish to do so, delete this exception statement from your
   *  version.
   */

#include <extdll.h>
#include <meta_api.h>
#include "amxxmodule.h"
#include "CString.h"
#include "Trie.h"

// right now there is no Linux Sven Co-op, but for the future...
#if defined __linux__
	#define EXTRAOFFSET_MONSTER			0
	#define EXTRAOFFSET_WEAPON			0
	#define EXTRAOFFSET_WBOX			0
	#define EXTRAOFFSET_PLAYER			0
#else
	#define EXTRAOFFSET_MONSTER			0
	#define EXTRAOFFSET_WEAPON			0
	#define EXTRAOFFSET_WBOX			0
	#define EXTRAOFFSET_PLAYER			0
#endif // defined __linux__

// 32-bit
#if !defined __amd64__
	#define OFFSET_LAST_HEALTH			834 // arbitrary, our own storage

	// monster offsets
	#define OFFSET_MONSTER_FRAGS		 9 + EXTRAOFFSET_MONSTER
	#define OFFSET_MONSTER_ALLY			10 + EXTRAOFFSET_MONSTER

	// player offsets (non-ammo)
	#define OFFSET_PLAYER_DEATHS		8624 + EXTRAOFFSET_PLAYER

	// weapon offsets
	#define OFFSET_WEAPON_TYPE			8224 + EXTRAOFFSET_WEAPON
	#define OFFSET_WEAPON_CLIP			8233 + EXTRAOFFSET_WBOX
	#define OFFSET_WBOX_AMMO			8255 + EXTRAOFFSET_WBOX

	// ammo offsets
	#define OFFSET_357_AMMO				8563 + EXTRAOFFSET_PLAYER
	#define OFFSET_9MM_AMMO				8557 + EXTRAOFFSET_PLAYER
	#define OFFSET_CROSSBOW_AMMO		8566 + EXTRAOFFSET_PLAYER
	#define OFFSET_SHOTGUN_AMMO			8558 + EXTRAOFFSET_PLAYER
	#define OFFSET_RPG_AMMO				8565 + EXTRAOFFSET_PLAYER
	#define OFFSET_ENERGY_AMMO			8564 + EXTRAOFFSET_PLAYER
	#define OFFSET_HORNETGUN_AMMO		8571 + EXTRAOFFSET_PLAYER
	#define OFFSET_HANDGRENADE_AMMO		8569 + EXTRAOFFSET_PLAYER
	#define OFFSET_TRIPMINE_AMMO		8567 + EXTRAOFFSET_PLAYER
	#define OFFSET_SATCHEL_AMMO			8568 + EXTRAOFFSET_PLAYER
	#define OFFSET_SNARK_AMMO			8570 + EXTRAOFFSET_PLAYER
	#define OFFSET_MEDKIT_AMMO			8559 + EXTRAOFFSET_PLAYER
	#define OFFSET_MINIGUN_AMMO			8560 + EXTRAOFFSET_PLAYER
	#define OFFSET_SNIPERRIFLE_AMMO		8561 + EXTRAOFFSET_PLAYER
	#define OFFSET_ARGRENADE_AMMO		8562 + EXTRAOFFSET_PLAYER

// 64-bit (no amd64 Sven Co-op, but thinking ahead)
#else
	#define OFFSET_OLD_HEALTH			834

	// monster offsets
	#define OFFSET_MONSTER_FRAGS		 9 + EXTRAOFFSET_MONSTER
	#define OFFSET_MONSTER_ALLY			10 + EXTRAOFFSET_MONSTER

	// player offsets (non-ammo)
	#define OFFSET_PLAYER_DEATHS		8624 + EXTRAOFFSET_PLAYER

	// weapon offsets
	#define OFFSET_WEAPON_TYPE			8224 + EXTRAOFFSET_WEAPON
	#define OFFSET_WEAPON_CLIP			8233 + EXTRAOFFSET_WBOX
	#define OFFSET_WBOX_AMMO			8255 + EXTRAOFFSET_WBOX

	// ammo offsets
	#define OFFSET_357_AMMO				8563 + EXTRAOFFSET_PLAYER
	#define OFFSET_9MM_AMMO				8557 + EXTRAOFFSET_PLAYER
	#define OFFSET_CROSSBOW_AMMO		8566 + EXTRAOFFSET_PLAYER
	#define OFFSET_SHOTGUN_AMMO			8558 + EXTRAOFFSET_PLAYER
	#define OFFSET_RPG_AMMO				8565 + EXTRAOFFSET_PLAYER
	#define OFFSET_ENERGY_AMMO			8564 + EXTRAOFFSET_PLAYER
	#define OFFSET_HORNETGUN_AMMO		8571 + EXTRAOFFSET_PLAYER
	#define OFFSET_HANDGRENADE_AMMO		8569 + EXTRAOFFSET_PLAYER
	#define OFFSET_TRIPMINE_AMMO		8567 + EXTRAOFFSET_PLAYER
	#define OFFSET_SATCHEL_AMMO			8568 + EXTRAOFFSET_PLAYER
	#define OFFSET_SNARK_AMMO			8570 + EXTRAOFFSET_PLAYER
	#define OFFSET_MEDKIT_AMMO			8559 + EXTRAOFFSET_PLAYER
	#define OFFSET_MINIGUN_AMMO			8560 + EXTRAOFFSET_PLAYER
	#define OFFSET_SNIPERRIFLE_AMMO		8561 + EXTRAOFFSET_PLAYER
	#define OFFSET_ARGRENADE_AMMO		8562 + EXTRAOFFSET_PLAYER
#endif

// weapon ids
enum
{
	SCW_CROWBAR = 1,
	SCW_9MMHANDGUN = 2,
	SCW_357 = 3,
	SCW_9MMAR = 4,
	SCW_CROSSBOW = 6,
	SCW_SHOTGUN = 7,
	SCW_RPG = 8,
	SCW_GAUSS = 9,
	SCW_EGON = 10,
	SCW_HORNETGUN = 11,
	SCW_HANDGRENADE = 12,
	SCW_TRIPMINE = 13,
	SCW_SATCHEL = 14,
	SCW_SNARK = 15,
	SCW_UZIAKIMBO = 16,
	SCW_UZI = 17,
	SCW_MEDKIT = 18,
	SCW_CROWBAR_ELECTRIC = 19,
	SCW_PIPEWRENCH = 20,
	SCW_MINIGUN = 21,
	SCW_GRAPPLE = 22,
	SCW_SNIPERRIFLE = 23,
	SCW_ARGRENADE = 24
};

//
// externs
//

extern AMX_NATIVE_INFO svencoop_Exports[];
extern int gmsgScoreInfo;
extern KeyValueData g_kvd;
extern Trie<char,String> g_allyNameTrie, g_enemyNameTrie;

//
// inline functions
//

inline bool UTIL_IsPlayer(edict_t *pPlayer)
{
	if(strcmp(STRING(pPlayer->v.classname), "player") == 0)
		return true;

	return false;
}

inline bool UTIL_IsMonster(edict_t *pMonster)
{
	if(strncmp(STRING(pMonster->v.classname), "monster_", 8) == 0)
		return true;

	return false;
}

//
// entity checking defines
//

#define CHECK_ENTITY(x) \
	if (x <= 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if (x != 0 && FNullEnt(INDEXENT(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}

#define CHECK_PLAYER(x) \
	if (x < 1 || x > gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player out of range (%d)", x); \
		return 0; \
	} else { \
		if (!MF_IsPlayerIngame(x) || FNullEnt(MF_GetPlayerEdict(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d", x); \
			return 0; \
		} \
	}

#define CHECK_NONPLAYER(x) \
	if (x < 1 || x <= gpGlobals->maxClients || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Non-player entity %d out of range", x); \
		return 0; \
	} else { \
		if (FNullEnt(INDEXENT(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid non-player entity %d", x); \
			return 0; \
		} \
	}

#define CHECK_MONSTER(x) \
	if (x < 1 || x <= gpGlobals->maxClients || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Monster entity %d out of range", x); \
		return 0; \
	} else { \
		if (FNullEnt(INDEXENT(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid monster entity %d", x); \
			return 0; \
		} else { \
			if(strncmp(STRING(GETEDICT(x)->v.classname),"monster_",8) != 0) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a monster_* entity", x, STRING(GETEDICT(x)->v.classname)); \
				return 0; \
			} \
		} \
	}

#define GETEDICT(n) \
	((n >= 1 && n <= gpGlobals->maxClients) ? MF_GetPlayerEdict(n) : INDEXENT(n))

