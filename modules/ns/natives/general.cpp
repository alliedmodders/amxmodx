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

#include <string.h>

#include "amxxmodule.h"

#include "cbase.h" // TakeDamage

#include "ns.h" 

#include "utilfunctions.h"
#include "NEW_Util.h"

#include "GameManager.h"
#include "SpawnManager.h"
#include "LocationManager.h"
#include "TitleManager.h"

#include "CPlayer.h"


edict_t* avhgameplay=NULL;

// drop-in replacement for user_kill
static cell AMX_NATIVE_CALL ns_user_kill(AMX *amx, cell *params)
{

	CreatePlayerPointer(amx,params[1]);

	// 2 is commander, never slay commander
	if (player->GetPev()->iuser3 == 2)
	{
		return 0;
	}

	if (MF_IsPlayerIngame(params[1]) && MF_IsPlayerAlive(params[1]))
	{
		REAL bef = player->GetPev()->frags;

		edict_t *pEntity = CREATE_NAMED_ENTITY(MAKE_STRING("trigger_hurt"));

		if (pEntity)
		{
			KeyValueData kvd;

			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="classname";
			kvd.szValue="trigger_hurt";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);

			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="dmg";
			kvd.szValue="50000.0";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);

			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="damagetype";
			kvd.szValue="1";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);

			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="origin";
			kvd.szValue="8192 8192 8192";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);

			MDLL_Spawn(pEntity);

			pEntity->v.classname=MAKE_STRING("slay");

			MDLL_Touch(pEntity,player->GetEdict());

			REMOVE_ENTITY(pEntity);
		}

		// If the optional parameter is 1, restore the frag count
		if (params[2]) 
		{
			player->GetPev()->frags = bef;
		}
		return 1;
	}

  return 0;
}
// drop-in replacement for user_slap
#define ANGLEVECTORS        (*g_engfuncs.pfnAngleVectors)
static cell AMX_NATIVE_CALL ns_user_slap(AMX *amx, cell *params) /* 2 param */
{

	CreatePlayerPointer(amx,params[1]);

	int power = abs((int)params[2]);

	// Check if commander, if player is comm then stop
	if (player->GetPev()->iuser3 == 2)
	{
		return 0;
	}

	if (MF_IsPlayerIngame(player->index()) && MF_IsPlayerAlive(player->index())) 
	{
		if (player->GetPev()->health <= power) 
		{
			float bef = player->GetPev()->frags;
			/*MDLL_ClientKill(pPlayer->pEdict);*/
			edict_t *pEntity = CREATE_NAMED_ENTITY(MAKE_STRING("trigger_hurt"));
			if (pEntity)
			{
				KeyValueData kvd;

				kvd.szClassName="trigger_hurt";
				kvd.szKeyName="classname";
				kvd.szValue="trigger_hurt";
				kvd.fHandled=0;
				MDLL_KeyValue(pEntity,&kvd);

				kvd.szClassName="trigger_hurt";
				kvd.szKeyName="dmg";
				kvd.szValue="20000.0";
				kvd.fHandled=0;
				MDLL_KeyValue(pEntity,&kvd);

				kvd.szClassName="trigger_hurt";
				kvd.szKeyName="damagetype";
				kvd.szValue="1";
				kvd.fHandled=0;
				MDLL_KeyValue(pEntity,&kvd);

				kvd.szClassName="trigger_hurt";
				kvd.szKeyName="origin";
				kvd.szValue="8192 8192 8192";
				kvd.fHandled=0;
				MDLL_KeyValue(pEntity,&kvd);

				MDLL_Spawn(pEntity);

				pEntity->v.classname=MAKE_STRING("slap");

				MDLL_Touch(pEntity,player->GetEdict());

				REMOVE_ENTITY(pEntity);
			}

			player->GetPev()->frags = bef;
		}
		else 
		{
			int numparam = *params/sizeof(cell);
			if (numparam<3 || params[3]) 
			{
				player->GetPev()->velocity.x += RANDOM_LONG(-600,600);
				player->GetPev()->velocity.y += RANDOM_LONG(-180,180);
				player->GetPev()->velocity.z += RANDOM_LONG(100,200);
			}
			else 
			{
				vec3_t v_forward, v_right;
				vec3_t vang = player->GetPev()->angles;
				float fang[3];
				fang[0] = vang.x;
				fang[1] = vang.y;
				fang[2] = vang.z;
				ANGLEVECTORS( fang, v_forward, v_right, NULL );
				player->GetPev()->velocity = player->GetPev()->velocity + v_forward * 220 + Vector(0,0,200);
			}
			player->GetPev()->punchangle.x = RANDOM_LONG(-10,10);
			player->GetPev()->punchangle.y = RANDOM_LONG(-10,10);

			player->GetPev()->health -= power;

			int armor = (int)player->GetPev()->armorvalue;
			armor -= power;

			if (armor < 0) 
			{
				armor = 0;
			}

			player->GetPev()->armorvalue = armor;

			player->GetPev()->dmg_inflictor = player->GetEdict();

			static const char *bit_sound[3] = {
				"weapons/cbar_hitbod1.wav",
				"weapons/cbar_hitbod2.wav",
				"weapons/cbar_hitbod3.wav" 
			};

			EMIT_SOUND_DYN2(player->GetEdict(), CHAN_VOICE, bit_sound[RANDOM_LONG(0,2)], 1.0, ATTN_NORM, 0, PITCH_NORM);
		}
		return 1;
	}
	return 0;
}
// native ns_get_locationname(Float:x, Float:y, name[], len, lookup=0);
static cell AMX_NATIVE_CALL ns_get_locationname(AMX *amx, cell *params)
{
	vec3_t	 location;
	

	location.x=amx_ctof2(params[1]);
	location.y=amx_ctof2(params[2]);

	if ((params[0] / sizeof(cell)) >= 5)
	{
		return MF_SetAmxString(amx,params[3],LocationMan.Lookup(location,params[5]),params[4]);
	}
	else
	{
		return MF_SetAmxString(amx,params[3],LocationMan.Lookup(location,0),params[4]);
	}
}
// ns_lookup_title(const Key[], Output[], Size)
static cell AMX_NATIVE_CALL ns_lookup_title(AMX *amx, cell *params)
{
	// FIX: some keys have upper case characters; to fix i store all keys as lower case
	ke::AString Input(UTIL_ToLowerCase(MF_GetAmxString(amx,params[1],0,NULL)));

	const char *Output=TitleMan.Lookup(Input);

	if (Output==NULL) // not found
	{
		return -1;
	}

	return MF_SetAmxString(amx,params[2],Output,params[3]);
};
// ns_round_in_progress()
static cell AMX_NATIVE_CALL ns_round_in_progress(AMX *amx, cell *params)
{
	return GameMan.RoundInProgress();
}
// ns_get_spawn(team,number=0,Float:ret[3])
static cell AMX_NATIVE_CALL ns_get_spawn(AMX *amx, cell *params)
{
	return SpawnMan.Lookup(amx,params);
}
// ns_get_mask(id,MASK_*)
static cell AMX_NATIVE_CALL ns_get_mask(AMX *amx, cell *params)
{
	CreateEdict(amx,params[1],-1);

	if (Entity->v.iuser4 & static_cast<int>(params[2]))
	{
		return 1;
	}

	return 0;
}
// ns_set_mask(id,MASK_*,1 or 0)
static cell AMX_NATIVE_CALL ns_set_mask(AMX *amx, cell *params)
{
	CreateEdict(amx,params[1],-1);

	if (static_cast<int>(params[3]) > 0)
	{
		if (Entity->v.iuser4 & static_cast<int>(params[2]))
		{
			return 0;
		}

		Entity->v.iuser4 |= static_cast<int>(params[2]);

		return 1;
	}

	if (Entity->v.iuser4 & static_cast<int>(params[2]))
	{
		Entity->v.iuser4 &= ~static_cast<int>(params[2]);

		return 1;
	}

	return 0;
}
// ns_popup(id (0 for all),"text",OnlyShowWithCLHelpOn=0)
static cell AMX_NATIVE_CALL ns_popup(AMX *amx, cell *params)
{
	GameMan.UpdateHudText2();

	if (params[1])
	{
		CreatePlayerPointer(amx, params[1]);

		if (!player->IsConnected())
		{
			return 0;
		}

		MESSAGE_BEGIN(MSG_ONE,GameMan.GetHudText2(),NULL,player->GetEdict());
	}
	else
	{
		MESSAGE_BEGIN(MSG_ALL,GameMan.GetHudText2());
	}

	char msg[190];
	strncpy(&msg[0],MF_GetAmxString(amx,params[2],0,NULL),188);

	WRITE_STRING(msg);
	WRITE_BYTE(params[3]);

	MESSAGE_END();

	return 1;
}
// ns_is_combat()
static cell AMX_NATIVE_CALL ns_is_combat(AMX *amx, cell *params)
{
	return GameMan.IsCombat();
}
/**
 * Pretty much a direct port of CheezyPeteza's unstick routine
 * from the old unstuck.amxx plugin.  This is included here
 * to remove the engine requirement.
 * -
 * Return values:
 *   1 success
 *   0 no spot to move to
 *  -1 invalid state (stunned/webbed)
 *  -2 invalid class (commander/gorge)
 *  -3 player is dead or a spectator
 *  -4 player is invalid (unknown)
 *  -5 player is invalid (disconnected)
 */

inline int GetPlayerHullSize(CPlayer *Player, int PlayerClass)
{
	switch (PlayerClass)
	{
	case CLASS_SKULK:
	case CLASS_GORGE:
	case CLASS_LERK:
		return static_cast<int>(head_hull);

	case CLASS_FADE:
	case CLASS_JETPACK:
	case CLASS_HEAVY:
	case CLASS_MARINE:
		if (Player->GetPev()->button & IN_DUCK || Player->GetPev()->flags & FL_DUCKING)
		{
			return static_cast<int>(head_hull);
		}

		return static_cast<int>(human_hull);

	case CLASS_ONOS:
		if (Player->GetPev()->button & IN_DUCK || Player->GetPev()->flags & FL_DUCKING)
		{
			return static_cast<int>(human_hull);
		}

		return static_cast<int>(large_hull);

	default:
		return -1;

	}
	return -1;
}
static cell AMX_NATIVE_CALL ns_unstick_player(AMX *amx, cell *params)
{
	CreatePlayerPointer(amx,params[1]);

	if (!player->IsConnected())
	{
		return -5;
	}

	if (player->GetPev()->iuser4 & (MASK_ENSNARED | MASK_PLAYER_STUNNED))
	{
		return -1;
	}

	int PlayerClass=player->GetClass();
	if (PlayerClass == CLASS_GESTATE || PlayerClass == CLASS_COMMANDER)
	{
		return -2;
	}

	if (PlayerClass == CLASS_DEAD || PlayerClass == CLASS_NOTEAM || PlayerClass == CLASS_UNKNOWN)
	{
		return -3;
	}

	int HullSize=GetPlayerHullSize(player, PlayerClass);


	if (HullSize==-1)
	{
		return -4;
	}

	Vector OriginalOrigin=player->GetPev()->origin;

	Vector NewOrigin;
	int Distance=params[2];
	int Attempts;
	TraceResult Result;

	while (Distance < 1000)
	{
		Attempts=params[3];

		while (Attempts--)
		{
			NewOrigin.x = RANDOM_FLOAT(OriginalOrigin.x - Distance,OriginalOrigin.x + Distance);
			NewOrigin.y = RANDOM_FLOAT(OriginalOrigin.y - Distance,OriginalOrigin.y + Distance);
			NewOrigin.z = RANDOM_FLOAT(OriginalOrigin.z - Distance,OriginalOrigin.z + Distance);

			// (const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
			TRACE_HULL(NewOrigin, NewOrigin, 0, HullSize, player->GetEdict(), &Result);

			if (Result.fInOpen && !Result.fAllSolid && !Result.fStartSolid)
			{
				SET_ORIGIN(player->GetEdict(),NewOrigin);
				return 1;
			}
		}
		Distance += params[2];
	}


	return 0; // Couldn't be found
}
// Type: 131072 = DoT
// ns_takedamage
static cell AMX_NATIVE_CALL ns_takedamage(AMX *amx, cell *params)
{
	// NASTY
	// Reinterprets pvPrivateData as CBaseEntity, then calls TakeDamage with the entvar of Inflictor, and Attacker, with the float value and damage type
	// The NS offset of TakeDamage hasn't changed from the HLSDK fortunately, so no offset digging is necessary
	return (reinterpret_cast<CBaseEntity *>(INDEXENT_NEW(params[1])->pvPrivateData))->TakeDamage(&(INDEXENT_NEW(params[2])->v),&(INDEXENT_NEW(params[3])->v),amx_ctof2(params[4]),static_cast<int>(params[5]));
}

static cell AMX_NATIVE_CALL ns_get_gameplay(AMX* amx, cell* params)
{
	if (avhgameplay == NULL)
	{
		avhgameplay = FIND_ENTITY_BY_CLASSNAME(NULL, "avhgameplay");
	}
	if (avhgameplay == NULL ||
		avhgameplay->pvPrivateData == NULL) // Still null? Get out of here
	{
		return NSGame_CantTell;
	}

	int ATeam /* i pity da foo */ = *reinterpret_cast<int*>(reinterpret_cast<char*>(avhgameplay->pvPrivateData) + MAKE_OFFSET(GAMEPLAY_TEAMA));
	int BTeam = *reinterpret_cast<int*>(reinterpret_cast<char*>(avhgameplay->pvPrivateData) + MAKE_OFFSET(GAMEPLAY_TEAMB));

	if (ATeam == 2 &&	// alien
		BTeam == 2)		// alien
	{
		return NSGame_AlienVAlien;
	}
	if (ATeam == 1 &&	// marine
		BTeam == 1)		// marine
	{
		return NSGame_MarineVMarine;
	}
	if (ATeam == 1 &&	// marine
		BTeam == 2)		// alien
	{
		return NSGame_MarineVAlien;
	}
	return NSGame_Unknown;

}

#ifdef DEVELOPER_BUILD
static cell AMX_NATIVE_CALL refmem(AMX *amx, cell *params)
{
	return *(reinterpret_cast<int *>(params[1]));
};
static cell AMX_NATIVE_CALL setmem(AMX *amx, cell *params)
{
	int *ptr=reinterpret_cast<int *>(params[1]);
	
	*ptr=params[2];

	return 1;
};
#endif

AMX_NATIVE_INFO general_natives[] = {

	{ "user_kill",				ns_user_kill }, // replacement natives
	{ "user_slap",				ns_user_slap }, // since ClientKill is changed in NS

	{ "ns_get_locationname",	ns_get_locationname },
	{ "ns_lookup_title",		ns_lookup_title },
	{ "ns_round_in_progress",	ns_round_in_progress },
	{ "ns_get_spawn",			ns_get_spawn },
	{ "ns_get_mask",			ns_get_mask },
	{ "ns_set_mask",			ns_set_mask },
	{ "ns_is_combat",			ns_is_combat  },
	{ "ns_unstick_player",		ns_unstick_player },

	{ "ns_popup",				ns_popup },

	{ "ns_takedamage",			ns_takedamage},

	{ "ns_get_gameplay",		ns_get_gameplay },

#ifdef DEVELOPER_BUILD
	{ "refmem",					refmem },
	{ "setmem",					setmem },
#endif

	{ NULL,						NULL }

};
void AddNatives_General()
{
	MF_AddNatives(general_natives);
}
