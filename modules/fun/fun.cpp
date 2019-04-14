// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fun Module
//

#include "fun.h"
#include <HLTypeConversion.h>

HLTypeConversion TypeConversion;
CPlayers Players;

// native get_client_listen(receiver, sender)
static cell AMX_NATIVE_CALL get_client_listening(AMX *amx, cell *params)
{
	enum args { arg_count, arg_receiver, arg_sender };

	CHECK_PLAYER(params[arg_receiver]);
	CHECK_PLAYER(params[arg_sender]);

	return GETCLIENTLISTENING(params[arg_receiver], params[arg_sender]);
}

// native set_client_listen(receiver, sender, listen)
static cell AMX_NATIVE_CALL set_client_listening(AMX *amx, cell *params)
{
	enum args { arg_count, arg_receiver, arg_sender, arg_listen };

	CHECK_PLAYER(params[arg_receiver]);
	CHECK_PLAYER(params[arg_sender]);

	return SETCLIENTLISTENING(params[arg_receiver], params[arg_sender], params[arg_listen]);
}

// native set_user_godmode(index, godmode = 0)
static cell AMX_NATIVE_CALL set_user_godmode(AMX *amx, cell *params)
{
	enum args { arg_count, arg_user, arg_godmode };

	CHECK_PLAYER(params[arg_user]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_user]);

	pPlayer->v.takedamage = params[arg_godmode] != 0 ? DAMAGE_NO : DAMAGE_AIM;

	return 1;
}

// native get_user_godmode(index)
static cell AMX_NATIVE_CALL get_user_godmode(AMX *amx, cell *params)
{
	enum args { arg_count, arg_user };

	CHECK_PLAYER(params[arg_user]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_user]);

	return pPlayer->v.takedamage == DAMAGE_NO;
}

// native give_item(index, const item[])
static cell AMX_NATIVE_CALL give_item(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_item };

	CHECK_PLAYER(params[arg_index]);

	auto itemLength = 0;
	const auto item = MF_GetAmxString(amx, params[arg_item], 1, &itemLength);

	if (!itemLength
		||(strncmp(item, "weapon_", 7) != 0
		&& strncmp(item, "ammo_", 5) != 0
		&& strncmp(item, "item_", 5) != 0
		&& strncmp(item, "tf_weapon_", 10) != 0))
	{
		return 0;
	}

	auto pEntity = CREATE_NAMED_ENTITY(ALLOC_STRING(item));

	if (FNullEnt(pEntity))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Item \"%s\" failed to create", item);
		return 0;
	}

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	pEntity->v.origin = pPlayer->v.origin;
	pEntity->v.spawnflags |= SF_NORESPAWN;

	MDLL_Spawn(pEntity);

	const auto oldSolid = pEntity->v.solid;

	MDLL_Touch(pEntity, pPlayer);

	if (pEntity->v.solid == oldSolid)
	{
		REMOVE_ENTITY(pEntity); // The function did not fail - we're just deleting the item
		return -1;
	}

	return TypeConversion.edict_to_id(pEntity);
}

// native spawn(index)
static cell AMX_NATIVE_CALL spawn(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index };

	CHECK_ENTITY(params[arg_index]);

	const auto pEntity = TypeConversion.id_to_edict(params[arg_index]);

	MDLL_Spawn(pEntity);

	return 1;
}

// native set_user_health(index, health)
static cell AMX_NATIVE_CALL set_user_health(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_health };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);
	const auto health  = float(params[arg_health]);

	if (health > 0.0f)
	{
		pPlayer->v.health = health;
	}
	else
	{
		MDLL_ClientKill(pPlayer);
	}

	return 1;
}

// native set_user_frags(index, frags)
static cell AMX_NATIVE_CALL set_user_frags(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_frags };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	pPlayer->v.frags = float(params[arg_frags]);

	return 1;
}

// native set_user_armor(index, armor)
static cell AMX_NATIVE_CALL set_user_armor(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_armor };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	pPlayer->v.armorvalue = float(params[arg_armor]);

	return 1;
}

// native set_user_origin(index, const origin[3])
static cell AMX_NATIVE_CALL set_user_origin(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_origin };

	CHECK_PLAYER(params[arg_index]);

	auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);
	const auto pVector = MF_GetAmxAddr(amx, params[arg_origin]);

	SET_SIZE(pPlayer, pPlayer->v.mins, pPlayer->v.maxs);
	SET_ORIGIN(pPlayer, Vector(float(pVector[0]), float(pVector[1]), float(pVector[2])));

	return 1;
}

// native set_user_rendering(index, fx = kRenderFxNone, r = 0, g = 0, b = 0, render = kRenderNormal, amount = 0)
static cell AMX_NATIVE_CALL set_user_rendering(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_fx, arg_red, arg_green, arg_blue, arg_render, arg_amount };

	CHECK_PLAYER(params[arg_index]);

	auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	pPlayer->v.renderfx    = params[arg_fx];
	pPlayer->v.rendercolor = Vector(float(params[arg_red]), float(params[arg_green]), float(params[arg_blue]));
	pPlayer->v.rendermode  = params[arg_render];
	pPlayer->v.renderamt   = float(params[arg_amount]);

	return 1;
}

// get_user_rendering(index, &fx = kRenderFxNone, &r = 0, &g = 0, &b = 0, &render = kRenderNormal, &amount = 0);
static cell AMX_NATIVE_CALL get_user_rendering(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_fx, arg_red, arg_green, arg_blue, arg_render, arg_amount };

	CHECK_PLAYER(params[arg_index]);

	auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	*MF_GetAmxAddr(amx, params[arg_fx])     = pPlayer->v.renderfx;
	*MF_GetAmxAddr(amx, params[arg_red])    = pPlayer->v.rendercolor[0];
	*MF_GetAmxAddr(amx, params[arg_green])  = pPlayer->v.rendercolor[1];
	*MF_GetAmxAddr(amx, params[arg_blue])   = pPlayer->v.rendercolor[2];
	*MF_GetAmxAddr(amx, params[arg_render]) = pPlayer->v.rendermode;
	*MF_GetAmxAddr(amx, params[arg_amount]) = pPlayer->v.renderamt;

	return 1;
}

// native set_user_maxspeed(index, Float:speed = -1.0)
static cell AMX_NATIVE_CALL set_user_maxspeed(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_speed };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer  = TypeConversion.id_to_edict(params[arg_index]);
	const auto newSpeed = amx_ctof(params[arg_speed]);

	SETCLIENTMAXSPEED(pPlayer, newSpeed);
	pPlayer->v.maxspeed = newSpeed;

	return 1;
}

// native Float:get_user_maxspeed(index)
static cell AMX_NATIVE_CALL get_user_maxspeed(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	return amx_ftoc(pPlayer->v.maxspeed);
}

// native set_user_gravity(index, Float:gravity = 1.0)
static cell AMX_NATIVE_CALL set_user_gravity(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_gravity };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	pPlayer->v.gravity = amx_ctof(params[arg_gravity]);

	return 1;
}

// native Float:get_user_gravity(index)
static cell AMX_NATIVE_CALL get_user_gravity(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	return amx_ftoc(pPlayer->v.gravity);
}

// native set_user_hitzones(index = 0, target = 0, body = HITZONES_DEFAULT)
static cell AMX_NATIVE_CALL set_user_hitzones(AMX *amx, cell *params)
{
	enum args { arg_count, arg_attacker, arg_target, arg_hitzones };

	const int attacker = params[arg_attacker];
	const int target   = params[arg_target];
	const int hitzones = params[arg_hitzones];

	if (attacker == 0 && target == 0)
	{
		Players.SetEveryoneBodyHits(hitzones);
	}
	else if (attacker == 0 && target != 0)
	{
		CHECK_PLAYER(target);

		Players.SetAttackersBodyHits(target, hitzones);
	}
	else if (attacker != 0 && target == 0)
	{
		CHECK_PLAYER(attacker);

		Players.SetTargetsBodyHits(attacker, hitzones);
	}
	else
	{
		CHECK_PLAYER(attacker);
		CHECK_PLAYER(target);

		Players.SetBodyHits(attacker, target, hitzones);
	}

	return 1;
}

// native get_user_hitzones(index, target)
static cell AMX_NATIVE_CALL get_user_hitzones(AMX *amx, cell *params)
{
	enum args { arg_count, arg_attacker, arg_target };

	const auto attacker = params[arg_attacker];

	CHECK_PLAYER(attacker);

	const auto target = params[arg_target];

	CHECK_PLAYER(target);

	return Players[attacker].GetBodyHits(target);
}

// native set_user_noclip(index, noclip = 0)
static cell AMX_NATIVE_CALL set_user_noclip(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_noclip };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	pPlayer->v.movetype = params[arg_noclip] != 0 ? MOVETYPE_NOCLIP : MOVETYPE_WALK;

	return 1;
}

// native get_user_noclip(index)
static cell AMX_NATIVE_CALL get_user_noclip(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index };

	CHECK_PLAYER(params[arg_index]);

	const auto pPlayer = TypeConversion.id_to_edict(params[arg_index]);

	return pPlayer->v.movetype == MOVETYPE_NOCLIP;
}

// native set_user_footsteps(id, set = 1)
static cell AMX_NATIVE_CALL set_user_footsteps(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index, arg_footsteps };

	const auto index = params[arg_index];

	CHECK_PLAYER(index);

	const auto pPlayer = TypeConversion.id_to_edict(index);

	if (params[arg_footsteps] != 0)
	{
		pPlayer->v.flTimeStepSound = 999;
		Players[index].SetSilentFootsteps(true);

		g_pFunctionTable->pfnPlayerPreThink = PlayerPreThink;
	}
	else
	{
		pPlayer->v.flTimeStepSound = STANDARDTIMESTEPSOUND;
		Players[index].SetSilentFootsteps(false);

		if (g_pFunctionTable->pfnPlayerPreThink && !Players.HaveSilentFootsteps())
		{
			g_pFunctionTable->pfnPlayerPreThink = nullptr;
		}
	}

	return 1;
}

// native get_user_footsteps(index)
static cell AMX_NATIVE_CALL get_user_footsteps(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index };

	const auto index = params[arg_index];

	CHECK_PLAYER(index);

	return Players[index].HasSilentFootsteps();
}

// native strip_user_weapons(index)
static cell AMX_NATIVE_CALL strip_user_weapons(AMX *amx, cell *params)
{
	enum args { arg_count, arg_index };

	const auto index = params[arg_index];

	CHECK_PLAYER(index);

	const auto pPlayer = TypeConversion.id_to_edict(index);
	const auto pEntity = CREATE_NAMED_ENTITY(MAKE_STRING("player_weaponstrip"));

	if (FNullEnt(pEntity))
	{
		return 0;
	}

	MDLL_Spawn(pEntity);
	MDLL_Use(pEntity, pPlayer);
	REMOVE_ENTITY(pEntity);

	*reinterpret_cast<int *>(MF_PlayerPropAddr(index, Player_CurrentWeapon)) = 0;

	return 1;
}


AMX_NATIVE_INFO fun_Exports[] =
{
	{ "get_client_listen" , get_client_listening },
	{ "set_client_listen" , set_client_listening },
	{ "set_user_godmode"  , set_user_godmode     },
	{ "get_user_godmode"  , get_user_godmode     },
	{ "set_user_health"   , set_user_health      },
	{ "give_item"         , give_item            },
	{ "spawn"             , spawn                },
	{ "set_user_frags"    , set_user_frags       },
	{ "set_user_armor"    , set_user_armor       },
	{ "set_user_origin"   , set_user_origin      },
	{ "set_user_rendering", set_user_rendering   },
	{ "get_user_rendering",	get_user_rendering   },
	{ "set_user_maxspeed" , set_user_maxspeed    },
	{ "get_user_maxspeed" , get_user_maxspeed    },
	{ "set_user_gravity"  , set_user_gravity     },
	{ "get_user_gravity"  , get_user_gravity     },
	{ "get_user_footsteps", get_user_footsteps   },
	{ "set_user_hitzones" , set_user_hitzones    },
	{ "get_user_hitzones" , get_user_hitzones    },
	{ "set_user_noclip"   , set_user_noclip      },
	{ "get_user_noclip"   , get_user_noclip      },
	{ "set_user_footsteps", set_user_footsteps   },
	{ "strip_user_weapons", strip_user_weapons   },
	{ nullptr             , nullptr              }
};


void PlayerPreThink(edict_t *pEntity)
{
	const auto index = TypeConversion.edict_to_id(pEntity);

	if (Players[index].HasSilentFootsteps())
	{
		pEntity->v.flTimeStepSound = 999;
		RETURN_META(MRES_HANDLED);
	}

	RETURN_META(MRES_IGNORED);
}

int ClientConnect(edict_t *pPlayer, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	const auto index = TypeConversion.edict_to_id(pPlayer);

	Players[index].Clear();

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *shooter, TraceResult *ptr)
{
	TRACE_LINE(v1, v2, fNoMonsters, shooter, ptr);

	if (ptr->pHit && (ptr->pHit->v.flags & (FL_CLIENT | FL_FAKECLIENT))
	    && shooter &&  (shooter->v.flags & (FL_CLIENT | FL_FAKECLIENT)) )
	{
		const auto shooterIndex = TypeConversion.edict_to_id(shooter);
		const auto targetIndex  = TypeConversion.edict_to_id(ptr->pHit);

		if (!(Players[shooterIndex].GetBodyHits(targetIndex) & (1 << ptr->iHitgroup)))
		{
			ptr->flFraction = 1.0;
		}
	}

	RETURN_META(MRES_SUPERCEDE);
}


void OnAmxxAttach()
{
	MF_AddNatives(fun_Exports);
}

void OnPluginsLoaded()
{
	Players.Clear();

	TypeConversion.init();

	g_pFunctionTable->pfnPlayerPreThink = nullptr;
	g_pengfuncsTable_Post->pfnTraceLine = nullptr;
}

void ServerDeactivate()
{
	g_pFunctionTable->pfnPlayerPreThink = nullptr;
	g_pengfuncsTable_Post->pfnTraceLine = nullptr;

	RETURN_META(MRES_IGNORED);
}
