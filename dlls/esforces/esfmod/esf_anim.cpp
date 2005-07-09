#include <extdll.h>
#include <meta_api.h>
#include <eiface.h>
#include <edict.h>
#include "esforces.h"

//use direction enums
// 0 = any
static cell AMX_NATIVE_CALL esf_is_swooping(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

    int anim = pEdict->v.sequence;
	int dir = params[2];

	if (!dir)
	{
		if (anim >= 36 && anim <= 41)
			return 1;
	} else if (dir == Direction_Up) {
		if (anim == 36)
			return 1;
	} else if (dir == Direction_Down) {
		if (anim == 37)
			return 1;
	} else if (dir == Direction_Left) {
		if (anim == 38)
			return 1;
	} else if (dir == Direction_Right) {
		if (anim == 39)
			return 1;
	} else if (dir == Direction_Forward) {
		if (anim == 40)
			return 1;
	} else if (dir == Direction_Backward) {
		if (anim == 41)
			return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_prepunch(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim >= 121 || anim <= 124)
		return 1;

	if (anim == 209)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_stance(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 126)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_stunned(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 127)
		return 1;

	return 0;
}
static cell AMX_NATIVE_CALL esf_is_grappling(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 128)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_crushed(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 208)
		return 1;

	return 0;
}

//player, combo, [attacked = 0]
static cell AMX_NATIVE_CALL esf_in_combo(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (params[2] == 1)
	{
		if (params[3])
		{
			if (pEdict->v.sequence == 213)
				return 1;
		} else {
			if (pEdict->v.sequence == 212)
				return 1;
		}
	} else if (params[2] == 2) {
		if (params[3])
		{
			if (pEdict->v.sequence == 215)
				return 1;
		} else {
			if (pEdict->v.sequence == 214)
				return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_advmeleeing(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim > 128 && anim < 153)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_advmeleed(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim > 152 && anim < 201)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_in_advmelee(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim > 120 && anim < 216)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_blocking(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim <= 42 && anim >= 46)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_kicking(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim <= 47 && anim >= 51)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_kicked(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	//only evens
	if (anim >= 52 && anim <= 62 && (anim % 2 == 0))
		return 1;

	return 0;
}

//1 = kicked
//2 = tumbling
//3 = laying
//4 = thrown to ground
static cell AMX_NATIVE_CALL esf_is_recovered(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;
	int mode = params[2];

	if (mode == 1)
	{
		//only odds
		if (anim >= 53 && anim <= 63 && (anim % 2 == 1))
			return 1;
	} else if (mode == 2) {
		if (anim == 65 || anim == 67 || anim == 69 || anim == 71)
			return 1;
		return 0;
	} else if (mode == 3) {
		if (anim == 73 || anim == 75)
			return 1;
	} else if (mode == 4) {
		if (anim >= 105 && anim <= 109)
			return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_flying(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim >= 26 && anim <= 34)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_tumbling(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 64 || anim == 66 || anim == 68 || anim == 70)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_lying(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 72 || anim == 74)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_has_died(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim >= 76 && anim <= 84)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_has_dragonball(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 92)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_threw_dragonball(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 93 || anim == 94)
		return 1;

	return 0;
}

//1 = kiblast
//2 = generic beam
static cell AMX_NATIVE_CALL esf_is_shooting(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (params[2] == 1)
	{
		if (anim == 95 || anim == 96)
			return 1;
	} else if (params[2] == 2) {
		if (anim == 98 || anim == 99)
			return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_transforming(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 101 || anim == 102)
		return 1;
	
	return 0;
}

static cell AMX_NATIVE_CALL esf_transform_done(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 103)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_groundthrown(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 104)
		return 1;

	return 0;
}

//for get_shotstate and get_attack
//1 = kamehameha
//2 = spiritbomb
//3 = galletgun
//4 = finalflash
//5 = renzoku
//6 = kametorpedo
//7 = generic beam
//8 = throw
static cell AMX_NATIVE_CALL esf_get_shotstate(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;
	int cls = pEdict->v.playerclass;
	int attack = params[2];

	if (attack == Attack_Kamehameha)
	{
		if ( (cls == Character_Goku || cls == Character_Gohan))
		{
			if (anim == 110)
				return ESF_CHARGING;
			if (anim == 111)
				return ESF_CONTROLLING;
		}
		if (cls == Character_Cell)
		{
			if (anim == 112)
				return ESF_CHARGING;
			if (anim == 113)
				return ESF_CONTROLLING;
		}
	} else if (attack == Attack_SpiritBomb) {
		if (anim == 112 || anim == 114)
			return ESF_CHARGING;
		if (anim == 113 || anim == 115)
			return ESF_SHOT;
	} else if (attack == Attack_GalletGun) {
		if (cls == Character_Vegeta)
		{ 
			if (anim == 111)
				return ESF_CHARGING;
			if (anim == 112)
				return ESF_CONTROLLING;
		}
		if (cls == Character_Cell)
		{
			if (anim == 110)
				return ESF_CHARGING;
			if (anim == 111)
				return ESF_CONTROLLING;
		}
	} else if (attack == Attack_FinalFlash) {
		if (cls == Character_Krillin)
		{
			if (anim == 119 || anim == 120)
				return ESF_SHOT;
		}
		if (cls == Character_Cell)
		{
			if (anim == 114 || anim == 115)
				return ESF_SHOT;
		}
		if (anim == 113)
			return ESF_CHARGING;
		if (anim == 114)
			return ESF_CONTROLLING;
	} else if (attack == Attack_Renzoku) {
		if (anim == 115)
			return ESF_CHARGING;
		if (anim >= 116 && anim <= 118)
			return ESF_SHOOTING;
	} else if (attack == Attack_Kametorpedo) {
		if (anim == 116)
			return ESF_CHARGING;
		if (anim == 118)
			return ESF_SHOT;
	} else if (attack == Attack_GenericBeam) {
		if (anim == 97)
			return ESF_CHARGING;
	} else if (attack == Attack_Throw) {
		if (anim == 201 || anim == 203 || anim == 204)
			return ESF_CHARGING;
	}

	return 0;
}

//experimental since animation states overlap not all weapons can be returned
static cell AMX_NATIVE_CALL esf_get_attack(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;
	int cls = pEdict->v.playerclass;

	if ( (cls == Character_Goku || cls == Character_Gohan) )
	{
		if (anim == 110 || anim == 111)
			return Attack_Kamehameha;
	} else if (cls == Character_Cell) {
		if (anim == 112 || anim == 113)
			return Attack_Kamehameha;
		if (anim == 110 || anim == 111)
			return Attack_GalletGun;
		if (anim == 114 || anim == 115)
			return Attack_FinalFlash;
	} else if (cls == Character_Vegeta) {
		if (anim == 111 || anim == 112)
			return Attack_GalletGun;
	} else if (cls == Character_Krillin) {
		if (anim == 119 || anim == 120)
			return Attack_FinalFlash;
	}

	if (anim == 97)
		return Attack_GenericBeam;

	if (anim == 201 || anim == 203 || anim == 204)
			return Attack_Throw;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_swung(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 202 || anim == 206)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_grabbedwall(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 13 ||
		anim == 15 ||
		anim == 17 ||
		anim == 19)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_walljumped(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim == 14 ||
		anim == 16 ||
		anim == 18 ||
		anim == 20)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_in_water(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim >= 21 && anim <= 23)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_powering(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim >= 24 && anim <= 27)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_idle(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int anim = pEdict->v.sequence;

	if (anim <=2)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL esf_is_beamjumping(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (pEdict->v.sequence == 100)
		return 1;

	return 0;
}

AMX_NATIVE_INFO g_AnimationNatives[] = {
 {"esf_is_swooping",	esf_is_swooping},
 {"esf_is_prepunch",	esf_is_prepunch},
 {"esf_is_stance",		esf_is_stance},
 {"esf_is_stunned",		esf_is_stunned},
 {"esf_is_grappling",	esf_is_grappling},
 {"esf_is_crushed",		esf_is_crushed},
 {"esf_in_combo",		esf_in_combo},
 {"esf_is_advmeleeing",	esf_is_advmeleeing},
 {"esf_is_advmeleed",	esf_is_advmeleed},
 {"esf_in_advmelee",	esf_in_advmelee},
 {"esf_is_blocking",	esf_is_blocking},
 {"esf_is_kicking",		esf_is_kicking},
 {"esf_is_kicked",		esf_is_kicked},
 {"esf_is_recovered",	esf_is_recovered},
 {"esf_is_flying",		esf_is_flying},
 {"esf_is_tumbling",	esf_is_tumbling},
 {"esf_is_lying",		esf_is_lying},
 {"esf_has_died",		esf_has_died},
 {"esf_has_dragonball",	esf_has_dragonball},
 {"esf_threw_dragonball",	esf_threw_dragonball},
 {"esf_is_shooting",	esf_is_shooting},
 {"esf_transforming",	esf_transforming},
 {"esf_transform_done",	esf_transform_done},
 {"esf_groundthrown",	esf_groundthrown},
 {"esf_get_shotstate",	esf_get_shotstate},
 {"esf_get_attack",		esf_get_attack},
 {"esf_is_swung",		esf_is_swung},
 {"esf_grabbedwall",	esf_grabbedwall},
 {"esf_walljumped",		esf_walljumped},
 {"esf_in_water",		esf_in_water},
 {"esf_is_powering",	esf_is_powering},
 {"esf_is_idle",		esf_is_idle},
 {"esf_is_beamjumping",	esf_is_beamjumping},
 {NULL,					NULL},
};
