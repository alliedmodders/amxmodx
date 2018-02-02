// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017 SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxxmodule.h"
#include "misc.h"
#pragma once

#define MAXWEAPS 37

#define TSE_FM_FULLAUTO 0
#define TSE_FM_SEMIAUTO (1<<16)
#define TSE_FM_BURST (1<<17)
#define TSE_FM_PUMP (TSE_FM_SEMIAUTO+TSE_FM_BURST)
#define TSE_FM_FREESEMI (1<<18)
#define TSE_FM_FREEFULL (TSE_FM_SEMIAUTO+TSE_FM_FREESEMI)

#define TSE_ATM_SILENCER	1
#define TSE_ATM_LASERSIGHT	2
#define TSE_ATM_FLASHLIGHT	4
#define TSE_ATM_SCOPE		8

struct WeaponOffsets
{
	short ammo, clip;
	uint64_t fmbase;
};

struct Weapon
{
	char name[24], logname[24];
	WeaponOffsets offsets;
	bool melee;
};

extern Weapon WeaponsList[];
extern uint32_t FireModes[6];
extern uint32_t GetFiremodeByMask(uint32_t mask);

extern IGameConfig *MainConfig;
extern IGameConfig *CommonConfig;