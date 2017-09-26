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
	short reloadtime;
};

struct Weapon
{
	char name[24], logname[24];
	WeaponOffsets offsets;
	int bonus;
	bool melee, custom;
};

extern Weapon WeaponsList[];
extern uint32_t FireModes[6];
extern uint32_t GetFiremodeByMask(uint32_t mask);
