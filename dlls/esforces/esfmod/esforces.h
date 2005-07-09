#ifndef _INCLUDE_ESFORCES_H
#define _INCLUDE_ESFORCES_H

#include "amxxmodule.h"

/**
 * This module was written by David "BAILOPAN" Anderson
 *  based on the offsets and stocks by Lynx.
 * Thanks for all of your help, Lynx!
 */

enum 
{
	Character_Buu = 1,		//this guy's cool too
	Character_Goku = 2,
	Character_Gohan = 3,	//my favorite :)
	Character_Krillin = 4,
	Character_Frieza = 5,
	Character_Piccolo = 6,
	Character_Trunks = 7,
	Character_Vegeta = 8,
	Character_Cell = 9,
};

enum
{
	Explosion_Blue = 0,
	Explosion_Green,
	Explosion_Orange,
	Explosion_Purple,
	Explosion_Yellow,
	Explosion_Red,
	Explosion_White,
	Explosions_Total,
};

enum
{
	Attack_Kamehameha=1,
	Attack_SpiritBomb,
	Attack_GalletGun,
	Attack_FinalFlash,
	Attack_Renzoku,
	Attack_Kametorpedo,
	Attack_GenericBeam,
	Attack_Throw,
};

enum
{
	Direction_Left=1,
	Direction_Right,
	Direction_Up,
	Direction_Down,
	Direction_Forward,
	Direction_Backward,
};

#define ESF_CHARGING		1
#define ESF_CONTROLLING		2
#define ESF_SHOOTING		3
#define ESF_SHOT			4

#define CHECKPLAYER(id) \
	if (id < 1 || id > gpGlobals->maxClients) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not a valid index", id); \
		return 0; \
	} else if (!MF_IsPlayerIngame(id)) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not in-game", id); \
		return 0; \
	}

extern AMX_NATIVE_INFO g_AnimationNatives[];
extern AMX_NATIVE_INFO g_PdataNatives[];
extern AMX_NATIVE_INFO g_EffectsNatives[];
extern AMX_NATIVE_INFO g_BaseNatives[];

#endif //_INCLUDE_ESFORCES_H
