/**
 * (C)2004-2005 AMX Mod X Development Team
 *   based on the stocks and information provided by LynX
 *   organized and released by BAILOPAN
 * This file is provided as is (no warranties).
 */
 
#if defined _esfconst_included
  #endinput
#endif
#define _esfconst_included

enum 
{
	Character_Buu = 1,		
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

enum 
{
	Recovery_Kicked=1,
	Recovery_Tumbled,
	Recovery_Lying,
	Recovery_Thrown,
};

#define ESF_CHARGING		1
#define ESF_CONTROLLING		2
#define ESF_SHOOTING		3
#define ESF_SHOT			4
