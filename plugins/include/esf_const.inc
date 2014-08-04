// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Based on stocks and information provided by LynX.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license
 
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
