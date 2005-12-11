/*
 * Copyright (c) 2003-2005 Twilight Suzuka
 *
 *    This file is part of TSXMod.
 *
 *    TS XMod is free software; you can redistribute it and/or modify it
 *    under the terms of the GNU General Public License as published by the
 *    Free Software Foundation; either version 2 of the License, or (at
 *    your option) any later version.
 *
 *    TS XMod is distributed in the hope that it will be useful, but
 *    WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with TS XMod; if not, write to the Free Software Foundation,
 *    Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *    In addition, as a special exception, the author gives permission to
 *    link the code of this program with the Half-Life Game Engine ("HL
 *    Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *    L.L.C ("Valve").  You must obey the GNU General Public License in all
 *    respects for all of the code used other than the HL Engine and MODs
 *    from Valve.  If you modify this file, you may extend this exception
 *    to your version of the file, but you are not obligated to do so.  If
 *    you do not wish to do so, delete this exception statement from your
 *    version.
 *
 */

#include "amxxmodule.h"
#include "tsx.h"
#include "MsgFunc.h"

int gmsgResetHUD;
int gmsgWeaponInfo;
int gmsgClipInfo;
int gmsgScoreInfo;
int gmsgTSHealth;

int gmsgWStatus;
int gmsgTSCash;
int gmsgTSSpace;
int gmsgPwUp;

struct sUserMsg 
{
	const char* name;
	int* id;
	funEventCall func;
	bool endmsg;
} 
g_user_msg[] = 
{
	{ "ResetHUD",&gmsgResetHUD,Client_ResetHUD_End,true },
	{ "WeaponInfo",&gmsgWeaponInfo,Client_WeaponInfo,false },
	{ "ClipInfo",&gmsgClipInfo,Client_ClipInfo,false },
	{ "ScoreInfo",&gmsgScoreInfo,Client_ScoreInfo,false },
	{ "TSHealth",&gmsgTSHealth,Client_TSHealth_End,true },

	{ "WStatus",&gmsgWStatus,Client_WStatus,false },
	{ "TSCash",&gmsgTSCash,Client_TSCash,false },
	{ "TSSpace",&gmsgTSSpace,Client_TSSpace,false },
	{ "PwUp",&gmsgPwUp,Client_PwUp,false},

	{ 0,0,0,false }
};