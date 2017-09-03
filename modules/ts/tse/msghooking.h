#include "amxxmodule.h"
#include "CPlayer.h"
#pragma once

// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017, SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

struct UserMsg {
	const char *name;
	int *idptr;
	void (*callback)(void*);
	bool endmsg;
};
extern UserMsg DeclaresMsgs[];
extern UserMsg GetMsgDeclareByID(int id);

extern void(*MFunction)(void*);
extern void(*MFunctionEnd)(void*);
extern void(*GameMsgs[MAX_REG_MSGS])(void*);
extern void(*GameMsgsEnd[MAX_REG_MSGS])(void*);

extern CPlayer *MsgPlayer;
extern int MsgState;
extern int MsgPlayerIndex;

extern int MsgID_WeaponInfo;
extern int MsgID_TSState;
extern int MsgID_PwUp;

extern void HookMsg_WeaponInfo(void*);
extern void HookMsg_TSState(void*);
extern void HookMsg_PwUp(void*);

extern int OnPlayerStunt;
extern int OnPlayerMeleeHit;
extern int OnPlayerPickupPwup;



