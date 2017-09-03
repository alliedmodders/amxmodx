#include "amxxmodule.h"
#include "msghooking.h"

// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017, SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

UserMsg DeclaresMsgs[] = {
	{ NULL, nullptr, nullptr, false },
	{ "WeaponInfo", &MsgID_WeaponInfo, &HookMsg_WeaponInfo, false },
	{ "TSState", &MsgID_TSState, &HookMsg_TSState, false },
	{ "PwUp", &MsgID_PwUp, &HookMsg_PwUp, false }
};

UserMsg GetMsgDeclareByID(int id)
{
	for (int i = 1; DeclaresMsgs[i].name; ++i)
		if (DeclaresMsgs[i].idptr != nullptr && *DeclaresMsgs[i].idptr == id)
			return DeclaresMsgs[i];
	return DeclaresMsgs[NULL];
}

CPlayer *MsgPlayer;
int MsgState = 0;
int MsgPlayerIndex;

int MsgID_WeaponInfo;
int MsgID_TSState;
int MsgID_PwUp;

void HookMsg_WeaponInfo(void*);
void HookMsg_TSState(void*);
void HookMsg_PwUp(void*);

int OnPlayerStunt;
int OnPlayerMeleeHit;
int OnPlayerPickupPwup;

void (*MFunction)(void*);
void (*MFunctionEnd)(void*);
void (*GameMsgs[MAX_REG_MSGS])(void*);
void (*GameMsgsEnd[MAX_REG_MSGS])(void*);
