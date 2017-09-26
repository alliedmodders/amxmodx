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
#include "CPlayer.h"
#pragma once

struct HookMsg {
	const char *name;
	int id;
	void (*callback)(void*);
	bool endmsg;
};
extern HookMsg DeclaresMsgs[];
extern HookMsg *GetMsgDeclareByID(int id);
extern HookMsg *GetMsgDeclareByName(const char * name);
extern const char *msgbinds[MAX_REG_MSGS];
extern int GetMsgIDByName(const char * name);

extern void (*MFunction)(void*);
extern void (*MFunctionEnd)(void*);
extern void (*GameMsgs[MAX_REG_MSGS])(void*);
extern void (*GameMsgsEnd[MAX_REG_MSGS])(void*);

extern CPlayer *MsgPlayer;
extern int MsgState;
extern int MsgID;

extern void HookMsg_WeaponInfo(void*);
extern void HookMsg_TSState(void*);
extern void HookMsg_PwUp(void*);




