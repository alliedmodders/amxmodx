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
#include "msghooking.h"

HookMsg DeclaresMsgs[] = {
	{ NULL, NULL, nullptr, false },
	{ "WeaponInfo", NULL, &HookMsg_WeaponInfo, false },
	{ "TSState", NULL, &HookMsg_TSState, false },
	{ "PwUp", NULL, &HookMsg_PwUp, false }
};
const char *msgbinds[MAX_REG_MSGS];

HookMsg *GetMsgDeclareByID(int id)
{
	for (int i = 1; DeclaresMsgs[i].name; i++)
		if (DeclaresMsgs[i].id == id)
			return &DeclaresMsgs[i];
	return &DeclaresMsgs[NULL];
}

HookMsg *GetMsgDeclareByName(const char * name)
{
	for (int i = 1; DeclaresMsgs[i].name; i++)
		if (strcmp(DeclaresMsgs[i].name, name) == 0)
			return &DeclaresMsgs[i];
	return &DeclaresMsgs[NULL];
}

int GetMsgIDByName(const char * name)
{
	for (int i = 0; i < MAX_REG_MSGS; i++)
		if (msgbinds[i])
			if (strcmp(msgbinds[i], name) == 0)
				return i;
	return NULL;
}

CPlayer *MsgPlayer;
int MsgState = 0;
int MsgID;

void (*MFunction)(void*);
void (*MFunctionEnd)(void*);
void (*GameMsgs[MAX_REG_MSGS])(void*);
void (*GameMsgsEnd[MAX_REG_MSGS])(void*);
