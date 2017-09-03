#include "amxxmodule.h"
#include "functions.h"

// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2017, SNMetamorph.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#if defined DEBUGMODE
#define PRINT (g_fn_PrintSrvConsole)
char * MessageBeginType = "";
#endif

// test
void OnAmxxAttach()
{
	MF_AddNatives(pl_funcs);
	MF_AddNatives(weap_funcs);
}

void OnPluginsLoaded()
{
	OnPlayerStunt = MF_RegisterForward("client_onstunt", ET_IGNORE, FP_CELL, FP_CELL, FP_DONE); 
	OnPlayerMeleeHit = MF_RegisterForward("client_onmeleehit", ET_STOP, FP_CELL, FP_FLOAT, FP_FLOAT, FP_DONE);
	OnPlayerPickupPwup = MF_RegisterForward("client_onpickuppwup", ET_IGNORE, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
}

void ServerActivate_Post(edict_t *plEdict, int edictcount, int clmax) 
{
	for (byte i = 1; i <= gpGlobals->maxClients; ++i)
		Player(i)->Init(plEdict + i, i);
	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink_Post(edict_t *player)
{
	CPlayer *pl = Player(ENTINDEX(player));

	// client_onstunt processing
	if (pl->HooksInfo.IsStateChecked) {
		byte stunttype;
		if (pl->HooksInfo.State == TSE_STN_NONE) 
			stunttype = TSE_STN_NONE;
		else if (pl->HooksInfo.State == TSE_STN_ROLL) 
			stunttype = TSE_STN_DIVE;
		else if (pl->HooksInfo.OldState == TSE_STN_ROLL) 
			stunttype = TSE_STN_GETUP;
		else if (pl->GetPDataInt(158) == 1) 
			stunttype = TSE_STN_ROLL;
		else if (pl->IsOnGround()) 
			stunttype = TSE_STN_DUCK;
		else 
			stunttype = TSE_STN_FLIP;
		pl->HooksInfo.IsStateChecked = false;
		MF_ExecuteForward(OnPlayerStunt, static_cast<cell>(pl->PlayerIndex), static_cast<cell>(stunttype));
	}

	// client_onkungfuhit processing
	float damage = pl->GetPDataFloat(483);
	float duration = pl->GetPDataFloat(482);
	if (pl->HooksInfo.KnockDamage == -1.0 && pl->HooksInfo.KnockTime == -1.0)
	{
		pl->HooksInfo.KnockTime = duration;
		pl->HooksInfo.KnockDamage = damage;
	}
	else
	{
		if (pl->HooksInfo.KnockTime != duration && pl->HooksInfo.KnockDamage != damage)
		{
			if (pl->GetPDataFloat(482) != 0.0 && pl->GetPDataFloat(483) != 0.0) {
				if (MF_ExecuteForward(OnPlayerMeleeHit, static_cast<cell>(pl->PlayerIndex), pl->GetPDataFloat(483), pl->GetPDataFloat(482)))
				{
					pl->SetPDataFloat(482, 0.0);
					pl->SetPDataFloat(483, 0.0);
					pl->HooksInfo.KnockTime = -1.0;
					pl->HooksInfo.KnockDamage = -1.0;
				}
			}
			pl->HooksInfo.KnockTime = duration;
			pl->HooksInfo.KnockDamage = damage;
		}
	}

	// client_onpickuppwup processing
	if (pl->HooksInfo.IsPwupTaken) {
		pl->HooksInfo.IsPwupTaken = false;
		if (pl->HooksInfo.PwupDuration != NULL && pl->HooksInfo.PwupType != NULL)
			MF_ExecuteForward(OnPlayerPickupPwup, static_cast<cell>(pl->PlayerIndex), static_cast<cell>(pl->HooksInfo.PwupType), static_cast<cell>(pl->HooksInfo.PwupDuration));
	}

	RETURN_META(MRES_IGNORED);
}

int RegUserMsg_Post(const char *msgname, int size)
{
	for (int i = 1; DeclaresMsgs[i].name; ++i)
	{
		if (!*DeclaresMsgs[i].idptr && strcmp(DeclaresMsgs[i].name, msgname) == 0)
		{
			int id = META_RESULT_ORIG_RET(int);
			*DeclaresMsgs[i].idptr = id;
			if (DeclaresMsgs[i].endmsg)
				GameMsgsEnd[id] = DeclaresMsgs[i].callback;
			else
				GameMsgs[id] = DeclaresMsgs[i].callback;
			#if defined DEBUGMODE
			PRINT("%s registered with ID %d\n", msgname, id);
			#endif
			break;
		}
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (ed) {
		MsgPlayerIndex = ENTINDEX(ed);
		MsgPlayer = Player(MsgPlayerIndex);
	}
	else {
		MsgPlayerIndex = 0;
		MsgPlayer = NULL;
	}
	MsgState = 0;
	if (msg_type < 0 || msg_type >= MAX_REG_MSGS)
		msg_type = 0;
	MFunction = GameMsgs[msg_type];
	MFunctionEnd = GameMsgsEnd[msg_type];
	#if defined DEBUGMODE
	MessageBeginType = (char *)GetMsgDeclareByID(msg_type).name;
	if ((char *)GetMsgDeclareByID(msg_type).name != NULL)
		PRINT("Hooked MSG_BEGIN of %s\n", MessageBeginType);
	#endif
	RETURN_META(MRES_IGNORED);
}

void MessageEnd_Post(void) {
	if (MFunctionEnd) (*MFunctionEnd)(NULL);
	RETURN_META(MRES_IGNORED);
}

void HookMsg_WeaponInfo(void* data)
{
	static int weapon;
	switch (MsgState++) {
		case 0: {
			weapon = *(int *)data;
			MsgPlayer->CurrentWeapon = weapon;
			break;
		}
		case 1: {
			MsgPlayer->Weapons[weapon].clip = *(int *)data;
			break;
		}
		case 2: {
			MsgPlayer->Weapons[weapon].ammo = *(int *)data;
			break;
		}
		case 3: {
			MsgPlayer->Weapons[weapon].mode = *(int *)data;
			break;
		}
		case 4: {
			MsgPlayer->Weapons[weapon].attachments = *(int *)data;
			break;
		}
	}
}

void HookMsg_PwUp(void* data)
{
	switch (MsgState++) {
		case 0: {
			if (!MsgPlayer->HooksInfo.PwupFlag) {
				MsgPlayer->HooksInfo.PwupType = *(USHORT *)data;
			}
			break;
		}
		case 1: {
			if (!MsgPlayer->HooksInfo.PwupFlag) {
				MsgPlayer->HooksInfo.PwupDuration = *(byte *)data;
				MsgPlayer->HooksInfo.IsPwupTaken = true;
			}
			else
				MsgPlayer->HooksInfo.PwupFlag = false;
			break;
		}
	}
}

void HookMsg_TSState(void* data)
{
	MsgPlayer->HooksInfo.OldState = MsgPlayer->HooksInfo.State;
	MsgPlayer->HooksInfo.State = *(byte *)data;
	MsgPlayer->HooksInfo.IsStateChecked = true;
}

void WriteByte_Post(int val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i)
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("BYTE | %d\n", val);
	#endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteChar_Post(int val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("CHAR | %d\n", val);
	}
	#endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteShort_Post(int val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("SHORT | %d\n", val);
	}
	#endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteLong_Post(int val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("LONG | %d\n", val);
	}
	#endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteAngle_Post(float val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("ANGLE | %f\n", val);
	}
    #endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteCoord_Post(float val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("COORD | %f\n", val);
	}
	#endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteString_Post(const char *sz) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("STRING | %s\n", sz);
	}
	#endif
	if (MFunction) (*MFunction)((void *)sz);
	RETURN_META(MRES_IGNORED);
}

void WriteEntity_Post(int val) {
	#if defined DEBUGMODE
	for (int i = 1; DeclaresMsgs[i].name; ++i) {
		if (MessageBeginType == DeclaresMsgs[i].name)
			PRINT("ENTITY | %d\n", val);
	}
	#endif
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}