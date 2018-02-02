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
#include "functions.h"
#include "misc.h"

#define PRINT (g_fn_PrintSrvConsole)
void CmdHandling();

// Variables
const char *deststypes[10] = 
{
	"Broadcast (unreliable)",
	NULL,
	"Broadcast",
	"Init",
	"PVS (unreliable)",
	"PAS (unreliable)",
	"PVS",
	"PAS",
	NULL,
	"HLTV"
};

HookMsg *HMDecl;
bool IsDebugMode = false;
bool hmlist[MAX_REG_MSGS];

int OnPlayerStunt;
int OnPlayerMeleeHit;
int OnPlayerPickupPwup;

IGameConfigManager *ConfigManager;
IGameConfig	*MainConfig;

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "ts") == 0)
		return AMXX_GAME_OK;
	return AMXX_GAME_BAD;
}

void OnAmxxAttach()
{
	MF_AddNatives(pl_funcs);
	MF_AddNatives(weap_funcs);
	REG_SVR_COMMAND("tse", CmdHandling);
	ConfigManager = MF_GetConfigManager();

	char error[256] = "";
	if (!ConfigManager->LoadGameConfigFile("modules.games", &MainConfig, error, sizeof(error)) && *error)
	{
		MF_Log("Could not read module.games gamedata: %s", error);
		return;
	}
}

void OnAmxxDetach()
{
	ConfigManager->CloseGameConfigFile(MainConfig);
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

void CmdHandling()
{
	const char *cmd = CMD_ARGV(1);
	if (strcmp(cmd, "version") == 0)
	{
		PRINT("%s | %s\n", MODULE_NAME, MODULE_VERSION);
		PRINT("Author: SNMetamorph\n");
		PRINT("Compiled: %s\n", __TIMESTAMP__);
		PRINT("Module URL: %s\n", MODULE_URL);
		PRINT("Credits: \n");
		PRINT("    AMX Mod X Development Team\n");
		PRINT("    Twilight Suzuka\n");
		PRINT("    XxAvalanchexX\n");
		PRINT("    KliPPy\n");
		PRINT("    WildCard65\n");
	}
	else if (strcmp(cmd, "debugmode") == 0)
	{
		IsDebugMode = !IsDebugMode;
		PRINT("The debug mode was switched %s.\n", IsDebugMode ? "on" : "off");
	}
	else if (strcmp(cmd, "incmsg") == 0)
	{
		const char *mname = CMD_ARGV(2);
		if (strcmp(mname, "ALL") == 0) {
			for (int i = 0; i < MAX_REG_MSGS; i++)
			{
				if (msgbinds[i]) {
					hmlist[i] = true;
					PRINT("Message \"%s\" recursively added to hooking list.\n", msgbinds[i]);
				}
			}
		}
		else {
			int msgid = GetMsgIDByName(mname);
			if (msgid) {
				hmlist[msgid] = true;
				PRINT("Message \"%s\" (ID: %d) successfully included to hooking list.\n", mname, msgid);
			}
			else
				PRINT("Including failed - cannot to find message with name \"%s\".\n", mname);
		}
	}
	else if (strcmp(cmd, "excmsg") == 0)
	{
		const char *mname = CMD_ARGV(2);
		if (strcmp(mname, "ALL") == 0) {
			for (int i = 0; i < MAX_REG_MSGS; i++)
				if (msgbinds[i]) 
					hmlist[i] = false;
			PRINT("Hooking list was cleaned up.\n");
		}
		else {
			int msgid = GetMsgIDByName(mname);
			if (msgid) {
				hmlist[msgid] = false;
				PRINT("Message \"%s\" (ID: %d) successfully excluded from hooking list.\n", mname, msgid);
			}
			else
				PRINT("Excluding failed - cannot to find message with name \"%s\".\n", mname);
		}
	}
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
		if (pl->HooksInfo.PwupDuration && pl->HooksInfo.PwupType)
			MF_ExecuteForward(OnPlayerPickupPwup, static_cast<cell>(pl->PlayerIndex), static_cast<cell>(pl->HooksInfo.PwupType), static_cast<cell>(pl->HooksInfo.PwupDuration));
	}

	RETURN_META(MRES_IGNORED);
}

int RegUserMsg_Post(const char *msgname, int size)
{
	int msgid = META_RESULT_ORIG_RET(int);
	msgbinds[msgid] = msgname;
	HookMsg *msgdecl = GetMsgDeclareByName(msgname);
	if (msgdecl->name && !msgdecl->id)
	{
		msgdecl->id = msgid;
		if (msgdecl->endmsg)
			GameMsgsEnd[msgid] = msgdecl->callback;
		else
			GameMsgs[msgid] = msgdecl->callback;
		if (IsDebugMode)
			PRINT("%s registered with ID %d\n", msgdecl->name, msgdecl->id);
	}
	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void MessageBegin_Post(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	if (ed)
		MsgPlayer = Player(ENTINDEX(ed));
	else
		MsgPlayer = NULL;
	MsgState = 0;
	if (msg_type < 0 || msg_type >= MAX_REG_MSGS)
		msg_type = 0;
	MFunction = GameMsgs[msg_type];
	MFunctionEnd = GameMsgsEnd[msg_type];
	MsgID = msg_type;
	if (IsDebugMode)
		if (hmlist[msg_type])
			PRINT("%s -> %s\n", msgbinds[msg_type], MsgPlayer ? STRING(MsgPlayer->PlayerEdict->v.netname) : deststypes[msg_dest]);
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
			if (!MsgPlayer->HooksInfo.PwupFlag)
				MsgPlayer->HooksInfo.PwupType = *(uint16_t *)data;
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
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("BYTE | %d\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteChar_Post(int val) {
	if (IsDebugMode) 
		if (hmlist[MsgID])
			PRINT("CHAR | %d\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteShort_Post(int val) {
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("SHORT | %d\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteLong_Post(int val) {
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("LONG | %d\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteAngle_Post(float val) {
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("ANGLE | %f\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteCoord_Post(float val) {
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("COORD | %f\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}

void WriteString_Post(const char *sz) {
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("STRING | %s\n", sz);
	if (MFunction) (*MFunction)((void *)sz);
	RETURN_META(MRES_IGNORED);
}

void WriteEntity_Post(int val) {
	if (IsDebugMode)
		if (hmlist[MsgID])
			PRINT("ENTITY | %d\n", val);
	if (MFunction) (*MFunction)((void *)&val);
	RETURN_META(MRES_IGNORED);
}