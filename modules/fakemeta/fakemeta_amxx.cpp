// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#include "fakemeta_amxx.h"
#include "sh_stack.h"

IGameConfig *CommonConfig;
IGameConfig *GamerulesConfig;
IGameConfigManager *ConfigManager;

HLTypeConversion TypeConversion;
void **GameRulesAddress;

void OnAmxxAttach()
{
	initialze_offsets();
	initialize_glb_offsets();

	MF_AddNatives(engfunc_natives);
	MF_AddNatives(dllfunc_natives);
	MF_AddNatives(pev_natives);
	MF_AddNatives(forward_natives);
	MF_AddNatives(pdata_natives);
	MF_AddNatives(tr_Natives);
	MF_AddNatives(glb_natives);
	MF_AddNatives(ext2_natives);
	MF_AddNatives(misc_natives);
	MF_AddNatives(pdata_entities_natives);
	MF_AddNatives(pdata_gamerules_natives);

	g_kvd_glb.kvd.szClassName = const_cast<char *>(g_kvd_glb.cls.chars());
	g_kvd_glb.kvd.szKeyName = const_cast<char *>(g_kvd_glb.key.chars());
	g_kvd_glb.kvd.szValue = const_cast<char *>(g_kvd_glb.val.chars());
	g_kvd_glb.kvd.fHandled = 0;

	ConfigManager = MF_GetConfigManager();

	char error[256] = "";

	if (!ConfigManager->LoadGameConfigFile("common.games", &CommonConfig, error, sizeof(error)) && error[0] != '\0')
	{
		MF_Log("get/set_ent_data* natives have been disabled because common.games gamedata could not be read: %s", error);
		return;
	}

	*error = '\0';

	if (!ConfigManager->LoadGameConfigFile("common.games/gamerules.games", &GamerulesConfig, error, sizeof(error)) && error[0] != '\0')
	{
		MF_Log("get/set_gamerules_* natives have been disabled because common.games/gamerules.games gamedata could not be read: %s", error);
		return;
	}

	void *address = nullptr;

	if (!CommonConfig->GetAddress("g_pGameRules", &address) || !address)
	{
		MF_Log("get/set_gamerules_* natives have been disabled because g_pGameRules address could not be found. ");
		return;
	}

#if defined(KE_WINDOWS)
	GameRulesAddress = *reinterpret_cast<void***>(address);
#else
	GameRulesAddress = reinterpret_cast<void**>(address);
#endif
}

void OnPluginsLoaded()
{
	TypeConversion.init();
}

extern CStack<TraceResult *> g_FreeTRs;
extern ke::Vector<KVD_Wrapper *> g_KVDWs;
extern ke::Vector<KVD_Wrapper *> g_FreeKVDWs;

void OnAmxxDetach()
{
	ConfigManager->CloseGameConfigFile(CommonConfig);
	ConfigManager->CloseGameConfigFile(GamerulesConfig);

	while (!g_FreeTRs.empty())
	{
		delete g_FreeTRs.front();
		g_FreeTRs.pop();
	}

	while (!g_KVDWs.empty())
		delete g_KVDWs.popCopy();

	while (!g_FreeKVDWs.empty())
		delete g_FreeKVDWs.popCopy();
}

void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	g_pFunctionTable_Post->pfnServerDeactivate = FMH_ServerDeactivate_Post;
	RETURN_META(MRES_IGNORED);
}

#define RESETD(tcall) \
	g_pFunctionTable->pfn##tcall =0; \
	g_pFunctionTable_Post->pfn##tcall =NULL; \
	Engine[FM_##tcall].clear(); \
	EnginePost[FM_##tcall].clear(); \
	EngineAddrs[FM_##tcall] = NULL; \
	EngineAddrsPost[FM_##tcall] = NULL;

#define RESETE(call) \
	g_pengfuncsTable->pfn##call = NULL; \
	g_pengfuncsTable_Post->pfn##call = NULL; \
	Engine[FM_##call].clear(); \
	EnginePost[FM_##call].clear(); \
	EngineAddrs[FM_##call] = NULL; \
	EngineAddrsPost[FM_##call] = NULL;

#define RESETN(call) \
	g_pNewFunctionsTable->pfn##call = NULL; \
	g_pNewFunctionsTable_Post->pfn##call = NULL; \
	Engine[FM_##call].clear(); \
	EnginePost[FM_##call].clear(); \
	EngineAddrs[FM_##call] = NULL; \
	EngineAddrsPost[FM_##call] = NULL;

void FMH_ServerDeactivate_Post()
{
	// Reset all call lists here.
	// NULL all function tables
	RESETE(PrecacheModel);
	RESETE(PrecacheSound);
	RESETE(SetModel);
	RESETE(ModelIndex);
	RESETE(ModelFrames);
	RESETE(SetSize);
	RESETE(ChangeLevel);
	RESETE(VecToYaw);
	RESETE(VecToAngles);
	RESETE(MoveToOrigin);
	RESETE(ChangeYaw);
	RESETE(ChangePitch);
	RESETE(FindEntityByString);
	RESETE(GetEntityIllum);
	RESETE(FindEntityInSphere);
	RESETE(FindClientInPVS);
	RESETE(EntitiesInPVS);
	RESETE(MakeVectors);
	RESETE(AngleVectors);
	RESETE(CreateEntity);
	RESETE(RemoveEntity);
	RESETE(CreateNamedEntity);
	RESETE(MakeStatic);
	RESETE(EntIsOnFloor);
	RESETE(DropToFloor);
	RESETE(WalkMove);
	RESETE(SetOrigin);
	RESETE(EmitSound);
	RESETE(EmitAmbientSound);
	RESETE(TraceLine);
	RESETE(TraceToss);
	RESETE(TraceMonsterHull);
	RESETE(TraceHull);
	RESETE(TraceModel);
	RESETE(TraceTexture);
	RESETE(TraceSphere);
	RESETE(GetAimVector);
	RESETE(ParticleEffect);
	RESETE(LightStyle);
	RESETE(DecalIndex);
	RESETE(PointContents);
	RESETE(FreeEntPrivateData);
	RESETE(SzFromIndex);
	RESETE(AllocString);
	RESETE(RegUserMsg);
	RESETE(AnimationAutomove);
	RESETE(GetBonePosition);
	RESETE(GetAttachment);
	RESETE(SetView);
	RESETE(Time);
	RESETE(CrosshairAngle);
	RESETE(FadeClientVolume);
	RESETE(SetClientMaxspeed);
	RESETE(CreateFakeClient);
	RESETE(RunPlayerMove);
	RESETE(NumberOfEntities);
	RESETE(StaticDecal);
	RESETE(PrecacheGeneric);
	RESETE(BuildSoundMsg);
	RESETE(GetPhysicsKeyValue);
	RESETE(SetPhysicsKeyValue);
	RESETE(GetPhysicsInfoString);
	RESETE(PrecacheEvent);
	RESETE(PlaybackEvent);
	RESETE(CheckVisibility);
	RESETE(GetCurrentPlayer);
	RESETE(CanSkipPlayer);
	RESETE(SetGroupMask);
	RESETE(Voice_GetClientListening);
	RESETE(Voice_SetClientListening);
	RESETE(InfoKeyValue);
	RESETE(SetKeyValue);
	RESETE(SetClientKeyValue);
	RESETE(MessageBegin);
	RESETE(MessageEnd);
	RESETE(WriteByte);
	RESETE(WriteChar);
	RESETE(WriteShort);
	RESETE(WriteLong);
	RESETE(WriteAngle);
	RESETE(WriteCoord);
	RESETE(WriteString);
	RESETE(WriteEntity);
	RESETE(CVarGetFloat);
	RESETE(CVarGetString);
	RESETE(CVarSetFloat);
	RESETE(CVarSetString);
	RESETE(AlertMessage);
	RESETE(CreateInstancedBaseline);
	RESETE(GetInfoKeyBuffer);
	RESETE(ClientPrintf);
	RESETE(GetPlayerAuthId);
	RESETE(GetPlayerWONId);
	RESETE(IsMapValid);
	RESETE(ServerPrint);

	RESETD(Spawn);
	RESETD(Think);
	RESETD(Use);
	RESETD(Touch);
	RESETD(Blocked);
	RESETD(KeyValue);
	RESETD(SetAbsBox);
	RESETD(ClientConnect);
	RESETD(ClientDisconnect);
	RESETD(ClientKill);
	RESETD(ClientPutInServer);
	RESETD(ClientCommand);
	RESETD(ServerDeactivate);
	RESETD(PlayerPreThink);
	RESETD(PlayerPostThink);
	RESETD(StartFrame);
	RESETD(ParmsNewLevel);
	RESETD(ParmsChangeLevel);
	RESETD(GetGameDescription);
	RESETD(SpectatorConnect);
	RESETD(SpectatorDisconnect);
	RESETD(SpectatorThink);
	RESETD(Sys_Error);
	RESETD(PM_FindTextureType);
	RESETD(RegisterEncoders);
	RESETD(CreateInstancedBaselines);
	RESETD(AllowLagCompensation);
	RESETD(ClientUserInfoChanged);
	RESETD(UpdateClientData);
	RESETD(AddToFullPack);
	RESETD(CmdStart);
	RESETD(CmdEnd);
	RESETD(CreateBaseline);

	RESETN(OnFreeEntPrivateData);
	RESETN(GameShutdown);
	RESETN(ShouldCollide);

	g_pFunctionTable->pfnServerActivate = ServerActivate;

	RETURN_META(MRES_IGNORED);
}
