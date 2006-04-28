#include "fakemeta_amxx.h"

edict_t *g_player_edicts[33]; // Used for INDEXENT() forward.

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
	g_kvd_2.szClassName = "";
	g_kvd_2.szKeyName = "";
	g_kvd_2.szValue = "";
	g_kvd_glb.kvd = &g_kvd_2;
}

int GetHullBounds(int hullnumber, float *mins, float *maxs);
// sawce:  Do not null out the forward for ServerActivate.  It's required for the INDEXENT() fix. (I don't think ServerActivate is planned on being forwarded anyway)
void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	for(int i = 1; i <= gpGlobals->maxClients;i++) 
		g_player_edicts[i]=pEdictList + i;
	RETURN_META(MRES_IGNORED);
}
#define RESETD(tcall) \
	g_pFunctionTable->pfn##tcall =0; \
	g_pFunctionTable_Post->pfn##tcall =NULL; \
	Engine[FM_##tcall].clear(); \
	EnginePost[FM_##tcall].clear()

#define RESETE(call) \
	g_pengfuncsTable->pfn##call = NULL; \
	g_pengfuncsTable_Post->pfn##call = NULL; \
	Engine[FM_##call].clear(); \
	EnginePost[FM_##call].clear() 

#define RESETN(call) \
	g_pNewFunctionsTable->pfn##call = NULL; \
	g_pNewFunctionsTable_Post->pfn##call = NULL; \
	Engine[FM_##call].clear(); \
	EnginePost[FM_##call].clear();

void OnPluginsLoaded()
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

	RESETN(OnFreeEntPrivateData);
	RESETN(GameShutdown);
	RESETN(ShouldCollide);

	g_pFunctionTable->pfnServerActivate = ServerActivate;
}
