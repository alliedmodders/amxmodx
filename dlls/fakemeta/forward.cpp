#include "fakemeta_amxx.h"


CVector<int> Engine[ENGFUNC_NUM+10];
CVector<int> EnginePost[ENGFUNC_NUM+10];
cell mCellResult;
cell mlCellResult;
float mFloatResult;
float mlFloatResult;
const char *mStringResult;
const char *mlStringResult;
int retType = 0;
int lastFmRes = FMRES_IGNORED;

#include "forwardmacros.h"

META_RES mswi(int fmres)
{
	if (fmres == FMRES_IGNORED)
		return MRES_IGNORED;
	if (fmres == FMRES_HANDLED)
		return MRES_HANDLED;
	if (fmres == FMRES_SUPERCEDE)
		return MRES_SUPERCEDE;
	if (fmres == FMRES_OVERRIDE)
		return MRES_OVERRIDE;
	return (META_RES)0;
}

void clfm()
{
	mCellResult = 0;
	mlCellResult = 0;
	mStringResult = "";
	mlStringResult = "";
	lastFmRes = FMRES_IGNORED;
	mFloatResult = 0.0;
	mlFloatResult = 0.0;
}

static cell AMX_NATIVE_CALL fm_return(AMX *amx, cell *params)
{
	int len;
	switch (params[1])
	{
	case FMV_STRING:
		{
			mStringResult = STRING(ALLOC_STRING(MF_GetAmxString(amx, params[2], 0 ,&len)));
			break;
		}
	case FMV_FLOAT:
		{
			mFloatResult = amx_ctof(params[2]);
			break;
		}
	case FMV_CELL:
		{
			mCellResult = params[2];
			break;
		}
	default:
		{
		return 0;
		break;
		}
	}

	retType = params[1];

	return 1;
}
/*
 * Begining of Game DLL->Engine hooks
 */

// pfnPrecacheModel
SIMPLE_INT_HOOK_STRING(PrecacheModel);

// pfnPrecacheSound
SIMPLE_INT_HOOK_STRING(PrecacheSound);

void SetModel(edict_t *e, const char *m)
{
	FM_ENG_HANDLE(FM_SetModel, (Engine[FM_SetModel].at(i), ENTINDEX(e), m));
	RETURN_META(mswi(lastFmRes));
}
void SetModel_post(edict_t *e, const char *m)
{
	FM_ENG_HANDLE(FM_SetModel, (Engine[FM_SetModel].at(i), ENTINDEX(e), m));
	RETURN_META(MRES_IGNORED);
}

// pfnModelIndex
SIMPLE_INT_HOOK_CONSTSTRING(ModelIndex);

// pfnModelFrames
SIMPLE_INT_HOOK_INT(ModelFrames);

// pfnSetSize
SIMPLE_VOID_HOOK_EDICT_CONSTVECT_CONSTVECT(SetSize);

// pfnChangeLevel
SIMPLE_VOID_HOOK_STRING_STRING(ChangeLevel);

// pfnVecToYaw
SIMPLE_FLOAT_HOOK_CONSTVECT(VecToYaw);

void VecToAngles(const float *rgflVectorIn, float *rgflVectorOut)
{
	cell vec1[3] = {amx_ftoc(rgflVectorIn[0]), amx_ftoc(rgflVectorIn[1]), amx_ftoc(rgflVectorIn[2])};
	cell vec2[3] = {amx_ftoc(rgflVectorOut[0]), amx_ftoc(rgflVectorOut[1]), amx_ftoc(rgflVectorOut[2])};;
	cell retvec1 = MF_PrepareCellArray(vec1, 3);
	cell retvec2 = MF_PrepareCellArray(vec2, 3);
	FM_ENG_HANDLE(FM_VecToAngles, (Engine[FM_VecToAngles].at(i),  retvec1, retvec2));
	RETURN_META(mswi(lastFmRes));
}
void VecToAngles_post(const float *rgflVectorIn, float *rgflVectorOut)
{
	cell vec1[3] = {amx_ftoc(rgflVectorIn[0]), amx_ftoc(rgflVectorIn[1]), amx_ftoc(rgflVectorIn[2])};
	cell vec2[3] = {amx_ftoc(rgflVectorOut[0]), amx_ftoc(rgflVectorOut[1]), amx_ftoc(rgflVectorOut[2])};;
	cell retvec1 = MF_PrepareCellArray(vec1, 3);
	cell retvec2 = MF_PrepareCellArray(vec2, 3);
	FM_ENG_HANDLE(FM_VecToAngles, (Engine[FM_VecToAngles].at(i),  retvec1, retvec2));
	RETURN_META(MRES_IGNORED);
}

void MoveToOrigin(edict_t *ent, const float *pflGoal, float dist, int iMoveType)
{
	cell vec[3] = {amx_ftoc(pflGoal[0]), amx_ftoc(pflGoal[1]), amx_ftoc(pflGoal[2])};
	cell retvec = MF_PrepareCellArray(vec, 3);
	FM_ENG_HANDLE(FM_MoveToOrigin, (Engine[FM_MoveToOrigin].at(i),  ENTINDEX(ent), retvec, dist, iMoveType));
	RETURN_META(mswi(lastFmRes));
}
void MoveToOrigin_post(edict_t *ent, const float *pflGoal, float dist, int iMoveType)
{
	cell vec[3] = {amx_ftoc(pflGoal[0]), amx_ftoc(pflGoal[1]), amx_ftoc(pflGoal[2])};
	cell retvec = MF_PrepareCellArray(vec, 3);
	FM_ENG_HANDLE(FM_MoveToOrigin, (Engine[FM_MoveToOrigin].at(i),  ENTINDEX(ent), retvec, dist, iMoveType));
	RETURN_META(MRES_IGNORED);
}

edict_t *FindEntityByString(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue)
{
	FM_ENG_HANDLE(FM_FindEntityByString, (Engine[FM_FindEntityByString].at(i),  ENTINDEX(pEdictStartSearchAfter), pszField, pszValue));
	RETURN_META_VALUE(mswi(lastFmRes), INDEXENT2((int)mlCellResult));
}
edict_t *FindEntityByString_post(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue)
{
	FM_ENG_HANDLE(FM_FindEntityByString, (Engine[FM_FindEntityByString].at(i),  ENTINDEX(pEdictStartSearchAfter), pszField, pszValue));
	RETURN_META_VALUE(MRES_IGNORED, INDEXENT2((int)mlCellResult));
}
// pfnGetEntityIllum
SIMPLE_INT_HOOK_EDICT(GetEntityIllum);

// pfnFindEntityInSphere
SIMPLE_EDICT_HOOK_EDICT_CONSTVECT_FLOAT(FindEntityInSphere);

// pfnFindClientInPVS
SIMPLE_EDICT_HOOK_EDICT(FindClientInPVS);

// pfnEntitiesInPVS
SIMPLE_EDICT_HOOK_EDICT(EntitiesInPVS);

// pfnMakeVectors
SIMPLE_VOID_HOOK_CONSTVECT(MakeVectors);

// pfnAngleVectors
SIMPLE_VOID_HOOK_CONSTVECT_VECT_VECT_VECT(AngleVectors);

// pfnChangeYaw
SIMPLE_VOID_HOOK_EDICT(ChangeYaw);

// pfnChangePitch
SIMPLE_VOID_HOOK_EDICT(ChangePitch);

// pfnCreateEntity
SIMPLE_EDICT_HOOK_VOID(CreateEntity);

// pfnRemoveEntity
SIMPLE_VOID_HOOK_EDICT(RemoveEntity);

// pfnCreateNamedEntity
SIMPLE_EDICT_HOOK_INT(CreateNamedEntity);

// pfnMakeStatic
SIMPLE_VOID_HOOK_EDICT(MakeStatic);

// pfnEntIsOnFloor
SIMPLE_INT_HOOK_EDICT(EntIsOnFloor);

// pfnDropToFloor
SIMPLE_INT_HOOK_EDICT(DropToFloor);

// pfnSetOrigin
SIMPLE_VOID_HOOK_EDICT_CONSTVECT(SetOrigin);

// pfnEmitSound
SIMPLE_VOID_HOOK_EDICT_INT_CONSTSTRING_FLOAT_FLOAT_INT_INT(EmitSound);

// pfnEmitAmbientSound
SIMPLE_VOID_HOOK_EDICT_VECT_CONSTSTRING_FLOAT_FLOAT_INT_INT(EmitAmbientSound);

// pfnGetAimVector
SIMPLE_VOID_HOOK_EDICT_FLOAT_VECT(GetAimVector);

// pfnParticleEffect
SIMPLE_VOID_HOOK_CONSTVECT_CONSTVECT_FLOAT_FLOAT(ParticleEffect);

// pfnLightStyle
SIMPLE_VOID_HOOK_INT_STRING(LightStyle);

// pfnDecalIndex
SIMPLE_INT_HOOK_CONSTSTRING(DecalIndex);

// pfnPointContents
SIMPLE_INT_HOOK_CONSTVECT(PointContents);

// pfnMessageBegin
SIMPLE_VOID_HOOK_INT_INT_CONSTVECT_EDICT(MessageBegin);

// pfnMessageEnd
SIMPLE_VOID_HOOK_VOID(MessageEnd);

// pfnWriteByte
SIMPLE_VOID_HOOK_INT(WriteByte);

// pfnWriteChar
SIMPLE_VOID_HOOK_INT(WriteChar);

// pfnWriteShort
SIMPLE_VOID_HOOK_INT(WriteShort);

// pfnWriteLong
SIMPLE_VOID_HOOK_INT(WriteLong);

// pfnWriteAngle
SIMPLE_VOID_HOOK_FLOAT(WriteAngle);

// pfnWriteCoord
SIMPLE_VOID_HOOK_FLOAT(WriteCoord);

// pfnWriteString
SIMPLE_VOID_HOOK_CONSTSTRING(WriteString);

// pfnWriteEntity
SIMPLE_VOID_HOOK_INT(WriteEntity);

SIMPLE_FLOAT_HOOK_CONSTSTRING(CVarGetFloat);

SIMPLE_CONSTSTRING_HOOK_CONSTSTRING(CVarGetString);

SIMPLE_VOID_HOOK_CONSTSTRING_FLOAT(CVarSetFloat);

SIMPLE_VOID_HOOK_CONSTSTRING_CONSTSTRING(CVarSetString);

// pfnFreeEntPrivateData
SIMPLE_VOID_HOOK_EDICT(FreeEntPrivateData);

// pfnSzFromIndex
SIMPLE_CONSTSTRING_HOOK_INT(SzFromIndex);

// pfnAllocString
SIMPLE_INT_HOOK_CONSTSTRING(AllocString);

SIMPLE_INT_HOOK_CONSTSTRING_INT(RegUserMsg);

// pfnAnimationAutomove
SIMPLE_VOID_HOOK_CONSTEDICT_FLOAT(AnimationAutomove);

// pfnSetView
SIMPLE_VOID_HOOK_CONSTEDICT_CONSTEDICT(SetView);

// pfnTime
SIMPLE_FLOAT_HOOK_VOID(Time);

// pfnCrosshairAngle
SIMPLE_VOID_HOOK_CONSTEDICT_FLOAT_FLOAT(CrosshairAngle);

SIMPLE_VOID_HOOK_CONSTEDICT_INT_INT_INT_INT(FadeClientVolume);

// pfnSetClientMaxspeed
SIMPLE_VOID_HOOK_CONSTEDICT_FLOAT(SetClientMaxspeed);

// pfnPrecacheGeneric
SIMPLE_INT_HOOK_STRING(PrecacheGeneric);

// pfnPrecacheEvent
SIMPLE_USHORT_HOOK_INT_CONSTSTRING(PrecacheEvent);

// pfnGetPhysicsKeyValue
SIMPLE_CONSTSTRING_HOOK_CONSTEDICT_CONSTSTRING(GetPhysicsKeyValue);

// pfnSetPhysicsKeyValue
SIMPLE_VOID_HOOK_CONSTEDICT_CONSTSTRING_CONSTSTRING(SetPhysicsKeyValue);

// pfnGetPhysicsInfoString
SIMPLE_CONSTSTRING_HOOK_CONSTEDICT(GetPhysicsInfoString);

// pfnPlaybackEvent
HOOK_PLAYBACK_EVENT(PlaybackEvent);

// pfnCreateFakeClient
SIMPLE_EDICT_HOOK_CONSTSTRING(CreateFakeClient);

// pfnNumberOfEntities
SIMPLE_INT_HOOK_VOID(NumberOfEntities);

// pfnGetCurrentPlayer
SIMPLE_INT_HOOK_VOID(GetCurrentPlayer);

// pfnCanSkipPlayer
SIMPLE_INT_HOOK_CONSTEDICT(CanSkipPlayer);

// pfnSetGroupMask
SIMPLE_VOID_HOOK_INT_INT(SetGroupMask);

// pfnVoice_GetClientListening
SIMPLE_BOOL_HOOK_INT_INT(Voice_GetClientListening);

// pfnVoice_SetClientListening
SIMPLE_BOOL_HOOK_INT_INT_BOOL(Voice_SetClientListening);

SIMPLE_STRING_HOOK_STRING_STRING(InfoKeyValue);

SIMPLE_VOID_HOOK_STRING_STRING_STRING(SetKeyValue);

SIMPLE_VOID_HOOK_INT_STRING_STRING_STRING(SetClientKeyValue);

SIMPLE_CONSTSTRING_HOOK_EDICT(GetPlayerAuthId);

SIMPLE_UINT_HOOK_EDICT(GetPlayerWONId);

SIMPLE_INT_HOOK_STRING(IsMapValid);

/*
 * Beginning of Engine->Game DLL hooks
 */

// pfnSpawn
SIMPLE_INT_HOOK_EDICT(Spawn);

// pfnThink
SIMPLE_VOID_HOOK_EDICT(Think);

// pfnUse
SIMPLE_VOID_HOOK_EDICT_EDICT(Use);

// pfnTouch
SIMPLE_VOID_HOOK_EDICT_EDICT(Touch);

// pfnBlocked
SIMPLE_VOID_HOOK_EDICT_EDICT(Blocked);

// pfnSetAbsBox
SIMPLE_VOID_HOOK_EDICT(SetAbsBox);

// pfnClientConnect
SIMPLE_BOOL_HOOK_EDICT_CONSTSTRING_CONSTSTRING_STRING128(ClientConnect);

// pfnClientDisconnect
SIMPLE_VOID_HOOK_EDICT(ClientDisconnect);

// pfnClientPutInServer
SIMPLE_VOID_HOOK_EDICT(ClientPutInServer);

// pfnClientKill
SIMPLE_VOID_HOOK_EDICT(ClientKill);

// pfnClientCommand
SIMPLE_VOID_HOOK_EDICT(ClientCommand);

// pfnServerDeactivate
SIMPLE_VOID_HOOK_VOID(ServerDeactivate);

// pfnPlayerPreThink
SIMPLE_VOID_HOOK_EDICT(PlayerPreThink);

// pfnPlayerPostThink
SIMPLE_VOID_HOOK_EDICT(PlayerPostThink);

// pfnStartFrame
SIMPLE_VOID_HOOK_VOID(StartFrame);

// pfnParmsNewLevel
SIMPLE_VOID_HOOK_VOID(ParmsNewLevel);

// pfnParmsChangeLevel
SIMPLE_VOID_HOOK_VOID(ParmsChangeLevel);

// pfnGetGameDescription
SIMPLE_CONSTSTRING_HOOK_VOID(GetGameDescription);

// pfnSpectatorConnect
SIMPLE_VOID_HOOK_EDICT(SpectatorConnect);

// pfnSpectatorDisconnect
SIMPLE_VOID_HOOK_EDICT(SpectatorDisconnect);

// pfnSpectatorThink
SIMPLE_VOID_HOOK_EDICT(SpectatorThink);

// pfnSys_Error
SIMPLE_VOID_HOOK_CONSTSTRING(Sys_Error);

// pfnPM_FindTextureType
SIMPLE_CHAR_HOOK_STRING(PM_FindTextureType);

// pfnRegisterEncoders
SIMPLE_VOID_HOOK_VOID(RegisterEncoders);

// pfnCreateInstancedBaselines
SIMPLE_VOID_HOOK_VOID(CreateInstancedBaselines);

// pfnAllowLagCompensation
SIMPLE_INT_HOOK_VOID(AllowLagCompensation);









static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params)
{
	int func = params[1];
	// You originally had both post coming from params[2] AND the function name.  I've moved post to 3.
	int post = params[3];
	if (func > FM_AllowLagCompensation || func < 1)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int len, fId=0;
	const char *funcname = MF_GetAmxString(amx, params[2], 0, &len);
	enginefuncs_t *engtable;

	DLL_FUNCTIONS *dlltable;

	if (post)
	{
		engtable = g_pengfuncsTable_Post;
		dlltable = g_pFunctionTable_Post;
	}
	else
	{
		engtable = g_pengfuncsTable;
		dlltable = g_pFunctionTable;
	}
	
	switch (func)
	{
	case FM_PrecacheModel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(PrecacheModel);
		break;
	case FM_PrecacheSound:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(PrecacheSound);
		break;
	case FM_SetModel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		ENGHOOK(SetModel);
		break;
	case FM_ModelIndex:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(ModelIndex);
		break;
	case FM_ModelFrames:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(ModelFrames);
		break;
	case FM_SetSize:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		ENGHOOK(SetSize);
		break;
	case FM_ChangeLevel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(ChangeLevel);
		break;
	case FM_VecToYaw:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		ENGHOOK(VecToYaw);
		break;
	case FM_VecToAngles:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_DONE);
		ENGHOOK(VecToAngles);
		break;
	case FM_MoveToOrigin:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_FLOAT, FP_CELL, FP_DONE);
		ENGHOOK(MoveToOrigin);
		break;
	case FM_ChangeYaw:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(ChangeYaw);
		break;
	case FM_ChangePitch:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(ChangePitch);
		break;
	case FM_FindEntityByString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(FindEntityByString);
		break;
	case FM_GetEntityIllum:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(GetEntityIllum);
		break;
	case FM_FindEntityInSphere:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_FLOAT, FP_DONE);
		ENGHOOK(FindEntityInSphere);
		break;
	case FM_FindClientInPVS:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(FindClientInPVS);
		break;
	case FM_EntitiesInPVS:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(EntitiesInPVS);
		break;
	case FM_MakeVectors:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		ENGHOOK(MakeVectors);
		break;
	case FM_AngleVectors:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_ARRAY, FP_ARRAY, FP_DONE);
		ENGHOOK(AngleVectors);
		break;
	case FM_CreateEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		ENGHOOK(CreateEntity);
		break;
	case FM_RemoveEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(RemoveEntity);
		break;
	case FM_CreateNamedEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(CreateNamedEntity);
		break;
	case FM_MakeStatic:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(MakeStatic);
		break;
	case FM_EntIsOnFloor:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(EntIsOnFloor);
		break;
	case FM_DropToFloor:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(DropToFloor);
		break;
	case FM_WalkMove:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_FLOAT, FP_CELL, FP_DONE);
//		ENGHOOK(WalkMove);
		break;
	case FM_SetOrigin:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_DONE);
		ENGHOOK(SetOrigin);
		break;
	case FM_EmitSound:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(EmitSound);
		break;
	case FM_EmitAmbientSound:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(EmitAmbientSound);
		break;
	case FM_TraceLine:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(TraceLine);
		break;
	case FM_TraceToss:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(TraceToss);
		break;
	case FM_TraceMonsterHull:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(TraceMonsterHull);
		break;
	case FM_TraceHull:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(TraceHull);
		break;
	case FM_TraceModel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(TraceModel);
		break;
	case FM_TraceTexture:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
//		ENGHOOK(TraceTexture);
		break;
	case FM_TraceSphere:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_FLOAT, FP_CELL, FP_DONE);
//		ENGHOOK(TraceSphere);
		break;
	case FM_GetAimVector:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_ARRAY, FP_DONE);
		ENGHOOK(GetAimVector);
		break;
	case FM_ParticleEffect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_DONE);
		ENGHOOK(ParticleEffect);
		break;
	case FM_LightStyle:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		ENGHOOK(LightStyle);
		break;
	case FM_DecalIndex:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(DecalIndex);
		break;
	case FM_PointContents:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		ENGHOOK(PointContents);
		break;

	case FM_MessageBegin:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
		ENGHOOK(MessageBegin);
		break;

	case FM_MessageEnd:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		ENGHOOK(MessageEnd);
		break;

	case FM_WriteByte:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(WriteByte);
		break;

	case FM_WriteChar:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(WriteChar);
		break;

	case FM_WriteShort:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(WriteShort);
		break;

	case FM_WriteLong:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(WriteLong);
		break;

	case FM_WriteAngle:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_FLOAT, FP_DONE);
		ENGHOOK(WriteAngle);
		break;

	case FM_WriteCoord:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_FLOAT, FP_DONE);
		ENGHOOK(WriteCoord);
		break;

	case FM_WriteString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(WriteString);
		break;

	case FM_WriteEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(WriteEntity);
		break;

	case FM_CVarGetFloat:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(CVarGetFloat);
		break;

	case FM_CVarGetString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(CVarGetString);
		break;

	case FM_CVarSetFloat:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_FLOAT, FP_DONE);
		ENGHOOK(CVarSetFloat);
		break;

	case FM_CVarSetString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(CVarSetString);
		break;

	case FM_FreeEntPrivateData:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		ENGHOOK(FreeEntPrivateData);
		break;

	case FM_SzFromIndex:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(SzFromIndex);
		break;

	case FM_AllocString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(AllocString);
		break;

	case FM_RegUserMsg:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_CELL, FP_DONE);
		ENGHOOK(RegUserMsg);
		break;

	case FM_AnimationAutomove:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_DONE);
		ENGHOOK(AnimationAutomove);
		break;

	case FM_GetBonePosition:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
//		ENGHOOK(GetBonePosition);
		break;

	case FM_GetAttachment:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
//		ENGHOOK(GetAttachment);
		break;

	case FM_SetView:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(SetView);
		break;

	case FM_Time:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		ENGHOOK(Time);
		break;
	case FM_CrosshairAngle:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_FLOAT, FP_DONE);
		ENGHOOK(CrosshairAngle);
		break;

	case FM_FadeClientVolume:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(FadeClientVolume);
		break;

	case FM_SetClientMaxspeed:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_DONE);
		ENGHOOK(SetClientMaxspeed);
		break;

	case FM_CreateFakeClient:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(CreateFakeClient);
		break;

	case FM_RunPlayerMove:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(RunPlayerMove);
		break;

	case FM_NumberOfEntities:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		ENGHOOK(NumberOfEntities);
		break;
	case FM_StaticDecal:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
//		ENGHOOK(StaticDecal);
		break;

	case FM_PrecacheGeneric:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(PrecacheGeneric);
		break;
	case FM_BuildSoundMsg:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
//		ENGHOOK(BuildSoundMsg);
		break;
	case FM_GetPhysicsKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		ENGHOOK(GetPhysicsKeyValue);
		break;
	case FM_SetPhysicsKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(SetPhysicsKeyValue);
		break;
	case FM_GetPhysicsInfoString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(GetPhysicsInfoString);
		break;
	case FM_PrecacheEvent:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		ENGHOOK(PrecacheEvent);
		break;
	case FM_PlaybackEvent:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(PlaybackEvent); // waaaaaaaaaah, its so damn big. :~(
		break;
	//EngFunc_CheckVisibility,			//)		( const edict_t *entity, unsigned char *pset );
	case FM_CheckVisibility:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
//		ENGHOOK(CheckVisibility);
		break;

	case FM_GetCurrentPlayer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		ENGHOOK(GetCurrentPlayer);
		break;
	//EngFunc_CanSkipPlayer,			//)			( const edict_t *player );
	case FM_CanSkipPlayer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(CanSkipPlayer);
		break;
	//EngFunc_SetGroupMask,				//)			( int mask, int op );
	case FM_SetGroupMask:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(SetGroupMask);
		break;
	//EngFunc_GetClientListening,	// bool (int iReceiver, int iSender)
	case FM_Voice_GetClientListening:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(Voice_GetClientListening);
		break;
	//EngFunc_SetClientListening,	// bool (int iReceiver, int iSender, bool Listen)
	case FM_Voice_SetClientListening:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_DONE); // TODO: bool as cell 3rd arg?
		ENGHOOK(Voice_SetClientListening);
		break;
	//EngFunc_InfoKeyValue,	// char*	)			(char *infobuffer, char *key);
	case FM_InfoKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(InfoKeyValue);
		break;
	//EngFunc_SetKeyValue,	// void )			(char *infobuffer, char *key, char *value);
	case FM_SetKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(SetKeyValue);
		break;
	//EngFunc_SetClientKeyValue	 // void )		(int clientIndex, char *infobuffer, char *key, char *value);
	case FM_SetClientKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_STRING, FP_DONE);
		ENGHOOK(SetClientKeyValue);
		break;
	case FM_GetPlayerAuthId:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(GetPlayerAuthId);
		break;
	case FM_GetPlayerWONId:
		fId = MF_RegisterSPForwardByName(amx,funcname,FP_CELL,FP_DONE);
		ENGHOOK(GetPlayerWONId);
		break;
	case FM_IsMapValid:
		fId = MF_RegisterSPForwardByName(amx, funcname,FP_STRING,FP_DONE);
		ENGHOOK(IsMapValid);
		break;



/*
 * Begin of DLLFuncs 
 */

	case FM_Spawn:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(Spawn);
		break;

	case FM_Think:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(Think);
		break;

	case FM_Use:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(Use);
		break;

	case FM_Touch:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(Touch);
		break;

	case FM_Blocked:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(Blocked);
		break;
	/*

	TODO: Expand the structure (simple: just a bunch of strings and a float.)

	//DLLFunc_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd );
	case FM_KeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_????, FP_DONE);
		break;
	*/
	//DLLFunc_SetAbsBox,			// void )			( edict_t *pent );
	case FM_SetAbsBox:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(SetAbsBox);
		break;

	case FM_ClientConnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_STRING, FP_DONE); // TODO: 4th arg must be of set size 128?
		DLLHOOK(ClientConnect);
		break;

	case FM_ClientDisconnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(ClientDisconnect);
		break;

	case FM_ClientKill:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(ClientKill);
		break;

	case FM_ClientPutInServer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(ClientPutInServer);
		break;

	case FM_ClientCommand:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(ClientCommand);
		break;

	case FM_ServerDeactivate:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(ServerDeactivate);
		break;

	case FM_PlayerPreThink:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(PlayerPreThink);
		break;

	case FM_PlayerPostThink:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(PlayerPostThink);
		break;

	case FM_StartFrame:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(StartFrame);
		break;

	case FM_ParmsNewLevel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(ParmsNewLevel);
		break;

	case FM_ParmsChangeLevel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(ParmsChangeLevel);
		break;

	case FM_GetGameDescription:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(GetGameDescription);
		break;

	case FM_SpectatorConnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(SpectatorConnect);
		break;

	case FM_SpectatorDisconnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(SpectatorDisconnect);
		break;

	case FM_SpectatorThink:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(SpectatorThink);
		break;

	case FM_Sys_Error:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		DLLHOOK(Sys_Error);
		break;

	case FM_PM_FindTextureType:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		DLLHOOK(PM_FindTextureType);
		break;

	case FM_RegisterEncoders:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(RegisterEncoders);
		break;

	case FM_CreateInstancedBaselines:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(CreateInstancedBaselines);
		break;

	case FM_AllowLagCompensation:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		DLLHOOK(AllowLagCompensation);
		break;
#if 0

	// I know this does not fit with DLLFUNC(), but I dont want another native just for it.
	MetaFunc_CallGameEntity	// bool	(plid_t plid, const char *entStr,entvars_t *pev);
#endif

	default:
		fId = 0;
		break;
	}

	if (!fId)
		return 0;

	if (post)
	{
		EnginePost[func].push_back(fId);
	} else {
		Engine[func].push_back(fId);
	}

	return 1;
}

AMX_NATIVE_INFO forward_natives[] = {
	{ "register_forward",	register_forward },
	{ "forward_return",		fm_return },
	{ NULL,					NULL }
};