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

#define ENGHOOK(pfnCall) \
	if (engtable->pfn##pfnCall == NULL) \
		engtable->pfn##pfnCall = pfnCall

#define FM_ENG_HANDLE(pfnCall, pfnArgs) \
	register unsigned int i = 0; \
	clfm(); \
	int fmres = FMRES_IGNORED; \
	for (i=0; i<Engine[pfnCall].size(); i++) \
	{ \
		fmres = MF_ExecuteForward pfnArgs; \
		if (fmres >= lastFmRes) { \
			if (retType == FMV_STRING) \
				mlStringResult = mStringResult; \
			else if (retType == FMV_CELL) \
				mlCellResult = mCellResult; \
			else if (retType == FMV_FLOAT) \
				mlFloatResult = mFloatResult; \
			lastFmRes = fmres; \
		} \
	}

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

int PrecacheModel(char *s)
{
	FM_ENG_HANDLE(FM_PrecacheModel, (Engine[FM_PrecacheModel].at(i), s));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

int PrecacheSound(char *s)
{
	FM_ENG_HANDLE(FM_PrecacheSound, (Engine[FM_PrecacheSound].at(i), s));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

void SetModel(edict_t *e, const char *m)
{
	FM_ENG_HANDLE(FM_SetModel, (Engine[FM_SetModel].at(i), ENTINDEX(e), m));
	RETURN_META(mswi(lastFmRes));
}

int ModelIndex(const char *m)
{
	FM_ENG_HANDLE(FM_ModelIndex, (Engine[FM_ModelIndex].at(i), m));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

int ModelFrames(int modelIndex)
{
	FM_ENG_HANDLE(FM_ModelIndex, (Engine[FM_ModelIndex].at(i), modelIndex));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

void SetSize(edict_t *e, const float *rgflMin, const float *rgflMax)
{
	cell vec1[3] = {amx_ftoc(rgflMin[0]), amx_ftoc(rgflMin[1]), amx_ftoc(rgflMin[2])};
	cell vec2[3] = {amx_ftoc(rgflMax[0]), amx_ftoc(rgflMax[1]), amx_ftoc(rgflMax[2])};;
	cell retvec1 = MF_PrepareCellArray(vec1, 3);
	cell retvec2 = MF_PrepareCellArray(vec2, 3);
	FM_ENG_HANDLE(FM_SetSize, (Engine[FM_SetSize].at(i), ENTINDEX(e), retvec1, retvec2));
	RETURN_META(mswi(lastFmRes));
}

void ChangeLevel(char *s1, char *s2)
{
	FM_ENG_HANDLE(FM_ChangeLevel, (Engine[FM_ChangeLevel].at(i), s1, s2));
	RETURN_META(mswi(lastFmRes));
}

float VecToYaw(const float *rgflVector)
{
	cell vec[3] = {amx_ftoc(rgflVector[0]), amx_ftoc(rgflVector[1]), amx_ftoc(rgflVector[2])};
	cell retvec = MF_PrepareCellArray(vec, 3);
	FM_ENG_HANDLE(FM_VecToYaw, (Engine[FM_VecToYaw].at(i), retvec));
	RETURN_META_VALUE(mswi(lastFmRes), mlFloatResult);
}

void VecToAngles(const float *rgflVectorIn, float *rgflVectorOut)
{
	cell vec1[3] = {amx_ftoc(rgflVectorIn[0]), amx_ftoc(rgflVectorIn[1]), amx_ftoc(rgflVectorIn[2])};
	cell vec2[3] = {amx_ftoc(rgflVectorOut[0]), amx_ftoc(rgflVectorOut[1]), amx_ftoc(rgflVectorOut[2])};;
	cell retvec1 = MF_PrepareCellArray(vec1, 3);
	cell retvec2 = MF_PrepareCellArray(vec2, 3);
	FM_ENG_HANDLE(FM_VecToAngles, (Engine[FM_VecToAngles].at(i),  retvec1, retvec2));
	RETURN_META(mswi(lastFmRes));
}

void MoveToOrigin(edict_t *ent, const float *pflGoal, float dist, int iMoveType)
{
	cell vec[3] = {amx_ftoc(pflGoal[0]), amx_ftoc(pflGoal[1]), amx_ftoc(pflGoal[2])};
	cell retvec = MF_PrepareCellArray(vec, 3);
	FM_ENG_HANDLE(FM_MoveToOrigin, (Engine[FM_MoveToOrigin].at(i),  ENTINDEX(ent), retvec, dist, iMoveType));
	RETURN_META(mswi(lastFmRes));
}

void ChangeYaw(edict_t *ent)
{
	FM_ENG_HANDLE(FM_ChangeYaw, (Engine[FM_ChangeYaw].at(i),  ENTINDEX(ent)));
	RETURN_META(mswi(lastFmRes));
}

void ChangePitch(edict_t *ent)
{
	FM_ENG_HANDLE(FM_ChangePitch, (Engine[FM_ChangePitch].at(i),  ENTINDEX(ent)));
	RETURN_META(mswi(lastFmRes));
}

edict_t *FindEntityByString(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue)
{
	FM_ENG_HANDLE(FM_FindEntityByString, (Engine[FM_FindEntityByString].at(i),  ENTINDEX(pEdictStartSearchAfter), pszField, pszValue));
	RETURN_META_VALUE(mswi(lastFmRes), INDEXENT((int)mlCellResult));
}

int GetEntityIllum(edict_t *pent)
{
	FM_ENG_HANDLE(FM_ChangePitch, (Engine[FM_ChangePitch].at(i),  ENTINDEX(pent)));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params)
{
	int func = params[1];
	int post = params[2];
	if (func > ENGFUNC_NUM || func < 1)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int len, fId=0;
	const char *funcname = MF_GetAmxString(amx, params[2], 0, &len);

	enginefuncs_t *engtable;

	if (post)
		engtable = &g_EngineFuncs_Table;
	else
		engtable = &g_EngineFuncs_Post_Table;
	
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
		break;
	case FM_FindClientInPVS:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_EntitiesInPVS:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_MakeVectors:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		break;
	case FM_AngleVectors:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_ARRAY, FP_ARRAY, FP_DONE);
		break;
	case FM_CreateEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	case FM_RemoveEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_CreateNamedEntity:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	case FM_MakeStatic:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_EntIsOnFloor:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_DropToFloor:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_WalkMove:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_FLOAT, FP_CELL, FP_DONE);
		break;
	case FM_SetOrigin:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_DONE);
		break;
	case FM_EmitSound:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_EmitAmbientSound:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_TraceLine:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_TraceToss:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_TraceMonsterHull:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_TraceHull:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_TraceModel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_TraceTexture:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		break;
	case FM_TraceSphere:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_FLOAT, FP_CELL, FP_DONE);
		break;
	case FM_GetAimVector:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_ARRAY, FP_DONE);
		break;
	case FM_ParticleEffect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_DONE);
		break;
	case FM_LightStyle:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		break;
	case FM_DecalIndex:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	case FM_PointContents:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		break;
	case FM_FreeEntPrivateData:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_DONE);
		break;
	case FM_SzFromIndex:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	case FM_AllocString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	case FM_RegUserMsg:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_CELL, FP_DONE);
		break;
	case FM_AnimationAutomove:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_DONE);
		break;
	case FM_GetBonePosition:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		break;
	case FM_GetAttachment:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		break;
	case FM_SetView:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_Time:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	case FM_CrosshairAngle:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_FLOAT, FP_DONE);
		break;
	case FM_FadeClientVolume:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_SetClientMaxspeed:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_FLOAT, FP_DONE);
		break;
	case FM_CreateFakeClient:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	case FM_RunPlayerMove:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_NumberOfEntities:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	case FM_StaticDecal:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		break;
	case FM_PrecacheGeneric:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	case FM_BuildSoundMsg:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
		break;
	case FM_GetPhysicsKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		break;
	case FM_SetPhysicsKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_DONE);
		break;
	case FM_GetPhysicsInfoString:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	case FM_PrecacheEvent:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		break;
	case FM_PlaybackEvent:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		break;
	//EngFunc_CheckVisibility,			//)		( const edict_t *entity, unsigned char *pset );
	case FM_CheckVisibility:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		break;
	//EngFunc_GetCurrentPlayer,			//)		( void );
	case FM_GetCurrentPlayer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	//EngFunc_CanSkipPlayer,			//)			( const edict_t *player );
	case FM_CanSkipPlayer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//EngFunc_SetGroupMask,				//)			( int mask, int op );
	case FM_SetGroupMask:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	//EngFunc_GetClientListening,	// bool (int iReceiver, int iSender)
	case FM_GetClientListening:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	//EngFunc_SetClientListening,	// bool (int iReceiver, int iSender, bool Listen)
	case FM_SetClientListening:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_DONE); // TODO: bool as cell 3rd arg?
		break;
	//EngFunc_MessageBegin,	// void (int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
	case FM_MessageBegin:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
		break;
	//EngFunc_WriteCoord,		// void (float)
	case FM_WriteCoord:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_FLOAT, FP_DONE);
		break;
	//EngFunc_WriteAngle,		// void (float)
	case FM_WriteAngle:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_FLOAT, FP_DONE);
		break;
	//EngFunc_InfoKeyValue,	// char*	)			(char *infobuffer, char *key);
	case FM_InfoKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_STRING, FP_DONE);
		break;
	//EngFunc_SetKeyValue,	// void )			(char *infobuffer, char *key, char *value);
	case FM_SetKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_STRING, FP_STRING, FP_DONE);
		break;
	//EngFunc_SetClientKeyValue	 // void )		(int clientIndex, char *infobuffer, char *key, char *value);
	case FM_SetClientKeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_STRING, FP_DONE);
		break;
	//DLLFunc_GameInit,	// void)			( void );				
	case FM_GameInit:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	//DLLFunc_Spawn,	// int )				( edict_t *pent );
	case FM_Spawn:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_Think,	// void )				( edict_t *pent );
	case FM_Think:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_Use,	// void )				( edict_t *pentUsed, edict_t *pentOther );
	case FM_Use:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	//DLLFunc_Touch,	// void )				( edict_t *pentTouched, edict_t *pentOther );
	case FM_Touch:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	//DLLFunc_Blocked,	// void )			( edict_t *pentBlocked, edict_t *pentOther );
	case FM_Blocked:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		break;
	//DLLFunc_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd );
	/*
	case FM_KeyValue:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_????, FP_DONE); // TODO: undefined structure?
		break;
	*/
	//DLLFunc_SetAbsBox,			// void )			( edict_t *pent );
	case FM_SetAbsBox:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;

	//DLLFunc_ClientConnect,		// bool)		( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] );
	case FM_ClientConnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_STRING, FP_DONE); // TODO: 4th arg must be of set size 128?
		break;
	//DLLFunc_ClientDisconnect,	// void )	( edict_t *pEntity );
	case FM_ClientDisconnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_ClientKill,		// void )		( edict_t *pEntity );
	case FM_ClientKill:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_ClientPutInServer,	// void )	( edict_t *pEntity );
	case FM_ClientPutInServer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_ClientCommand,		// void )		( edict_t *pEntity );
	case FM_ClientCommand:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;

	//DLLFunc_ServerDeactivate,	// void)	( void );
	case FM_ServerDeactivate:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;

	//DLLFunc_PlayerPreThink,		// void )	( edict_t *pEntity );
	case FM_PlayerPreThink:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_PlayerPostThink,		// void )	( edict_t *pEntity );
	case FM_PlayerPostThink:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;

	//DLLFunc_StartFrame,		// void )		( void );
	case FM_StartFrame:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	//DLLFunc_ParmsNewLevel,		// void )		( void );
	case FM_ParmsNewLevel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	//DLLFunc_ParmsChangeLevel,	// void )	( void );
	case FM_ParmsChangeLevel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;

	// Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	//DLLFunc_GetGameDescription,	 // const char * )( void );     
	case FM_GetGameDescription:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;

	// Spectator funcs
	//DLLFunc_SpectatorConnect,	// void)		( edict_t *pEntity );
	case FM_SpectatorConnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_SpectatorDisconnect,	// void )	( edict_t *pEntity );
	case FM_SpectatorDisconnect:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;
	//DLLFunc_SpectatorThink,		// void )		( edict_t *pEntity );
	case FM_SpectatorThink:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		break;

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	//DLLFunc_Sys_Error,		// void )			( const char *error_string );
	case FM_Sys_Error:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;

	//DLLFunc_PM_FindTextureType,	// char )( char *name );
	case FM_PM_FindTextureType:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		break;
	//DLLFunc_RegisterEncoders,	// void )	( void );
	case FM_RegisterEncoders:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	//DLLFunc_GetHullBounds,	// int)	( int hullnumber, float *mins, float *maxs );
	case FM_GetHullBounds:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		break;

	// Create baselines for certain "unplaced" items.
	//DLLFunc_CreateInstancedBaselines,	// void ) ( void );
	case FM_CreateInstancedBaselines:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		break;
	//DLLFunc_pfnAllowLagCompensation,	// int )( void );
	case FM_pfnAllowLagCompensation:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
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
