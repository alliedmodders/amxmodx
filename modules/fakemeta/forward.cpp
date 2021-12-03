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

ke::Vector<int> Engine[ENGFUNC_NUM+10];
ke::Vector<int> EnginePost[ENGFUNC_NUM + 10];
void *EngineAddrs[ENGFUNC_NUM+10];
void *EngineAddrsPost[ENGFUNC_NUM+10];
cell mCellResult;
cell mlCellResult;
float mFloatResult;
float mlFloatResult;
const char *mStringResult;
const char *mlStringResult;
int retType = 0;
KVD_Wrapper g_kvd_hook;
clientdata_t *g_cd_hook;
entity_state_t *g_es_hook;
usercmd_t *g_uc_hook;

cell origCellRet;
float origFloatRet;
const char *origStringRet;

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
			mFloatResult = amx_ctof(*(MF_GetAmxAddr(amx,params[2])));
			break;
		}
	case FMV_CELL:
		{
			mCellResult = *(MF_GetAmxAddr(amx,params[2]));
			break;
		}
	default:
		{
			return 0;
		}
	}

	retType = params[1];

	return 1;
}
/*
 * Begining of Game DLL->Engine hooks
 */

// pfnPrecacheModel
SIMPLE_INT_HOOK_CONSTSTRING(PrecacheModel);

// pfnPrecacheSound
SIMPLE_INT_HOOK_CONSTSTRING(PrecacheSound);

void ClientUserInfoChanged(edict_t *e, char *infobuffer)
{
	FM_ENG_HANDLE(FM_ClientUserInfoChanged, (Engine[FM_ClientUserInfoChanged].at(i), (cell)ENTINDEX(e), (cell)infobuffer));
	RETURN_META(mswi(lastFmRes));
}

void ClientUserInfoChanged_post(edict_t *e, char *infobuffer)
{
	FM_ENG_HANDLE_POST(FM_ClientUserInfoChanged, (EnginePost[FM_ClientUserInfoChanged].at(i), (cell)ENTINDEX(e), (cell)infobuffer)); 
	RETURN_META(MRES_IGNORED);
}

void SetModel(edict_t *e, const char *m)
{
	FM_ENG_HANDLE(FM_SetModel, (Engine[FM_SetModel].at(i), (cell)ENTINDEX(e), m));
	RETURN_META(mswi(lastFmRes));
}
void SetModel_post(edict_t *e, const char *m)
{
	FM_ENG_HANDLE_POST(FM_SetModel, (EnginePost[FM_SetModel].at(i), (cell)ENTINDEX(e), m));
	RETURN_META(MRES_IGNORED);
}

void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr)
{
	/* 
	TRACE_LINE(v1, v2, fNoMonsters, pentToSkip, ptr);	//from fun module - prevents crash? - t(+)rget/freecode // well i guess that's what you get for listening, isnt it? =/
	*/
	gfm_tr=ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE(FM_TraceLine, (Engine[FM_TraceLine].at(i), p_v1, p_v2, (cell)fNoMonsters, (cell)ENTINDEX(pentToSkip) , (cell)ptr));
	RETURN_META(mswi(lastFmRes));
}

void TraceLine_post(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr=ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE_POST(FM_TraceLine, (EnginePost[FM_TraceLine].at(i), p_v1, p_v2, (cell)fNoMonsters, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META(MRES_IGNORED);
}

void TraceToss(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr)
{
	gfm_tr = ptr;
	FM_ENG_HANDLE(FM_TraceToss, (Engine[FM_TraceToss].at(i), (cell)ENTINDEX(pent), (cell)ENTINDEX(pentToIgnore), (cell)ptr));
	RETURN_META(mswi(lastFmRes));
}

void TraceToss_post(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr)
{
	gfm_tr = ptr;
	FM_ENG_HANDLE_POST(FM_TraceToss, (EnginePost[FM_TraceToss].at(i), (cell)ENTINDEX(pent), (cell)ENTINDEX(pentToIgnore), (cell)ptr));
	RETURN_META(MRES_IGNORED);
}

int TraceMonsterHull(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE(FM_TraceMonsterHull, (Engine[FM_TraceMonsterHull].at(i), (cell)ENTINDEX(pEdict), p_v1, p_v2, (cell)fNoMonsters, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

int TraceMonsterHull_post(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	origCellRet = META_RESULT_ORIG_RET(int);
	FM_ENG_HANDLE_POST(FM_TraceMonsterHull, (EnginePost[FM_TraceMonsterHull].at(i), (cell)ENTINDEX(pEdict), p_v1, p_v2, (cell)fNoMonsters, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult);
}

void TraceHull(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE(FM_TraceHull, (Engine[FM_TraceHull].at(i), p_v1, p_v2, (cell)fNoMonsters, (cell)hullNumber, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META(mswi(lastFmRes));
}

void TraceHull_post(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE_POST(FM_TraceHull, (EnginePost[FM_TraceHull].at(i), p_v1, p_v2, (cell)fNoMonsters, (cell)hullNumber, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META(MRES_IGNORED);
}

void TraceModel(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE(FM_TraceModel, (Engine[FM_TraceModel].at(i), p_v1, p_v2, (cell)hullNumber, (cell)ENTINDEX(pent), (cell)ptr));
	RETURN_META(mswi(lastFmRes));
}

void TraceModel_post(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE_POST(FM_TraceModel, (EnginePost[FM_TraceModel].at(i), p_v1, p_v2, (cell)hullNumber, (cell)ENTINDEX(pent), (cell)ptr));
	RETURN_META(MRES_IGNORED);
}

const char *TraceTexture(edict_t *pTextureEntity, const float *v1, const float *v2)
{
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE(FM_TraceTexture, (Engine[FM_TraceTexture].at(i), (cell)ENTINDEX(pTextureEntity), p_v1, p_v2));
	RETURN_META_VALUE(mswi(lastFmRes), mlStringResult);
}

const char *TraceTexture_post(edict_t *pTextureEntity, const float *v1, const float *v2)
{
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	origStringRet = META_RESULT_ORIG_RET(const char *);
	FM_ENG_HANDLE_POST(FM_TraceTexture, (EnginePost[FM_TraceTexture].at(i), (cell)ENTINDEX(pTextureEntity), p_v1, p_v2));
	RETURN_META_VALUE(MRES_IGNORED, mlStringResult);
}

void TraceSphere(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE(FM_TraceSphere, (Engine[FM_TraceSphere].at(i), p_v1, p_v2, (cell)fNoMonsters, radius, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META(mswi(lastFmRes));
}

void TraceSphere_post(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr)
{
	gfm_tr = ptr;
	PREPARE_VECTOR(v1);
	PREPARE_VECTOR(v2);
	FM_ENG_HANDLE_POST(FM_TraceSphere, (EnginePost[FM_TraceSphere].at(i), p_v1, p_v2, (cell)fNoMonsters, radius, (cell)ENTINDEX(pentToSkip), (cell)ptr));
	RETURN_META(MRES_IGNORED);
}


/*
// Passed to pfnKeyValue
typedef struct KeyValueData_s
{
	char	*szClassName;	// in: entity classname
	char	*szKeyName;		// in: name of key
	char	*szValue;		// in: value of key
	int32	fHandled;		// out: DLL sets to true if key-value pair was understood
} KeyValueData;
*/
void KeyValue(edict_t* entity, KeyValueData* data)
{
	FM_ENG_HANDLE(FM_KeyValue, (Engine[FM_KeyValue].at(i), (cell)ENTINDEX(entity), (cell)(data)));
	RETURN_META(mswi(lastFmRes));
}

void KeyValue_post(edict_t* entity, KeyValueData* data)
{
	FM_ENG_HANDLE_POST(FM_KeyValue, (EnginePost[FM_KeyValue].at(i), (cell)ENTINDEX(entity), (cell)(data)));
	RETURN_META(MRES_IGNORED);
}

void AlertMessage(ALERT_TYPE atype, const char *szFmt, ...)
{
	static char buf[2048];
	va_list ap;
	va_start(ap, szFmt);
	vsprintf(buf, szFmt, ap);
	va_end(ap);
	FM_ENG_HANDLE(FM_AlertMessage, (Engine[FM_AlertMessage].at(i), (cell)atype, buf));
	RETURN_META(mswi(lastFmRes));
}

void AlertMessage_post(ALERT_TYPE atype, const char *szFmt, ...)
{
	static char buf[2048];
	va_list ap;
	va_start(ap, szFmt);
	vsprintf(buf, szFmt, ap);
	va_end(ap);
	FM_ENG_HANDLE_POST(FM_AlertMessage, (EnginePost[FM_AlertMessage].at(i), (cell)atype, buf));
	RETURN_META(MRES_IGNORED);
}

// pfnModelIndex
SIMPLE_INT_HOOK_CONSTSTRING(ModelIndex);

// pfnModelFrames
SIMPLE_INT_HOOK_INT(ModelFrames);

// pfnSetSize
SIMPLE_VOID_HOOK_EDICT_CONSTVECT_CONSTVECT(SetSize);

// pfnChangeLevel
SIMPLE_VOID_HOOK_CONSTSTRING_CONSTSTRING(ChangeLevel);

// pfnVecToYaw
SIMPLE_FLOAT_HOOK_CONSTVECT(VecToYaw);

void VecToAngles(const float *rgflVectorIn, float *rgflVectorOut)
{
	PREPARE_VECTOR(rgflVectorIn);
	PREPARE_VECTOR(rgflVectorOut);
	FM_ENG_HANDLE(FM_VecToAngles, (Engine[FM_VecToAngles].at(i), p_rgflVectorIn, p_rgflVectorOut));
	RETURN_META(mswi(lastFmRes));
}
void VecToAngles_post(const float *rgflVectorIn, float *rgflVectorOut)
{
	PREPARE_VECTOR(rgflVectorIn);
	PREPARE_VECTOR(rgflVectorOut);
	FM_ENG_HANDLE_POST(FM_VecToAngles, (EnginePost[FM_VecToAngles].at(i), p_rgflVectorIn, p_rgflVectorOut));
	RETURN_META(MRES_IGNORED);
}

void MoveToOrigin(edict_t *ent, const float *pflGoal, float dist, int iMoveType)
{
	PREPARE_VECTOR(pflGoal);
	FM_ENG_HANDLE(FM_MoveToOrigin, (Engine[FM_MoveToOrigin].at(i), (cell)ENTINDEX(ent), p_pflGoal, dist, (cell)iMoveType));
	RETURN_META(mswi(lastFmRes));
}

void MoveToOrigin_post(edict_t *ent, const float *pflGoal, float dist, int iMoveType)
{
	PREPARE_VECTOR(pflGoal);
	FM_ENG_HANDLE_POST(FM_MoveToOrigin, (EnginePost[FM_MoveToOrigin].at(i), (cell)ENTINDEX(ent), p_pflGoal, dist, (cell)iMoveType));
	RETURN_META(MRES_IGNORED);
}

edict_t *FindEntityByString(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue)
{
	FM_ENG_HANDLE(FM_FindEntityByString, (Engine[FM_FindEntityByString].at(i), (cell)ENTINDEX(pEdictStartSearchAfter), pszField, pszValue));
	RETURN_META_VALUE(mswi(lastFmRes), TypeConversion.id_to_edict((int)mlCellResult));
}

edict_t *FindEntityByString_post(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue)
{
	origCellRet = ENTINDEX(META_RESULT_ORIG_RET(edict_t *));
	FM_ENG_HANDLE_POST(FM_FindEntityByString, (EnginePost[FM_FindEntityByString].at(i), (cell)ENTINDEX(pEdictStartSearchAfter), pszField, pszValue));
	RETURN_META_VALUE(MRES_IGNORED, TypeConversion.id_to_edict((int)mlCellResult));
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

// pfnWalkMove
SIMPLE_INT_HOOK_EDICT_FLOAT_FLOAT_INT(WalkMove);

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
SIMPLE_VOID_HOOK_INT_CONSTSTRING(LightStyle);

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

void GetBonePosition(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles)
{
	PREPARE_VECTOR(rgflOrigin);
	PREPARE_VECTOR(rgflAngles);
	FM_ENG_HANDLE(FM_GetBonePosition, (Engine[FM_GetBonePosition].at(i), (cell)ENTINDEX(pEdict), (cell)iBone, p_rgflOrigin, p_rgflAngles));
	RETURN_META(mswi(lastFmRes));
}

void GetBonePosition_post(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles)
{
	PREPARE_VECTOR(rgflOrigin);
	PREPARE_VECTOR(rgflAngles);
	FM_ENG_HANDLE_POST(FM_GetBonePosition, (EnginePost[FM_GetBonePosition].at(i), (cell)ENTINDEX(pEdict), (cell)iBone, p_rgflOrigin, p_rgflAngles));
	RETURN_META(MRES_IGNORED);
}

void GetAttachment(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles)
{
	PREPARE_VECTOR(rgflOrigin);
	PREPARE_VECTOR(rgflAngles);
	FM_ENG_HANDLE(FM_GetAttachment, (Engine[FM_GetAttachment].at(i), (cell)ENTINDEX(pEdict), (cell)iAttachment, p_rgflOrigin, p_rgflAngles));
	RETURN_META(mswi(lastFmRes));
}

void GetAttachment_post(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles)
{
	PREPARE_VECTOR(rgflOrigin);
	PREPARE_VECTOR(rgflAngles);
	FM_ENG_HANDLE_POST(FM_GetAttachment, (EnginePost[FM_GetAttachment].at(i), (cell)ENTINDEX(pEdict), (cell)iAttachment, p_rgflOrigin, p_rgflAngles));
	RETURN_META(MRES_IGNORED);
}

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
SIMPLE_INT_HOOK_CONSTSTRING(PrecacheGeneric);

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

void RunPlayerMove(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec)
{
	PREPARE_VECTOR(viewangles);
	FM_ENG_HANDLE(FM_RunPlayerMove, (Engine[FM_RunPlayerMove].at(i), (cell)ENTINDEX(fakeclient), p_viewangles, forwardmove, sidemove, upmove, (cell)buttons, (cell)impulse, (cell)msec));
	RETURN_META(mswi(lastFmRes));
}

void RunPlayerMove_post(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec)
{
	PREPARE_VECTOR(viewangles);
	FM_ENG_HANDLE_POST(FM_RunPlayerMove, (EnginePost[FM_RunPlayerMove].at(i), (cell)ENTINDEX(fakeclient), p_viewangles, forwardmove, sidemove, upmove, (cell)buttons, (cell)impulse, (cell)msec));
	RETURN_META(MRES_IGNORED);
}

// pfnNumberOfEntities
SIMPLE_INT_HOOK_VOID(NumberOfEntities);

void StaticDecal(const float *origin, int decalIndex, int entityIndex, int modelIndex)
{
	PREPARE_VECTOR(origin);
	FM_ENG_HANDLE(FM_StaticDecal, (Engine[FM_StaticDecal].at(i), p_origin, (cell)decalIndex, (cell)entityIndex, (cell)modelIndex));
	RETURN_META(mswi(lastFmRes));
}

void StaticDecal_post(const float *origin, int decalIndex, int entityIndex, int modelIndex)
{
	PREPARE_VECTOR(origin);
	FM_ENG_HANDLE_POST(FM_StaticDecal, (EnginePost[FM_StaticDecal].at(i), p_origin, (cell)decalIndex, (cell)entityIndex, (cell)modelIndex));
	RETURN_META(MRES_IGNORED);
}

void BuildSoundMsg(edict_t *entity, int channel, const char *sample, float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	PREPARE_VECTOR(pOrigin);
	FM_ENG_HANDLE(FM_BuildSoundMsg, (Engine[FM_BuildSoundMsg].at(i), (cell)ENTINDEX(entity), (cell)channel, sample, volume, attenuation, (cell)fFlags, (cell)pitch, (cell)msg_dest, (cell)msg_type, p_pOrigin, (cell)ENTINDEX(ed)));
	RETURN_META(mswi(lastFmRes));
}

void BuildSoundMsg_post(edict_t *entity, int channel, const char *sample, float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
{
	PREPARE_VECTOR(pOrigin);
	FM_ENG_HANDLE_POST(FM_BuildSoundMsg, (EnginePost[FM_BuildSoundMsg].at(i), (cell)ENTINDEX(entity), (cell)channel, sample, volume, attenuation, (cell)fFlags, (cell)pitch, (cell)msg_dest, (cell)msg_type, p_pOrigin, (cell)ENTINDEX(ed)));
	RETURN_META(MRES_IGNORED);
}

int CheckVisibility(const edict_t *entity, unsigned char *pset)
{
	FM_ENG_HANDLE(FM_CheckVisibility, (Engine[FM_CheckVisibility].at(i), (cell)ENTINDEX(entity), (cell)pset));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

int CheckVisibility_post(const edict_t *entity, unsigned char *pset)
{
	origCellRet = META_RESULT_ORIG_RET(int);
	FM_ENG_HANDLE(FM_CheckVisibility, (Engine[FM_CheckVisibility].at(i), (cell)ENTINDEX(entity), (cell)pset));
	RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult);
}

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

SIMPLE_STRING_HOOK_STRING_CONSTSTRING(InfoKeyValue);

SIMPLE_VOID_HOOK_STRING_CONSTSTRING_CONSTSTRING(SetKeyValue);

SIMPLE_VOID_HOOK_INT_STRING_CONSTSTRING_CONSTSTRING(SetClientKeyValue);

SIMPLE_CONSTSTRING_HOOK_EDICT(GetPlayerAuthId);

SIMPLE_UINT_HOOK_EDICT(GetPlayerWONId);

SIMPLE_INT_HOOK_CONSTSTRING(IsMapValid);

int CreateInstancedBaseline(int classname, struct entity_state_s *baseline)
{
	g_es_hook = baseline;
	FM_ENG_HANDLE(FM_CreateInstancedBaseline, (Engine[FM_CreateInstancedBaseline].at(i), (cell)classname, (cell)baseline));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

int CreateInstancedBaseline_post(int classname, struct entity_state_s *baseline)
{
	g_es_hook = baseline;
	origCellRet = META_RESULT_ORIG_RET(int);
	FM_ENG_HANDLE_POST(FM_CreateInstancedBaseline, (EnginePost[FM_CreateInstancedBaseline].at(i), (cell)classname, (cell)baseline));
	RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult);
}

char *GetInfoKeyBuffer(edict_t *e)
{
	FM_ENG_HANDLE(FM_GetInfoKeyBuffer, (Engine[FM_GetInfoKeyBuffer].at(i), (cell)ENTINDEX(e)));
	RETURN_META_VALUE(mswi(lastFmRes), reinterpret_cast<char *>(mlCellResult));
}

char *GetInfoKeyBuffer_post(edict_t *e)
{
	origCellRet = reinterpret_cast<cell>(META_RESULT_ORIG_RET(char *));
	FM_ENG_HANDLE(FM_GetInfoKeyBuffer, (Engine[FM_GetInfoKeyBuffer].at(i), (cell)ENTINDEX(e)));
	RETURN_META_VALUE(MRES_IGNORED, reinterpret_cast<char *>(mlCellResult));
}

void ClientPrintf(edict_t* pEdict, PRINT_TYPE ptype, const char *szMsg)
{
	FM_ENG_HANDLE(FM_ClientPrintf, (Engine[FM_ClientPrintf].at(i), (cell)ENTINDEX(pEdict), (cell)ptype, szMsg));
	RETURN_META(mswi(lastFmRes));
}

void ClientPrintf_post(edict_t* pEdict, PRINT_TYPE ptype, const char *szMsg)
{
	FM_ENG_HANDLE(FM_ClientPrintf, (Engine[FM_ClientPrintf].at(i), (cell)ENTINDEX(pEdict), (cell)ptype, szMsg));
	RETURN_META(MRES_IGNORED);
}

SIMPLE_VOID_HOOK_CONSTSTRING(ServerPrint);

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
SIMPLE_CHAR_HOOK_CONSTSTRING(PM_FindTextureType);

// pfnRegisterEncoders
SIMPLE_VOID_HOOK_VOID(RegisterEncoders);

// pfnCreateInstancedBaselines
SIMPLE_VOID_HOOK_VOID(CreateInstancedBaselines);

// pfnAllowLagCompensation
SIMPLE_INT_HOOK_VOID(AllowLagCompensation);

void UpdateClientData(const struct edict_s *ent, int sendweapons, struct clientdata_s *cd)
{
	g_cd_hook = cd;
	FM_ENG_HANDLE(FM_UpdateClientData, (Engine[FM_UpdateClientData].at(i), (cell)ENTINDEX(ent), (cell)sendweapons, (cell)cd));
	RETURN_META(mswi(lastFmRes));
}

void UpdateClientData_post(const struct edict_s *ent, int sendweapons, struct clientdata_s *cd)
{
	g_cd_hook = cd;
	FM_ENG_HANDLE_POST(FM_UpdateClientData, (EnginePost[FM_UpdateClientData].at(i), (cell)ENTINDEX(ent), (cell)sendweapons, (cell)cd));
	RETURN_META(MRES_IGNORED);
}

int AddToFullPack(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet)
{
	g_es_hook = state;
	FM_ENG_HANDLE(FM_AddToFullPack, (Engine[FM_AddToFullPack].at(i), (cell)state, (cell)e, (cell)ENTINDEX(ent), (cell)ENTINDEX(host), (cell)hostflags, (cell)player, (cell)pSet));
	RETURN_META_VALUE(mswi(lastFmRes), (int)mlCellResult);
}

int AddToFullPack_post(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet)
{
	g_es_hook = state;
	origCellRet = META_RESULT_ORIG_RET(int);
	FM_ENG_HANDLE_POST(FM_AddToFullPack, (EnginePost[FM_AddToFullPack].at(i), (cell)state, (cell)e, (cell)ENTINDEX(ent), (cell)ENTINDEX(host), (cell)hostflags, (cell)player, (cell)pSet));
	RETURN_META_VALUE(MRES_IGNORED, (int)mlCellResult);
}

void CmdStart(const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed)
{
	g_uc_hook = const_cast<usercmd_t *>(cmd);
	FM_ENG_HANDLE(FM_CmdStart, (Engine[FM_CmdStart].at(i), (cell)ENTINDEX(player), (cell)cmd, (cell)random_seed));
	RETURN_META(mswi(lastFmRes));
}

void CmdStart_post(const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed)
{
	g_uc_hook = const_cast<usercmd_t *>(cmd);
	FM_ENG_HANDLE_POST(FM_CmdStart, (EnginePost[FM_CmdStart].at(i), (cell)ENTINDEX(player), (cell)cmd, (cell)random_seed));
	RETURN_META(MRES_IGNORED);
}

void CreateBaseline(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs)
{
	g_es_hook = baseline;
	PREPARE_VECTOR(player_mins);
	PREPARE_VECTOR(player_maxs);
	FM_ENG_HANDLE(FM_CreateBaseline, (Engine[FM_CreateBaseline].at(i), (cell)player, (cell)eindex, (cell)baseline, (cell)ENTINDEX(entity), (cell)playermodelindex, p_player_mins, p_player_maxs));
	RETURN_META(mswi(lastFmRes));
}

void CreateBaseline_post(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs)
{
	g_es_hook = baseline;
	PREPARE_VECTOR(player_mins);
	PREPARE_VECTOR(player_maxs);
	FM_ENG_HANDLE_POST(FM_CreateBaseline, (EnginePost[FM_CreateBaseline].at(i), (cell)player, (cell)eindex, (cell)baseline, (cell)ENTINDEX(entity), (cell)playermodelindex, p_player_mins, p_player_maxs));
	RETURN_META(MRES_IGNORED);
}

// pfnCmdEnd
SIMPLE_VOID_HOOK_CONSTEDICT(CmdEnd);

/*
 * NEW_DLL_FUNCTIONS
 */
// pfnOnFreeEntPrivateData
SIMPLE_VOID_HOOK_EDICT(OnFreeEntPrivateData);
// pfnGameShutdown
SIMPLE_VOID_HOOK_VOID(GameShutdown);
// pfnShouldCollide
SIMPLE_INT_HOOK_EDICT_EDICT(ShouldCollide);


static cell AMX_NATIVE_CALL unregister_forward(AMX *amx, cell *params)
{
	int func = params[1];
	int func_id = params[2];
	int post = params[3];

	if (func >= FM_LAST_DONT_USE_ME || func < 1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid function: %d", func);
		return 0;
	}

	void *patchAddr = NULL;

	ke::Vector<int> *peng = NULL;
	if (post)
	{
		peng = &(EnginePost[func]);
		patchAddr = EngineAddrsPost[func];
	} else {
		peng = &(Engine[func]);
		patchAddr = EngineAddrs[func];
	}
	for (size_t i = 0; i < peng->length(); ++i)
	{
		if (peng->at(i) == func_id)
		{
			peng->remove(i);
			MF_UnregisterSPForward(func_id);
			if (!peng->length() && patchAddr != NULL && func != FM_ServerDeactivate)
			{
				/* Clear out this forward if we no longer need it */
				*(void **)patchAddr = NULL;
			}
			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL register_forward(AMX *amx, cell *params)
{
	int func = params[1];
	// You originally had both post coming from params[2] AND the function name.  I've moved post to 3.
	int post = params[3];
	if (func >= FM_LAST_DONT_USE_ME || func < 1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid function: %d", func);
		return 0;
	}

	int len, fId=0;
	const char *funcname = MF_GetAmxString(amx, params[2], 0, &len);
	enginefuncs_t *engtable;

	DLL_FUNCTIONS *dlltable;

	NEW_DLL_FUNCTIONS *newdlltable;

	if (post)
	{
		engtable = g_pengfuncsTable_Post;
		dlltable = g_pFunctionTable_Post;
		newdlltable = g_pNewFunctionsTable_Post;
	}
	else
	{
		engtable = g_pengfuncsTable;
		dlltable = g_pFunctionTable;
		newdlltable = g_pNewFunctionsTable;
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
		ENGHOOK(WalkMove);
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
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(TraceLine);
		break;
	case FM_TraceToss:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(TraceToss);
		break;
	case FM_TraceMonsterHull:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(TraceMonsterHull);
		break;
	case FM_TraceHull:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(TraceHull);
		break;
	case FM_TraceModel:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(TraceModel);
		break;
	case FM_TraceTexture:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		ENGHOOK(TraceTexture);
		break;
	case FM_TraceSphere:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_ARRAY, FP_CELL, FP_FLOAT, FP_CELL, FP_DONE);
		ENGHOOK(TraceSphere);
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
		ENGHOOK(GetBonePosition);
		break;

	case FM_GetAttachment:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		ENGHOOK(GetAttachment);
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
		ENGHOOK(RunPlayerMove);
		break;

	case FM_NumberOfEntities:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		ENGHOOK(NumberOfEntities);
		break;
	case FM_StaticDecal:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_ARRAY, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(StaticDecal);
		break;
	case FM_PrecacheGeneric:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(PrecacheGeneric);
		break;
	case FM_BuildSoundMsg:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_STRING, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_ARRAY, FP_CELL, FP_DONE);
		ENGHOOK(BuildSoundMsg);
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
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(CheckVisibility);
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

	case FM_AlertMessage:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_DONE);
		ENGHOOK(AlertMessage);
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
	//TODO: Expand the structure (simple: just a bunch of strings and a float.)

	//DLLFunc_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd );
	case FM_KeyValue:
		//fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_STRING, FP_STRING, FP_STRING, FP_CELL, FP_DONE);
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(KeyValue);
		break;
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
	// NEW_DLL_FUNCTIONS:
	case FM_OnFreeEntPrivateData:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		NEWDLLHOOK(OnFreeEntPrivateData);
		break;
	// Maybe it's not possible to hook this forward? O_o
	case FM_GameShutdown:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_DONE);
		NEWDLLHOOK(GameShutdown);
		break;
	case FM_ShouldCollide:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		NEWDLLHOOK(ShouldCollide);
		break;
	case FM_ClientUserInfoChanged:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(ClientUserInfoChanged);
		break;
	case FM_UpdateClientData:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(UpdateClientData);
		break;
	case FM_AddToFullPack:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(AddToFullPack);
		break;
	case FM_CmdStart:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
		DLLHOOK(CmdStart);
		break;
	case FM_CmdEnd:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		DLLHOOK(CmdEnd);
		break;
	case FM_CreateInstancedBaseline:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_DONE);
		ENGHOOK(CreateInstancedBaseline);
		break;
	case FM_CreateBaseline:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_ARRAY, FP_ARRAY, FP_DONE);
		DLLHOOK(CreateBaseline);
		break;
	case FM_GetInfoKeyBuffer:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_DONE);
		ENGHOOK(GetInfoKeyBuffer);
		break;
	case FM_ClientPrintf:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_STRING, FP_DONE);
		ENGHOOK(ClientPrintf);
		break;
	case FM_ServerPrint:
		fId = MF_RegisterSPForwardByName(amx, funcname, FP_STRING, FP_DONE);
		ENGHOOK(ServerPrint);
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
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Function not found (%d, %s)", func, funcname);
		return 0;
	}

	if (post)
	{
		EnginePost[func].append(fId);
	} else {
		Engine[func].append(fId);
	}

	return fId;
}

static cell AMX_NATIVE_CALL get_orig_retval(AMX *amx, cell *params)
{
	int paramCount = params[0] / sizeof(cell);
	cell *refFloatRet = 0;

	switch (paramCount)
	{
	case 0:
		return origCellRet;
	case 1:
		refFloatRet = MF_GetAmxAddr(amx, params[1]);
		*refFloatRet = amx_ftoc(origFloatRet);
		return 1;
	case 2:
		MF_SetAmxString(amx, params[1], origStringRet, params[2]);
		return 1;
	default:
		MF_LogError(amx, AMX_ERR_NATIVE, "Too many parameters passed");
		return 0;
	}
}

AMX_NATIVE_INFO forward_natives[] = {
	{ "register_forward",	register_forward },
	{ "unregister_forward", unregister_forward },
	{ "forward_return",		fm_return },
	{ "get_orig_retval",	get_orig_retval },
	{ NULL,					NULL }
};
