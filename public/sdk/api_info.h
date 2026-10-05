#pragma once

#define MM_PRE_HOOK
#define MM_POST_HOOK

enum : uint8
{
	P_PRE,		// plugin function called before gamedll
	P_POST,		// plugin function called after gamedll
};

struct api_info_t
{
	size_t		offset;
	const char*	name;
	size_t		loglevel;
};

// DLL api functions
struct dllapi_info_t
{
	api_info_t pfnGameInit;
	api_info_t pfnSpawn;
	api_info_t pfnThink;
	api_info_t pfnUse;
	api_info_t pfnTouch;
	api_info_t pfnBlocked;
	api_info_t pfnKeyValue;
	api_info_t pfnSave;
	api_info_t pfnRestore;
	api_info_t pfnSetAbsBox;
	api_info_t pfnSaveWriteFields;
	api_info_t pfnSaveReadFields;
	api_info_t pfnSaveGlobalState;
	api_info_t pfnRestoreGlobalState;
	api_info_t pfnResetGlobalState;
	api_info_t pfnClientConnect;
	api_info_t pfnClientDisconnect;
	api_info_t pfnClientKill;
	api_info_t pfnClientPutInServer;
	api_info_t pfnClientCommand;
	api_info_t pfnClientUserInfoChanged;
	api_info_t pfnServerActivate;
	api_info_t pfnServerDeactivate;
	api_info_t pfnPlayerPreThink;
	api_info_t pfnPlayerPostThink;
	api_info_t pfnStartFrame;
	api_info_t pfnParmsNewLevel;
	api_info_t pfnParmsChangeLevel;
	api_info_t pfnGetGameDescription;
	api_info_t pfnPlayerCustomization;
	api_info_t pfnSpectatorConnect;
	api_info_t pfnSpectatorDisconnect;
	api_info_t pfnSpectatorThink;
	api_info_t pfnSys_Error;
	api_info_t pfnPM_Move;
	api_info_t pfnPM_Init;
	api_info_t pfnPM_FindTextureType;
	api_info_t pfnSetupVisibility;
	api_info_t pfnUpdateClientData;
	api_info_t pfnAddToFullPack;
	api_info_t pfnCreateBaseline;
	api_info_t pfnRegisterEncoders;
	api_info_t pfnGetWeaponData;
	api_info_t pfnCmdStart;
	api_info_t pfnCmdEnd;
	api_info_t pfnConnectionlessPacket;
	api_info_t pfnGetHullBounds;
	api_info_t pfnCreateInstancedBaselines;
	api_info_t pfnInconsistentFile;
	api_info_t pfnAllowLagCompensation;

	// end
	api_info_t END;
};

// "New" api functions
struct newapi_info_t
{
	api_info_t pfnOnFreeEntPrivateData;
	api_info_t pfnGameShutdown;
	api_info_t pfnShouldCollide;
	api_info_t pfnCvarValue;
	api_info_t pfnCvarValue2;

	// end
	api_info_t END;
};

// g_engine functions
struct engine_info_t
{
	api_info_t pfnPrecacheModel;
	api_info_t pfnPrecacheSound;
	api_info_t pfnSetModel;
	api_info_t pfnModelIndex;
	api_info_t pfnModelFrames;
	api_info_t pfnSetSize;
	api_info_t pfnChangeLevel;
	api_info_t pfnGetSpawnParms;
	api_info_t pfnSaveSpawnParms;
	api_info_t pfnVecToYaw;
	api_info_t pfnVecToAngles;
	api_info_t pfnMoveToOrigin;
	api_info_t pfnChangeYaw;
	api_info_t pfnChangePitch;
	api_info_t pfnFindEntityByString;
	api_info_t pfnGetEntityIllum;
	api_info_t pfnFindEntityInSphere;
	api_info_t pfnFindClientInPVS;
	api_info_t pfnEntitiesInPVS;
	api_info_t pfnMakeVectors;
	api_info_t pfnAngleVectors;
	api_info_t pfnCreateEntity;
	api_info_t pfnRemoveEntity;
	api_info_t pfnCreateNamedEntity;
	api_info_t pfnMakeStatic;
	api_info_t pfnEntIsOnFloor;
	api_info_t pfnDropToFloor;
	api_info_t pfnWalkMove;
	api_info_t pfnSetOrigin;
	api_info_t pfnEmitSound;
	api_info_t pfnEmitAmbientSound;
	api_info_t pfnTraceLine;
	api_info_t pfnTraceToss;
	api_info_t pfnTraceMonsterHull;
	api_info_t pfnTraceHull;
	api_info_t pfnTraceModel;
	api_info_t pfnTraceTexture;
	api_info_t pfnTraceSphere;
	api_info_t pfnGetAimVector;
	api_info_t pfnServerCommand;
	api_info_t pfnServerExecute;
	api_info_t pfnClientCommand;
	api_info_t pfnParticleEffect;
	api_info_t pfnLightStyle;
	api_info_t pfnDecalIndex;
	api_info_t pfnPointContents;
	api_info_t pfnMessageBegin;
	api_info_t pfnMessageEnd;
	api_info_t pfnWriteByte;
	api_info_t pfnWriteChar;
	api_info_t pfnWriteShort;
	api_info_t pfnWriteLong;
	api_info_t pfnWriteAngle;
	api_info_t pfnWriteCoord;
	api_info_t pfnWriteString;
	api_info_t pfnWriteEntity;
	api_info_t pfnCVarRegister;
	api_info_t pfnCVarGetFloat;
	api_info_t pfnCVarGetString;
	api_info_t pfnCVarSetFloat;
	api_info_t pfnCVarSetString;
	api_info_t pfnAlertMessage;
	api_info_t pfnEngineFprintf;
	api_info_t pfnPvAllocEntPrivateData;
	api_info_t pfnPvEntPrivateData;
	api_info_t pfnFreeEntPrivateData;
	api_info_t pfnSzFromIndex;
	api_info_t pfnAllocString;
	api_info_t pfnGetVarsOfEnt;
	api_info_t pfnPEntityOfEntOffset;
	api_info_t pfnEntOffsetOfPEntity;
	api_info_t pfnIndexOfEdict;
	api_info_t pfnPEntityOfEntIndex;
	api_info_t pfnFindEntityByVars;
	api_info_t pfnGetModelPtr;
	api_info_t pfnRegUserMsg;
	api_info_t pfnAnimationAutomove;
	api_info_t pfnGetBonePosition;
	api_info_t pfnFunctionFromName;
	api_info_t pfnNameForFunction;
	api_info_t pfnClientPrintf;
	api_info_t pfnServerPrint;
	api_info_t pfnCmd_Args;
	api_info_t pfnCmd_Argv;
	api_info_t pfnCmd_Argc;
	api_info_t pfnGetAttachment;
	api_info_t pfnCRC32_Init;
	api_info_t pfnCRC32_ProcessBuffer;
	api_info_t pfnCRC32_ProcessByte;
	api_info_t pfnCRC32_Final;
	api_info_t pfnRandomLong;
	api_info_t pfnRandomFloat;
	api_info_t pfnSetView;
	api_info_t pfnTime;
	api_info_t pfnCrosshairAngle;
	api_info_t pfnLoadFileForMe;
	api_info_t pfnFreeFile;
	api_info_t pfnEndSection;
	api_info_t pfnCompareFileTime;
	api_info_t pfnGetGameDir;
	api_info_t pfnCvar_RegisterVariable;
	api_info_t pfnFadeClientVolume;
	api_info_t pfnSetClientMaxspeed;
	api_info_t pfnCreateFakeClient;
	api_info_t pfnRunPlayerMove;
	api_info_t pfnNumberOfEntities;
	api_info_t pfnGetInfoKeyBuffer;
	api_info_t pfnInfoKeyValue;
	api_info_t pfnSetKeyValue;
	api_info_t pfnSetClientKeyValue;
	api_info_t pfnIsMapValid;
	api_info_t pfnStaticDecal;
	api_info_t pfnPrecacheGeneric;
	api_info_t pfnGetPlayerUserId;
	api_info_t pfnBuildSoundMsg;
	api_info_t pfnIsDedicatedServer;
	api_info_t pfnCVarGetPointer;
	api_info_t pfnGetPlayerWONId;
	api_info_t pfnInfo_RemoveKey;
	api_info_t pfnGetPhysicsKeyValue;
	api_info_t pfnSetPhysicsKeyValue;
	api_info_t pfnGetPhysicsInfoString;
	api_info_t pfnPrecacheEvent;
	api_info_t pfnPlaybackEvent;
	api_info_t pfnSetFatPVS;
	api_info_t pfnSetFatPAS;
	api_info_t pfnCheckVisibility;
	api_info_t pfnDeltaSetField;
	api_info_t pfnDeltaUnsetField;
	api_info_t pfnDeltaAddEncoder;
	api_info_t pfnGetCurrentPlayer;
	api_info_t pfnCanSkipPlayer;
	api_info_t pfnDeltaFindField;
	api_info_t pfnDeltaSetFieldByIndex;
	api_info_t pfnDeltaUnsetFieldByIndex;
	api_info_t pfnSetGroupMask;
	api_info_t pfnCreateInstancedBaseline;
	api_info_t pfnCvar_DirectSet;
	api_info_t pfnForceUnmodified;
	api_info_t pfnGetPlayerStats;
	api_info_t pfnAddServerCommand;
	api_info_t pfnVoice_GetClientListening;
	api_info_t pfnVoice_SetClientListening;
	api_info_t pfnGetPlayerAuthId;
	api_info_t pfnSequenceGet;
	api_info_t pfnSequencePickSentence;
	api_info_t pfnGetFileSize;
	api_info_t pfnGetApproxWavePlayLen;
	api_info_t pfnIsCareerMatch;
	api_info_t pfnGetLocalizedStringLength;
	api_info_t pfnRegisterTutorMessageShown;
	api_info_t pfnGetTimesTutorMessageShown;
	api_info_t pfnProcessTutorMessageDecayBuffer;
	api_info_t pfnConstructTutorMessageDecayBuffer;
	api_info_t pfnResetTutorMessageDecayData;
	api_info_t pfnQueryClientCvarValue;
	api_info_t pfnQueryClientCvarValue2;
	api_info_t pfnEngCheckParm;

	// end
	api_info_t END;
};

extern dllapi_info_t g_dllapi_info;
extern newapi_info_t g_newapi_info;
extern engine_info_t g_engineapi_info;
