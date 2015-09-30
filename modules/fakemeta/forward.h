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

#ifndef _INCLUDE_FORWARD_H
#define _INCLUDE_FORWARD_H

#include <amtl/am-vector.h>

#define ENGFUNC_NUM		FM_LAST_DONT_USE_ME // 131

#define	FMV_STRING		1
#define FMV_FLOAT		2
#define FMV_CELL		3

#define FMRES_HANDLED	2
#define FMRES_SUPERCEDE	4
#define FMRES_IGNORED	1
#define FMRES_OVERRIDE	3

enum {
	FM_FIRST_DONT_USE_ME = 0,
	FM_PrecacheModel ,
	FM_PrecacheSound,
	FM_SetModel,	
	FM_ModelIndex,
	FM_ModelFrames,
	FM_SetSize,
	FM_ChangeLevel,
	FM_VecToYaw,
	FM_VecToAngles,
	FM_MoveToOrigin,
	FM_ChangeYaw,
	FM_ChangePitch,
	FM_FindEntityByString,
	FM_GetEntityIllum,
	FM_FindEntityInSphere,
	FM_FindClientInPVS,
	FM_EntitiesInPVS,
	FM_MakeVectors,
	FM_AngleVectors,
	FM_CreateEntity,
	FM_RemoveEntity,
	FM_CreateNamedEntity,
	FM_MakeStatic,
	FM_EntIsOnFloor,
	FM_DropToFloor,
	FM_WalkMove,
	FM_SetOrigin,
	FM_EmitSound,
	FM_EmitAmbientSound,
	FM_TraceLine,
	FM_TraceToss,
	FM_TraceMonsterHull,
	FM_TraceHull,
	FM_TraceModel,
	FM_TraceTexture,
	FM_TraceSphere,
	FM_GetAimVector,
	FM_ParticleEffect,
	FM_LightStyle,
	FM_DecalIndex,
	FM_PointContents,
	FM_MessageBegin,
	FM_MessageEnd,
	FM_WriteByte,
	FM_WriteChar,
	FM_WriteShort,
	FM_WriteLong,
	FM_WriteAngle,
	FM_WriteCoord,
	FM_WriteString,
	FM_WriteEntity,
	FM_CVarGetFloat,
	FM_CVarGetString,
	FM_CVarSetFloat,
	FM_CVarSetString,
	FM_FreeEntPrivateData,
	FM_SzFromIndex,
	FM_AllocString,
	FM_RegUserMsg,
	FM_AnimationAutomove,
	FM_GetBonePosition,
	FM_GetAttachment,
	FM_SetView,
	FM_Time,
	FM_CrosshairAngle,
	FM_FadeClientVolume,
	FM_SetClientMaxspeed,
	FM_CreateFakeClient,
	FM_RunPlayerMove,
	FM_NumberOfEntities,
	FM_StaticDecal,
	FM_PrecacheGeneric,
	FM_BuildSoundMsg,
	FM_GetPhysicsKeyValue,
	FM_SetPhysicsKeyValue,
	FM_GetPhysicsInfoString,
	FM_PrecacheEvent,
	FM_PlaybackEvent,
	FM_CheckVisibility,
	FM_GetCurrentPlayer,
	FM_CanSkipPlayer,
	FM_SetGroupMask,
	FM_Voice_GetClientListening,
	FM_Voice_SetClientListening,
	FM_InfoKeyValue,
	FM_SetKeyValue,
	FM_SetClientKeyValue,
	FM_GetPlayerAuthId,
	FM_GetPlayerWONId,
	FM_IsMapValid,

	FM_Spawn,
	FM_Think,
	FM_Use,
	FM_Touch,
	FM_Blocked,
	FM_KeyValue,
	FM_SetAbsBox,
	FM_ClientConnect,

	FM_ClientDisconnect,
	FM_ClientKill,
	FM_ClientPutInServer,
	FM_ClientCommand,

	FM_ServerDeactivate,

	FM_PlayerPreThink,
	FM_PlayerPostThink,

	FM_StartFrame,
	FM_ParmsNewLevel,
	FM_ParmsChangeLevel,

	// Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	// This also gets called when the server is queried for information (for example, by a server browser tool)
	FM_GetGameDescription,

	// Spectator funcs
	FM_SpectatorConnect,
	FM_SpectatorDisconnect,
	FM_SpectatorThink,

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	FM_Sys_Error,

	FM_PM_FindTextureType,
	FM_RegisterEncoders,

	// Create baselines for certain "unplaced" items.
	FM_CreateInstancedBaselines,

	FM_AllowLagCompensation,
	FM_AlertMessage,

	// NEW_DLL_FUNCTIONS:
	FM_OnFreeEntPrivateData,
	FM_GameShutdown,
	FM_ShouldCollide,

	FM_ClientUserInfoChanged,	//passes id only

	FM_UpdateClientData,
	FM_AddToFullPack,
	FM_CmdStart,
	FM_CmdEnd,
	FM_CreateInstancedBaseline,
	FM_CreateBaseline,
	FM_GetInfoKeyBuffer,
	FM_ClientPrintf,
	FM_ServerPrint,
	FM_LAST_DONT_USE_ME
};

extern ke::Vector<int> Engine[];
extern ke::Vector<int> EnginePost[];
extern void *EngineAddrs[ENGFUNC_NUM+10];
extern void *EngineAddrsPost[ENGFUNC_NUM+10];
extern cell mCellResult;
extern float mFloatResult;
extern const char *mStringResult;
extern cell mlCellResult;
extern float mlFloatResult;
extern const char *mlStringResult;
extern int lastFmRes;
extern int retType;

#endif //_INCLUDE_FORWARD_H

