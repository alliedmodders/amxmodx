// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Module Config
//

#ifndef __MODULECONFIG_H__
#define __MODULECONFIG_H__

#include <amxmodx_version.h>

// Module info
#define MODULE_NAME "nVault"
#define MODULE_VERSION AMXX_VERSION
#define MODULE_AUTHOR "AMX Mod X Dev Team"
#define MODULE_URL "http://www.amxmodx.org/"
#define MODULE_LOGTAG "nVault"
#define MODULE_LIBRARY "nvault"
#define MODULE_LIBCLASS ""
// If you want the module not to be reloaded on mapchange, remove / comment out the next line
//#define MODULE_RELOAD_ON_MAPCHANGE

#ifdef __DATE__
#define MODULE_DATE __DATE__
#else // __DATE__
#define MODULE_DATE "Unknown"
#endif // __DATE__

// metamod plugin?
// #define USE_METAMOD

// use memory manager/tester?
// note that if you use this, you cannot construct/allocate 
// anything before the module attached (OnAmxxAttach).
// be careful of default constructors using new/malloc!
// #define MEMORY_TEST

// Unless you use STL or exceptions, keep this commented.
// It allows you to compile without libstdc++.so as a dependency
// #define NO_ALLOC_OVERRIDES

// Uncomment this if you are using MSVC8 or greater and want to fix some of the compatibility issues yourself 
// #define NO_MSVC8_AUTO_COMPAT

/** 
 * AMXX Init functions
 * Also consider using FN_META_*
 */

/** AMXX query */
//#define FN_AMXX_QUERY OnAmxxQuery

/** AMXX attach
 * Do native functions init here (MF_AddNatives)
 */
#define FN_AMXX_ATTACH OnAmxxAttach

/** AMXX Detach (unload) */
//#define FN_AMXX_DETACH OnAmxxDetach

/** All plugins loaded
 * Do forward functions init here (MF_RegisterForward)
 */
//#define FN_AMXX_PLUGINSLOADED OnPluginsLoaded

/** All plugins are about to be unloaded */
//#define FN_AMXX_PLUGINSUNLOADING OnPluginsUnloading

/** All plugins are now unloaded */
#define FN_AMXX_PLUGINSUNLOADED OnPluginsUnloaded

/**** METAMOD ****/
// If your module doesn't use metamod, you may close the file now :)
#ifdef USE_METAMOD
// ----
// Hook Functions
// Uncomment these to be called
// You can also change the function name

// - Metamod init functions
// Also consider using FN_AMXX_*
// Meta query
//#define FN_META_QUERY OnMetaQuery
// Meta attach
//#define FN_META_ATTACH OnMetaAttach
// Meta detach
//#define FN_META_DETACH OnMetaDetach

// (wd) are Will Day's notes
// - GetEntityAPI2 functions
// #define FN_GameDLLInit				GameDLLInit					/* pfnGameInit() */
// #define FN_DispatchSpawn				DispatchSpawn				/* pfnSpawn() */
// #define FN_DispatchThink				DispatchThink				/* pfnThink() */
// #define FN_DispatchUse				DispatchUse					/* pfnUse() */
// #define FN_DispatchTouch				DispatchTouch				/* pfnTouch() */
// #define FN_DispatchBlocked			DispatchBlocked				/* pfnBlocked() */
// #define FN_DispatchKeyValue			DispatchKeyValue			/* pfnKeyValue() */
// #define FN_DispatchSave				DispatchSave				/* pfnSave() */
// #define FN_DispatchRestore			DispatchRestore				/* pfnRestore() */
// #define FN_DispatchObjectCollsionBox	DispatchObjectCollsionBox	/* pfnSetAbsBox() */
// #define FN_SaveWriteFields			SaveWriteFields				/* pfnSaveWriteFields() */
// #define FN_SaveReadFields			SaveReadFields				/* pfnSaveReadFields() */
// #define FN_SaveGlobalState			SaveGlobalState				/* pfnSaveGlobalState() */
// #define FN_RestoreGlobalState		RestoreGlobalState			/* pfnRestoreGlobalState() */
// #define FN_ResetGlobalState			ResetGlobalState			/* pfnResetGlobalState() */
// #define FN_ClientConnect				ClientConnect				/* pfnClientConnect()			(wd) Client has connected */
// #define FN_ClientDisconnect			ClientDisconnect			/* pfnClientDisconnect()		(wd) Player has left the game */
// #define FN_ClientKill				ClientKill					/* pfnClientKill()				(wd) Player has typed "kill" */
// #define FN_ClientPutInServer			ClientPutInServer			/* pfnClientPutInServer()		(wd) Client is entering the game */
// #define FN_ClientCommand				ClientCommand				/* pfnClientCommand()			(wd) Player has sent a command (typed or from a bind) */
// #define FN_ClientUserInfoChanged		ClientUserInfoChanged		/* pfnClientUserInfoChanged()	(wd) Client has updated their setinfo structure */
// #define FN_ServerActivate			ServerActivate				/* pfnServerActivate()			(wd) Server is starting a new map */
// #define FN_ServerDeactivate			ServerDeactivate			/* pfnServerDeactivate()		(wd) Server is leaving the map (shutdown or changelevel); SDK2 */
// #define FN_PlayerPreThink			PlayerPreThink				/* pfnPlayerPreThink() */
// #define FN_PlayerPostThink			PlayerPostThink				/* pfnPlayerPostThink() */
// #define FN_StartFrame				StartFrame					/* pfnStartFrame() */
// #define FN_ParmsNewLevel				ParmsNewLevel				/* pfnParmsNewLevel() */
// #define FN_ParmsChangeLevel			ParmsChangeLevel			/* pfnParmsChangeLevel() */
// #define FN_GetGameDescription		GetGameDescription			/* pfnGetGameDescription()		Returns string describing current .dll.  E.g. "TeamFotrress 2" "Half-Life" */
// #define FN_PlayerCustomization		PlayerCustomization			/* pfnPlayerCustomization()	Notifies .dll of new customization for player. */
// #define FN_SpectatorConnect			SpectatorConnect			/* pfnSpectatorConnect()		Called when spectator joins server */
// #define FN_SpectatorDisconnect		SpectatorDisconnect			/* pfnSpectatorDisconnect()	Called when spectator leaves the server */
// #define FN_SpectatorThink			SpectatorThink				/* pfnSpectatorThink()			Called when spectator sends a command packet (usercmd_t) */
// #define FN_Sys_Error					Sys_Error					/* pfnSys_Error()				Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.  SDK2 */
// #define FN_PM_Move					PM_Move						/* pfnPM_Move()				(wd) SDK2 */
// #define FN_PM_Init					PM_Init						/* pfnPM_Init()				Server version of player movement initialization; (wd) SDK2 */
// #define FN_PM_FindTextureType		PM_FindTextureType			/* pfnPM_FindTextureType()		(wd) SDK2 */
// #define FN_SetupVisibility			SetupVisibility				/* pfnSetupVisibility()		Set up PVS and PAS for networking for this client; (wd) SDK2 */
// #define FN_UpdateClientData			UpdateClientData			/* pfnUpdateClientData()		Set up data sent only to specific client; (wd) SDK2 */
// #define FN_AddToFullPack				AddToFullPack				/* pfnAddToFullPack()			(wd) SDK2 */
// #define FN_CreateBaseline			CreateBaseline				/* pfnCreateBaseline()			Tweak entity baseline for network encoding allows setup of player baselines too.; (wd) SDK2 */
// #define FN_RegisterEncoders			RegisterEncoders			/* pfnRegisterEncoders()		Callbacks for network encoding; (wd) SDK2 */
// #define FN_GetWeaponData				GetWeaponData				/* pfnGetWeaponData()			(wd) SDK2 */
// #define FN_CmdStart					CmdStart					/* pfnCmdStart()				(wd) SDK2 */
// #define FN_CmdEnd					CmdEnd						/* pfnCmdEnd()					(wd) SDK2 */
// #define FN_ConnectionlessPacket		ConnectionlessPacket		/* pfnConnectionlessPacket()	(wd) SDK2 */
// #define FN_GetHullBounds				GetHullBounds				/* pfnGetHullBounds()			(wd) SDK2 */
// #define FN_CreateInstancedBaselines	CreateInstancedBaselines	/* pfnCreateInstancedBaselines()	(wd) SDK2 */
// #define FN_InconsistentFile			InconsistentFile			/* pfnInconsistentFile()		(wd) SDK2 */
// #define FN_AllowLagCompensation		AllowLagCompensation		/* pfnAllowLagCompensation()	(wd) SDK2 */

// - GetEntityAPI2_Post functions
// #define FN_GameDLLInit_Post					GameDLLInit_Post
// #define FN_DispatchSpawn_Post				DispatchSpawn_Post
// #define FN_DispatchThink_Post				DispatchThink_Post
// #define FN_DispatchUse_Post					DispatchUse_Post
// #define FN_DispatchTouch_Post				DispatchTouch_Post
// #define FN_DispatchBlocked_Post				DispatchBlocked_Post
// #define FN_DispatchKeyValue_Post				DispatchKeyValue_Post
// #define FN_DispatchSave_Post					DispatchSave_Post
// #define FN_DispatchRestore_Post				DispatchRestore_Post
// #define FN_DispatchObjectCollsionBox_Post	DispatchObjectCollsionBox_Post
// #define FN_SaveWriteFields_Post				SaveWriteFields_Post
// #define FN_SaveReadFields_Post				SaveReadFields_Post
// #define FN_SaveGlobalState_Post				SaveGlobalState_Post
// #define FN_RestoreGlobalState_Post			RestoreGlobalState_Post
// #define FN_ResetGlobalState_Post				ResetGlobalState_Post
// #define FN_ClientConnect_Post				ClientConnect_Post
// #define FN_ClientDisconnect_Post				ClientDisconnect_Post
// #define FN_ClientKill_Post					ClientKill_Post
// #define FN_ClientPutInServer_Post			ClientPutInServer_Post
// #define FN_ClientCommand_Post				ClientCommand_Post
// #define FN_ClientUserInfoChanged_Post		ClientUserInfoChanged_Post
// #define FN_ServerActivate_Post				ServerActivate_Post
// #define FN_ServerDeactivate_Post				ServerDeactivate_Post
// #define FN_PlayerPreThink_Post				PlayerPreThink_Post
// #define FN_PlayerPostThink_Post				PlayerPostThink_Post
// #define FN_StartFrame_Post					StartFrame_Post
// #define FN_ParmsNewLevel_Post				ParmsNewLevel_Post
// #define FN_ParmsChangeLevel_Post				ParmsChangeLevel_Post
// #define FN_GetGameDescription_Post			GetGameDescription_Post
// #define FN_PlayerCustomization_Post			PlayerCustomization_Post
// #define FN_SpectatorConnect_Post				SpectatorConnect_Post
// #define FN_SpectatorDisconnect_Post			SpectatorDisconnect_Post
// #define FN_SpectatorThink_Post				SpectatorThink_Post
// #define FN_Sys_Error_Post					Sys_Error_Post
// #define FN_PM_Move_Post						PM_Move_Post
// #define FN_PM_Init_Post						PM_Init_Post
// #define FN_PM_FindTextureType_Post			PM_FindTextureType_Post
// #define FN_SetupVisibility_Post				SetupVisibility_Post
// #define FN_UpdateClientData_Post				UpdateClientData_Post
// #define FN_AddToFullPack_Post				AddToFullPack_Post
// #define FN_CreateBaseline_Post				CreateBaseline_Post
// #define FN_RegisterEncoders_Post				RegisterEncoders_Post
// #define FN_GetWeaponData_Post				GetWeaponData_Post
// #define FN_CmdStart_Post						CmdStart_Post
// #define FN_CmdEnd_Post						CmdEnd_Post
// #define FN_ConnectionlessPacket_Post			ConnectionlessPacket_Post
// #define FN_GetHullBounds_Post				GetHullBounds_Post
// #define FN_CreateInstancedBaselines_Post		CreateInstancedBaselines_Post
// #define FN_InconsistentFile_Post				InconsistentFile_Post
// #define FN_AllowLagCompensation_Post			AllowLagCompensation_Post

// - GetEngineAPI functions
// #define FN_PrecacheModel						PrecacheModel
// #define FN_PrecacheSound						PrecacheSound
// #define FN_SetModel							SetModel
// #define FN_ModelIndex						ModelIndex
// #define FN_ModelFrames						ModelFrames
// #define FN_SetSize							SetSize
// #define FN_ChangeLevel						ChangeLevel
// #define FN_GetSpawnParms						GetSpawnParms
// #define FN_SaveSpawnParms					SaveSpawnParms
// #define FN_VecToYaw							VecToYaw
// #define FN_VecToAngles						VecToAngles
// #define FN_MoveToOrigin						MoveToOrigin
// #define FN_ChangeYaw							ChangeYaw
// #define FN_ChangePitch						ChangePitch
// #define FN_FindEntityByString				FindEntityByString
// #define FN_GetEntityIllum					GetEntityIllum
// #define FN_FindEntityInSphere				FindEntityInSphere
// #define FN_FindClientInPVS					FindClientInPVS
// #define FN_EntitiesInPVS						EntitiesInPVS
// #define FN_MakeVectors						MakeVectors
// #define FN_AngleVectors						AngleVectors
// #define FN_CreateEntity						CreateEntity
// #define FN_RemoveEntity						RemoveEntity
// #define FN_CreateNamedEntity					CreateNamedEntity
// #define FN_MakeStatic						MakeStatic
// #define FN_EntIsOnFloor						EntIsOnFloor
// #define FN_DropToFloor						DropToFloor
// #define FN_WalkMove							WalkMove
// #define FN_SetOrigin							SetOrigin
// #define FN_EmitSound							EmitSound
// #define FN_EmitAmbientSound					EmitAmbientSound
// #define FN_TraceLine							TraceLine
// #define FN_TraceToss							TraceToss
// #define FN_TraceMonsterHull					TraceMonsterHull
// #define FN_TraceHull							TraceHull
// #define FN_TraceModel						TraceModel
// #define FN_TraceTexture						TraceTexture
// #define FN_TraceSphere						TraceSphere
// #define FN_GetAimVector						GetAimVector
// #define FN_ServerCommand						ServerCommand
// #define FN_ServerExecute						ServerExecute
// #define FN_engClientCommand					engClientCommand
// #define FN_ParticleEffect					ParticleEffect
// #define FN_LightStyle						LightStyle
// #define FN_DecalIndex						DecalIndex
// #define FN_PointContents						PointContents
// #define FN_MessageBegin						MessageBegin
// #define FN_MessageEnd						MessageEnd
// #define FN_WriteByte							WriteByte
// #define FN_WriteChar							WriteChar
// #define FN_WriteShort						WriteShort
// #define FN_WriteLong							WriteLong
// #define FN_WriteAngle						WriteAngle
// #define FN_WriteCoord						WriteCoord
// #define FN_WriteString						WriteString
// #define FN_WriteEntity						WriteEntity
// #define FN_CVarRegister						CVarRegister
// #define FN_CVarGetFloat						CVarGetFloat
// #define FN_CVarGetString						CVarGetString
// #define FN_CVarSetFloat						CVarSetFloat
// #define FN_CVarSetString						CVarSetString
// #define FN_AlertMessage						AlertMessage
// #define FN_EngineFprintf						EngineFprintf
// #define FN_PvAllocEntPrivateData				PvAllocEntPrivateData
// #define FN_PvEntPrivateData					PvEntPrivateData
// #define FN_FreeEntPrivateData				FreeEntPrivateData
// #define FN_SzFromIndex						SzFromIndex
// #define FN_AllocString						AllocString
// #define FN_GetVarsOfEnt						GetVarsOfEnt
// #define FN_PEntityOfEntOffset				PEntityOfEntOffset
// #define FN_EntOffsetOfPEntity				EntOffsetOfPEntity
// #define FN_IndexOfEdict						IndexOfEdict
// #define FN_PEntityOfEntIndex					PEntityOfEntIndex
// #define FN_FindEntityByVars					FindEntityByVars
// #define FN_GetModelPtr						GetModelPtr
// #define FN_RegUserMsg						RegUserMsg
// #define FN_AnimationAutomove					AnimationAutomove
// #define FN_GetBonePosition					GetBonePosition
// #define FN_FunctionFromName					FunctionFromName
// #define FN_NameForFunction					NameForFunction
// #define FN_ClientPrintf						ClientPrintf
// #define FN_ServerPrint						ServerPrint
// #define FN_Cmd_Args							Cmd_Args
// #define FN_Cmd_Argv							Cmd_Argv
// #define FN_Cmd_Argc							Cmd_Argc
// #define FN_GetAttachment						GetAttachment
// #define FN_CRC32_Init						CRC32_Init
// #define FN_CRC32_ProcessBuffer				CRC32_ProcessBuffer
// #define FN_CRC32_ProcessByte					CRC32_ProcessByte
// #define FN_CRC32_Final						CRC32_Final
// #define FN_RandomLong						RandomLong
// #define FN_RandomFloat						RandomFloat
// #define FN_SetView							SetView
// #define FN_Time								Time
// #define FN_CrosshairAngle					CrosshairAngle
// #define FN_LoadFileForMe						LoadFileForMe
// #define FN_FreeFile							FreeFile
// #define FN_EndSection						EndSection
// #define FN_CompareFileTime					CompareFileTime
// #define FN_GetGameDir						GetGameDir
// #define FN_Cvar_RegisterVariable				Cvar_RegisterVariable
// #define FN_FadeClientVolume					FadeClientVolume
// #define FN_SetClientMaxspeed					SetClientMaxspeed
// #define FN_CreateFakeClient					CreateFakeClient
// #define FN_RunPlayerMove						RunPlayerMove
// #define FN_NumberOfEntities					NumberOfEntities
// #define FN_GetInfoKeyBuffer					GetInfoKeyBuffer
// #define FN_InfoKeyValue						InfoKeyValue
// #define FN_SetKeyValue						SetKeyValue
// #define FN_SetClientKeyValue					SetClientKeyValue
// #define FN_IsMapValid						IsMapValid
// #define FN_StaticDecal						StaticDecal
// #define FN_PrecacheGeneric					PrecacheGeneric
// #define FN_GetPlayerUserId					GetPlayerUserId
// #define FN_BuildSoundMsg						BuildSoundMsg
// #define FN_IsDedicatedServer					IsDedicatedServer
// #define FN_CVarGetPointer					CVarGetPointer
// #define FN_GetPlayerWONId					GetPlayerWONId
// #define FN_Info_RemoveKey					Info_RemoveKey
// #define FN_GetPhysicsKeyValue				GetPhysicsKeyValue
// #define FN_SetPhysicsKeyValue				SetPhysicsKeyValue
// #define FN_GetPhysicsInfoString				GetPhysicsInfoString
// #define FN_PrecacheEvent						PrecacheEvent
// #define FN_PlaybackEvent						PlaybackEvent
// #define FN_SetFatPVS							SetFatPVS
// #define FN_SetFatPAS							SetFatPAS
// #define FN_CheckVisibility					CheckVisibility
// #define FN_DeltaSetField						DeltaSetField
// #define FN_DeltaUnsetField					DeltaUnsetField
// #define FN_DeltaAddEncoder					DeltaAddEncoder
// #define FN_GetCurrentPlayer					GetCurrentPlayer
// #define FN_CanSkipPlayer						CanSkipPlayer
// #define FN_DeltaFindField					DeltaFindField
// #define FN_DeltaSetFieldByIndex				DeltaSetFieldByIndex
// #define FN_DeltaUnsetFieldByIndex			DeltaUnsetFieldByIndex
// #define FN_SetGroupMask						SetGroupMask
// #define FN_engCreateInstancedBaseline		engCreateInstancedBaseline
// #define FN_Cvar_DirectSet					Cvar_DirectSet
// #define FN_ForceUnmodified					ForceUnmodified
// #define FN_GetPlayerStats					GetPlayerStats
// #define FN_AddServerCommand					AddServerCommand
// #define FN_Voice_GetClientListening			Voice_GetClientListening
// #define FN_Voice_SetClientListening			Voice_SetClientListening
// #define FN_GetPlayerAuthId					GetPlayerAuthId

// - GetEngineAPI_Post functions
// #define FN_PrecacheModel_Post				PrecacheModel_Post
// #define FN_PrecacheSound_Post				PrecacheSound_Post
// #define FN_SetModel_Post						SetModel_Post
// #define FN_ModelIndex_Post					ModelIndex_Post
// #define FN_ModelFrames_Post					ModelFrames_Post
// #define FN_SetSize_Post						SetSize_Post
// #define FN_ChangeLevel_Post					ChangeLevel_Post
// #define FN_GetSpawnParms_Post				GetSpawnParms_Post
// #define FN_SaveSpawnParms_Post				SaveSpawnParms_Post
// #define FN_VecToYaw_Post						VecToYaw_Post
// #define FN_VecToAngles_Post					VecToAngles_Post
// #define FN_MoveToOrigin_Post					MoveToOrigin_Post
// #define FN_ChangeYaw_Post					ChangeYaw_Post
// #define FN_ChangePitch_Post					ChangePitch_Post
// #define FN_FindEntityByString_Post			FindEntityByString_Post
// #define FN_GetEntityIllum_Post				GetEntityIllum_Post
// #define FN_FindEntityInSphere_Post			FindEntityInSphere_Post
// #define FN_FindClientInPVS_Post				FindClientInPVS_Post
// #define FN_EntitiesInPVS_Post				EntitiesInPVS_Post
// #define FN_MakeVectors_Post					MakeVectors_Post
// #define FN_AngleVectors_Post					AngleVectors_Post
// #define FN_CreateEntity_Post					CreateEntity_Post
// #define FN_RemoveEntity_Post					RemoveEntity_Post
// #define FN_CreateNamedEntity_Post			CreateNamedEntity_Post
// #define FN_MakeStatic_Post					MakeStatic_Post
// #define FN_EntIsOnFloor_Post					EntIsOnFloor_Post
// #define FN_DropToFloor_Post					DropToFloor_Post
// #define FN_WalkMove_Post						WalkMove_Post
// #define FN_SetOrigin_Post					SetOrigin_Post
// #define FN_EmitSound_Post					EmitSound_Post
// #define FN_EmitAmbientSound_Post				EmitAmbientSound_Post
// #define FN_TraceLine_Post					TraceLine_Post
// #define FN_TraceToss_Post					TraceToss_Post
// #define FN_TraceMonsterHull_Post				TraceMonsterHull_Post
// #define FN_TraceHull_Post					TraceHull_Post
// #define FN_TraceModel_Post					TraceModel_Post
// #define FN_TraceTexture_Post					TraceTexture_Post
// #define FN_TraceSphere_Post					TraceSphere_Post
// #define FN_GetAimVector_Post					GetAimVector_Post
// #define FN_ServerCommand_Post				ServerCommand_Post
// #define FN_ServerExecute_Post				ServerExecute_Post
// #define FN_engClientCommand_Post				engClientCommand_Post
// #define FN_ParticleEffect_Post				ParticleEffect_Post
// #define FN_LightStyle_Post					LightStyle_Post
// #define FN_DecalIndex_Post					DecalIndex_Post
// #define FN_PointContents_Post				PointContents_Post
// #define FN_MessageBegin_Post					MessageBegin_Post
// #define FN_MessageEnd_Post					MessageEnd_Post
// #define FN_WriteByte_Post					WriteByte_Post
// #define FN_WriteChar_Post					WriteChar_Post
// #define FN_WriteShort_Post					WriteShort_Post
// #define FN_WriteLong_Post					WriteLong_Post
// #define FN_WriteAngle_Post					WriteAngle_Post
// #define FN_WriteCoord_Post					WriteCoord_Post
// #define FN_WriteString_Post					WriteString_Post
// #define FN_WriteEntity_Post					WriteEntity_Post
// #define FN_CVarRegister_Post					CVarRegister_Post
// #define FN_CVarGetFloat_Post					CVarGetFloat_Post
// #define FN_CVarGetString_Post				CVarGetString_Post
// #define FN_CVarSetFloat_Post					CVarSetFloat_Post
// #define FN_CVarSetString_Post				CVarSetString_Post
// #define FN_AlertMessage_Post					AlertMessage_Post
// #define FN_EngineFprintf_Post				EngineFprintf_Post
// #define FN_PvAllocEntPrivateData_Post		PvAllocEntPrivateData_Post
// #define FN_PvEntPrivateData_Post				PvEntPrivateData_Post
// #define FN_FreeEntPrivateData_Post			FreeEntPrivateData_Post
// #define FN_SzFromIndex_Post					SzFromIndex_Post
// #define FN_AllocString_Post					AllocString_Post
// #define FN_GetVarsOfEnt_Post					GetVarsOfEnt_Post
// #define FN_PEntityOfEntOffset_Post			PEntityOfEntOffset_Post
// #define FN_EntOffsetOfPEntity_Post			EntOffsetOfPEntity_Post
// #define FN_IndexOfEdict_Post					IndexOfEdict_Post
// #define FN_PEntityOfEntIndex_Post			PEntityOfEntIndex_Post
// #define FN_FindEntityByVars_Post				FindEntityByVars_Post
// #define FN_GetModelPtr_Post					GetModelPtr_Post
// #define FN_RegUserMsg_Post					RegUserMsg_Post
// #define FN_AnimationAutomove_Post			AnimationAutomove_Post
// #define FN_GetBonePosition_Post				GetBonePosition_Post
// #define FN_FunctionFromName_Post				FunctionFromName_Post
// #define FN_NameForFunction_Post				NameForFunction_Post
// #define FN_ClientPrintf_Post					ClientPrintf_Post
// #define FN_ServerPrint_Post					ServerPrint_Post
// #define FN_Cmd_Args_Post						Cmd_Args_Post
// #define FN_Cmd_Argv_Post						Cmd_Argv_Post
// #define FN_Cmd_Argc_Post						Cmd_Argc_Post
// #define FN_GetAttachment_Post				GetAttachment_Post
// #define FN_CRC32_Init_Post					CRC32_Init_Post
// #define FN_CRC32_ProcessBuffer_Post			CRC32_ProcessBuffer_Post
// #define FN_CRC32_ProcessByte_Post			CRC32_ProcessByte_Post
// #define FN_CRC32_Final_Post					CRC32_Final_Post
// #define FN_RandomLong_Post					RandomLong_Post
// #define FN_RandomFloat_Post					RandomFloat_Post
// #define FN_SetView_Post						SetView_Post
// #define FN_Time_Post							Time_Post
// #define FN_CrosshairAngle_Post				CrosshairAngle_Post
// #define FN_LoadFileForMe_Post				LoadFileForMe_Post
// #define FN_FreeFile_Post						FreeFile_Post
// #define FN_EndSection_Post					EndSection_Post
// #define FN_CompareFileTime_Post				CompareFileTime_Post
// #define FN_GetGameDir_Post					GetGameDir_Post
// #define FN_Cvar_RegisterVariable_Post		Cvar_RegisterVariable_Post
// #define FN_FadeClientVolume_Post				FadeClientVolume_Post
// #define FN_SetClientMaxspeed_Post			SetClientMaxspeed_Post
// #define FN_CreateFakeClient_Post				CreateFakeClient_Post
// #define FN_RunPlayerMove_Post				RunPlayerMove_Post
// #define FN_NumberOfEntities_Post				NumberOfEntities_Post
// #define FN_GetInfoKeyBuffer_Post				GetInfoKeyBuffer_Post
// #define FN_InfoKeyValue_Post					InfoKeyValue_Post
// #define FN_SetKeyValue_Post					SetKeyValue_Post
// #define FN_SetClientKeyValue_Post			SetClientKeyValue_Post
// #define FN_IsMapValid_Post					IsMapValid_Post
// #define FN_StaticDecal_Post					StaticDecal_Post
// #define FN_PrecacheGeneric_Post				PrecacheGeneric_Post
// #define FN_GetPlayerUserId_Post				GetPlayerUserId_Post
// #define FN_BuildSoundMsg_Post				BuildSoundMsg_Post
// #define FN_IsDedicatedServer_Post			IsDedicatedServer_Post
// #define FN_CVarGetPointer_Post				CVarGetPointer_Post
// #define FN_GetPlayerWONId_Post				GetPlayerWONId_Post
// #define FN_Info_RemoveKey_Post				Info_RemoveKey_Post
// #define FN_GetPhysicsKeyValue_Post			GetPhysicsKeyValue_Post
// #define FN_SetPhysicsKeyValue_Post			SetPhysicsKeyValue_Post
// #define FN_GetPhysicsInfoString_Post			GetPhysicsInfoString_Post
// #define FN_PrecacheEvent_Post				PrecacheEvent_Post
// #define FN_PlaybackEvent_Post				PlaybackEvent_Post
// #define FN_SetFatPVS_Post					SetFatPVS_Post
// #define FN_SetFatPAS_Post					SetFatPAS_Post
// #define FN_CheckVisibility_Post				CheckVisibility_Post
// #define FN_DeltaSetField_Post				DeltaSetField_Post
// #define FN_DeltaUnsetField_Post				DeltaUnsetField_Post
// #define FN_DeltaAddEncoder_Post				DeltaAddEncoder_Post
// #define FN_GetCurrentPlayer_Post				GetCurrentPlayer_Post
// #define FN_CanSkipPlayer_Post				CanSkipPlayer_Post
// #define FN_DeltaFindField_Post				DeltaFindField_Post
// #define FN_DeltaSetFieldByIndex_Post			DeltaSetFieldByIndex_Post
// #define FN_DeltaUnsetFieldByIndex_Post		DeltaUnsetFieldByIndex_Post
// #define FN_SetGroupMask_Post					SetGroupMask_Post
// #define FN_engCreateInstancedBaseline_Post	engCreateInstancedBaseline_Post
// #define FN_Cvar_DirectSet_Post				Cvar_DirectSet_Post
// #define FN_ForceUnmodified_Post				ForceUnmodified_Post
// #define FN_GetPlayerStats_Post				GetPlayerStats_Post
// #define FN_AddServerCommand_Post				AddServerCommand_Post
// #define FN_Voice_GetClientListening_Post		Voice_GetClientListening_Post
// #define FN_Voice_SetClientListening_Post		Voice_SetClientListening_Post
// #define FN_GetPlayerAuthId_Post				GetPlayerAuthId_Post

// #define FN_OnFreeEntPrivateData				OnFreeEntPrivateData				
// #define FN_GameShutdown						GameShutdown
// #define FN_ShouldCollide						ShouldCollide

// #define FN_OnFreeEntPrivateData_Post			OnFreeEntPrivateData_Post
// #define FN_GameShutdown_Post					GameShutdown_Post
// #define FN_ShouldCollide_Post				ShouldCollide_Post


#endif // USE_METAMOD

#endif // __MODULECONFIG_H__
