// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Stocks
//

#if !defined _fakemeta_included
	#include <fakemeta>
#endif

#if defined _fakemeta_stocks_included
  #endinput
#endif
#define _fakemeta_stocks_included

// EngFuncs
stock EF_PrecacheModel(const string[]) {
	return engfunc(EngFunc_PrecacheModel, string);
}

stock EF_PrecacheSound(const string[]) {
	return engfunc(EngFunc_PrecacheSound, string);
}
stock EF_SetModel(const ID, const STRING[]) {
	return engfunc(EngFunc_SetModel, ID, STRING);
}
stock EF_ModelIndex(const STRING[]) {
	return engfunc(EngFunc_ModelIndex, STRING);
}
stock EF_ModelFrames(modelIndex) {
	return engfunc(EngFunc_ModelFrames, modelIndex);
}

stock EF_SetSize(const ENTITY, const Float:MIN[3], const Float:MAX[3]) {
	return engfunc(EngFunc_SetSize, ENTITY, MIN, MAX);
}
stock EF_ChangeLevel(const S1[], const S2[]) {
	return engfunc(EngFunc_ChangeLevel, S1, S2);
}
stock EF_VecToYaw(const Float:VECTOR[3], &Float:returnValue) {
	return engfunc(EngFunc_VecToYaw, VECTOR, returnValue);
}
stock EF_VecToAngles(const Float:VECTORIN[3], const Float:VECTOROUT[3]) {
	return engfunc(EngFunc_VecToAngles, VECTORIN, VECTOROUT);
}
stock EF_MoveToOrigin(const ENTITY, const Float:GOAL[3], const Float:DISTANCE, const MOVETYPE) {
	return engfunc(EngFunc_MoveToOrigin, ENTITY, GOAL, DISTANCE, MOVETYPE);
}

stock EF_ChangeYaw(const ENTITY) {
	return engfunc(EngFunc_ChangeYaw, ENTITY);
}
stock EF_ChangePitch(const ENTITY) {
	return engfunc(EngFunc_ChangePitch, ENTITY);
}
stock EF_FindEntityByString(const STARTSEARCHAFTER, const FIELD[], const VALUE[]) {
	return engfunc(EngFunc_FindEntityByString, STARTSEARCHAFTER, FIELD, VALUE);
}
stock EF_GetEntityIllum(const ENTITY) {
	return engfunc(EngFunc_GetEntityIllum, ENTITY);
}
stock EF_FindEntityInSphere(const STARTSEARCHAFTER, const Float:ORIGIN[3], Float:radius) {
	return engfunc(EngFunc_FindEntityInSphere, STARTSEARCHAFTER, ORIGIN, radius);
}

stock EF_FindClientInPVS(const CLIENT) {
	return engfunc(EngFunc_FindClientInPVS, CLIENT);
}
stock EF_EntitiesInPVS(const CLIENT) {
	return engfunc(EngFunc_EntitiesInPVS, CLIENT);
}
stock EF_MakeVectors(const Float:VECTOR[3]) {
	return engfunc(EngFunc_MakeVectors, VECTOR);
}
stock EF_AngleVectors(const Float:VECTOR[3], Float:forward_[3], Float:right[3], Float:up[3]) {
	return engfunc(EngFunc_AngleVectors, VECTOR, forward_, right, up);
}
stock EF_CreateEntity() {
	return engfunc(EngFunc_CreateEntity);
}

stock EF_RemoveEntity(const ENTITY) {
	return engfunc(EngFunc_RemoveEntity, ENTITY);
}
stock EF_CreateNamedEntity(const CLASSNAME) {
	return engfunc(EngFunc_CreateNamedEntity, CLASSNAME);
}
stock EF_MakeStatic(const ENTITY) {
	return engfunc(EngFunc_MakeStatic, ENTITY);
}
stock EF_EntIsOnFloor(const ENTITY) {
	return engfunc(EngFunc_EntIsOnFloor, ENTITY);
}
stock EF_DropToFloor(const ENTITY) {
	return engfunc(EngFunc_DropToFloor, ENTITY);
}

stock EF_WalkMove(const ENTITY, Float:yaw, Float:distance, iMode) {
	return engfunc(EngFunc_WalkMove, ENTITY, yaw, distance, iMode);
}
stock EF_SetOrigin(const ENTITY, const Float:ORIGIN[3]) {
	return engfunc(EngFunc_SetOrigin, ENTITY, ORIGIN);
}
stock EF_EmitSound(const ENTITY, channel, const SAMPLE[], Float:volume, Float:attenuation, fFlags, pitch) {
	return engfunc(EngFunc_EmitSound, ENTITY, channel, SAMPLE, volume, attenuation, fFlags, pitch);
}
stock EF_EmitAmbientSound(const ENTITY, Float:pos[3], const SAMPLE[], Float:volume, Float:attenuation, fFlags, pitch) {
	return engfunc(EngFunc_EmitAmbientSound, ENTITY, pos, SAMPLE, volume, attenuation, fFlags, pitch);
}
stock EF_TraceLine(const Float:V1[3], const Float:V2[3], fNoMonsters, const ENT_TO_SKIP) {
	return engfunc(EngFunc_TraceLine, V1, V2, fNoMonsters, ENT_TO_SKIP);
}

stock EF_TraceToss(const ENTITY, const ENTITY_TO_IGNORE) {
	return engfunc(EngFunc_TraceToss, ENTITY, ENTITY_TO_IGNORE);
}
stock EF_TraceMonsterHull(const ENTITY, const Float:V1[3], const Float:V2[3], fNoMonsters, const ENTITY_TO_SKIP) {
	return engfunc(EngFunc_TraceMonsterHull, ENTITY, V1, V2, fNoMonsters, ENTITY_TO_SKIP);
}
stock EF_TraceHull(const Float:V1[3], const Float:V2[3], fNoMonsters, hullNumber, const ENTITY_TO_SKIP) {
	return engfunc(EngFunc_TraceHull, V1, V2, fNoMonsters, hullNumber, ENTITY_TO_SKIP);
}
stock EF_TraceModel(const Float:V1[3], const Float:V2[3], hullNumber, const ENTITY) {
	return engfunc(EngFunc_TraceModel, V1, V2, hullNumber, ENTITY);
}
stock EF_TraceTexture(const TEXTURE_ENTITY, const Float:V1[3], const Float:V2[3]) {
	return engfunc(EngFunc_TraceTexture, TEXTURE_ENTITY, V1, V2);
}

stock EF_TraceSphere(const Float:V1[3], const Float:V2[3], fNoMonsters, Float:radius, const ENTITY_TO_SKIP) {
	return engfunc(EngFunc_TraceSphere, V1, V2, fNoMonsters, radius, ENTITY_TO_SKIP);
}
stock EF_GetAimVector(const ENTITY, Float:speed, Float:returnVector[3]) {
	return engfunc(EngFunc_GetAimVector, ENTITY, speed, returnVector);
}
stock EF_ParticleEffect(const Float:ORIGIN[3], const Float:DIRECTION[3], Float:color, Float:count) {
	return engfunc(EngFunc_ParticleEffect, ORIGIN, DIRECTION, color, count);
}
stock EF_LightStyle(style, val[]) {
	return engfunc(EngFunc_LightStyle, style, val);
}
stock EF_DecalIndex(const NAME[]) {
	return engfunc(EngFunc_DecalIndex, NAME);
}

stock EF_PointContents(const Float:VECTOR[3]) {
	return engfunc(EngFunc_PointContents, VECTOR);
}
stock EF_FreeEntPrivateData(const ENTITY) {
	return engfunc(EngFunc_FreeEntPrivateData, ENTITY);
}
stock EF_SzFromIndex(iString) {
	return engfunc(EngFunc_SzFromIndex, iString);
}
stock EF_AllocString(const STRING[]) {
	return engfunc(EngFunc_AllocString, STRING);
}
stock EF_RegUserMsg(const NAME[], iSize) {
	return engfunc(EngFunc_RegUserMsg, NAME, iSize);
}

stock EF_AnimationAutomove(const ENTITY, Float:flTime) {
	return engfunc(EngFunc_AnimationAutomove, ENTITY, flTime);
}
stock EF_GetBonePosition(const ENTITY, iBone, Float:origin[3], Float:angles[3]) {
	return engfunc(EngFunc_GetBonePosition, ENTITY, iBone, origin, angles);
}
stock EF_GetAttachment(const ENTITY, iAttachment, Float:origin[3], Float:angles[3]) {
	return engfunc(EngFunc_GetAttachment, ENTITY, iAttachment, origin, angles);
}
stock EF_SetView(const CLIENT, const VIEW_ENTITY) {
	return engfunc(EngFunc_SetView, CLIENT, VIEW_ENTITY);
}
stock EF_Time(&Float:returnValue) {
	return engfunc(EngFunc_Time, returnValue);
}

stock EF_CrosshairAngle(const CLIENT, Float:pitch, Float:yaw) {
	return engfunc(EngFunc_CrosshairAngle, CLIENT, pitch, yaw);
}
stock EF_FadeClientVolume(const ENTITY, fadePercent, fadeOutSeconds, holdTime, fadeInSeconds) {
	return engfunc(EngFunc_FadeClientVolume, ENTITY, fadePercent, fadeOutSeconds, holdTime, fadeInSeconds);
}
stock EF_SetClientMaxspeed(const ENTITY, Float:newMaxspeed) {
	return engfunc(EngFunc_SetClientMaxspeed, ENTITY, newMaxspeed);
}
stock EF_CreateFakeClient(const NETNAME[]) {
	return engfunc(EngFunc_CreateFakeClient, NETNAME);
}
stock EF_RunPlayerMove(const FAKECLIENT, const Float:VIEWANGLES[3], Float:forwardmove, Float:sidemove, Float:upmove, buttons, impulse, msec) {
	return engfunc(EngFunc_RunPlayerMove, FAKECLIENT, VIEWANGLES, forwardmove, sidemove, upmove, buttons, impulse, msec);
}

stock EF_NumberOfEntities() {
	return engfunc(EngFunc_NumberOfEntities);
}
stock EF_StaticDecal(const Float:ORIGIN[3], decalIndex, entityIndex, modelIndex)
	return engfunc(EngFunc_StaticDecal, ORIGIN, decalIndex, entityIndex, modelIndex);
stock EF_PrecacheGeneric(const STRING[]) {
	return engfunc(EngFunc_PrecacheGeneric, STRING);
}
stock EF_BuildSoundMSG(const ENTITY, channel, const SAMPLE[], Float:volume, Float:attenuation, fFlags, pitch, msg_dest, msg_type, const Float:ORIGIN[3], const ED) {
	return engfunc(EngFunc_BuildSoundMsg, ENTITY, channel, SAMPLE, volume, attenuation, fFlags, pitch, msg_dest, msg_type, ORIGIN, ED);
}
stock EF_GetPhysicsKeyValue(const CLIENT, const KEY[]) {
	return engfunc(EngFunc_GetPhysicsKeyValue, CLIENT, KEY);
}

stock EF_SetPhysicsKeyValue(const CLIENT, const KEY[], const VALUE[]) {
	return engfunc(EngFunc_SetPhysicsKeyValue, CLIENT, KEY, VALUE);
}
stock EF_GetPhysicsInfoString(const CLIENT, returnString[], maxLength) {
	return engfunc(EngFunc_GetPhysicsInfoString, CLIENT, returnString, maxLength);
}
stock EF_PrecacheEvent(type, const STRING[]) {
	return engfunc(EngFunc_PrecacheEvent, type, STRING);
}
stock EF_PlaybackEvent(flags, const INVOKER, eventindex, Float:delay, Float:origin[3], Float:angles[3], Float:fparam1, Float:fparam2, iparam1, iparam2, bparam1, bparam2) {
	return engfunc(EngFunc_PlaybackEvent, flags, INVOKER, eventindex, delay, origin, angles, fparam1, fparam2, iparam1, iparam2, bparam1, bparam2);
}
stock EF_CheckVisibility(const ENTITY, set) {
	return engfunc(EngFunc_CheckVisibility, ENTITY, set);
}

stock EF_GetCurrentPlayer() {
	return engfunc(EngFunc_GetCurrentPlayer);
}
stock EF_CanSkipPlayer(const PLAYER) {
	return engfunc(EngFunc_CanSkipPlayer, PLAYER);
}
stock EF_SetGroupMask(mask, op) {
	return engfunc(EngFunc_SetGroupMask, mask, op);
}
stock EF_GetClientListening(receiver, sender) {
	return engfunc(EngFunc_GetClientListening, receiver, sender);
}
stock EF_SetClientListening(receiver, sender, bool:listen) {
	return engfunc(EngFunc_SetClientListening, receiver, sender, listen);
}

stock EF_MessageBegin(msg_dest, msg_type, const Float:ORIGIN[3], const ED) {
	return engfunc(EngFunc_MessageBegin, msg_dest, msg_type, ORIGIN, ED);
}
stock EF_WriteCoord(Float:value) {
	return engfunc(EngFunc_WriteCoord, value);
}
stock EF_WriteAngle(Float:value) {
	return engfunc(EngFunc_WriteAngle, value);
}
stock EF_InfoKeyValue(const INFOBUFFER, const KEY[], returnValue[], maxLength) {
	return engfunc(EngFunc_InfoKeyValue, INFOBUFFER, KEY, returnValue, maxLength);
}
stock EF_SetKeyValue(const INFOBUFFER, const KEY[], const VALUE[]) {
	return engfunc(EngFunc_SetKeyValue, INFOBUFFER, KEY, VALUE);
}

stock EF_SetClientKeyValue(const ID, const INFOBUFFER, const KEY[], const VALUE[]) {
	return engfunc(EngFunc_SetClientKeyValue, ID, INFOBUFFER, KEY, VALUE);
}

stock EF_CreateInstBaseline(CLASSNAME, baseline) {
	return engfunc(EngFunc_CreateInstBaseline, CLASSNAME, baseline);
}

// Returns pointer to info buffer that can be used with the INFOBUFFER param
// of EF_InfoKeyValue, EF_SetKeyValue, and EF_SetClientKeyValue
stock EF_GetInfoKeyBuffer(const ENTITY) {
	return engfunc(EngFunc_GetInfoKeyBuffer, ENTITY);
}
stock EF_ClientPrintf(const ENTITY, const printType, const MESSAGE[]) {
	return engfunc(EngFunc_ClientPrintf, ENTITY, printType, MESSAGE);
}
stock EF_ServerPrint(const MESSAGE[]) {
	return engfunc(EngFunc_ServerPrint, MESSAGE);
}

// DLLFuncs
stock DF_GameInit() {
	return dllfunc(DLLFunc_GameInit);
}
stock DF_Spawn(const ENTITY) {
	return dllfunc(DLLFunc_Spawn, ENTITY);
}
stock DF_Think(const ENTITY) {
	return dllfunc(DLLFunc_Think, ENTITY);
}
stock DF_Use(const ENT_Used, const ENT_User) {
	return dllfunc(DLLFunc_Use, ENT_Used, ENT_User);
}
stock DF_Touch(const ENT_Touched, const ENT_Toucher) {
	return dllfunc(DLLFunc_Touch, ENT_Touched, ENT_Toucher);
}

stock DF_Blocked(const ENT_Blocked, const ENT_Other) {
	return dllfunc(DLLFunc_Blocked, ENT_Blocked, ENT_Other);
}
stock DF_SetAbsBox(const ENTITY) {
	return dllfunc(DLLFunc_SetAbsBox, ENTITY);
}
stock DF_ClientConnect(const ENTITY, const NAME[], const ADDRESS[], RejectReason[128]) {
	return dllfunc(DLLFunc_ClientConnect, ENTITY, NAME, ADDRESS, RejectReason);
}
stock DF_ClientDisconnect(const ENTITY) {
	return dllfunc(DLLFunc_ClientDisconnect, ENTITY);
}
stock DF_ClientKill(const ENTITY) {
	return dllfunc(DLLFunc_ClientKill, ENTITY);
}

stock DF_ClientPutInServer(const ENTITY) {
	return dllfunc(DLLFunc_ClientPutInServer, ENTITY);
}
stock DF_ClientCommand(const ENTITY) {
	return dllfunc(DLLFunc_ClientCommand, ENTITY);
}
stock DF_ServerDeactivate() {
	return dllfunc(DLLFunc_ServerDeactivate);
}
stock DF_PlayerPreThink(const ENTITY) {
	return dllfunc(DLLFunc_PlayerPreThink, ENTITY);
}
stock DF_PlayerPostThink(const ENTITY) {
	return dllfunc(DLLFunc_PlayerPostThink, ENTITY);
}

stock DF_StartFrame() {
	return dllfunc(DLLFunc_StartFrame);
}
stock DF_ParmsNewLevel() {
	return dllfunc(DLLFunc_ParmsNewLevel);
}
stock DF_ParmsChangeLevel() {
	return dllfunc(DLLFunc_ParmsChangeLevel);
}
stock DF_GetGameDescription() {
	return dllfunc(DLLFunc_GetGameDescription);
}
stock DF_SpectatorConnect(const ENTITY) {
	return dllfunc(DLLFunc_SpectatorConnect, ENTITY);
}

stock DF_SpectatorDisconnect(const ENTITY) {
	return dllfunc(DLLFunc_SpectatorDisconnect, ENTITY);
}
stock DF_SpectatorThink(const ENTITY) {
	return dllfunc(DLLFunc_SpectatorThink, ENTITY);
}
stock DF_Sys_Error(const ERROR_STRING[]) {
	return dllfunc(DLLFunc_Sys_Error, ERROR_STRING);
}
stock DF_PM_FindTextureType(name[]) {
	return dllfunc(DLLFunc_PM_FindTextureType, name);
}
stock DF_RegisterEncoders() {
	return dllfunc(DLLFunc_RegisterEncoders);
}

stock DF_GetHullBounds(hullnumber, Float:mins[3], Float:maxs[3]) {
	return dllfunc(DLLFunc_GetHullBounds, hullnumber, mins, maxs);
}
stock DF_CreateInstBaselines() {
	return dllfunc(DLLFunc_CreateInstBaselines);
}
stock DF_pfnAllowLagCompensation() {
	return dllfunc(DLLFunc_pfnAllowLagCompensation);
}
stock DF_MetaFunc_CallGameEntity(const STRING[], const ENTITY) {
	return dllfunc(MetaFunc_CallGameEntity, STRING, ENTITY);
}
stock DF_ClientUserInfoChanged(const IDPLAYER) {
	return dllfunc(DLLFunc_ClientUserInfoChanged, IDPLAYER);
}
	
stock DF_UpdateClientData(const ENTITY, sendweapons, const cd/* = 0*/) {
	return dllfunc(DLLFunc_UpdateClientData, ENTITY, sendweapons, cd);
}
stock DF_AddToFullPack(const STATE/* = 0*/, e, ENT, HOST, hostflags, player, set) {
	return dllfunc(DLLFunc_AddToFullPack, STATE, e, ENT, HOST, hostflags, player, set);
}
stock DF_CmdStart(const PLAYER, const CMD/* = 0*/, randomSeed) {
	return dllfunc(DLLFunc_CmdStart, PLAYER, CMD, randomSeed);
}
stock DF_CmdEnd(const PLAYER) {
	return dllfunc(DLLFunc_CmdEnd, PLAYER);
}
stock DF_CreateBaseline(PLAYER, eIndex, baseline, playerModelIndex, Float:playerMins[3], Float:playerMaxs[3]) {
	return dllfunc(DLLFunc_CreateBaseline, PLAYER, eIndex, baseline, playerModelIndex, playerMins, playerMaxs);
}
