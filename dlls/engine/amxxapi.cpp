#include "engine.h"

int AmxStringToEngine(AMX *amx, cell param, int &len)
{
	char *szString = MF_GetAmxString(amx, param, 0, &len);
	char *szCopy = new char[len+1];

	memset(szCopy, 0, len+1);
	strcpy(szCopy, szString);

	return MAKE_STRING(szCopy);
}

void OnAmxxAttach()
{
	pfnTouchForward = 0;
	pfnThinkForward = 0;
	PlayerPreThinkForward = 0;
	PlayerPostThinkForward = 0;
	ClientKillForward = 0;
	CmdStartForward = 0;
	StartFrameForward = 0;
	MF_AddNatives(msg_Natives);
	MF_AddNatives(ent_Natives);
	MF_AddNatives(engine_Natives);
	MF_AddNatives(global_Natives);
	memset(glinfo.szLastLights, 0x0, 128);
	memset(glinfo.szRealLights, 0x0, 128);
	glinfo.bLights = false;
	glinfo.fNextLights = 0;
	glinfo.bCheckLights = false;
}

void OnPluginsLoaded()
{
	pfnThinkForward = MF_RegisterForward("pfn_think", ET_STOP, FP_CELL, FP_DONE);
	PlayerPreThinkForward = MF_RegisterForward("client_PreThink", ET_STOP, FP_CELL, FP_DONE);
	PlayerPostThinkForward = MF_RegisterForward("client_PostThink", ET_STOP, FP_CELL, FP_DONE);
	ClientKillForward = MF_RegisterForward("client_kill", ET_STOP, FP_CELL, FP_DONE);
	CmdStartForward = MF_RegisterForward("client_impulse", ET_STOP, FP_CELL, FP_CELL, FP_DONE);
	StartFrameForward = MF_RegisterForward("ServerFrame", ET_IGNORE, FP_DONE);
//	DispatchKeyForward = MF_RegisterForward("Dispatch_KeyVal", ET_STOP, FP_CELL, FP_DONE);
	PlaybackForward = MF_RegisterForward("PlaybackEvent", ET_STOP, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	ChangelevelForward = MF_RegisterForward("server_changelevel", ET_STOP, FP_STRING, FP_DONE);
	SpawnForward = MF_RegisterForward("pfn_spawn", ET_IGNORE, FP_CELL, FP_DONE);
	DispatchUseForward = MF_RegisterForward("pfn_use", ET_STOP, FP_CELL, FP_CELL, FP_DONE);
	pfnTouchForward = MF_RegisterForward("pfn_touch", ET_STOP, FP_CELL, FP_CELL, FP_DONE);
}

qboolean Voice_SetClientListening(int iReceiver, int iSender, qboolean bListen)
{
	if((plinfo[iSender].iSpeakFlags & SPEAK_MUTED) != 0) {
		(g_engfuncs.pfnVoice_SetClientListening)(iReceiver, iSender, false);
		RETURN_META_VALUE(MRES_SUPERCEDE, false);
	}

	if((plinfo[iSender].iSpeakFlags & SPEAK_ALL) != 0) {
		(g_engfuncs.pfnVoice_SetClientListening)(iReceiver, iSender, true);
		RETURN_META_VALUE(MRES_SUPERCEDE, true);
	}

	if((plinfo[iReceiver].iSpeakFlags & SPEAK_LISTENALL) != 0) {
		(g_engfuncs.pfnVoice_SetClientListening)(iReceiver, iSender, true);
		RETURN_META_VALUE(MRES_SUPERCEDE, true);
	}

	RETURN_META_VALUE(MRES_IGNORED, bListen);
}

int AddToFullPack(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet)
{
	if(ent == host) {
		if(FStrEq(STRING(ent->v.classname), "player")) {
			if(plinfo[ENTINDEX(ent)].iViewType != CAMERA_NONE) {
				ent->v.rendermode = kRenderTransTexture;
				ent->v.renderamt = 100;
				RETURN_META_VALUE(MRES_IGNORED, 0);
			}
		}
	}

	if(FStrEq(STRING(ent->v.classname), "player")) {
		if(plinfo[ENTINDEX(ent)].iViewType != CAMERA_NONE) {
			ent->v.rendermode = plinfo[ENTINDEX(ent)].iRenderMode;
			ent->v.renderamt = plinfo[ENTINDEX(ent)].fRenderAmt;
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ClientDisconnect(edict_t *pEntity)
{
	int id = ENTINDEX(pEntity);

	plinfo[id].iSpeakFlags = SPEAK_NORMAL;
	plinfo[id].iViewType = CAMERA_NONE;
	plinfo[id].iRenderMode = 0;
	plinfo[id].fRenderAmt = 0;

	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	int id = ENTINDEX(pEntity);

	plinfo[id].iSpeakFlags = SPEAK_NORMAL;
	plinfo[id].iViewType = CAMERA_NONE;
	plinfo[id].pViewEnt = NULL;
	plinfo[id].iRenderMode = 0;
	plinfo[id].fRenderAmt = 0;

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ServerDeactivate()
{
	memset(glinfo.szLastLights, 0x0, 128);
	memset(glinfo.szRealLights, 0x0, 128);
	glinfo.bLights = false;
	glinfo.fNextLights = 0;

	RETURN_META(MRES_IGNORED);

}

void LightStyle(int style, char *val) {
	if (!style) {
		memset(glinfo.szRealLights, 0x0, 128);
		memcpy(glinfo.szRealLights, val, strlen(val));
	}

	RETURN_META(MRES_IGNORED);
}
