#include "engine.h"

BOOL CheckForPublic(const char *publicname);

edict_t *g_player_edicts[33];

int AmxStringToEngine(AMX *amx, cell param, int &len)
{
	char *szString = MF_GetAmxString(amx, param, 0, &len);

	return ALLOC_STRING(szString);
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
	g_CameraCount=0;
	pfnThinkForward = MF_RegisterForward("pfn_think", ET_STOP, FP_CELL, FP_DONE);  // done
	PlayerPreThinkForward = MF_RegisterForward("client_PreThink", ET_STOP, FP_CELL, FP_DONE); // done
	PlayerPostThinkForward = MF_RegisterForward("client_PostThink", ET_STOP, FP_CELL, FP_DONE); // done
	ClientKillForward = MF_RegisterForward("client_kill", ET_STOP, FP_CELL, FP_DONE); // done
	CmdStartForward = MF_RegisterForward("client_impulse", ET_STOP, FP_CELL, FP_CELL, FP_DONE); // done
	StartFrameForward = MF_RegisterForward("server_frame", ET_IGNORE, FP_DONE); // done
	DispatchKeyForward = MF_RegisterForward("pfn_keyvalue", ET_STOP, FP_CELL, FP_DONE); // done
	PlaybackForward = MF_RegisterForward("pfn_playbackevent", ET_STOP, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE); // done
	SpawnForward = MF_RegisterForward("pfn_spawn", ET_IGNORE, FP_CELL, FP_DONE); // done
	pfnTouchForward = MF_RegisterForward("pfn_touch", ET_STOP, FP_CELL, FP_CELL, FP_DONE);  // done
	VexdTouchForward = MF_RegisterForward("vexd_pfntouch", ET_IGNORE, FP_CELL, FP_CELL, FP_DONE); // done
	VexdServerForward = MF_RegisterForward("ServerFrame", ET_IGNORE, FP_DONE); // done
	// Reset all standard engine callbacks

	// These will be reset through native calls, if need be

	g_pFunctionTable->pfnAddToFullPack=NULL;

	g_pFunctionTable->pfnKeyValue=NULL;
	if (CheckForPublic("pfn_keyvalue"))
		g_pFunctionTable->pfnKeyValue=KeyValue;

	g_pengfuncsTable->pfnPlaybackEvent=NULL; // "pfn_playbackevent"
	if (CheckForPublic("pfn_playbackevent"))
		g_pengfuncsTable->pfnPlaybackEvent=PlaybackEvent;

	g_pFunctionTable->pfnPlayerPreThink=NULL; // "client_PreThink"
	if (CheckForPublic("client_PreThink"))
		g_pFunctionTable->pfnPlayerPreThink=PlayerPreThink;

	g_pFunctionTable->pfnPlayerPostThink=NULL; // "client_PostThink"
	if (CheckForPublic("client_PostThink"))
		g_pFunctionTable->pfnPlayerPostThink=PlayerPostThink_Post;

	g_pFunctionTable->pfnSpawn=NULL; // "pfn_spawn"
	if (CheckForPublic("pfn_spawn"))
		g_pFunctionTable->pfnSpawn=Spawn;

	g_pFunctionTable->pfnClientKill=NULL; // "client_kill"
	if (CheckForPublic("client_kill"))
		g_pFunctionTable->pfnClientKill=ClientKill;

	g_pFunctionTable->pfnCmdStart=NULL; // "client_impulse","register_impulse"
	if (CheckForPublic("client_impulse"))
		g_pFunctionTable->pfnCmdStart=CmdStart;
	
	g_pFunctionTable->pfnThink=NULL; // "pfn_think", "register_think"
	if (CheckForPublic("pfn_think"))
		g_pFunctionTable->pfnThink=Think;

	g_pFunctionTable->pfnStartFrame=NULL; // "server_frame","ServerFrame"
	if (CheckForPublic("server_frame"))
		g_pFunctionTable->pfnStartFrame=StartFrame;

	if (CheckForPublic("ServerFrame"))
		g_pFunctionTable->pfnStartFrame=StartFrame;


	g_pFunctionTable->pfnTouch=NULL; // "pfn_touch","vexd_pfntouch"
	if (CheckForPublic("pfn_touch"))
		g_pFunctionTable->pfnTouch=pfnTouch;

	if (CheckForPublic("vexd_pfntouch"))
		g_pFunctionTable->pfnTouch=pfnTouch;
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

	if (plinfo[ENTINDEX(pEntity)].iViewType != CAMERA_NONE) // Verify that they were originally in a modified view
	{
		g_CameraCount--;
		if (g_CameraCount < 0)
			g_CameraCount=0;
		if (g_CameraCount==0) // Reset the AddToFullPack pointer if there's no more cameras in use...
			g_pFunctionTable->pfnAddToFullPack=NULL;
	}

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
	Msg.clear();
	register int i = 0;
	for (i=0; i<256; i++) {
		msgHooks[i] = 0;
		msgBlocks[i] = 0;
	}

	Touches.clear();
	Impulses.clear();
	Thinks.clear();
	
	// Reset all forwarding function tables (so that forwards won't be called before plugins are initialized)
	g_pFunctionTable->pfnAddToFullPack=NULL;
	g_pFunctionTable->pfnKeyValue=NULL;
	g_pengfuncsTable->pfnPlaybackEvent=NULL; // "pfn_playbackevent"
	g_pFunctionTable->pfnPlayerPreThink=NULL; // "client_PreThink"
	g_pFunctionTable->pfnPlayerPostThink=NULL; // "client_PostThink"
	g_pFunctionTable->pfnSpawn=NULL; // "pfn_spawn"
	g_pFunctionTable->pfnClientKill=NULL; // "client_kill"
	g_pFunctionTable->pfnCmdStart=NULL; // "client_impulse","register_impulse"
	g_pFunctionTable->pfnThink=NULL; // "pfn_think", "register_think"
	g_pFunctionTable->pfnStartFrame=NULL; // "server_frame","ServerFrame"
	g_pFunctionTable->pfnTouch=NULL; // "pfn_touch","vexd_pfntouch"

	RETURN_META(MRES_IGNORED);
}

void ServerActivate(edict_t *pEdictList, int edictCount, int clientMax)
{
	for(int f = 1; f <= gpGlobals->maxClients;f++) 
		g_player_edicts[f]=pEdictList + f;
	Msg.clear();
	register int i = 0;
	for (i=0; i<256; i++) {
		msgHooks[i] = 0;
		msgBlocks[i] = 0;
	}

	RETURN_META(MRES_IGNORED);
}

void LightStyle(int style, char *val) {
	if (!style) {
		memset(glinfo.szRealLights, 0x0, 128);
		memcpy(glinfo.szRealLights, val, strlen(val));
	}

	RETURN_META(MRES_IGNORED);
}

BOOL CheckForPublic(const char *publicname)
{
	AMX* amx;
	char blah[64];
	strncpy(blah,publicname,63);
	int iFunctionIndex;
	int i=0;
	// Loop through all running scripts
	while((amx=MF_GetScriptAmx(i++))!=NULL)
	{ 
		// Scan for public
		if (MF_AmxFindPublic(amx, blah, &iFunctionIndex) == AMX_ERR_NONE)
		{
			// Public was found.
			return TRUE;
		}
	}

	return FALSE; // no public found in any loaded script
}
