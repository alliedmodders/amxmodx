// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Engine Module
//

#include "engine.h"

BOOL CheckForPublic(const char *publicname);
void CreateDetours();
void DestroyDetours();

CDetour *LightStyleDetour = NULL;
HLTypeConversion TypeConversion;

int AmxStringToEngine(AMX *amx, cell param, int &len)
{
	char *szString = MF_GetAmxString(amx, param, 0, &len);

	return ALLOC_STRING(szString);
}

void ClearHooks()
{
	size_t i;

	for (i=0; i<Touches.length(); i++)
		delete Touches[i];
	for (i=0; i<Impulses.length(); i++)
		delete Impulses[i];
	for (i=0; i<Thinks.length(); i++)
		delete Thinks[i];

	Touches.clear();
	Impulses.clear();
	Thinks.clear();
}

void OnAmxxAttach()
{
	pfnTouchForward = 0;
	pfnThinkForward = 0;
	PlayerPreThinkForward = 0;
	PlayerPostThinkForward = 0;
	ClientKillForward = 0;
	ClientImpulseForward = 0;
	CmdStartForward = 0;
	StartFrameForward = 0;
	MF_AddNatives(ent_Natives);
	MF_AddNewNatives(ent_NewNatives);
	MF_AddNatives(engine_Natives);
	MF_AddNewNatives(engine_NewNatives);
	MF_AddNatives(global_Natives);
	memset(glinfo.szLastLights, 0x0, 128);
	memset(glinfo.szRealLights, 0x0, 128);
	glinfo.bCheckLights = false;

	CreateDetours();
}

void OnAmxxDetach()
{
	DestroyDetours();
}

void OnPluginsLoaded()
{
	TypeConversion.init();

	g_CameraCount=0;
	pfnThinkForward = MF_RegisterForward("pfn_think", ET_STOP, FP_CELL, FP_DONE);  // done
	PlayerPreThinkForward = MF_RegisterForward("client_PreThink", ET_STOP, FP_CELL, FP_DONE); // done
	PlayerPostThinkForward = MF_RegisterForward("client_PostThink", ET_STOP, FP_CELL, FP_DONE); // done
	ClientKillForward = MF_RegisterForward("client_kill", ET_STOP, FP_CELL, FP_DONE); // done
	ClientImpulseForward = MF_RegisterForward("client_impulse", ET_STOP, FP_CELL, FP_CELL, FP_DONE); // done
	CmdStartForward = MF_RegisterForward("client_cmdStart", ET_STOP, FP_CELL, FP_DONE); // done
	StartFrameForward = MF_RegisterForward("server_frame", ET_IGNORE, FP_DONE); // done
	DispatchKeyForward = MF_RegisterForward("pfn_keyvalue", ET_STOP, FP_CELL, FP_DONE); // done
	PlaybackForward = MF_RegisterForward("pfn_playbackevent", ET_STOP, FP_CELL, FP_CELL, FP_CELL, FP_FLOAT, FP_ARRAY, FP_ARRAY, FP_FLOAT, FP_FLOAT, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE); // done
	SpawnForward = MF_RegisterForward("pfn_spawn", ET_STOP, FP_CELL, FP_DONE); // done
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

	g_pFunctionTable_Post->pfnPlayerPostThink=NULL; // "client_PostThink"
	if (CheckForPublic("client_PostThink"))
		g_pFunctionTable->pfnPlayerPostThink=PlayerPostThink_Post;

	g_pFunctionTable->pfnSpawn=NULL; // "pfn_spawn"
	//if (CheckForPublic("pfn_spawn")) // JGHG: I commented this if out because we always need the Spawn to precache the rocket mdl used with SetView native
	g_pFunctionTable->pfnSpawn=Spawn;

	g_pFunctionTable->pfnClientKill=NULL; // "client_kill"
	if (CheckForPublic("client_kill"))
		g_pFunctionTable->pfnClientKill=ClientKill;

	g_pFunctionTable->pfnCmdStart=NULL; // "client_impulse","register_impulse","client_cmdStart"
	if (CheckForPublic("client_impulse") || CheckForPublic("client_cmdStart"))
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

int AddToFullPack_Post(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet)
{
	if( player && ent == host && plinfo[ENTINDEX(ent)].iViewType != CAMERA_NONE )
	{
		state->rendermode = kRenderTransTexture;
		state->renderamt = 100;
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ClientDisconnect(edict_t *pEntity)
{
	int id = ENTINDEX(pEntity);

	if (plinfo[id].iViewType != CAMERA_NONE) // Verify that they were originally in a modified view
	{
		g_CameraCount--;
		if (g_CameraCount < 0)
			g_CameraCount=0;
		if (g_CameraCount==0) // Reset the AddToFullPack pointer if there's no more cameras in use...
			g_pFunctionTable->pfnAddToFullPack=NULL;
	}

	plinfo[id].iSpeakFlags = SPEAK_NORMAL;
	plinfo[id].iViewType = CAMERA_NONE;

	RETURN_META(MRES_IGNORED);
}

BOOL ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	int id = ENTINDEX(pEntity);

	plinfo[id].iSpeakFlags = SPEAK_NORMAL;
	plinfo[id].iViewType = CAMERA_NONE;
	plinfo[id].pViewEnt = NULL;

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void ServerDeactivate()
{
	memset(glinfo.szLastLights, 0x0, 128);
	memset(glinfo.szRealLights, 0x0, 128);
	glinfo.bCheckLights = false;
	
	// Reset all forwarding function tables (so that forwards won't be called before plugins are initialized)
	g_pFunctionTable->pfnAddToFullPack=NULL;
	g_pFunctionTable->pfnKeyValue=NULL;
	g_pengfuncsTable->pfnPlaybackEvent=NULL; // "pfn_playbackevent"
	g_pFunctionTable->pfnPlayerPreThink=NULL; // "client_PreThink"
	g_pFunctionTable_Post->pfnPlayerPostThink=NULL; // "client_PostThink"
	g_pFunctionTable->pfnSpawn=NULL; // "pfn_spawn"
	g_pFunctionTable->pfnClientKill=NULL; // "client_kill"
	g_pFunctionTable->pfnCmdStart=NULL; // "client_impulse","register_impulse"
	g_pFunctionTable->pfnThink=NULL; // "pfn_think", "register_think"
	g_pFunctionTable->pfnStartFrame=NULL; // "server_frame","ServerFrame"
	g_pFunctionTable->pfnTouch=NULL; // "pfn_touch","vexd_pfntouch"

	ClearHooks();

	RETURN_META(MRES_IGNORED);
}

DETOUR_DECL_STATIC2(LightStyle, void, int, style, const char *, val) // void (*pfnLightStyle) (int style, const char* val);
{
	DETOUR_STATIC_CALL(LightStyle)(style, val);

	if (!style && strcmp(val, glinfo.szRealLights)) {
		memset(glinfo.szRealLights, 0x0, 128);
		memcpy(glinfo.szRealLights, val, ke::Min(strlen(val), (size_t)127));
	}

	if (glinfo.bCheckLights && strcmp(val, glinfo.szLastLights))
		g_pFunctionTable_Post->pfnStartFrame = StartFrame_Post;
}

void StartFrame_Post()
{
	g_pFunctionTable_Post->pfnStartFrame = NULL;

	LightStyleDetour->DisableDetour();
	LIGHT_STYLE(0, glinfo.szLastLights);
	LightStyleDetour->EnableDetour();

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

void CreateDetours()
{
	LightStyleDetour = DETOUR_CREATE_STATIC_FIXED(LightStyle, (void*)(g_engfuncs.pfnLightStyle));
}

void DestroyDetours()
{
	LightStyleDetour->Destroy();
}