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

#ifndef _ENGINE_INCLUDE_H
#define _ENGINE_INCLUDE_H

#include "amxxmodule.h"
#include <extdll.h>	
#include <string.h> 
#include <meta_api.h>
#include <sdk_util.h>
#include <usercmd.h>
#include "entity.h"
#include "gpglobals.h"
#include "entity_state.h"
#include <amtl/am-vector.h>
#include <amtl/am-string.h>
#include <CDetour/detours.h>
#include <HLTypeConversion.h>

extern DLL_FUNCTIONS *g_pFunctionTable;
extern DLL_FUNCTIONS *g_pFunctionTable_Post;
extern enginefuncs_t *g_pengfuncsTable;
extern enginefuncs_t *g_pengfuncsTable_Post;

extern int SpawnForward;
extern int ChangelevelForward;
extern int PlaybackForward;
extern int DispatchKeyForward;
extern int pfnTouchForward;
extern int pfnThinkForward;
extern int PlayerPreThinkForward;
extern int PlayerPostThinkForward;
extern int ClientKillForward;
extern int ClientImpulseForward;
extern int CmdStartForward;
extern int StartFrameForward;
extern int DispatchUseForward;
extern int VexdTouchForward;
extern int VexdServerForward;

extern CDetour *LightStyleDetour;
extern HLTypeConversion TypeConversion;

#define AMS_OFFSET 0.01

#define GETINFOKEYBUFFER	(*g_engfuncs.pfnGetInfoKeyBuffer)
#define INFO_KEY_BUFFER		(*g_engfuncs.pfnGetInfoKeyBuffer)
#define INFO_KEY_VALUE		(*g_engfuncs.pfnInfoKeyValue)

#define SPEAK_NORMAL 0
#define SPEAK_MUTED 1
#define SPEAK_ALL 2
#define SPEAK_LISTENALL 4

#define CAMERA_NONE 0
#define CAMERA_3RDPERSON 1
#define CAMERA_UPLEFT 2
#define CAMERA_TOPDOWN 3

enum
{
	usercmd_float_start,
	usercmd_forwardmove,		// Float
	usercmd_sidemove,			// Float
	usercmd_upmove,				// Float
	usercmd_float_end,
	usercmd_int_start,
	usercmd_lerp_msec,			// short
	usercmd_msec,				// byte
	usercmd_lightlevel,			// byte
	usercmd_buttons,			// unsigned short
	usercmd_impulse,			// byte
	usercmd_weaponselect,		// byte

	usercmd_impact_index,		// in
	usercmd_int_end,
	usercmd_vec_start,
	usercmd_viewangles,			// Vector
	usercmd_impact_position,	// vec
	usercmd_vec_end

};

// Used by the traceresult() native.
enum
{
	TR_AllSolid,			// (int) if true, plane is not valid
	TR_StartSolid,		// (int) if true, the initial point was in a solid area
	TR_InOpen,		// (int)
	TR_InWater,	// (int)
	TR_Fraction,			// (float) time completed, 1.0 = didn't hit anything
	TR_EndPos,			// (vector) final position
	TR_PlaneDist,		// (float)
	TR_PlaneNormal,		// (vector) surface normal at impact
	TR_Hit,				// (entity) entity the surface is on
	TR_Hitgroup			// (int) 0 == generic, non zero is specific body part
};
enum {
	Meta_GetUserMsgID,		// int )		(plid_t plid, const char *msgname, int *size);
	Meta_GetUserMsgName,	// const char *)	(plid_t plid, int msgid, int *size);
	Meta_GetPluginPath,		// const char *)		(plid_t plid);
	Meta_GetGameInfo		// const char *)		(plid_t plid, ginfo_t tag);
};

//These two structs are relics from VexD
struct PlayerInfo {
	int iSpeakFlags;
	edict_t *pViewEnt;
	int iViewType;
	//int iRenderMode;
	//float fRenderAmt;
};

struct GlobalInfo {
	char szLastLights[128];
	char szRealLights[128];
	bool bCheckLights;
};

class Impulse
{
public:
	~Impulse()
	{
		if (Forward != -1)
			MF_UnregisterSPForward(Forward);
	}
	int Forward;
	int Check;
};

class Touch
{
public:
	int Forward;
	ke::AString Toucher;
	ke::AString Touched;
	~Touch()
	{
		if (Forward != -1)
			MF_UnregisterSPForward(Forward);
	}
};

class EntClass
{
public:
	int Forward;
	ke::AString Class;
	~EntClass()
	{
		if (Forward != -1)
			MF_UnregisterSPForward(Forward);
	}
};

int is_ent_valid(int iEnt);
int AmxStringToEngine(AMX *amx, cell param, int &len);
edict_t *UTIL_FindEntityInSphere(edict_t *pStart, const Vector &vecCenter, float flRadius);

extern int g_CameraCount;

int Spawn(edict_t *pEntity);
void PlaybackEvent(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2);
void KeyValue(edict_t *pEntity, KeyValueData *pkvd);
void StartFrame();
void CmdStart(const edict_t *player, const struct usercmd_s *_cmd, unsigned int random_seed);
void ClientKill(edict_t *pEntity);
void PlayerPreThink(edict_t *pEntity);
void PlayerPostThink_Post(edict_t *pEntity);
void pfnTouch(edict_t *pToucher, edict_t *pTouched);
void Think(edict_t *pent);
void StartFrame_Post();

#define CHECK_ENTITY_SIMPLE(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x != 0 && FNullEnt(TypeConversion.id_to_edict(x))) { \
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
			return 0; \
		} \
	}

#define CHECK_ENTITY(x) \
	if (x < 0 || x > gpGlobals->maxEntities) { \
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", x); \
		return 0; \
	} else { \
		if (x > 0 && x <= gpGlobals->maxClients) { \
			if (!MF_IsPlayerIngame(x)) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", x); \
				return 0; \
			} \
		} else { \
			if (x != 0 && FNullEnt(TypeConversion.id_to_edict(x))) { \
				MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", x); \
				return 0; \
			} \
		} \
	}

extern bool g_inKeyValue;
extern KeyValueData *g_pkvd;
extern bool incmd;
extern struct usercmd_s *g_cmd;
extern struct PlayerInfo plinfo[33];
extern struct GlobalInfo glinfo;
extern AMX_NATIVE_INFO engine_Natives[];
extern AMX_NATIVE_INFO engine_NewNatives[];
extern ke::Vector<Impulse *> Impulses;
extern ke::Vector<EntClass *> Thinks;
extern ke::Vector<Touch *> Touches;

#endif //_ENGINE_INCLUDE_H

