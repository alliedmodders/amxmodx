#ifndef _ENGINE_INCLUDE_H
#define _ENGINE_INCLUDE_H

#include <vector>
#include <string>
#include <extdll.h>	
#include <string.h> 
#include <meta_api.h>
#include <sdk_util.h>
#ifndef CBASEPLAYER_H
#define CBASEPLAYER_H
#include <cbase.h>
#include <player.h>
#endif
#include "amxxmodule.h"
#include <usercmd.h>
#include "messages.h"
#include "entity.h"
#include "gpglobals.h"

extern int SpawnForward;
extern int ChangelevelForward;
extern int PlaybackForward;
extern int DispatchKeyForward;
extern int pfnTouchForward;
extern int pfnThinkForward;
extern int PlayerPreThinkForward;
extern int PlayerPostThinkForward;
extern int ClientKillForward;
extern int CmdStartForward;
extern int StartFrameForward;
extern int DispatchUseForward;

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

#define ANGLEVECTORS_FORWARD	1
#define ANGLEVECTORS_RIGHT		2
#define ANGLEVECTORS_UP			3

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

//These two structs are relics from VexD
struct PlayerInfo {
	int iSpeakFlags;
	edict_t *pViewEnt;
	int iViewType;
	int iRenderMode;
	float fRenderAmt;
};

struct GlobalInfo {
	bool bLights;
	float fNextLights;
	char *szLastLights[128];
	char *szRealLights[128];
	bool bCheckLights;
};

int is_ent_valid(int iEnt);
int AmxStringToEngine(AMX *amx, cell param, int &len);
edict_t *UTIL_FindEntityInSphere(edict_t *pStart, const Vector &vecCenter, float flRadius);

extern bool inKeyValue;
extern KeyValueData *g_pkvd;
extern bool incmd;
extern struct usercmd_s *g_cmd;
extern struct PlayerInfo plinfo[33];
extern struct GlobalInfo glinfo;
extern AMX_NATIVE_INFO engine_Natives[];

#endif //_ENGINE_INCLUDE_H