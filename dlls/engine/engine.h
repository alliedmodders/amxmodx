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

#define CHECK_ENTITY(x) if (x != 0 && (FNullEnt(INDEXENT(x)) || x < 0 || x > gpGlobals->maxEntities)) { MF_RaiseAmxError(amx,AMX_ERR_NATIVE); return 0; }

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

enum {
	EngFunc_PrecacheModel,	// int  )			(char* s);
	EngFunc_PrecacheSound,	// int  )			(char* s);
	EngFunc_SetModel,		// void )				(edict_t *e, const char *m);
	EngFunc_ModelIndex,		// int  )			(const char *m);
	EngFunc_ModelFrames,	// int	)			(int modelIndex);
	EngFunc_SetSize,		// void )				(edict_t *e, const float *rgflMin, const float *rgflMax);
	EngFunc_ChangeLevel,			// void )			(char* s1, char* s2);
	EngFunc_VecToYaw,			// float)				(const float *rgflVector);
	EngFunc_VecToAngles,			// void )			(const float *rgflVectorIn, float *rgflVectorOut);
	EngFunc_MoveToOrigin,		// void )			(edict_t *ent, const float *pflGoal, float dist, int iMoveType);
	EngFunc_ChangeYaw,			// void )				(edict_t* ent);
	EngFunc_ChangePitch,			// void )			(edict_t* ent);
	EngFunc_FindEntityByString,	// edict)	(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue);
	EngFunc_GetEntityIllum,		// int	)		(edict_t* pEnt);
	EngFunc_FindEntityInSphere,	// edict)	(edict_t *pEdictStartSearchAfter, const float *org, float rad);
	EngFunc_FindClientInPVS,		// edict)		(edict_t *pEdict);
	EngFunc_EntitiesInPVS,		// edict)			(edict_t *pplayer);
	EngFunc_MakeVectors,			// void )			(const float *rgflVector);
	EngFunc_AngleVectors,		// void )			(const float *rgflVector, float *forward, float *right, float *up);
	EngFunc_CreateEntity,		// edict)			(void);
	EngFunc_RemoveEntity,		// void )			(edict_t* e);
	EngFunc_CreateNamedEntity,	// edict)		(int className);
	EngFunc_MakeStatic,			// void )			(edict_t *ent);
	EngFunc_EntIsOnFloor,		// int  )			(edict_t *e);
	EngFunc_DropToFloor,			// int  )			(edict_t* e);
	EngFunc_WalkMove,			// int  )				(edict_t *ent, float yaw, float dist, int iMode);
	EngFunc_SetOrigin,			// void )				(edict_t *e, const float *rgflOrigin);
	EngFunc_EmitSound,			// void )				(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch);
	EngFunc_EmitAmbientSound,	// void )		(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch);
	EngFunc_TraceLine,			// void )				(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_TraceToss,			// void )				(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr);
	EngFunc_TraceMonsterHull,	// int  )		(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_TraceHull,			// void )				(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_TraceModel,			// void )			(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
	EngFunc_TraceTexture,		// const char *)			(edict_t *pTextureEntity, const float *v1, const float *v2 );
	EngFunc_TraceSphere,			// void )			(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_GetAimVector,		// void )			(edict_t* ent, float speed, float *rgflReturn);
	EngFunc_ParticleEffect,		// void )		(const float *org, const float *dir, float color, float count);
	EngFunc_LightStyle,			// void )			(int style, char* val);
	EngFunc_DecalIndex,			// int  )			(const char *name);
	EngFunc_PointContents,		// int )			(const float *rgflVector);
	EngFunc_FreeEntPrivateData,	// void )	(edict_t *pEdict);
	EngFunc_SzFromIndex,			// const char * )			(int iString);
	EngFunc_AllocString,			// int )			(const char *szValue);
	EngFunc_RegUserMsg,			// int	)			(const char *pszName, int iSize);
	EngFunc_AnimationAutomove,	// void )		(const edict_t* pEdict, float flTime);
	EngFunc_GetBonePosition,		// void )		(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles );
	EngFunc_GetAttachment,		// void	)			(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );
	EngFunc_SetView,				// void )				(const edict_t *pClient, const edict_t *pViewent );
	EngFunc_Time,				// float)					( void );
	EngFunc_CrosshairAngle,		// void )		(const edict_t *pClient, float pitch, float yaw);
	EngFunc_FadeClientVolume,	// void )      (const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
	EngFunc_SetClientMaxspeed,	// void )     (const edict_t *pEdict, float fNewMaxspeed);
	EngFunc_CreateFakeClient,	// edict)		(const char *netname);	// returns NULL if fake client can't be created
	EngFunc_RunPlayerMove,		// void )			(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec );
	EngFunc_NumberOfEntities,	// int  )		(void);
	EngFunc_StaticDecal,			// void )			( const float *origin, int decalIndex, int entityIndex, int modelIndex );
	EngFunc_PrecacheGeneric,		// int  )		(char* s);
	EngFunc_BuildSoundMsg,		// void )			(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
	EngFunc_GetPhysicsKeyValue,	// const char* )	( const edict_t *pClient, const char *key );
	EngFunc_SetPhysicsKeyValue,	// void )	( const edict_t *pClient, const char *key, const char *value );
	EngFunc_GetPhysicsInfoString,// const char* )	( const edict_t *pClient );
	EngFunc_PrecacheEvent,		// unsigned short )		( int type, const char*psz );
	EngFunc_PlaybackEvent,		// void )			( int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2 );
	EngFunc_CheckVisibility,			//)		( const edict_t *entity, unsigned char *pset );
	EngFunc_GetCurrentPlayer,			//)		( void );
	EngFunc_CanSkipPlayer,			//)			( const edict_t *player );
	EngFunc_SetGroupMask,				//)			( int mask, int op );
	EngFunc_GetClientListening,	// bool (int iReceiver, int iSender)
	EngFunc_SetClientListening,	// bool (int iReceiver, int iSender, bool Listen)
	EngFunc_MessageBegin,	// void (int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
	EngFunc_WriteCoord,		// void (float)
	EngFunc_WriteAngle,		// void (float)
	EngFunc_InfoKeyValue,	// char*	)			(char *infobuffer, char *key);
	EngFunc_SetKeyValue,	// void )			(char *infobuffer, char *key, char *value);
	EngFunc_SetClientKeyValue	 // void )		(int clientIndex, char *infobuffer, char *key, char *value);
};
enum
{
	DLLFunc_GameInit,	// void)			( void );				
	DLLFunc_Spawn,	// int )				( edict_t *pent );
	DLLFunc_Think,	// void )				( edict_t *pent );
	DLLFunc_Use,	// void )				( edict_t *pentUsed, edict_t *pentOther );
	DLLFunc_Touch,	// void )				( edict_t *pentTouched, edict_t *pentOther );
	DLLFunc_Blocked,	// void )			( edict_t *pentBlocked, edict_t *pentOther );
	DLLFunc_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd );
	DLLFunc_SetAbsBox,			// void )			( edict_t *pent );
	DLLFunc_ClientConnect,		// bool)		( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] );
	
	DLLFunc_ClientDisconnect,	// void )	( edict_t *pEntity );
	DLLFunc_ClientKill,		// void )		( edict_t *pEntity );
	DLLFunc_ClientPutInServer,	// void )	( edict_t *pEntity );
	DLLFunc_ClientCommand,		// void )		( edict_t *pEntity );

	DLLFunc_ServerDeactivate,	// void)	( void );

	DLLFunc_PlayerPreThink,		// void )	( edict_t *pEntity );
	DLLFunc_PlayerPostThink,		// void )	( edict_t *pEntity );

	DLLFunc_StartFrame,		// void )		( void );
	DLLFunc_ParmsNewLevel,		// void )		( void );
	DLLFunc_ParmsChangeLevel,	// void )	( void );

	 // Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	DLLFunc_GetGameDescription,	 // const char * )( void );     

	// Spectator funcs
	DLLFunc_SpectatorConnect,	// void)		( edict_t *pEntity );
	DLLFunc_SpectatorDisconnect,	// void )	( edict_t *pEntity );
	DLLFunc_SpectatorThink,		// void )		( edict_t *pEntity );

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	DLLFunc_Sys_Error,		// void )			( const char *error_string );

	DLLFunc_PM_FindTextureType,	// char )( char *name );
	DLLFunc_RegisterEncoders,	// void )	( void );

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	DLLFunc_GetHullBounds,	// int)	( int hullnumber, float *mins, float *maxs );

	// Create baselines for certain "unplaced" items.
	DLLFunc_CreateInstancedBaselines,	// void ) ( void );
	DLLFunc_pfnAllowLagCompensation,	// int )( void );
	// I know this does not fit with DLLFUNC(), but I dont want another native just for it.
	MetaFunc_CallGameEntity	// bool	(plid_t plid, const char *entStr,entvars_t *pev);
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

class Impulse
{
public:
	int Forward;
	int Check;
};

class Touch
{
public:
	int Forward;
	char *Toucher;
	char *Touched;
	~Touch()
	{
		if (Toucher) {
			delete [] Toucher;
			Toucher = 0;
		}
		if (Touched) {
			delete [] Touched;
			Touched = 0;
		}
	}
};

class EntClass
{
public:
	int Forward;
	char *Class;
	~EntClass()
	{
		if (Class) {
			delete [] Class;
			Class = 0;
		}
	}
};

int is_ent_valid(int iEnt);
int AmxStringToEngine(AMX *amx, cell param, int &len);
edict_t *UTIL_FindEntityInSphere(edict_t *pStart, const Vector &vecCenter, float flRadius);

extern bool g_inKeyValue;
extern KeyValueData *g_pkvd;
extern bool incmd;
extern struct usercmd_s *g_cmd;
extern struct PlayerInfo plinfo[33];
extern struct GlobalInfo glinfo;
extern AMX_NATIVE_INFO engine_Natives[];
extern std::vector<Impulse *> Impulses;
extern std::vector<EntClass *> Thinks;
extern std::vector<EntClass *> Uses;
extern std::vector<Touch *> Touches;

#endif //_ENGINE_INCLUDE_H