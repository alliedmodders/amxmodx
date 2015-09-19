// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Constants
//

#if defined _fakemeta_const_included
  #endinput
#endif
#define _fakemeta_const_included

// For forward_return
#define	FMV_STRING		1
#define FMV_FLOAT		2
#define FMV_CELL		3

#include <hlsdk_const>

/* The actual return value of the function, use these instead of PLUGIN_HANDLED etc when
 * returning from registered forwards.
 */
#define FMRES_IGNORED	1	// Calls target function, returns normal value
#define FMRES_HANDLED	2	// Tells metamod you did something, still calls target function and returns normal value
#define FMRES_OVERRIDE	3	// Supposed to still call the target function but return your value instead
							// however this does not work properly with metamod; use supercede instead.
#define FMRES_SUPERCEDE	4	// Block the target call, and use your return value (if applicable)

// Use this with GetInfoKeyBuffer if you want the server's localinfo buffer
#define FM_NULLENT		-1


/* Used with engfunc()
 */
enum {
	EngFunc_PrecacheModel,				// int  )			(char *s);
	EngFunc_PrecacheSound,				// int  )			(char *s);
	EngFunc_SetModel,					// void )			(edict_t *e, const char *m);
	EngFunc_ModelIndex,					// int  )			(const char *m);
	EngFunc_ModelFrames,				// int	)			(int modelIndex);
	EngFunc_SetSize,					// void )			(edict_t *e, const float *rgflMin, const float *rgflMax);
	EngFunc_ChangeLevel,				// void )			(char* s1, char* s2);
	EngFunc_VecToYaw,					// float)			(const float *rgflVector);
	EngFunc_VecToAngles,				// void )			(const float *rgflVectorIn, float *rgflVectorOut);
	EngFunc_MoveToOrigin,				// void )			(edict_t *ent, const float *pflGoal, float dist, int iMoveType);
	EngFunc_ChangeYaw,					// void )			(edict_t* ent);
	EngFunc_ChangePitch,				// void )			(edict_t* ent);
	EngFunc_FindEntityByString,			// edict)			(edict_t *pEdictStartSearchAfter, const char *pszField, const char *pszValue);
	EngFunc_GetEntityIllum,				// int	)			(edict_t* pEnt);
	EngFunc_FindEntityInSphere,			// edict)			(edict_t *pEdictStartSearchAfter, const float *org, float rad);
	EngFunc_FindClientInPVS,			// edict)			(edict_t *pEdict);
	EngFunc_EntitiesInPVS,				// edict)			(edict_t *pplayer);
	EngFunc_MakeVectors,				// void )			(const float *rgflVector);
	EngFunc_AngleVectors,				// void )			(const float *rgflVector, float *forward, float *right, float *up);
	EngFunc_CreateEntity,				// edict)			(void);
	EngFunc_RemoveEntity,				// void )			(edict_t *e);
	EngFunc_CreateNamedEntity,			// edict)			(int className);
	EngFunc_MakeStatic,					// void )			(edict_t *ent);
	EngFunc_EntIsOnFloor,				// int  )			(edict_t *e);
	EngFunc_DropToFloor,				// int  )			(edict_t *e);
	EngFunc_WalkMove,					// int  )			(edict_t *ent, float yaw, float dist, int iMode);
	EngFunc_SetOrigin,					// void )			(edict_t *e, const float *rgflOrigin);
	EngFunc_EmitSound,					// void )			(edict_t *entity, int channel, const char *sample, float volume, float attenuation, int fFlags, int pitch);
	EngFunc_EmitAmbientSound,			// void )			(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch);

	//With 1.71 you can pass an optional TraceLine ptr for trace natives
	// it can be 0, for meaning "global tr handle" (for get/set_tr2), or
	// it can be any other TR handle (such as one from a TR hook)
	EngFunc_TraceLine,					// void )			(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_TraceToss,					// void )			(edict_t *pent, edict_t *pentToIgnore, TraceResult *ptr);
	EngFunc_TraceMonsterHull,			// int  )			(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_TraceHull,					// void )			(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
	EngFunc_TraceModel,					// void )			(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
	EngFunc_TraceTexture,				// const char *)	(edict_t *pTextureEntity, const float *v1, const float *v2 );
	EngFunc_TraceSphere,				// void )			(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
	
	EngFunc_GetAimVector,				// void )			(edict_t *ent, float speed, float *rgflReturn);
	EngFunc_ParticleEffect,				// void )			(const float *org, const float *dir, float color, float count);
	EngFunc_LightStyle,					// void )			(int style, char *val);
	EngFunc_DecalIndex,					// int  )			(const char *name);
	EngFunc_PointContents,				// int  )			(const float *rgflVector);
	EngFunc_FreeEntPrivateData,			// void )			(edict_t *pEdict);
	EngFunc_SzFromIndex,				// const char *)	(int iString);
	EngFunc_AllocString,				// int  )			(const char *szValue);
	EngFunc_RegUserMsg,					// int	)			(const char *pszName, int iSize);
	EngFunc_AnimationAutomove,			// void )			(const edict_t *pEdict, float flTime);
	EngFunc_GetBonePosition,			// void )			(const edict_t *pEdict, int iBone, float *rgflOrigin, float *rgflAngles);
	EngFunc_GetAttachment,				// void	)			(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles);
	EngFunc_SetView,					// void )			(const edict_t *pClient, const edict_t *pViewent);
	EngFunc_Time,						// float)			( void );
	EngFunc_CrosshairAngle,				// void )			(const edict_t *pClient, float pitch, float yaw);
	EngFunc_FadeClientVolume,			// void )			(const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
	EngFunc_SetClientMaxspeed,			// void )			(const edict_t *pEdict, float fNewMaxspeed);
	EngFunc_CreateFakeClient,			// edict)			(const char *netname);	// returns NULL if fake client can't be created
	EngFunc_RunPlayerMove,				// void )			(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec);
	EngFunc_NumberOfEntities,			// int  )			( void );
	EngFunc_StaticDecal,				// void )			(const float *origin, int decalIndex, int entityIndex, int modelIndex);
	EngFunc_PrecacheGeneric,			// int  )			(char* s);
	EngFunc_BuildSoundMsg,				// void )			(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
	EngFunc_GetPhysicsKeyValue,			// const char *)	(const edict_t *pClient, const char *key);
	EngFunc_SetPhysicsKeyValue,			// void )			(const edict_t *pClient, const char *key, const char *value);
	EngFunc_GetPhysicsInfoString,		// const char *)	(const edict_t *pClient);
	EngFunc_PrecacheEvent,				// unsigned short)	(int type, const char*psz);
	EngFunc_PlaybackEvent,				// void )			(int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2);
	EngFunc_CheckVisibility,			// int  )			(const edict_t *entity, unsigned char *pset);
	EngFunc_GetCurrentPlayer,			// int  )			( void );
	EngFunc_CanSkipPlayer,				// int  )			(const edict_t *player);
	EngFunc_SetGroupMask,				// void )			(int mask, int op);
	EngFunc_GetClientListening,			// bool )			(int iReceiver, int iSender)
	EngFunc_SetClientListening,			// bool )			(int iReceiver, int iSender, bool Listen)
	EngFunc_MessageBegin,				// void )			(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
	EngFunc_WriteCoord,					// void )			(float flValue)
	EngFunc_WriteAngle,					// void )			(float flValue)
	EngFunc_InfoKeyValue,				// char*)			(char *infobuffer, char *key);
	EngFunc_SetKeyValue,				// void )			(char *infobuffer, char *key, char *value);
	EngFunc_SetClientKeyValue,			// void )			(int clientIndex, char *infobuffer, char *key, char *value);
	EngFunc_CreateInstBaseline,			// int  )			(int classname, struct entity_state_s *baseline);
	
	// Returns pointer to info buffer that can be used with the infobuffer param of InfoKeyValue, SetKeyValue, and SetClientKeyValue
	EngFunc_GetInfoKeyBuffer,			// char*)			(edict_t *e);
	EngFunc_AlertMessage,				// void )			(ALERT_TYPE atype, char *szFmt, ...);
	EngFunc_ClientPrintf,				// void )			(edict_t* pEdict, PRINT_TYPE ptype, const char *szMsg);
	EngFunc_ServerPrint					// void )			(const char *szMsg);
};

/* Used with dllfunc()
 */
enum
{
	DLLFunc_GameInit,					// void )			( void );				
	DLLFunc_Spawn,						// int  )			(edict_t *pent);
	DLLFunc_Think,						// void )			(edict_t *pent);
	DLLFunc_Use,						// void )			(edict_t *pentUsed, edict_t *pentOther);
	DLLFunc_Touch,						// void )			(edict_t *pentTouched, edict_t *pentOther);
	DLLFunc_Blocked,					// void )			(edict_t *pentBlocked, edict_t *pentOther);
	
	//You can pass in 0 for glb kvd handle or a kvd handle here
	DLLFunc_KeyValue,					// void )			(edict_t *pentKeyvalue, KeyValueData *pkvd);
	DLLFunc_SetAbsBox,					// void )			(edict_t *pent);
	DLLFunc_ClientConnect,				// bool )			(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128]);
	
	DLLFunc_ClientDisconnect,			// void )			(edict_t *pEntity);
	DLLFunc_ClientKill,					// void )			(edict_t *pEntity);
	DLLFunc_ClientPutInServer,			// void )			(edict_t *pEntity);
	DLLFunc_ClientCommand,				// void )			(edict_t *pEntity);

	DLLFunc_ServerDeactivate,			// void )			( void );

	DLLFunc_PlayerPreThink,				// void )			(edict_t *pEntity);
	DLLFunc_PlayerPostThink,			// void )			(edict_t *pEntity);

	DLLFunc_StartFrame,					// void )			( void );
	DLLFunc_ParmsNewLevel,				// void )			( void );
	DLLFunc_ParmsChangeLevel,			// void )			( void );

	// Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	// This also gets called when the server is queried for information (for example, by a server browser tool)
	DLLFunc_GetGameDescription,	 		// const char *)	( void );     

	// Spectator funcs
	DLLFunc_SpectatorConnect,			// void )			(edict_t *pEntity);
	DLLFunc_SpectatorDisconnect,		// void )			(edict_t *pEntity);
	DLLFunc_SpectatorThink,				// void )			(edict_t *pEntity);

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	DLLFunc_Sys_Error,					// void )			(const char *error_string);

	DLLFunc_PM_FindTextureType,			// char )			(char *name);
	DLLFunc_RegisterEncoders,			// void )			( void );

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	DLLFunc_GetHullBounds,				// int  )			(int hullnumber, float *mins, float *maxs);

	// Create baselines for certain "unplaced" items.
	DLLFunc_CreateInstBaselines,		// void )			( void );
	DLLFunc_pfnAllowLagCompensation,	// int  )			( void );
	// I know this does not fit with DLLFUNC(), but I don't want another native just for it.
	MetaFunc_CallGameEntity,			// bool	)			(plid_t plid, const char *entStr,entvars_t *pev);
	DLLFunc_ClientUserInfoChanged,		// void	)			(edict *pEntity, char *infobuffer);
	// You can pass in 0 for global cd handle or another cd handle here
	DLLFunc_UpdateClientData,			// void )			(const struct edict_s *ent, int sendweapons, struct clientdata_s *cd);
	// You can pass in 0 for global entity state handle or another entity state handle here
	DLLFunc_AddToFullPack,				// int  )			(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet);
	// You can pass in 0 for global usercmd handle or another usercmd handle here
	DLLFunc_CmdStart,					// void )			(const edict_t *player, const struct usercmd_s *cmd, unsigned int random_seed);
	DLLFunc_CmdEnd,						// void )			(const edict_t *player);
	DLLFunc_CreateBaseline				// void )			(int player, int eindex, struct entity_state_s *baseline, struct edict_s *entity, int playermodelindex, vec3_t player_mins, vec3_t player_maxs);
};

enum {
	pev_string_start = 0,
	pev_classname,
	pev_globalname,
	pev_model,
	pev_target,
	pev_targetname,
	pev_netname,
	pev_message,
	pev_noise,
	pev_noise1,
	pev_noise2,
	pev_noise3,
	pev_string_end,
	pev_edict_start,
	pev_chain,
	pev_dmg_inflictor,
	pev_enemy,
	pev_aiment,
	pev_owner,
	pev_groundentity,
	pev_euser1,
	pev_euser2,
	pev_euser3,
	pev_euser4,
	pev_edict_end,
	pev_float_start,
	pev_impacttime,
	pev_starttime,
	pev_idealpitch,
	pev_ideal_yaw,
	pev_pitch_speed,
	pev_yaw_speed,
	pev_ltime,
	pev_nextthink,
	pev_gravity,
	pev_friction,
	pev_frame,
	pev_animtime,
	pev_framerate,
	pev_scale,
	pev_renderamt,
	pev_health,
	pev_frags,
	pev_takedamage,
	pev_max_health,
	pev_teleport_time,
	pev_armortype,
	pev_armorvalue,
	pev_dmg_take,
	pev_dmg_save,
	pev_dmg,
	pev_dmgtime,
	pev_speed,
	pev_air_finished,
	pev_pain_finished,
	pev_radsuit_finished,
	pev_maxspeed,
	pev_fov,
	pev_flFallVelocity,
	pev_fuser1,
	pev_fuser2,
	pev_fuser3,
	pev_fuser4,
	pev_float_end,
	pev_int_start,
	pev_fixangle,
	pev_modelindex,
	pev_viewmodel,
	pev_weaponmodel,
	pev_movetype,
	pev_solid,
	pev_skin,
	pev_body,
	pev_effects,
	pev_light_level,
	pev_sequence,
	pev_gaitsequence,
	pev_rendermode,
	pev_renderfx,
	pev_weapons,
	pev_deadflag,
	pev_button,
	pev_impulse,
	pev_spawnflags,
	pev_flags,
	pev_colormap,
	pev_team,
	pev_waterlevel,
	pev_watertype,
	pev_playerclass,
	pev_weaponanim,
	pev_pushmsec,
	pev_bInDuck,
	pev_flTimeStepSound,
	pev_flSwimTime,
	pev_flDuckTime,
	pev_iStepLeft,
	pev_gamestate,
	pev_oldbuttons,
	pev_groupinfo,
	pev_iuser1,
	pev_iuser2,
	pev_iuser3,
	pev_iuser4,
	pev_int_end,
	pev_byte_start,
	pev_controller_0,
	pev_controller_1,
	pev_controller_2,
	pev_controller_3,
	pev_blending_0,
	pev_blending_1,
	pev_byte_end,
	pev_bytearray_start,
	pev_controller,
	pev_blending,
	pev_bytearray_end,
	pev_vecarray_start,
	pev_origin,
	pev_oldorigin,
	pev_velocity,
	pev_basevelocity,
	pev_clbasevelocity,
	pev_movedir,
	pev_angles,
	pev_avelocity,
	pev_v_angle,
	pev_endpos,
	pev_startpos,
	pev_absmin,
	pev_absmax,
	pev_mins,
	pev_maxs,
	pev_size,
	pev_rendercolor,
	pev_view_ofs,
	pev_vuser1,
	pev_vuser2,
	pev_vuser3,
	pev_vuser4,
	pev_punchangle,
	pev_vecarray_end,
	pev_string2_begin,	/* anything after here are string corrections */
	pev_weaponmodel2,
	pev_viewmodel2,
	pev_string2_end,
	pev_edict2_start,	/* edict corrections */
	pev_pContainingEntity,
	pev_absolute_end
};

/* Used with global_get()
 */
enum
{
	glb_start_int = 0, 
	glb_trace_hitgroup, 
	glb_trace_flags, 
	glb_msg_entity, 
	glb_cdAudioTrack, 
	glb_maxClients, 
	glb_maxEntities, 
	glb_end_int, 
	glb_start_float, 
	glb_time, 
	glb_frametime, 
	glb_force_retouch, 
	glb_deathmatch, 
	glb_coop, 
	glb_teamplay, 
	glb_serverflags, 
	glb_found_secrets, 
	glb_trace_allsolid, 
	glb_trace_startsolid, 
	glb_trace_fraction, 
	glb_trace_plane_dist, 
	glb_trace_inopen, 
	glb_trace_inwater, 
	glb_end_float, 
	glb_start_edict, 
	glb_trace_ent, 
	glb_end_edict, 
	glb_start_vector, 
	glb_v_forward, 
	glb_v_up, 
	glb_v_right, 
	glb_trace_endpos, 
	glb_trace_plane_normal, 
	glb_vecLandmarkOffset,
	glb_end_vector,
	glb_start_string, 
	glb_mapname, 
	glb_startspot, 
	glb_end_string, 
	glb_start_pchar, 
	glb_pStringBase, 
	glb_end_pchar
};

/* Used with register_forward()
 */
enum {
	FM_PrecacheModel = 1,
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
	FM_CreateInstBaselines,
	
	FM_AllowLagCompensation,
	FM_AlertMessage,
	
	// NEW_DLL_FUNCTIONS:
	FM_OnFreeEntPrivateData,
	FM_GameShutdown,
	FM_ShouldCollide,
	
	// LATE ADDITIONS (v1.71)
	FM_ClientUserInfoChanged,
	
	// LATE ADDITIONS (v1.75)
	FM_UpdateClientData,
	FM_AddToFullPack,
	FM_CmdStart,
	FM_CmdEnd,
	FM_CreateInstBaseline,
	FM_CreateBaseline,
	FM_GetInfoKeyBuffer,
	FM_ClientPrintf,
	
	// LATE ADDITIONS (v1.80)
	FM_ServerPrint
};

enum TraceResult
{
	TR_AllSolid,		// int
	TR_StartSolid,		// int
	TR_InOpen,			// int
	TR_InWater,			// int
	TR_flFraction,		// float
	TR_vecEndPos,		// float array[3]
	TR_flPlaneDist,		// float
	TR_vecPlaneNormal,	// float array[3]
	TR_pHit,			// int (edict_t*)
	TR_iHitgroup,		// int
};

enum KeyValueData
{
	KV_ClassName,		// string
	KV_KeyName,			// string
	KV_Value,			// string
	KV_fHandled			// int
};

enum ClientData
{
	CD_Origin,			// float array[3]
	CD_Velocity,		// float array[3]
	CD_ViewModel,		// int
	CD_PunchAngle,		// float array[3]
	CD_Flags,			// int
	CD_WaterLevel,		// int
	CD_WaterType,		// int
	CD_ViewOfs,			// float array[3]
	CD_Health,			// float
	CD_bInDuck,			// int
	CD_Weapons,			// int
	CD_flTimeStepSound,	// int
	CD_flDuckTime,		// int
	CD_flSwimTime,		// int
	CD_WaterJumpTime,	// int
	CD_MaxSpeed,		// float
	CD_FOV,				// float
	CD_WeaponAnim,		// int
	CD_ID,				// int
	CD_AmmoShells,		// int
	CD_AmmoNails,		// int
	CD_AmmoCells,		// int
	CD_AmmoRockets,		// int
	CD_flNextAttack,	// float
	CD_tfState,			// int
	CD_PushMsec,		// int
	CD_DeadFlag,		// int
	CD_PhysInfo,		// string[256]
	CD_iUser1,			// int
	CD_iUser2,			// int
	CD_iUser3,			// int
	CD_iUser4,			// int
	CD_fUser1,			// float
	CD_fUser2,			// float
	CD_fUser3,			// float
	CD_fUser4,			// float
	CD_vUser1,			// float array[3]
	CD_vUser2,			// float array[3]
	CD_vUser3,			// float array[3]
	CD_vUser4			// float array[3]
};

enum EntityState
{
	// Fields which are filled in by routines outside of delta compression
	ES_EntityType,		// int
	// Index into cl_entities array for this entity
	ES_Number,			// int
	ES_MsgTime,			// float
	
	// Message number last time the player/entity state was updated
	ES_MessageNum,		// int
	
	// Fields which can be transitted and reconstructed over the network stream
	ES_Origin,			// float array[3]
	ES_Angles,			// float array[3]
	
	ES_ModelIndex,		// int
	ES_Sequence,		// int
	ES_Frame,			// float
	ES_ColorMap,		// int
	ES_Skin,			// short
	ES_Solid,			// short
	ES_Effects,			// int 
	ES_Scale,			// float
	ES_eFlags,			// byte
	
	// Render information
	ES_RenderMode,		// int
	ES_RenderAmt,		// int
	ES_RenderColor,		// byte array[3], RGB value
	ES_RenderFx,		// int
	
	ES_MoveType,		// int
	ES_AnimTime,		// float
	ES_FrameRate,		// float
	ES_Body,			// int
	ES_Controller,		// byte array[4]
	ES_Blending,		// byte array[4]
	ES_Velocity,		// float array[3]
	
	// Send bbox down to client for use during prediction
	ES_Mins,			// float array[3]
	ES_Maxs,			// float array[3]
	
	ES_AimEnt,			// int
	// If owned by a player, the index of that player (for projectiles)
	ES_Owner,			// int
	
	// Friction, for prediction
	ES_Friction,		// float
	// Gravity multiplier
	ES_Gravity,			// float
	
	// PLAYER SPECIFIC
	ES_Team,			// int
	ES_PlayerClass,		// int
	ES_Health,			// int
	ES_Spectator,		// bool
	ES_WeaponModel,		// int
	ES_GaitSequence,	// int
	// If standing on conveyor, e.g.
	ES_BaseVelocity,	// float array[3]
	// Use the crouched hull, or the regular player hull
	ES_UseHull,			// int
	// Latched buttons last time state updated
	ES_OldButtons,		// int
	// -1 = in air, else pmove entity number
	ES_OnGround,		// int
	ES_iStepLeft,		// int
	// How fast we are falling
	ES_flFallVelocity,	// float
	
	ES_FOV,				// float
	ES_WeaponAnim,		// int	
	
	// Parametric movement overrides
	ES_StartPos,		// float array[3]
	ES_EndPos,			// float array[3]
	ES_ImpactTime,		// float
	ES_StartTime,		// float
	
	// For mods
	ES_iUser1,			// int
	ES_iUser2,			// int
	ES_iUser3,			// int
	ES_iUser4,			// int
	ES_fUser1,			// float
	ES_fUser2,			// float
	ES_fUser3,			// float
	ES_fUser4,			// float
	ES_vUser1,			// float array[3]
	ES_vUser2,			// float array[3]
	ES_vUser3,			// float array[3]
	ES_vUser4			// float array[3]
};

enum UserCmd
{
	// Interpolation time on client
	UC_LerpMsec,		// short
	// Duration in ms of command
	UC_Msec,			// byte
	// Command view angles
	UC_ViewAngles,		// float array[3]
	
	// Intended velocities
	// Forward velocity
	UC_ForwardMove,		// float
	// Sideways velocity
	UC_SideMove,		// float
	// Upward velocity
	UC_UpMove,			// float
	// Light level at spot where we are standing
	UC_LightLevel,		// byte
	// Attack buttons
	UC_Buttons,			// unsigned short
	// Impulse command issued
	UC_Impulse,			// byte
	// Current weapon id
	UC_WeaponSelect,	// byte
	
	// Experimental player impact stuff
	UC_ImpactIndex,		// int
	UC_ImpactPosition	// float array[3]
};

enum AlertType
{
	at_notice = 0,
	at_console,		// same as at_notice, but forces a ConPrintf, not a message box
	at_aiconsole,	// same as at_console, but only shown if developer level is 2!
	at_warning,
	at_error,
	at_logged		// Server print to console (only in multiplayer games)
};

/**
 * Data field types for use with find_ent_data_info().
 */
enum FieldType
{
	FIELD_NONE,
	FIELD_FLOAT,        // Floating point value
	FIELD_STRINGINT,    // String ID (return from ALLOC_STRING)
	FIELD_STRINGPTR,    // String, pointer-to-char
	FIELD_STRING,       // String, fixed size
	FIELD_CLASSPTR,     // Classes pointer derived of CBaseEntity
	FIELD_CLASS,        // Arbitrary classes, direct
	FIELD_STRUCTURE,    // Arbitrary structures, direct
	FIELD_EHANDLE,      // Entity handle
	FIELD_ENTVARS,      // entvars_t*
	FIELD_EDICT,        // edict_t*
	FIELD_VECTOR,       // Vector
	FIELD_POINTER,      // Arbitrary data pointer
	FIELD_INTEGER,      // Integer or enum
	FIELD_FUNCTION,     // Class function pointer (Think, Use, etc)
	FIELD_BOOLEAN,      // Boolean
	FIELD_SHORT,        // 2 bytes integer
	FIELD_CHARACTER,    // 1 byte
};

/**
 * Base data field types for use with get_ent_data_basetype().
 */
enum BaseFieldType
{
	BASEFIELD_NONE,
	BASEFIELD_INTEGER,
	BASEFIELD_FLOAT,
	BASEFIELD_VECTOR,
	BASEFIELD_ENTITY,
	BASEFIELD_STRING,
};
