/* Fake meta constants
 * This file is provided as-is (no warrenties)
 */
#if defined _fakemeta_const_included
  #endinput
#endif
#define _fakemeta_const_included

// For forward_return
#define	FMV_STRING		1
#define FMV_FLOAT		2
#define FMV_CELL		3


/* The actual return value of the function, use these instead of PLUGIN_HANDLED etc when
 * returning from registered forwards.
 */
#define FMRES_HANDLED	2
#define FMRES_SUPERCEDE	4
#define FMRES_IGNORED	1
#define FMRES_OVERRIDE	3


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
	EngFunc_CheckVisibility,			// int )			(const edict_t *entity, unsigned char *pset);
	EngFunc_GetCurrentPlayer,			// int )			( void );
	EngFunc_CanSkipPlayer,				// int )			(const edict_t *player);
	EngFunc_SetGroupMask,				// void )			(int mask, int op);
	EngFunc_GetClientListening,			// bool )			(int iReceiver, int iSender)
	EngFunc_SetClientListening,			// bool )			(int iReceiver, int iSender, bool Listen)
	EngFunc_MessageBegin,				// void )			(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed)
	EngFunc_WriteCoord,					// void )			(float flValue)
	EngFunc_WriteAngle,					// void )			(float flValue)
	EngFunc_InfoKeyValue,				// char*)			(char *infobuffer, char *key);
	EngFunc_SetKeyValue,				// void )			(char *infobuffer, char *key, char *value);
	EngFunc_SetClientKeyValue,			// void )			(int clientIndex, char *infobuffer, char *key, char *value);
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
	DLLFunc_CreateInstancedBaseline,	// void )			( void );
	DLLFunc_pfnAllowLagCompensation,	// int  )			( void );
	// I know this does not fit with DLLFUNC(), but I dont want another native just for it.
	MetaFunc_CallGameEntity,			// bool	)			(plid_t plid, const char *entStr,entvars_t *pev);
	DLLFunc_ClientUserInfoChanged		// void	)			(idplayer)
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
	pev_absolute_end
};

/* Used with get_global()
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
	FM_CreateInstancedBaselines,
	
	FM_AllowLagCompensation,
	FM_AlertMessage,
	
	// NEW_DLL_FUNCTIONS:
	FM_OnFreeEntPrivateData,
	FM_GameShutdown,
	FM_ShouldCollide,
	
	//LATE ADDITIONS (v1.71)
	FM_ClientInfoChanged	//passes id only
};

enum TraceResult
{
	TR_AllSolid,
	TR_StartSolid,
	TR_InOpen,
	TR_InWater,
	TR_flFraction,
	TR_vecEndPos,
	TR_flPlaneDist,
	TR_vecPlaneNormal,
	TR_pHit,
	TR_iHitgroup,
};

enum KeyValueData
{
	KV_ClassName,
	KV_KeyName,
	KV_Value,
	KV_fHandled
};

