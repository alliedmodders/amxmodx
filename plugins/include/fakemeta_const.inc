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


// The actual return value of the function
#define FMRES_HANDLED	2
#define FMRES_SUPERCEDE	4
#define FMRES_IGNORED	1
#define FMRES_OVERRIDE	3



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
	DLLFunc_CreateInstancedBaseline,	// void ) ( void );
	DLLFunc_pfnAllowLagCompensation,	// int )( void );
	// I know this does not fit with DLLFUNC(), but I dont want another native just for it.
	MetaFunc_CallGameEntity	// bool	(plid_t plid, const char *entStr,entvars_t *pev);
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
	pev_vecarray_end
};
enum {
	FM_PrecacheModel = 1,	// done
	FM_PrecacheSound,	// done
	FM_SetModel,		// done
	FM_ModelIndex,		// done
	FM_ModelFrames,	// done
	FM_SetSize,		// done
	FM_ChangeLevel,			// done
	FM_VecToYaw,			// done
	FM_VecToAngles,			// done
	FM_MoveToOrigin,		// done
	FM_ChangeYaw,			// done
	FM_ChangePitch,			// done
	FM_FindEntityByString,	// done
	FM_GetEntityIllum,		// done
	FM_FindEntityInSphere,	// done
	FM_FindClientInPVS,		// done
	FM_EntitiesInPVS,		// done
	FM_MakeVectors,			// done
	FM_AngleVectors,		// done
	FM_CreateEntity,		// done
	FM_RemoveEntity,		// done
	FM_CreateNamedEntity,	// done
	FM_MakeStatic,			// done
	FM_EntIsOnFloor,		// done
	FM_DropToFloor,			// done
	FM_WalkMove,			// int  )				(edict_t *ent, float yaw, float dist, int iMode); -- does't work as of 0.20 RC2
	FM_SetOrigin,			// done
	FM_EmitSound,			// done
	FM_EmitAmbientSound,	// done
	FM_TraceLine,			// void )				(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr); -- does't work as of 0.20 RC2
	FM_TraceToss,			// void )				(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr); -- does't work as of 0.20 RC2
	FM_TraceMonsterHull,	// int  )		(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr); -- does't work as of 0.20 RC2
	FM_TraceHull,			// void )				(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);  -- does't work as of 0.20 RC2
	FM_TraceModel,			// void )			(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);  -- does't work as of 0.20 RC2
	FM_TraceTexture,		// const char *)			(edict_t *pTextureEntity, const float *v1, const float *v2 ); -- does't work as of 0.20 RC2
	FM_TraceSphere,			// void )			(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);  -- does't work as of 0.20 RC2
	FM_GetAimVector,		// done
	FM_ParticleEffect,		// done
	FM_LightStyle,			// done
	FM_DecalIndex,			// done
	FM_PointContents,		// done
	FM_MessageBegin,		// done
	FM_MessageEnd,			// done
	FM_WriteByte,			// done
	FM_WriteChar,			// done
	FM_WriteShort,			// done
	FM_WriteLong,			// done
	FM_WriteAngle,			// done
	FM_WriteCoord,			// done
	FM_WriteString,			// done
	FM_WriteEntity,			// done
	FM_CVarGetFloat,		// done
	FM_CVarGetString,		// done
	FM_CVarSetFloat,		// done
	FM_CVarSetString,		// done
	FM_FreeEntPrivateData,	// done
	FM_SzFromIndex,			// done
	FM_AllocString,			// done
	FM_RegUserMsg,			// done
	FM_AnimationAutomove,	// done
	FM_GetBonePosition,		// void )		(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles );  -- does't work as of 0.20 RC2
	FM_GetAttachment,		// void	)			(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );  -- does't work as of 0.20 RC2
	FM_SetView,				// done
	FM_Time,				// done
	FM_CrosshairAngle,		// done
	FM_FadeClientVolume,	// done
	FM_SetClientMaxspeed,	// done
	FM_CreateFakeClient,	// done
	FM_RunPlayerMove,		// void )			(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec ); -- does't work as of 0.20 RC2
	FM_NumberOfEntities,	// done
	FM_StaticDecal,			// done
	FM_PrecacheGeneric,		// done
	FM_BuildSoundMsg,		// void )			(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);  -- does't work as of 0.20 RC2
	FM_GetPhysicsKeyValue,	// done
	FM_SetPhysicsKeyValue,	// done
	FM_GetPhysicsInfoString,// done
	FM_PrecacheEvent,		// done
	FM_PlaybackEvent,		// done
	FM_CheckVisibility,			//)		(
	FM_GetCurrentPlayer,			// done
	FM_CanSkipPlayer,			// done
	FM_SetGroupMask,				//done
	FM_Voice_GetClientListening,	// done
	FM_Voice_SetClientListening,	// done
	FM_InfoKeyValue,	// done
	FM_SetKeyValue,	// done
	FM_SetClientKeyValue,	 // done
	FM_GetPlayerAuthId,	// done
	FM_GetPlayerWONId,	// done
	FM_IsMapValid,	// done


	FM_Spawn,	// done
	FM_Think,	// done
	FM_Use,	// done
	FM_Touch,	// done
	FM_Blocked,	// done
	FM_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd ); -- does't work as of 0.20 RC2
	FM_SetAbsBox,			// done
	FM_ClientConnect,		// done
	
	FM_ClientDisconnect,	// done
	FM_ClientKill,		// done
	FM_ClientPutInServer,	// done
	FM_ClientCommand,		// done

	FM_ServerDeactivate,	// done

	FM_PlayerPreThink,		// done
	FM_PlayerPostThink,		// done

	FM_StartFrame,		// done
	FM_ParmsNewLevel,		// done
	FM_ParmsChangeLevel,	// done

	 // Returns string describing current .dll.  E.g., TeamFotrress 2, Half-Life
	FM_GetGameDescription,	 // done

	// Spectator funcs
	FM_SpectatorConnect,	// done
	FM_SpectatorDisconnect,	// done
	FM_SpectatorThink,		// done

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	FM_Sys_Error,		// done

	FM_PM_FindTextureType,	// done
	FM_RegisterEncoders,	// done

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise

	// Create baselines for certain "unplaced" items.
	FM_CreateInstancedBaseline,	// done 
	FM_AllowLagCompensation,	// done
};
