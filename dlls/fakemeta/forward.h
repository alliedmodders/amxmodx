#ifndef _INCLUDE_FORWARD_H
#define _INCLUDE_FORWARD_H

#define ENGFUNC_NUM		108

#define	FMV_STRING		1
#define FMV_FLOAT		2
#define FMV_CELL		3

#define FMRES_HANDLED	2
#define FMRES_SUPERCEDE	4
#define FMRES_IGNORED	1
#define FMRES_OVERRIDE	3

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
	FM_FindEntityInSphere,	// edict)	(edict_t *pEdictStartSearchAfter, const float *org, float rad);
	FM_FindClientInPVS,		// edict)		(edict_t *pEdict);
	FM_EntitiesInPVS,		// edict)			(edict_t *pplayer);
	FM_MakeVectors,			// void )			(const float *rgflVector);
	FM_AngleVectors,		// void )			(const float *rgflVector, float *forward, float *right, float *up);
	FM_CreateEntity,		// done
	FM_RemoveEntity,		// done
	FM_CreateNamedEntity,	// done
	FM_MakeStatic,			// done
	FM_EntIsOnFloor,		// done
	FM_DropToFloor,			// done
	FM_WalkMove,			// int  )				(edict_t *ent, float yaw, float dist, int iMode);
	FM_SetOrigin,			// void )				(edict_t *e, const float *rgflOrigin);
	FM_EmitSound,			// void )				(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch);
	FM_EmitAmbientSound,	// void )		(edict_t *entity, float *pos, const char *samp, float vol, float attenuation, int fFlags, int pitch);
	FM_TraceLine,			// void )				(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	FM_TraceToss,			// void )				(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr);
	FM_TraceMonsterHull,	// int  )		(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	FM_TraceHull,			// void )				(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
	FM_TraceModel,			// void )			(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
	FM_TraceTexture,		// const char *)			(edict_t *pTextureEntity, const float *v1, const float *v2 );
	FM_TraceSphere,			// void )			(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
	FM_GetAimVector,		// void )			(edict_t* ent, float speed, float *rgflReturn);
	FM_ParticleEffect,		// void )		(const float *org, const float *dir, float color, float count);
	FM_LightStyle,			// void )			(int style, char* val);
	FM_DecalIndex,			// done
	FM_PointContents,		// int )			(const float *rgflVector);
	FM_FreeEntPrivateData,	// done
	FM_SzFromIndex,			// const char * )			(int iString);
	FM_AllocString,			// done
	FM_RegUserMsg,			// int	)			(const char *pszName, int iSize);
	FM_AnimationAutomove,	// void )		(const edict_t* pEdict, float flTime);
	FM_GetBonePosition,		// void )		(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles );
	FM_GetAttachment,		// void	)			(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );
	FM_SetView,				// void )				(const edict_t *pClient, const edict_t *pViewent );
	FM_Time,				// float)					( void );
	FM_CrosshairAngle,		// void )		(const edict_t *pClient, float pitch, float yaw);
	FM_FadeClientVolume,	// void )      (const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
	FM_SetClientMaxspeed,	// void )     (const edict_t *pEdict, float fNewMaxspeed);
	FM_CreateFakeClient,	// edict)		(const char *netname);	// returns NULL if fake client can't be created
	FM_RunPlayerMove,		// void )			(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec );
	FM_NumberOfEntities,	// done
	FM_StaticDecal,			// void )			( const float *origin, int decalIndex, int entityIndex, int modelIndex );
	FM_PrecacheGeneric,		// done
	FM_BuildSoundMsg,		// void )			(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
	FM_GetPhysicsKeyValue,	// const char* )	( const edict_t *pClient, const char *key );
	FM_SetPhysicsKeyValue,	// void )	( const edict_t *pClient, const char *key, const char *value );
	FM_GetPhysicsInfoString,// const char* )	( const edict_t *pClient );
	FM_PrecacheEvent,		// unsigned short )		( int type, const char*psz );
	FM_PlaybackEvent,		// void )			( int flags, const edict_t *pInvoker, unsigned short eventindex, float delay, float *origin, float *angles, float fparam1, float fparam2, int iparam1, int iparam2, int bparam1, int bparam2 );
	FM_CheckVisibility,			//)		( const edict_t *entity, unsigned char *pset );
	FM_GetCurrentPlayer,			//)		( void );
	FM_CanSkipPlayer,			//)			( const edict_t *player );
	FM_SetGroupMask,				//)			( int mask, int op );
	FM_Voice_GetClientListening,	// bool (int iReceiver, int iSender)
	FM_Voice_SetClientListening,	// bool (int iReceiver, int iSender, bool Listen)
	FM_InfoKeyValue,	// char*	)			(char *infobuffer, char *key);
	FM_SetKeyValue,	// void )			(char *infobuffer, char *key, char *value);
	FM_SetClientKeyValue,	 // void )		(int clientIndex, char *infobuffer, char *key, char *value);



	// FM_GameInit,	// Removed -- it will *never* be called after plugins are loaded
	FM_Spawn,	// done
	FM_Think,	// done
	FM_Use,	// done
	FM_Touch,	// done
	FM_Blocked,	// done
	FM_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd );
	FM_SetAbsBox,			// done
	FM_ClientConnect,		// bool)		( edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[ 128 ] );
	
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
	FM_GetGameDescription,	 // const char * )( void );     

	// Spectator funcs
	FM_SpectatorConnect,	// done
	FM_SpectatorDisconnect,	// done
	FM_SpectatorThink,		// done

	// Notify game .dll that engine is going to shut down.  Allows mod authors to set a breakpoint.
	FM_Sys_Error,		// done

	FM_PM_FindTextureType,	// done
	FM_RegisterEncoders,	// done

	// Enumerates player hulls.  Returns 0 if the hull number doesn't exist, 1 otherwise
	FM_GetHullBounds,	// int)	( int hullnumber, float *mins, float *maxs );

	// Create baselines for certain "unplaced" items.
	FM_CreateInstancedBaselines,	// done 
	FM_AllowLagCompensation,	// done
};

extern CVector<int> Engine[];
extern CVector<int> EnginePost[];
extern cell mCellResult;
extern float mFloatResult;
extern const char *mStringResult;
extern cell mlCellResult;
extern float mlFloatResult;
extern const char *mlStringResult;
extern int lastFmRes;
extern int retType;

#endif //_INCLUDE_FORWARD_H