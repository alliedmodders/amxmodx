#ifndef _INCLUDE_FORWARD_H
#define _INCLUDE_FORWARD_H

#define ENGFUNC_NUM		118

#define	FMV_STRING		1
#define FMV_FLOAT		2
#define FMV_CELL		3

#define FMRES_HANDLED	2
#define FMRES_SUPERCEDE	4
#define FMRES_IGNORED	1
#define FMRES_OVERRIDE	3

enum {
  FM_FIRST_DONT_USE_ME = 0,
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
	FM_WalkMove,			// int  )				(edict_t *ent, float yaw, float dist, int iMode);
	FM_SetOrigin,			// done
	FM_EmitSound,			// done
	FM_EmitAmbientSound,	// done
	FM_TraceLine,			// void )				(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	FM_TraceToss,			// void )				(edict_t* pent, edict_t* pentToIgnore, TraceResult *ptr);
	FM_TraceMonsterHull,	// int  )		(edict_t *pEdict, const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr);
	FM_TraceHull,			// void )				(const float *v1, const float *v2, int fNoMonsters, int hullNumber, edict_t *pentToSkip, TraceResult *ptr);
	FM_TraceModel,			// void )			(const float *v1, const float *v2, int hullNumber, edict_t *pent, TraceResult *ptr);
	FM_TraceTexture,		// const char *)			(edict_t *pTextureEntity, const float *v1, const float *v2 );
	FM_TraceSphere,			// void )			(const float *v1, const float *v2, int fNoMonsters, float radius, edict_t *pentToSkip, TraceResult *ptr);
	FM_GetAimVector,		// void )			(edict_t* ent, float speed, float *rgflReturn);
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
	FM_GetBonePosition,		// void )		(const edict_t* pEdict, int iBone, float *rgflOrigin, float *rgflAngles );
	FM_GetAttachment,		// void	)			(const edict_t *pEdict, int iAttachment, float *rgflOrigin, float *rgflAngles );
	FM_SetView,				// done
	FM_Time,				// done
	FM_CrosshairAngle,		// done
	FM_FadeClientVolume,	// void )      (const edict_t *pEdict, int fadePercent, int fadeOutSeconds, int holdTime, int fadeInSeconds);
	FM_SetClientMaxspeed,	// done
	FM_CreateFakeClient,	// done
	FM_RunPlayerMove,		// void )			(edict_t *fakeclient, const float *viewangles, float forwardmove, float sidemove, float upmove, unsigned short buttons, byte impulse, byte msec );
	FM_NumberOfEntities,	// done
	FM_StaticDecal,			// void )			( const float *origin, int decalIndex, int entityIndex, int modelIndex );
	FM_PrecacheGeneric,		// done
	FM_BuildSoundMsg,		// void )			(edict_t *entity, int channel, const char *sample, /*int*/float volume, float attenuation, int fFlags, int pitch, int msg_dest, int msg_type, const float *pOrigin, edict_t *ed);
	FM_GetPhysicsKeyValue,	// done
	FM_SetPhysicsKeyValue,	// done
	FM_GetPhysicsInfoString,// done
	FM_PrecacheEvent,		// done
	FM_PlaybackEvent,		// done
	FM_CheckVisibility,			//)		( const edict_t *entity, unsigned char *pset );
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


	// FM_GameInit,	// Removed -- it will *never* be called after plugins are loaded
	FM_Spawn,	// done
	FM_Think,	// done
	FM_Use,	// done
	FM_Touch,	// done
	FM_Blocked,	// done
	FM_KeyValue,	// void )			( edict_t *pentKeyvalue, KeyValueData *pkvd );
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
	FM_CreateInstancedBaselines,	// done 
	FM_AllowLagCompensation,	// done
	FM_AlertMessage,
	FM_LAST_DONT_USE_ME,
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

