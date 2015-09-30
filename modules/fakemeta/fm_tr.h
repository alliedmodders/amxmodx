// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fakemeta Module
//

#ifndef _INCLUDE_TR_H
#define _INCLUDE_TR_H

#include <amtl/am-string.h>

extern TraceResult *gfm_tr;

//these also don't fit in here but gaben does not care.  GABEN DOES NOT CARE!!!
extern TraceResult g_tr_2;
extern clientdata_t g_cd_glb;
extern clientdata_t *g_cd_hook;
extern entity_state_t g_es_glb;
extern entity_state_t *g_es_hook;
extern usercmd_t g_uc_glb;
extern usercmd_t *g_uc_hook;

struct KVD_Wrapper
{
	KeyValueData kvd;
	ke::AString cls;
	ke::AString key;
	ke::AString val;
};

extern KVD_Wrapper g_kvd_glb;

enum
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

enum KeyValue
{
	KV_ClassName,
	KV_KeyName,
	KV_Value,
	KV_fHandled
};

enum ClientData
{
	CD_Origin,
	CD_Velocity,
	CD_ViewModel,
	CD_PunchAngle,
	CD_Flags,
	CD_WaterLevel,
	CD_WaterType,
	CD_ViewOfs,
	CD_Health,
	CD_bInDuck,
	CD_Weapons,
	CD_flTimeStepSound,
	CD_flDuckTime,
	CD_flSwimTime,
	CD_WaterJumpTime,
	CD_MaxSpeed,
	CD_FOV,
	CD_WeaponAnim,
	CD_ID,
	CD_AmmoShells,
	CD_AmmoNails,
	CD_AmmoCells,
	CD_AmmoRockets,
	CD_flNextAttack,
	CD_tfState,
	CD_PushMsec,
	CD_DeadFlag,
	CD_PhysInfo,
	CD_iUser1,
	CD_iUser2,
	CD_iUser3,
	CD_iUser4,
	CD_fUser1,
	CD_fUser2,
	CD_fUser3,
	CD_fUser4,
	CD_vUser1,
	CD_vUser2,
	CD_vUser3,
	CD_vUser4
};

enum EntityState
{
	// Fields which are filled in by routines outside of delta compression
	ES_EntityType,
	// Index into cl_entities array for this entity
	ES_Number,
	ES_MsgTime,

	// Message number last time the player/entity state was updated
	ES_MessageNum,

	// Fields which can be transitted and reconstructed over the network stream
	ES_Origin,
	ES_Angles,

	ES_ModelIndex,
	ES_Sequence,
	ES_Frame,
	ES_ColorMap,
	ES_Skin,
	ES_Solid,
	ES_Effects,
	ES_Scale,
	ES_eFlags,

	// Render information
	ES_RenderMode,
	ES_RenderAmt,
	ES_RenderColor,
	ES_RenderFx,

	ES_MoveType,
	ES_AnimTime,
	ES_FrameRate,
	ES_Body,
	ES_Controller,
	ES_Blending,
	ES_Velocity,

	// Send bbox down to client for use during prediction
	ES_Mins,
	ES_Maxs,

	ES_AimEnt,
	// If owned by a player, the index of that player (for projectiles)
	ES_Owner,

	// Friction, for prediction
	ES_Friction,
	// Gravity multiplier
	ES_Gravity,

	// PLAYER SPECIFIC
	ES_Team,
	ES_PlayerClass,
	ES_Health,
	ES_Spectator,
	ES_WeaponModel,
	ES_GaitSequence,
	// If standing on conveyor, e.g.
	ES_BaseVelocity,
	// Use the crouched hull, or the regular player hull
	ES_UseHull,
	// Latched buttons last time state updated
	ES_OldButtons,
	// -1 = in air, else pmove entity number
	ES_OnGround,
	ES_iStepLeft,
	// How fast we are falling
	ES_flFallVelocity,

	ES_FOV,
	ES_WeaponAnim,

	// Parametric movement overrides
	ES_StartPos,
	ES_EndPos,
	ES_ImpactTime,
	ES_StartTime,

	// For mods
	ES_iUser1,
	ES_iUser2,
	ES_iUser3,
	ES_iUser4,
	ES_fUser1,
	ES_fUser2,
	ES_fUser3,
	ES_fUser4,
	ES_vUser1,
	ES_vUser2,
	ES_vUser3,
	ES_vUser4
};

enum UserCmd
{
	UC_LerpMsec,		// Interpolation time on client
	UC_Msec,			// Duration in ms of command
	UC_ViewAngles,		// Command view angles

	// Intended velocities
	UC_ForwardMove,		// Forward velocity
	UC_SideMove,		// Sideways velocity
	UC_UpMove,			// Upward velocity
	UC_LightLevel,		// Light level at spot where we are standing
	UC_Buttons,			// Attack buttons
	UC_Impulse,			// Impulse command issued
	UC_WeaponSelect,	// Current weapon id

	// Experimental player impact stuff
	UC_ImpactIndex,
	UC_ImpactPosition
};

extern AMX_NATIVE_INFO tr_Natives[];
extern AMX_NATIVE_INFO ext2_natives[];

#endif //_INCLUDE_TR_H

