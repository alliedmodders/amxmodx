/* Ham Sandwich
 *   Copyright 2007
 *   By the AMX Mod X Development Team
 *
 *  Ham Sandwich is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 2 of the License, or (at
 *  your option) any later version.
 *
 *  Ham Sandwich is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with Ham Sandwich; if not, write to the Free Software Foundation,
 *  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *  In addition, as a special exception, the author gives permission to
 *  link the code of Ham Sandwich with the Half-Life Game Engine ("HL
 *  Engine") and Modified Game Libraries ("MODs") developed by Valve,
 *  L.L.C ("Valve"). You must obey the GNU General Public License in all
 *  respects for all of the code used other than the HL Engine and MODs
 *  from Valve. If you modify this file, you may extend this exception
 *  to your version of the file, but you are not obligated to do so. If
 *  you do not wish to do so, delete this exception statement from your
 *  version.
 */

#ifndef HAM_CONST_H
#define HAM_CONST_H


enum 
{
	HAM_UNSET = 0,
	HAM_IGNORED,
	HAM_HANDLED,
	HAM_OVERRIDE,
	HAM_SUPERCEDE
};

enum
{
	Ham_Spawn = 0,
	Ham_Precache,
	Ham_Keyvalue,
	Ham_ObjectCaps,
	Ham_Activate,
	Ham_SetObjectCollisionBox,
	Ham_Classify,
	Ham_DeathNotice,
	Ham_TraceAttack,
	Ham_TakeDamage,
	Ham_TakeHealth,
	Ham_Killed,
	Ham_BloodColor,
	Ham_TraceBleed,
	Ham_IsTriggered,
	Ham_MyMonsterPointer,
	Ham_MySquadMonsterPointer,
	Ham_GetToggleState,
	Ham_AddPoints,
	Ham_AddPointsToTeam,
	Ham_AddPlayerItem,
	Ham_RemovePlayerItem,
	Ham_GiveAmmo,
	Ham_GetDelay,
	Ham_IsMoving,
	Ham_OverrideReset,
	Ham_DamageDecal,
	Ham_SetToggleState,
	Ham_StartSneaking,
	Ham_StopSneaking,
	Ham_OnControls,
	Ham_IsSneaking,
	Ham_IsAlive,
	Ham_IsBSPModel,
	Ham_ReflectGauss,
	Ham_HasTarget,
	Ham_IsInWorld,
	Ham_IsPlayer,
	Ham_IsNetClient,
	Ham_TeamId,
	Ham_GetNextTarget,
	Ham_Think,
	Ham_Touch,
	Ham_Use,
	Ham_Blocked,
	Ham_Respawn,
	Ham_UpdateOwner,
	Ham_FBecomeProne,
	Ham_Center,
	Ham_EyePosition,
	Ham_EarPosition,
	Ham_BodyTarget,
	Ham_Illumination,
	Ham_FVisible,
	Ham_FVecVisible,

	Ham_Player_Jump,
	Ham_Player_Duck,
	Ham_Player_PreThink,
	Ham_Player_PostThink,
	Ham_Player_GetGunPosition,
	Ham_Player_ShouldFadeOnDeath,
	Ham_Player_ImpulseCommands,
	Ham_Player_UpdateClientData,

	Ham_Item_AddToPlayer,
	Ham_Item_AddDuplicate,
	Ham_Item_CanDeploy,
	Ham_Item_Deploy,
	Ham_Item_CanHolster,
	Ham_Item_Holster,
	Ham_Item_UpdateItemInfo,
	Ham_Item_PreFrame,
	Ham_Item_PostFrame,
	Ham_Item_Drop,
	Ham_Item_Kill,
	Ham_Item_AttachToPlayer,
	Ham_Item_PrimaryAmmoIndex,
	Ham_Item_SecondaryAmmoIndex,
	Ham_Item_UpdateClientData,
	Ham_Item_GetWeaponPtr,
	Ham_Item_ItemSlot,

	Ham_Weapon_ExtractAmmo,
	Ham_Weapon_ExtractClipAmmo,
	Ham_Weapon_AddWeapon,
	Ham_Weapon_PlayEmptySound,
	Ham_Weapon_ResetEmptySound,
	Ham_Weapon_SendWeaponAnim,
	Ham_Weapon_IsUsable,
	Ham_Weapon_PrimaryAttack,
	Ham_Weapon_SecondaryAttack,
	Ham_Weapon_Reload,
	Ham_Weapon_WeaponIdle,
	Ham_Weapon_RetireWeapon,
	Ham_Weapon_ShouldWeaponIdle,
	Ham_Weapon_UseDecrement,

	Ham_TS_BreakableRespawn,
	Ham_TS_CanUsedThroughWalls,
	Ham_TS_RespawnWait,

	Ham_CS_Restart,
	Ham_CS_RoundRespawn,
	Ham_CS_Item_CanDrop,
	Ham_CS_Item_GetMaxSpeed,

	Ham_DOD_RoundRespawn,
	Ham_DOD_RoundRespawnEnt,
	Ham_DOD_RoundStore,
	Ham_DOD_AreaSetIndex,
	Ham_DOD_AreaSendStatus,
	Ham_DOD_GetState,
	Ham_DOD_GetStateEnt,
	Ham_DOD_Item_CanDrop,

	Ham_TFC_EngineerUse,
	Ham_TFC_Finished,
	Ham_TFC_EmpExplode,
	Ham_TFC_CalcEmpDmgRad,
	Ham_TFC_TakeEmpBlast,
	Ham_TFC_EmpRemove,
	Ham_TFC_TakeConcussionBlast,
	Ham_TFC_Concuss,

	Ham_ESF_IsEnvModel, // Only valid in ESF Open Beta
	Ham_ESF_TakeDamage2, // Only valid in ESF Open Beta

	Ham_NS_GetPointValue,
	Ham_NS_AwardKill,
	Ham_NS_ResetEntity,
	Ham_NS_UpdateOnRemove,


	HAM_LAST_ENTRY_DONT_USE_ME_LOL
};

enum 
{
	HAM_OK = 0,
	
	HAM_INVALID_FUNC,			// The function is not valid
	HAM_FUNC_NOT_CONFIGURED,	// This function is not configured in hamdata.ini
	
	HAM_ERR_END
};

#endif
