#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <extdll.h>
#include "sdk/amxxmodule.h"


#include "CVector.h"

#include "hook.h"
#include "forward.h"
#include "hook_callbacks.h"
#include "call_funcs.h"
#include "ecall_funcs.h"
#include "hook_create.h"
#include "offsets.h"
#include "hooklist.h"
#include "ham_utils.h"

OffsetManager Offsets;

CVector<Hook *> hooks[HAM_LAST_ENTRY_DONT_USE_ME_LOL];


#define V(__STUFF__) reinterpret_cast<void *>(Hook_##__STUFF__), Create_##__STUFF__, Call_##__STUFF__, eCall_##__STUFF__

hook_t hooklist[] =
{
	{ 0, 0, "spawn",					true, 0,  V(Void_Void) }, // Spawn
	{ 0, 0, "precache",					true, 0,  V(Void_Void) }, // Precache
	{ 0, 0, "keyvalue",					true, 1,  V(Void_Int) }, // Keyvalue
	{ 0, 0, "objectcaps",				false, 0, V(Int_Void) }, // ObjectCaps
	{ 0, 0, "activate",					true, 0,  V(Void_Void) }, // Activate
	{ 0, 0, "setobjectcollisionbox",	true, 0,  V(Void_Void) }, // SetObjectCollisionBox
	{ 0, 0, "classify",					false, 0, V(Int_Void) }, // Classify
	{ 0, 0, "deathnotice",				true, 1,  V(Void_Entvar) }, // DeathNotice
	{ 0, 0, "traceattack",				true, 7, V(Void_Entvar_Float_Vector_Trace_Int) }, // TraceAttack
	{ 0, 0, "takedamage",				false, 4, V(Int_Entvar_Entvar_Float_Int) },  // Takedamage
	{ 0, 0, "takehealth",				false, 2, V(Int_Float_Int) }, // TakeHealth
	{ 0, 0, "killed",					true,  2, V(Void_Entvar_Int) }, // Killed
	{ 0, 0, "bloodcolor",				false, 0, V(Int_Void) }, // BloodColor
	{ 0, 0, "tracebleed",				true, 6, V(Void_Float_Vector_TraceResult_Int) }, // TraceBleed
	{ 0, 0, "istriggered",				false, 1, V(Int_Cbase) }, // IsTriggered
	{ 0, 0, "gettogglestate",			false, 0, V(Int_Void) }, // GetToggleState
	{ 0, 0, "addpoints",				true, 2,  V(Void_Int_Int) }, // AddPoints
	{ 0, 0, "addpointstoteam",			true, 2,  V(Void_Int_Int) }, // AddPointsToTeam
	{ 0, 0, "addplayeritem",			false, 1, V(Int_Cbase) }, // AddPlayerItem
	{ 0, 0, "removeplayeritem",			false, 1, V(Int_Cbase) }, // RemovePlayerItem
	{ 0, 0, "giveammo",					false, 3, V(Int_Int_Str_Int) }, // GiveAmmo
	{ 0, 0, "getdelay",					false, 0, V(Int_Void) }, // GetDelay
	{ 0, 0, "ismoving",					false, 0, V(Int_Void) }, // IsMoving
	{ 0, 0, "overridereset",			true, 0,  V(Void_Void) }, // OverrideReset
	{ 0, 0, "damagedecal",				false, 1, V(Int_Int) }, // DamageDecal
	{ 0, 0, "settogglestate",			true, 1,  V(Void_Int) }, // SetToggleState
	{ 0, 0, "startsneaking",			true, 0, V(Void_Void) }, // StartSneaking
	{ 0, 0, "stopsneaking",				true, 0, V(Void_Void) }, // StopSneaking
	{ 0, 0, "oncontrols",				false, 1, V(Int_Entvar) }, // OnControls
	{ 0, 0, "issneaking",				false, 0, V(Int_Void) }, // IsSneaking
	{ 0, 0, "isalive",					false, 0, V(Int_Void) }, // IsAlive
	{ 0, 0, "isbspmodel",				false, 0, V(Int_Void) }, // IsBSPModel
	{ 0, 0, "reflectgauss",				false, 0, V(Int_Void) }, // ReflectGauss
	{ 0, 0, "hastarget",				false, 1, V(Int_Int) }, // HasTarget
	{ 0, 0, "isinworld",				false, 0, V(Int_Void) }, // IsInWorld
	{ 0, 0, "isplayer",					false, 0, V(Int_Void) }, // IsPlayer
	{ 0, 0, "isnetclient",				false, 0, V(Int_Void) }, // IsNetClient
	{ 0, 0, "teamid",					false, 0, V(Str_Void) }, // TeamID
	{ 0, 0, "getnexttarget",			false, 0, V(Cbase_Void) }, // GetNextTarget 
	{ 0, 0, "think",					true, 0,  V(Void_Void) }, // Think
	{ 0, 0, "touch",					true, 1,  V(Void_Cbase) }, // Touch
	{ 0, 0, "use",						true, 4,  V(Void_Cbase_Cbase_Int_Float) }, // Use
	{ 0, 0, "blocked",					true, 1,  V(Void_Cbase) }, // Blocked
	{ 0, 0, "respawn",					false, 0, V(Cbase_Void) }, // Respawn TODO: Cbase this
	{ 0, 0, "updateowner",				true, 0,  V(Void_Void) }, // UpdateOwner
	{ 0, 0, "fbecomeprone",				false, 0, V(Int_Void) }, // FBecomeProne


	// TODO: These
	{ 0, 0, "center",					false, 0, V(Vector_Void) }, // Center
	{ 0, 0, "eyeposition",				false, 0, V(Vector_Void) }, // EyePosition
	{ 0, 0, "earposition",				false, 0, V(Vector_Void) }, // EarPosition
	{ 0, 0, "bodytarget",				false, 1, V(Vector_pVector) }, // BodyTarget
	{ 0, 0, "illumination",				false, 0, V(Int_Void) }, // Illumination
	{ 0, 0, "fvisible",					false, 1, V(Int_Cbase) }, // FVisible
	{ 0, 0, "fvecvisible",				false, 1, V(Int_pVector) }, // FVecVisible
	/** Mod specific hooks **/

	/* The Specialists */
	{ 0, 0, "ts_breakablerespawn",		false, 1, V(Int_Int) },		// TS_BreakableRespawn
	{ 0, 0, "ts_canusedthroughwalls",	false, 0, V(Int_Void) },		// TS_CanUsedThroughWalls
	{ 0, 0, "ts_respawnwait",			false, 0, V(Int_Void) },		// TS_RespawnWait

	/* Counter-Strike */
	{ 0, 0, "cstrike_restart",			true, 0, V(Void_Void) },		// CS_Restart

	/* Day of Defeat */
	{ 0, 0, "dod_roundrespawn",			true, 0, V(Void_Void) },			// DOD_RoundRespawn
	{ 0, 0, "dod_roundrespawnent",		true, 0, V(Void_Void) },			// DOD_RoundRespawnEnt
	{ 0, 0, "dod_roundstore",			true, 0, V(Void_Void) },			// DOD_RoundStore
	{ 0, 0, "dod_areasetindex",			true, 1, V(Void_Int) },			// DOD_AreaSetIndex
	{ 0, 0, "dod_areasendstatus",		true, 1, V(Void_Cbase) },			// DOD_AreaSendStatus
	{ 0, 0, "dod_getstate",				false, 0, V(Int_Void) },		// DOD_GetState
	{ 0, 0, "dod_getstateent",			false, 1, V(Int_Cbase) },							// DOD_GetStateEnt

	
	/* Team Fortress Classic */
	// This next one is just a huge guess
	{ 0, 0, "tfc_dbgetitemname",		false, 0, V(Str_Void) },							// TFC_DbGetItemName
	{ 0, 0, "tfc_engineeruse",			false, 1, V(Int_Cbase) },		// TFC_EngineerUse
	{ 0, 0, "tfc_finished",				true, 0, V(Void_Void) },							// TFC_Finished
	{ 0, 0, "tfc_empexplode",			true, 3, V(Void_Entvar_Float_Float) },	// TFC_EmpExplode
	{ 0, 0, "tfc_calcempdmgrad",		false, 2, V(Int_pFloat_pFloat) }, // TFC_CalcEmpDmgRad
	{ 0, 0, "tfc_takeempblast",			true, 1, V(Void_Entvar) },		// TFC_TakeEmpBlast
	{ 0, 0, "tfc_empremove",			true, 0, V(Void_Void) },					// TFC_EmpRemove
	{ 0, 0, "tfc_takeconcussionblast",	true, 2, V(Void_Entvar_Float) },	// TFC_TakeConcussionBlast
	{ 0, 0, "tfc_concuss",				true, 1, V(Void_Entvar) },		// TFC_Concuss

	/* Natural-Selection */
	{ 0, 0, "ns_getpointvalue",			false, 0, V(Int_Void) },						// NS_GetPointValue
	{ 0, 0, "ns_awardkill",				true, 1, V(Void_Entvar) },		// NS_AwardKill
	{ 0, 0, "ns_resetentity",			true, 0, V(Void_Void) },							// NS_ResetEntity
	{ 0, 0, "ns_updateonremove",		true, 0, V(Void_Void) },							// NS_UpdateOnRemove

};


void FailPlugin(AMX *amx, int id, int err, const char *reason)
{
	int fwd=MF_RegisterSPForwardByName(amx, "__fatal_ham_error", FP_CELL, FP_CELL, FP_STRING, FP_DONE);

	MF_ExecuteForward(fwd, id, err, reason);

	MF_UnregisterSPForward(fwd);
}
static cell AMX_NATIVE_CALL RegisterHam(AMX *amx, cell *params)
{
	// Make sure the function we're requesting is within bounds
	int func=params[1];
	int post=params[4];

	CHECK_FUNCTION(func);

	char *function=MF_GetAmxString(amx, params[2], 0, NULL);
	char *classname=MF_GetAmxString(amx, params[3], 1, NULL);
	
	// Check the entity

	// create an entity, assign it the gamedll's class, hook it and destroy it
	edict_t *Entity=CREATE_ENTITY();

	CALL_GAME_ENTITY(PLID,classname,&Entity->v);

	if (Entity->pvPrivateData == NULL)
	{
		REMOVE_ENTITY(Entity);

		MF_LogError(amx, AMX_ERR_NATIVE,"Failed to retrieve classtype for \"%s\", hook for \"%s\" not active.",classname,function);

		return 0;
	}
	void **vtable=GetVTable(Entity->pvPrivateData, Offsets.GetBase());

	REMOVE_ENTITY(Entity);

	if (vtable == NULL)
	{
		MF_LogError(amx, AMX_ERR_NATIVE,"Failed to retrieve vtable for \"%s\", hook for \"%s\" not active.",classname,function);

		return 0;
	}

	// Verify that the function is valid
	// Don't fail the plugin if this fails, just emit a normal error
	int fwd=hooklist[func].makefunc(amx, function);

	printf("\n\n----> FORWARD = %d\n\n\n",fwd);
	if (fwd == -1)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Function %s not found.", function);

		return 0;
	}

	// We've passed all tests...

	int **ivtable=(int **)vtable;

	void *vfunction=(void *)ivtable[hooklist[func].vtid];

	// Check the list of this function's hooks, see if the function we have is a hook

	CVector<Hook *>::iterator end=hooks[func].end();
	for (CVector<Hook *>::iterator i=hooks[func].begin();
		 i!=end;
		 ++i)
	{
		if ((*i)->tramp == vfunction)
		{
			// Yes, this function is hooked
			if (post)
			{
				(*i)->post.push_back(new Forward(fwd));
			}
			else
			{
				(*i)->pre.push_back(new Forward(fwd));
			}
			return 1;
		}
	}

	// If we got here, the function is not hooked
	Hook *hook=new Hook(vtable, hooklist[func].vtid, hooklist[func].targetfunc, hooklist[func].isvoid, hooklist[func].paramcount, classname);
	hooks[func].push_back(hook);

	if (post)
	{
		hook->post.push_back(new Forward(fwd));
	}
	else
	{
		hook->pre.push_back(new Forward(fwd));
	}

	return 1;
}
static cell AMX_NATIVE_CALL ExecuteHam(AMX *amx, cell *params)
{
	int func=params[1];

	CHECK_FUNCTION(func);

	return hooklist[func].call(amx, params);
}
static cell AMX_NATIVE_CALL ExecuteHamB(AMX *amx, cell *params)
{
	int func=params[1];

	CHECK_FUNCTION(func);

	return hooklist[func].ecall(amx, params);
}


static cell AMX_NATIVE_CALL IsHamValid(AMX *amx, cell *params)
{
	int func=params[1];

	if (func >= 0 && 
		func < HAM_LAST_ENTRY_DONT_USE_ME_LOL &&
		hooklist[func].isset!=0)
	{
		return 1;
	}
	return 0;
}

AMX_NATIVE_INFO RegisterNatives[] =
{
	{ "RegisterHam",			RegisterHam },
	{ "ExecuteHam",				ExecuteHam },
	{ "ExecuteHamB",			ExecuteHamB },
	{ "IsHamValid",				IsHamValid },

	{ NULL,						NULL }
};
