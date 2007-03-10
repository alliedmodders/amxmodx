#if defined _hamsandwich_included
  #endinput
#endif
#define _hamsandwich_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib hamsandwich
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib hamsandwich
 #endif
#else
 #pragma library hamsandwich
#endif

#if !defined _amxmodx_included
	#include <amxmodx>
#endif

native ham_addplayeritem(idPlayer,idItem);
native ham_eaddplayeritem(idPlayer,idItem);

native ham_addpoints(idEntity,points,bool:allownegative);
native ham_eaddpoints(idEntity,points,bool:allownegative);

native ham_addpointstoteam(idEntity,points,bool:allownegative);
native ham_eaddpointstoteam(idEntity,points,bool:allownegative);

native ham_blocked(idEntity,idOther);
native ham_eblocked(idEntity,idOther);

native ham_bloodcolor(idEntity);
native ham_ebloodcolor(idEntity);

native ham_classify(idEntity);
native ham_eclassify(idEntity);

native ham_gettogglestate(idEntity);
native ham_egettogglestate(idEntity);

native ham_isalive(idEntity);
native ham_eisalive(idEntity);

native ham_isbspmodel(idEntity);
native ham_eisbspmodel(idEntity);

native ham_isinworld(idEntity);
native ham_eisinworld(idEntity);

native ham_isnetclient(idEntity);
native ham_eisnetclient(idEntity);

native ham_isplayer(idEntity);
native ham_eisplayer(idEntity);

native ham_issneaking(idEntity);
native ham_eissneaking(idEntity);

native ham_killed(idEntity,idAttacker,iGib);
native ham_ekilled(idEntity,idAttacker,iGib);

native ham_objectcaps(idEntity);
native ham_eobjectcaps(idEntity);

native ham_removeplayeritem(idEntity,idItem);
native ham_eremoveplayeritem(idEntity,idItem);

native ham_respawn(idEntity);
native ham_erespawn(idEntity);

native ham_restart(idEntity);
native ham_erestart(idEntity);

native ham_takedamage(idEntity,idInflictor,idAttacker,Float:damage,dmgtype);
native ham_etakedamage(idEntity,idInflictor,idAttacker,Float:damage,dmgtype);

native ham_takehealth(idEntity,Float:health,dmgtype);
native ham_etakehealth(idEntity,Float:health,dmgtype);

native ham_think(idEntity);
native ham_ethink(idEntity);

native ham_touch(idEntity,idOther);
native ham_etouch(idEntity,idOther);

native ham_use(idEntity,idActivator,idCaller,use_type,Float:value);
native ham_euse(idEntity,idActivator,idCaller,use_type,Float:value);



enum 
{
	HAM_UNSET = 0,
	HAM_IGNORED,
	HAM_HANDLED,
	HAM_OVERRIDE,
	HAM_SUPERCEDE
};



enum HAMHooks
{
	HAM_TakeDamage,
	HAM_Use,
	HAM_AddPoints,
	HAM_AddPointsToTeam,
	HAM_Blocked,
	HAM_Killed,
	HAM_Respawn,
	HAM_Restart,
	HAM_TakeHealth,
	HAM_AddPlayerItem,
	HAM_RemovePlayerItem,
	HAM_BloodColor,
	HAM_Classify,
	HAM_GetToggleState,
	HAM_IsAlive,
	HAM_IsBSPModel,
	HAM_IsInWorld,
	HAM_IsMoving,
	HAM_IsNetClient,
	HAM_IsPlayer,
	HAM_IsSneaking,
	HAM_ObjectCaps,
	HAM_Think,
	HAM_Touch,


	HAM_END_DONT_USE_ME
};

native ham_register(HAMHooks:hook, const classname[], const function[], post=0);

public __fatal_ham_error(const reason[])
{
	set_fail_state(reason);
}