// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Fun Module
//

#include <string.h>
#include "fun.h"
#include <HLTypeConversion.h>

/*
	JGHG says:

	Ok this is what I use below, it may probably not be right with all natives etc but I try to maintain this style to natives.
	Note that this is still very much subject to change, regarding return values etc!
	(Ok I haven't checked all natives that they comply with this yet, this is just a model I'm working on and which I might implement soon.)

	static cell AMX_NATIVE_CALL nativename(AMX *amx, cell *params) // nativename(argument1, argument2); = 2 params
	{
		// Description what this native does.					<--- Description what this native does
		// params[1] = argument1								<--- Description of each argument, so we don't have to allocate new variables and can
		// params[2] = argument2								<--- use the ones in params[n] directly, to save some time.

		// Check receiver and sender validity.					<--- Check ents, maybe need to do this better and more proper later?
		CHECK_PLAYER(params[1])
		CHECK_PLAYER(params[2])

		// Get * pointer.
		edict_t *pPlayer = MF_GetPlayerEdict(params[1]);		<--- Players require a different function than INDEXENT because of an HLSDK bug

		return 1												<--- If native succeeded, return 1, if the native isn't supposed to return a specific value.		
																Note: Should be able to do: if (thenative()) and it should return false when it fails, and true when succeeds... is -1 treated as false, or is 0 a must?
	}
*/

char g_bodyhits[33][33];	// where can the guy in the first dimension hit the people in the 2nd dimension? :-)
bool g_silent[33];			// used for set_user_footsteps()

HLTypeConversion TypeConversion;

// ######## Utils:
void FUNUTIL_ResetPlayer(int index)
{
	//MF_PrintSrvConsole("Resetting player index %d! maxclients: %d\n", index, gpGlobals->maxClients);
	for (int i = 1; i <= gpGlobals->maxClients; i++) {
		g_bodyhits[index][i] = (char)((1<<HITGROUP_GENERIC) | 
						(1<<HITGROUP_HEAD) | 
						(1<<HITGROUP_CHEST) | 
						(1<<HITGROUP_STOMACH) | 
						(1<<HITGROUP_LEFTARM) | 
						(1<<HITGROUP_RIGHTARM)| 
						(1<<HITGROUP_LEFTLEG) | 
						(1<<HITGROUP_RIGHTLEG));
	}
	// Reset silent slippers
	g_silent[index] = false;
}

// ######## Natives:
static cell AMX_NATIVE_CALL get_client_listening(AMX *amx, cell *params) // get_client_listening(receiver, sender); = 2 params
{
	// Gets who can listen to who.
	// params[1] = receiver
	// params[2] = sender

	// Check receiver and sender validity.
	CHECK_PLAYER(params[1]);
	CHECK_PLAYER(params[2]);

	// GET- AND SETCLIENTLISTENING returns "qboolean", an int, probably 0 or 1...
	return GETCLIENTLISTENING(params[1], params[2]);
}

static cell AMX_NATIVE_CALL set_client_listening(AMX *amx, cell *params) // set_client_listening(receiver, sender, listen); = 3 params
{
	// Sets who can listen to who.
	// params[1] = receiver
	// params[2] = sender
	// params[3] = listen

	// Check receiver and sender validity.
	CHECK_PLAYER(params[1]);
	CHECK_PLAYER(params[2]);

	// Make a check on params[3] here later, and call run time error when it's wrong.
	// To do: find out the possible values to set (0, 1?)

	// GET- AND SETCLIENTLISTENING returns "qboolean", an int, probably 0 or 1...
	return SETCLIENTLISTENING(params[1], params[2], params[3]);
}

static cell AMX_NATIVE_CALL set_user_godmode(AMX *amx, cell *params) // set_user_godmode(index, godmode = 0); = 2 params
{
	/* Sets player godmode. If you want to disable godmode set only first parameter. */
	// params[1] = index
	// params[2] = godmode = 0

	// Check index.
	CHECK_PLAYER(params[1]);

	// Get player pointer.
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	if (params[2] == 1) {
		// Enable godmode
		pPlayer->v.takedamage = 0.0;	// 0.0, the player doesn't seem to be able to get hurt.
	}
	else {
		// Disable godmode
		pPlayer->v.takedamage = 2.0;	// 2.0 seems to be standard value?
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_godmode(AMX *amx, cell *params) // get_user_godmode(index); = 1 param
{
	/* Returns 1 if godmode is set. */
	// params[1] = index

	// Check index.
	CHECK_PLAYER(params[1]);

	// Get player pointer.
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	int godmode = 0;

	if (pPlayer->v.takedamage == 0.0) {
		// God mode is enabled
		godmode = 1;
	}

	return godmode;
}

static cell AMX_NATIVE_CALL give_item(AMX *amx, cell *params) // native give_item(index, const item[]); = 2 params
{
	/* Gives item to player, name of item can start
	* with weapon_, ammo_ and item_. This event
	* is announced with proper message to all players. */
	// params[1] = index
	// params[2] = item...

	// Check index.
	CHECK_PLAYER(params[1]);

	// Get player pointer.
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	// Create item entity pointer
	edict_t	*pItemEntity;

	// Make an "intstring" out of 2nd parameter
	int length;
	const char *szItem = MF_GetAmxString(amx, params[2], 1, &length);

	//check for valid item
	if (strncmp(szItem, "weapon_", 7) && 
		strncmp(szItem, "ammo_", 5) && 
		strncmp(szItem, "item_", 5) &&
		strncmp(szItem, "tf_weapon_", 10)
	) {
		return 0;
	}

	//string_t item = MAKE_STRING(szItem);
	string_t item = ALLOC_STRING(szItem); // Using MAKE_STRING makes "item" contents get lost when we leave this scope! ALLOC_STRING seems to allocate properly...
	// Create the entity, returns to pointer
	pItemEntity = CREATE_NAMED_ENTITY(item);

	if (FNullEnt(pItemEntity)) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Item \"%s\" failed to create", szItem);
		return 0;
	}

	//VARS(pItemEntity)->origin = VARS(pPlayer)->origin; // nice to do VARS(ent)->origin instead of ent->v.origin? :-I
	//I'm not sure, normally I use macros too =P
	pItemEntity->v.origin = pPlayer->v.origin;
	pItemEntity->v.spawnflags |= SF_NORESPAWN;	//SF_NORESPAWN;

	MDLL_Spawn(pItemEntity);

	int save = pItemEntity->v.solid;

	MDLL_Touch(pItemEntity, ENT(pPlayer));

	//The problem with the original give_item was the
	// item was not removed.  I had tried this but it
	// did not work.  OLO's implementation is better.
	/*
	int iEnt = ENTINDEX(pItemEntity->v.owner);
	if (iEnt > 32 || iEnt <1 ) {
		MDLL_Think(pItemEntity);
	}*/

	if (pItemEntity->v.solid == save) {
		REMOVE_ENTITY(pItemEntity);
		//the function did not fail - we're just deleting the item
		return -1;
	}

	return ENTINDEX(pItemEntity);
}

static cell AMX_NATIVE_CALL spawn(AMX *amx, cell *params) // spawn(id) = 1 param
{
	// Spawns an entity, this can be a user/player -> spawns at spawnpoints, or created entities seems to need this as a final "kick" into the game? :-)
	// params[1] = entity to spawn

	CHECK_ENTITY(params[1]);

	edict_t *pEnt = TypeConversion.id_to_edict(params[1]);

	MDLL_Spawn(pEnt);

	return 1;
}

static cell AMX_NATIVE_CALL set_user_health(AMX *amx, cell *params) // set_user_health(index, health); = 2 arguments
{
	// Sets user health. If health is 0 and below, also kill...
	// params[1] = index
	// params[2] = health

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	// Kill if health too low.
	if (params[2] > 0)
		pPlayer->v.health = float(params[2]);
	else
		MDLL_ClientKill(pPlayer);

	return 1;
}

static cell AMX_NATIVE_CALL set_user_frags(AMX *amx, cell *params) // set_user_frags(index, frags); = 2 arguments
{
	// Sets user frags.
	// params[1] = index
	// params[2] = frags

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	pPlayer->v.frags = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_armor(AMX *amx, cell *params) // set_user_armor(index, armor); = 2 arguments
{
	// Sets user armor.
	// params[1] = index
	// params[2] = armor

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	pPlayer->v.armorvalue = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_origin(AMX *amx, cell *params) // set_user_origin(index, origin[3]); = 2 arguments
{
	// Sets user origin.
	// params[1] = index
	// params[2] = origin

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	cell *newVectorCell = MF_GetAmxAddr(amx, params[2]);

	SET_SIZE(pPlayer, pPlayer->v.mins, pPlayer->v.maxs);
	SET_ORIGIN(pPlayer, Vector((float)newVectorCell[0], (float)newVectorCell[1], (float)newVectorCell[2]));
	
	return 1;
}

static cell AMX_NATIVE_CALL set_user_rendering(AMX *amx, cell *params) // set_user_rendering(index, fx = kRenderFxNone, r = 255, g = 255, b = 255, render = kRenderNormal, amount = 16); = 7 arguments
{
	// Sets user rendering.
	// params[1] = index
	// params[2] = fx
	// params[3] = r
	// params[4] = g
	// params[5] = b
	// params[6] = render
	// params[7] = amount

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	pPlayer->v.renderfx = params[2];
	Vector newVector = Vector(float(params[3]), float(params[4]), float(params[5]));
	pPlayer->v.rendercolor = newVector;
	pPlayer->v.rendermode = params[6];
	pPlayer->v.renderamt = params[7];
	
	return 1;
}


static cell AMX_NATIVE_CALL set_user_maxspeed(AMX *amx, cell *params) // set_user_maxspeed(index, Float:speed = -1.0) = 2 arguments
{
	// Sets user maxspeed.
	// params[1] = index
	// params[2] = speed (should be -1.0 if not specified) (JGHG: unspecified parameters seems to always be -1.0!)

	REAL fNewSpeed = amx_ctof(params[2]);

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	SETCLIENTMAXSPEED(pPlayer, fNewSpeed);
	pPlayer->v.maxspeed = fNewSpeed;

	return 1;
}

static cell AMX_NATIVE_CALL get_user_maxspeed(AMX *amx, cell *params) // Float:get_user_maxspeed(index) = 1 argument
{
	// Gets user maxspeed.
	// params[1] = index

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	return amx_ftoc(pPlayer->v.maxspeed);
}

static cell AMX_NATIVE_CALL set_user_gravity(AMX *amx, cell *params) // set_user_gravity(index, Float:gravity = 1.0) = 2 arguments
{
	// Sets user gravity.
	// params[1] = index
	// params[2] = gravity (=-1.0)
	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	pPlayer->v.gravity = amx_ctof(params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL get_user_gravity(AMX *amx, cell *params) // Float:get_user_gravity(index) = 1 argument
{
	// Gets user gravity.
	// params[1] = index

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	return amx_ftoc(pPlayer->v.gravity); 
}

static cell AMX_NATIVE_CALL set_user_hitzones(AMX *amx, cell *params) // set_user_hitzones(index = 0, target = 0, body = 255); = 3 arguments
{
	// Sets user hitzones.
	// params[1] = the one(s) who shoot(s), shooter
	int shooter = params[1];

	// params[2] = the one getting hit
	int gettingHit = params[2];

	// params[3] = specified hit zones
	int hitzones = params[3];

	//set_user_hitzones(id, 0, 0) // Makes ID not able to shoot EVERYONE - id can shoot on 0 (all) at 0
	//set_user_hitzones(0, id, 0) // Makes EVERYONE not able to shoot ID - 0 (all) can shoot id at 0
	if (shooter == 0 && gettingHit == 0) {
		for (int i = 1; i <= gpGlobals->maxClients; i++) {
			for (int j = 1; j <= gpGlobals->maxClients; j++) {
				g_bodyhits[i][j] = hitzones;
			}
			//g_zones_toHit[i] = hitzones;
			//g_zones_getHit[i] = hitzones;
		}
	}
	else if (shooter == 0 && gettingHit != 0) {
		// "All" shooters, target (gettingHit) should be existing player id
		CHECK_PLAYER(gettingHit);
		// Where can all hit gettingHit?
		for (int i = 1; i <= gpGlobals->maxClients; i++)
			g_bodyhits[i][gettingHit] = hitzones;
	}
	else if (shooter != 0 && gettingHit == 0) {
		// Shooter can hit all in bodyparts.
		CHECK_PLAYER(shooter);
		for (int i = 1; i <= gpGlobals->maxClients; i++)
			g_bodyhits[shooter][i] = hitzones;
	}
	else {
		// Specified, where can player A hit player B?
		CHECK_PLAYER(shooter);
		CHECK_PLAYER(gettingHit);
		g_bodyhits[shooter][gettingHit] = hitzones;
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_hitzones(AMX *amx, cell *params) // get_user_hitzones(index, target); = 2 arguments
{
	int shooter = params[1];
	CHECK_PLAYER(shooter);
	int target = params[2];
	CHECK_PLAYER(target);
	return g_bodyhits[shooter][target];
}

static cell AMX_NATIVE_CALL set_user_noclip(AMX *amx, cell *params) // set_user_noclip(index, noclip = 0); = 2 arguments
{
	// Sets user to no clipping mode.
	// params[1] = index
	// params[2] = no clip or not...

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	if (params[2] == 1)
		pPlayer->v.movetype = MOVETYPE_NOCLIP;
	else
		pPlayer->v.movetype = MOVETYPE_WALK;

	return 1;
}

static cell AMX_NATIVE_CALL get_user_noclip(AMX *amx, cell *params) // get_user_noclip(index); = 1 argument
{
	// Gets user noclip.
	// params[1] = index

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	return pPlayer->v.movetype == MOVETYPE_NOCLIP;
}

// JustinHoMi made this one originally
static cell AMX_NATIVE_CALL set_user_footsteps(AMX *amx, cell *params) // set_user_footsteps(id, set = 1); = 2 params
{
	// Gives player silent footsteps.
	// if set=0 it will return footsteps to normal
	// params[1] = index of player
	// params[2] = 0 = normal footstep sound, 1 = silent slippers

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = TypeConversion.id_to_edict(params[1]);

	if (params[2]) {
		pPlayer->v.flTimeStepSound = 999;
		g_silent[params[1]] = true;
	}
	else {
		pPlayer->v.flTimeStepSound = STANDARDTIMESTEPSOUND;
		g_silent[params[1]] = false;
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_footsteps(AMX *amx, cell *params)
{
	CHECK_PLAYER(params[1]);

	return g_silent[params[1]];
}

// SidLuke
static cell AMX_NATIVE_CALL strip_user_weapons(AMX *amx, cell *params) // index
{
	CHECK_PLAYER(params[1]);

	edict_t* pPlayer = TypeConversion.id_to_edict(params[1]);

	string_t item = MAKE_STRING("player_weaponstrip");
	edict_t *pent = CREATE_NAMED_ENTITY(item);

	if (FNullEnt(pent))
	{
		return 0;
	}

	MDLL_Spawn(pent);
	MDLL_Use(pent, pPlayer);
	REMOVE_ENTITY(pent);

	*reinterpret_cast<int *>(MF_PlayerPropAddr(params[1], Player_CurrentWeapon)) = 0;

	return 1;
}

AMX_NATIVE_INFO fun_Exports[] = {
	{"get_client_listen",		get_client_listening},
	{"set_client_listen",		set_client_listening},
	{"set_user_godmode",		set_user_godmode},
	{"get_user_godmode",		get_user_godmode},
	{"set_user_health",			set_user_health},
	{"give_item",				give_item},
	{"spawn",					spawn},
	{"set_user_frags",			set_user_frags},
	{"set_user_armor",			set_user_armor},
	{"set_user_origin",			set_user_origin},
	{"set_user_rendering",		set_user_rendering},
	{"set_user_maxspeed",		set_user_maxspeed},
	{"get_user_maxspeed",		get_user_maxspeed},
	{"set_user_gravity",		set_user_gravity},
	{"get_user_gravity",		get_user_gravity},
	{"get_user_footsteps",		get_user_footsteps},
	{"set_user_hitzones",		set_user_hitzones},
	{"get_user_hitzones",		get_user_hitzones},
	{"set_user_noclip",			set_user_noclip},
	{"get_user_noclip",			get_user_noclip},
	{"set_user_footsteps",		set_user_footsteps},
	{"strip_user_weapons",		strip_user_weapons},
	  /////////////////// <--- 19 chars max in current small version
	{NULL,					NULL}
};

/******************************************************************************************/
void PlayerPreThink(edict_t *pEntity)
{
	if (g_silent[ENTINDEX(pEntity)]) {
		pEntity->v.flTimeStepSound = 999; 
		RETURN_META(MRES_HANDLED);
	}

	RETURN_META(MRES_IGNORED);
}

int ClientConnect(edict_t *pPlayer, const char *pszName, const char *pszAddress, char szRejectReason[128])
{
	// Reset stuff:
	FUNUTIL_ResetPlayer(ENTINDEX(pPlayer));

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *shooter, TraceResult *ptr) {
	TRACE_LINE(v1, v2, fNoMonsters, shooter, ptr);
	if ( ptr->pHit && (ptr->pHit->v.flags& (FL_CLIENT | FL_FAKECLIENT))
	&& shooter && (shooter->v.flags & (FL_CLIENT | FL_FAKECLIENT)) ) {
		int shooterIndex = ENTINDEX(shooter);
		if ( !(g_bodyhits[shooterIndex][ENTINDEX(ptr->pHit)] & (1<<ptr->iHitgroup)) )
			ptr->flFraction = 1.0;
	}
	RETURN_META(MRES_SUPERCEDE);
}


//int g_hitIndex, g_canTargetGetHit, g_canShooterHitThere;
//void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *shooter, TraceResult *ptr) {
//	if (!pentToSkip || (pentToSkip->v.flags & (FL_CLIENT | FL_FAKECLIENT)) == false || pentToSkip->v.deadflag != DEAD_NO)
//		RETURN_META(MRES_IGNORED);
//
//	TRACE_LINE(v1, v2, fNoMonsters, shooter, ptr); // Filter shooter
//
//	if (!ptr->pHit || (ptr->pHit->v.flags & (FL_CLIENT | FL_FAKECLIENT)) == false )
//		RETURN_META(MRES_SUPERCEDE);
//
//	g_hitIndex = ENTINDEX(ptr->pHit);
//	//bool blocked = false;
//	g_canTargetGetHit = g_zones_getHit[g_hitIndex] & (1 << ptr->iHitgroup);
//	g_canShooterHitThere = g_zones_toHit[ENTINDEX(shooter)] & (1 << ptr->iHitgroup);
//
//	if (!g_canTargetGetHit || !g_canShooterHitThere) {
//		ptr->flFraction = 1.0;	// set to not hit anything (1.0 = shot doesn't hit anything)
//		//blocked = true;
//	}
//	/*
//	if (blocked) {
//		MF_PrintSrvConsole("%s was blocked from hitting %s: %d and %d\n", MF_GetPlayerName(ENTINDEX(pentToSkip)), MF_GetPlayerName(hitIndex), canTargetGetHit, canShooterHitThere);
//	}
//	else {
//		MF_PrintSrvConsole("%s was NOT blocked from hitting %s: %d and %d\n", MF_GetPlayerName(ENTINDEX(pentToSkip)), MF_GetPlayerName(hitIndex), canTargetGetHit, canShooterHitThere);
//	}
//	*/
//
//	RETURN_META(MRES_SUPERCEDE);
//}

void OnAmxxAttach()
{
	MF_AddNatives(fun_Exports);
}

// The content of OnPluginsLoaded() was moved from OnAmxxAttach with AMXx 1.5 because for some reason gpGlobals->maxClients wasn't
// initialized to its proper value until some time after OnAmxxAttach(). In OnAmxxAttach() it always showed 0. /JGHG
void OnPluginsLoaded() {
	// Reset stuff - hopefully this should
	for (int i = 1; i <= gpGlobals->maxClients; i++) {
		// Reset all hitzones
		FUNUTIL_ResetPlayer(i);
	}

	TypeConversion.init();
}
/*
void ClientConnectFakeBot(int index)
{
	FUNUTIL_ResetPlayer(index);
	//MF_Log("A bot connects, forwarded to fun! The bot is %d!", index);
	//CPlayer* player;
}
*/
