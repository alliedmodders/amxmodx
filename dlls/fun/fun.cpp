/* AMX Mod X
*   Fun Module
*
* by the AMX Mod X Development Team
*
* This file is part of AMX Mod X.
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "fun.h"

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
		if (params[1] < 1 || params[1] > gpGlobals->maxClients
		|| params[2] < 1 || params[2] > gpGlobals->maxClients)
		{
			MF_RaiseAmxError(amx, AMX_ERR_NATIVE);				<--- Call this, it will end up as RUN TIME ERROR 10 in server console.
			return 0;											<--- Standard return 0 with run time errors? (note in small only 0 returns false, everything else returns true)
		}

		// Get * pointer.
		edict_t *pPlayer = INDEXENT(params[1]);

		if (FNullEnt(pPlayer)) {								<--- Test this pointer this way, return 0...
			MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
			return 0;
		}

		return 1												<--- If native succeeded, return 1, if the native isn't supposed to return a specific value.		
																Note: Should be able to do: if (thenative()) and it should return false when it fails, and true when succeeds... is -1 treated as false, or is 0 a must?
	}
*/

// ######## Utils:
void FUNUTIL_ResetPlayer(int index)
{
	g_zones_toHit[index] = (1<<HITGROUP_GENERIC) | 
						(1<<HITGROUP_HEAD) | 
						(1<<HITGROUP_CHEST) | 
						(1<<HITGROUP_STOMACH) | 
						(1<<HITGROUP_LEFTARM) | 
						(1<<HITGROUP_RIGHTARM)| 
						(1<<HITGROUP_LEFTLEG) | 
						(1<<HITGROUP_RIGHTLEG); 	 
	g_zones_getHit[index] = (1<<HITGROUP_GENERIC) | 
						(1<<HITGROUP_HEAD) |
						(1<<HITGROUP_CHEST) | 
						(1<<HITGROUP_STOMACH) | 
						(1<<HITGROUP_LEFTARM) | 
						(1<<HITGROUP_RIGHTARM)| 
						(1<<HITGROUP_LEFTLEG) | 
						(1<<HITGROUP_RIGHTLEG); 
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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients
	|| params[2] < 1 || params[2] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients
	|| params[2] < 1 || params[2] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Get player pointer.
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Get player pointer.
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Get player pointer.
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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

    if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
    {
        MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
        return 0;
    }
	edict_t *pEnt = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pEnt)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	MDLL_Spawn(pEnt);

	return 1;
}

static cell AMX_NATIVE_CALL set_user_health(AMX *amx, cell *params) // set_user_health(index, health); = 2 arguments
{
	// Sets user health. If health is 0 and below, also kill...
	// params[1] = index
	// params[2] = health

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.frags = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_armor(AMX *amx, cell *params) // set_user_armor(index, armor); = 2 arguments
{
	// Sets user armor.
	// params[1] = index
	// params[2] = armor

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.armorvalue = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL set_user_origin(AMX *amx, cell *params) // set_user_origin(index, origin[3]); = 2 arguments
{
	// Sets user origin.
	// params[1] = index
	// params[2] = origin

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	SETCLIENTMAXSPEED(pPlayer, fNewSpeed);
	pPlayer->v.maxspeed = fNewSpeed;

	return 1;
}

static cell AMX_NATIVE_CALL get_user_maxspeed(AMX *amx, cell *params) // Float:get_user_maxspeed(index) = 1 argument
{
	// Gets user maxspeed.
	// params[1] = index

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return amx_ftoc(pPlayer->v.maxspeed);
}

static cell AMX_NATIVE_CALL set_user_gravity(AMX *amx, cell *params) // set_user_gravity(index, Float:gravity = 1.0) = 2 arguments
{
	// Sets user gravity.
	// params[1] = index
	// params[2] = gravity (=-1.0)
	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	pPlayer->v.gravity = amx_ctof(params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL get_user_gravity(AMX *amx, cell *params) // Float:get_user_gravity(index) = 1 argument
{
	// Gets user gravity.
	// params[1] = index

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return amx_ftoc(pPlayer->v.gravity); 
}

static cell AMX_NATIVE_CALL set_user_hitzones(AMX *amx, cell *params) // set_user_hitzones(index = 0, target = 0, body = 255); = 3 arguments
{
	// Sets user hitzones.
	// params[1] = the one(s) who shoot(s), shooter
	int shooter = params[1];
	if (shooter == -1)
		shooter = 0;
	// params[2] = the one getting hit
	int gettingHit = params[2];
	if (gettingHit == -1)
		gettingHit = 0;
	// params[3] = specified hit zones
	int hitzones = params[3];
	if (hitzones == -1)
		hitzones = 255;

	//set_user_hitzones(id, 0, 0) // Makes ID not able to shoot EVERYONE - id can shoot on 0 (all) at 0
	//set_user_hitzones(0, id, 0) // Makes EVERYONE not able to shoot ID - 0 (all) can shoot id at 0
	if (shooter == 0 && gettingHit == 0) {
		// set hitzones for ALL, both where people can hit and where they can _get_ hit.
		for (int i = 1; i <= 32; i++) {
			g_zones_toHit[i] = hitzones;
			g_zones_getHit[i] = hitzones;
		}
	}
	else {
		if (shooter == 0) {
			// "All" shooters, target (gettingHit) should be existing player id
			if (gettingHit < 1 || gettingHit > gpGlobals->maxClients) {
				MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
				return 0;
			}
			// Where can gettingHit get hit by all?
			g_zones_getHit[gettingHit] = hitzones;
		}
		else {
			// "shooter" will now only be able to hit other people in "hitzones". (target should be 0 here)
			g_zones_toHit[shooter] = hitzones;
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL get_user_hitzones(AMX *amx, cell *params) // get_user_hitzones(index, target); = 2 arguments
{
	// Gets user hitzones.
	// params[1] = if this is not 0, return what zones this player can hit
	int shooter = params[1];
	// params[2] = if shooter was 0, and if this is a player, return what zones this player can get hit in, else... make runtime error?
	int gettingHit = params[2];

	if (shooter) {
		if (FNullEnt(shooter)) {
			MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
			return 0;
		}

		return g_zones_toHit[shooter];
	}
	else {
		if (!gettingHit) {
			MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
			return 0;
		}
		else {
			if (FNullEnt(gettingHit)) {
				MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
				return 0;
			}
			else {
				return g_zones_getHit[gettingHit];
			}
		}
	}
}

static cell AMX_NATIVE_CALL set_user_noclip(AMX *amx, cell *params) // set_user_noclip(index, noclip = 0); = 2 arguments
{
	// Sets user to no clipping mode.
	// params[1] = index
	// params[2] = no clip or not...

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check validity.
	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check validity.
	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

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
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check validity.
	if (FNullEnt(pPlayer)) {
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
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

// SidLuke
static cell AMX_NATIVE_CALL strip_user_weapons(AMX *amx, cell *params) { // index 
	if (!MF_IsPlayerIngame(params[1]))
	{
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pPlayer = INDEXENT(params[1]);

	string_t item = MAKE_STRING("trigger_once");
	edict_t *pent = CREATE_NAMED_ENTITY( item );
	if ( FNullEnt( pent ) ){
		return 0;
	}

	KeyValueData pkvd;

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "target"; // type
	pkvd.szValue = "stripper";
	pkvd.fHandled = false;
	MDLL_KeyValue(pent, &pkvd);

	MDLL_Spawn(pent);

	item = MAKE_STRING("player_weaponstrip");
	edict_t   *pent2 = CREATE_NAMED_ENTITY( item );
	if ( FNullEnt( pent2 ) ) {
		return 0;
	}

	pkvd.szClassName = (char *)STRING(pent->v.classname);
	pkvd.szKeyName = "targetname"; // type
	pkvd.szValue =  "stripper";
	pkvd.fHandled = false;
	MDLL_KeyValue(pent2, &pkvd);

	MDLL_Spawn(pent2);

	pent->v.origin = pPlayer->v.origin;

	MDLL_Touch(pent, pPlayer);

	REMOVE_ENTITY(pent);
	REMOVE_ENTITY(pent2);

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

void TraceLine(const float *v1, const float *v2, int fNoMonsters, edict_t *pentToSkip, TraceResult *ptr) {
	TRACE_LINE(v1, v2, fNoMonsters, pentToSkip, ptr); // pentToSkip gotta be the one that is shooting, so filter it

	int hitIndex = ENTINDEX(ptr->pHit);
	if (hitIndex >= 1 && hitIndex <= gpGlobals->maxClients) {
		if ( !(
			g_zones_getHit[hitIndex] & (1 << ptr->iHitgroup) // can ptr->pHit get hit in ptr->iHitgroup at all?
		&&	g_zones_toHit[hitIndex] & (1 << ptr->iHitgroup) ) // can pentToSkip hit other people in that hit zone?
		) {
			ptr->flFraction = 1.0;	// set to not hit anything (1.0 = shot doesn't hit anything)
		}
	}

	RETURN_META(MRES_SUPERCEDE);
}

void OnAmxxAttach()
{
	MF_AddNatives(fun_Exports);

	// Reset stuff - hopefully this should
	for (int i = 1; i <= 32; i++) {
		// Reset all hitzones
		FUNUTIL_ResetPlayer(i);
	}
}

/*
void ClientConnectFakeBot(int index)
{
	FUNUTIL_ResetPlayer(index);
	//MF_Log("A bot connects, forwarded to fun! The bot is %d!", index);
	//CPlayer* player;
}
*/
