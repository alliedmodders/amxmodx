#include "cstrike.h"

/* AMX Mod X 
   *   Counter-Strike Module 
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

// Utils first

bool UTIL_IsPlayer(AMX* amx, edict_t* pPlayer) {
	bool player = false;
	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	if (strcmp(STRING(pPlayer->v.classname), "player") == 0)
		player = true;

	return player;
}

void UTIL_TextMsg_Generic(edict_t* pPlayer, char* message)
{
	MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "TextMsg", NULL), NULL, pPlayer);
	WRITE_BYTE(HUD_PRINTCENTER); // 1 = console, 2 = console, 3 = chat, 4 = center
	WRITE_STRING(message);
	MESSAGE_END();
	/*
	The byte above seems to use these:
	#define HUD_PRINTNOTIFY		1
	#define HUD_PRINTCONSOLE	2
	#define HUD_PRINTTALK		3
	#define HUD_PRINTCENTER		4
	However both 1 and 2 seems to go to console with Steam CS.
	*/
}

// Then natives

static cell AMX_NATIVE_CALL cs_set_user_money(AMX *amx, cell *params) // cs_set_user_money(index, money, flash = 1); = 3 arguments
{
	// Give money to user
	// params[1] = user
	// params[2] = money
	// params[3] = flash money?

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Give money
	*((int *)pPlayer->pvPrivateData + OFFSET_CSMONEY) = params[2];

	// Update display
	MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "Money", NULL), NULL, pPlayer);
	WRITE_LONG(params[2]);
	WRITE_BYTE(params[3] ? 1 : 0); // if params[3] is 0, there will be no +/- flash of money in display...
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_money(AMX *amx, cell *params) // cs_get_user_money(index); = 1 argument
{
	// Give money to user
	// params[1] = user

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Return money
	return (int)*((int *)pPlayer->pvPrivateData + OFFSET_CSMONEY);
}

static cell AMX_NATIVE_CALL cs_get_user_deaths(AMX *amx, cell *params) // cs_get_user_deaths(index); = 1 param
{
	// Gets user deaths in cs.
	// params[1] = user

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return *((int *)pPlayer->pvPrivateData + OFFSET_CSDEATHS);
}

static cell AMX_NATIVE_CALL cs_set_user_deaths(AMX *amx, cell *params) // cs_set_user_deaths(index, newdeaths); = 2 arguments
{
	// Sets user deaths in cs.
	// params[1] = user
	// params[2] = new deaths

	// Check index
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Fetch player pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Set deaths
	*((int *)pPlayer->pvPrivateData + OFFSET_CSDEATHS) = params[2];

	// Update scoreboard here..?
	MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreInfo", NULL));
	WRITE_BYTE(params[1]);
	WRITE_SHORT(pPlayer->v.frags);
	WRITE_SHORT(params[2]);
	WRITE_SHORT(0); // dunno what this parameter is (doesn't seem to be vip)
	WRITE_SHORT(*((int *)pPlayer->pvPrivateData + OFFSET_TEAM));
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_id(AMX *amx, cell *params) // cs_get_hostage_id(index) = 1 param
{
	// Gets unique id of a CS hostage.
	// params[1] = hostage entity index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into class pointer
	edict_t *pEdict = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pEdict)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make sure this is a hostage.
	if (strcmp(STRING(pEdict->v.classname), "hostage_entity") != 0) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Return value at offset
	return (int)*((int *)pEdict->pvPrivateData + OFFSET_HOSTAGEID);
}

static cell AMX_NATIVE_CALL cs_get_weapon_silenced(AMX *amx, cell *params) // cs_get_weapon_silenced(index); = 1 param
{
	// Is weapon silenced? Does only work on M4A1 and USP.
	// params[1] = weapon index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pWeapon)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int weapontype = (int)*((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
	int *silencemode = ((int *)pWeapon->pvPrivateData + OFFSET_SILENCER_FIREMODE);
	switch (weapontype) {
		case CSW_M4A1:
			if (*silencemode == M4A1_SILENCED)
				return 1;
		case CSW_USP:
			if (*silencemode == USP_SILENCED)
				return 1;
	}

	// All else return 0.
	return 0;
}

static cell AMX_NATIVE_CALL cs_set_weapon_silenced(AMX *amx, cell *params) // cs_set_weapon_silenced(index, silence = 1); = 2 params
{
	// Silence/unsilence gun. Does only work on M4A1 and USP.
	// params[1] = weapon index
	// params[2] = 1, and we silence the gun, 0 and we unsilence gun-

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pWeapon)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int weapontype = (int)*((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
	int *silencemode = ((int *)pWeapon->pvPrivateData + OFFSET_SILENCER_FIREMODE);

	switch (weapontype) {
		case CSW_M4A1:
			if (params[2])
				*silencemode = M4A1_SILENCED;
			else
				*silencemode = M4A1_UNSILENCED;
			break;
		case CSW_USP:
			if (params[2])
				*silencemode = USP_SILENCED;
			else
				*silencemode = USP_UNSILENCED;
				break;
		default:
			return 0;
	}


	return 1;
}

static cell AMX_NATIVE_CALL cs_get_weapon_burstmode(AMX *amx, cell *params) // cs_get_weapon_burstmode(index); = 1 param
{
	// Is weapon in burst mode? Does only work with FAMAS and GLOCK.
	// params[1] = weapon index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pWeapon)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int weapontype = (int)*((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
	int* firemode = ((int *)pWeapon->pvPrivateData + OFFSET_SILENCER_FIREMODE);
	switch (weapontype) {
		case CSW_GLOCK18:
			if (*firemode == GLOCK_BURSTMODE)
				return 1;
		case CSW_FAMAS:
			if (*firemode == FAMAS_BURSTMODE)
				return 1;
	}

	// All else return 0.
	return 0;
}

static cell AMX_NATIVE_CALL cs_set_weapon_burstmode(AMX *amx, cell *params) // cs_set_weapon_burstmode(index, burstmode = 1); = 2 params
{
	// Set/unset burstmode. Does only work with FAMAS and GLOCK.
	// params[1] = weapon index
	// params[2] = 1, and we set burstmode, 0 and we unset it.

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxEntities)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pWeapon)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int weapontype = (int)*((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);

	int* firemode = ((int *)pWeapon->pvPrivateData + OFFSET_SILENCER_FIREMODE);
	int previousMode = *firemode;

	switch (weapontype) {
		case CSW_GLOCK18:
			if (params[2]) {
				if (previousMode != GLOCK_BURSTMODE) {
					*firemode = GLOCK_BURSTMODE;

					// Is this weapon's owner a player? If so send this message...
					if (UTIL_IsPlayer(amx, pWeapon->v.owner))
						UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_BurstFire");
				}
			}
			else if (previousMode != GLOCK_SEMIAUTOMATIC) {
				*firemode = GLOCK_SEMIAUTOMATIC;

				// Is this weapon's owner a player? If so send this message...
				if (UTIL_IsPlayer(amx, pWeapon->v.owner))
					UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_SemiAuto");
			}
			break;
		case CSW_FAMAS:
			if (params[2]) {
				if (previousMode != FAMAS_BURSTMODE) {
					*firemode = FAMAS_BURSTMODE;

					// Is this weapon's owner a player? If so send this message...
					if (UTIL_IsPlayer(amx, pWeapon->v.owner))
						UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_BurstFire");
				}
			}
			else if (previousMode != FAMAS_AUTOMATIC) {
				*firemode = FAMAS_AUTOMATIC;

				// Is this weapon's owner a player? If so send this message...
				if (UTIL_IsPlayer(amx, pWeapon->v.owner))
					UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_FullAuto");
			}
			break;
		default:
			return 0;
	}

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_vip(AMX *amx, cell *params) // cs_get_user_vip(index); = 1 param
{
	// Is user vip?
	// params[1] = user index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if ( *((int *)pPlayer->pvPrivateData + OFFSET_VIP) & PLAYER_IS_VIP )
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_vip(AMX *amx, cell *params) // cs_set_user_vip(index, vip = 1); = 2 params
{
	// Set user vip
	// params[1] = user index
	// params[2] = if 1, activate vip, else deactivate vip.

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (params[2])
		*((int *)pPlayer->pvPrivateData + OFFSET_VIP) |= PLAYER_IS_VIP;
	else
		*((int *)pPlayer->pvPrivateData + OFFSET_VIP) &= ~PLAYER_IS_VIP;

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_team(AMX *amx, cell *params) // cs_get_user_team(index); = 1 param
{
	// Get user team
	// params[1] = user index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return (int)*((int *)pPlayer->pvPrivateData + OFFSET_TEAM);
}

static cell AMX_NATIVE_CALL cs_set_user_team(AMX *amx, cell *params) // cs_set_user_team(index, team); = 2 params
{
	// Set user team
	// params[1] = user index
	// params[2] = team

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Just set team. Removed check of 1-2-3, because maybe scripters want to create new teams, 4, 5 etc?
	*((int *)pPlayer->pvPrivateData + OFFSET_TEAM) = params[2];
	
	/*switch (params[2]) {
		case TEAM_T:
		case TEAM_CT:
		case TEAM_SPECTATOR:
			(int)*((int *)pPlayer->pvPrivateData + OFFSET_TEAM) = params[2];
			break;
		default:
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
	}
	*/

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_inside_buyzone(AMX *amx, cell *params) // cs_get_user_inside_buyzone(index); = 1 param
{
	// Is user inside buy zone?
	// params[1] = user index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return (int)*((int *)pPlayer->pvPrivateData + OFFSET_BUYZONE); // This offset is 0 when outside, 1 when inside.
}

static cell AMX_NATIVE_CALL cs_get_user_plant(AMX *amx, cell *params) // cs_get_user_plant(index); = 1 param
{
	// Can user plant a bomb if he has one?
	// params[1] = user index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if ((int)*((int *)pPlayer->pvPrivateData + OFFSET_DEFUSE_PLANT) & CAN_PLANT_BOMB)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_plant(AMX *amx, cell *params) // cs_set_user_plant(index, plant = 1, showbombicon = 1); = 1 param
{
	// Set user plant "skill".
	// params[1] = user index
	// params[2] = 1 = able to
	// params[3] = show bomb icon?

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	int* plantskill = ((int *)pPlayer->pvPrivateData + OFFSET_DEFUSE_PLANT);

	if (params[2]) {
		*plantskill |= CAN_PLANT_BOMB;
		if (params[3]) {
			MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "StatusIcon", NULL), NULL, pPlayer);
			WRITE_BYTE(1); // show
			WRITE_STRING("c4");
			WRITE_BYTE(DEFUSER_COLOUR_R);
			WRITE_BYTE(DEFUSER_COLOUR_G);
			WRITE_BYTE(DEFUSER_COLOUR_B);
			MESSAGE_END();
		}
	}
	else {
		*plantskill &= ~CAN_PLANT_BOMB;
		MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "StatusIcon", NULL), NULL, pPlayer);
		WRITE_BYTE(0); // hide
		WRITE_STRING("c4");
		MESSAGE_END();
	}

	/*
	L 02/20/2004 - 16:58:00: [JGHG Trace] {MessageBegin type=StatusIcon(107), dest=MSG_ONE(1), classname=player netname=JGHG
	L 02/20/2004 - 16:58:00: [JGHG Trace] WriteByte byte=2
	L 02/20/2004 - 16:58:00: [JGHG Trace] WriteString string=c4
	L 02/20/2004 - 16:58:00: [JGHG Trace] WriteByte byte=0
	L 02/20/2004 - 16:58:00: [JGHG Trace] WriteByte byte=160
	L 02/20/2004 - 16:58:00: [JGHG Trace] WriteByte byte=0
	L 02/20/2004 - 16:58:00: [JGHG Trace] MessageEnd}
	*/

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_defusekit(AMX *amx, cell *params) // cs_get_user_defusekit(index); = 1 param
{
	// Does user have defusekit?
	// params[1] = user index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if ((int)*((int *)pPlayer->pvPrivateData + OFFSET_DEFUSE_PLANT) & HAS_DEFUSE_KIT)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_defusekit(AMX *amx, cell *params) // cs_set_user_defusekit(index, defusekit = 1, r = 0, g = 160, b = 0, icon[] = "defuser", flash = 0); = 7 params
{
	// Give/take defusekit.
	// params[1] = user index
	// params[2] = 1 = give
	// params[3] = r
	// params[4] = g
	// params[5] = b
	// params[6] = icon[]
	// params[7] = flash = 0

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	int* defusekit = ((int *)pPlayer->pvPrivateData + OFFSET_DEFUSE_PLANT);

	if (params[2]) {
		int colour[3] = {DEFUSER_COLOUR_R, DEFUSER_COLOUR_G, DEFUSER_COLOUR_B};
		for (int i = 0; i < 3; i++) {
			if (params[i + 3] != -1)
				colour[i] = params[i + 3];
		}

		char* icon;
		if (params[6] != -1) {
			int len;
			icon = GET_AMXSTRING(amx, params[6], 1, len);
		}
		else
			icon = "defuser";

		*defusekit |= HAS_DEFUSE_KIT;
		MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "StatusIcon", NULL), NULL, pPlayer);
		WRITE_BYTE(params[7] == 1 ? 2 : 1); // show (if params[7] == 1, then this should flash, so we should set two here, else just 1 to show normally)
		WRITE_STRING(icon);
		WRITE_BYTE(colour[0]);
		WRITE_BYTE(colour[1]);
		WRITE_BYTE(colour[2]);
		MESSAGE_END();
	}
	else {
		*defusekit &= ~HAS_DEFUSE_KIT;
		MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "StatusIcon", NULL), NULL, pPlayer);
		WRITE_BYTE(0); // hide
		WRITE_STRING("defuser");
		MESSAGE_END();
	}

	/*
	to show:
	L 02/20/2004 - 16:10:26: [JGHG Trace] {MessageBegin type=StatusIcon(107), dest=MSG_ONE(1), classname=player netname=JGHG
	L 02/20/2004 - 16:10:26: [JGHG Trace] WriteByte byte=1
	L 02/20/2004 - 16:10:26: [JGHG Trace] WriteString string=defuser
	L 02/20/2004 - 16:10:26: [JGHG Trace] WriteByte byte=0
	L 02/20/2004 - 16:10:26: [JGHG Trace] WriteByte byte=160
	L 02/20/2004 - 16:10:26: [JGHG Trace] WriteByte byte=0
	L 02/20/2004 - 16:10:26: [JGHG Trace] MessageEnd}

	to hide:
	L 02/20/2004 - 16:10:31: [JGHG Trace] {MessageBegin type=StatusIcon(107), dest=MSG_ONE(1), classname=player netname=JGHG
	L 02/20/2004 - 16:10:31: [JGHG Trace] WriteByte byte=0
	L 02/20/2004 - 16:10:31: [JGHG Trace] WriteString string=defuser
	L 02/20/2004 - 16:10:31: [JGHG Trace] MessageEnd}	
	*/
	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_backpackammo(AMX *amx, cell *params) // cs_get_user_backpackammo(index, weapon); = 2 params
{
	// Get amount of ammo in a user's backpack for a specific weapon type.
	// params[1] = user index
	// params[2] = weapon, as in CSW_*

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int offset;

	switch (params[2]) {
		case CSW_AWP:
			offset = OFFSET_AWM_AMMO;
			break;
		case CSW_SCOUT:
		case CSW_AK47:
		case CSW_G3SG1:
			offset = OFFSET_SCOUT_AMMO;
			break;
		case CSW_M249:
			offset = OFFSET_PARA_AMMO;
			break;
		case CSW_FAMAS:
		case CSW_M4A1:
		case CSW_AUG:
		case CSW_SG550:
		case CSW_GALI:
		case CSW_SG552:
			offset = OFFSET_FAMAS_AMMO;
			break;
		case CSW_M3:
		case CSW_XM1014:
			offset = OFFSET_M3_AMMO;
			break;
		case CSW_USP:
		case CSW_UMP45:
		case CSW_MAC10:
			offset = OFFSET_USP_AMMO;
			break;
		case CSW_FIVESEVEN:
		case CSW_P90:
			offset = OFFSET_FIVESEVEN_AMMO;
			break;
		case CSW_DEAGLE:
			offset = OFFSET_DEAGLE_AMMO;
			break;
		case CSW_P228:
			offset = OFFSET_P228_AMMO;
			break;
		case CSW_GLOCK18:
		case CSW_MP5NAVY:
		case CSW_TMP:
		case CSW_ELITE:
			offset = OFFSET_GLOCK_AMMO;
			break;
		case CSW_FLASHBANG:
			offset = OFFSET_FLASH_AMMO;
			break;
		case CSW_HEGRENADE:
			offset = OFFSET_HE_AMMO;
			break;
		case CSW_SMOKEGRENADE:
			offset = OFFSET_SMOKE_AMMO;
			break;
		case CSW_C4:
			offset = OFFSET_C4_AMMO;
			break;
		default:
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
	}

	return (int)*((int *)pPlayer->pvPrivateData + offset);
	
	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_backpackammo(AMX *amx, cell *params) // cs_set_user_backpackammo(index, weapon, amount); = 3 params
{
	// Set amount of ammo in a user's backpack for a specific weapon type.
	// params[1] = user index
	// params[2] = weapon, as in CSW_*
	// params[3] = new amount

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int offset;

	switch (params[2]) {
		case CSW_AWP:
			offset = OFFSET_AWM_AMMO;
			break;
		case CSW_SCOUT:
		case CSW_AK47:
		case CSW_G3SG1:
			offset = OFFSET_SCOUT_AMMO;
			break;
		case CSW_M249:
			offset = OFFSET_PARA_AMMO;
			break;
		case CSW_FAMAS:
		case CSW_M4A1:
		case CSW_AUG:
		case CSW_SG550:
		case CSW_GALI:
		case CSW_SG552:
			offset = OFFSET_FAMAS_AMMO;
			break;
		case CSW_M3:
		case CSW_XM1014:
			offset = OFFSET_M3_AMMO;
			break;
		case CSW_USP:
		case CSW_UMP45:
		case CSW_MAC10:
			offset = OFFSET_USP_AMMO;
			break;
		case CSW_FIVESEVEN:
		case CSW_P90:
			offset = OFFSET_FIVESEVEN_AMMO;
			break;
		case CSW_DEAGLE:
			offset = OFFSET_DEAGLE_AMMO;
			break;
		case CSW_P228:
			offset = OFFSET_P228_AMMO;
			break;
		case CSW_GLOCK18:
		case CSW_MP5NAVY:
		case CSW_TMP:
		case CSW_ELITE:
			offset = OFFSET_GLOCK_AMMO;
			break;
		case CSW_FLASHBANG:
			offset = OFFSET_FLASH_AMMO;
			break;
		case CSW_HEGRENADE:
			offset = OFFSET_HE_AMMO;
			break;
		case CSW_SMOKEGRENADE:
			offset = OFFSET_SMOKE_AMMO;
			break;
		case CSW_C4:
			offset = OFFSET_C4_AMMO;
			break;
		default:
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
	}

	*((int *)pPlayer->pvPrivateData + offset) = params[3];
	
	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_nvg(AMX *amx, cell *params) // cs_get_user_nvg(index); = 1 param
{
	// Does user have night vision goggles?
	// params[1] = user index

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if ((int)*((int *)pPlayer->pvPrivateData + OFFSET_NVGOGGLES) & HAS_NVGOGGLES)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_nvg(AMX *amx, cell *params) // cs_set_user_nvg(index, nvgoggles = 1); = 2 params
{
	// Give/take nvgoggles..
	// params[1] = user index
	// params[2] = 1 = give, 0 = remove

	// Valid entity should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	int* defusekit = ((int *)pPlayer->pvPrivateData + OFFSET_NVGOGGLES);

	if (params[2]) {
		if (*defusekit & HAS_NVGOGGLES)
			UTIL_TextMsg_Generic(pPlayer, "#Already_Have_One");
		else
			*defusekit |= HAS_NVGOGGLES;
	}
	else
		*defusekit &= ~HAS_NVGOGGLES;
	/*L 02/27/2004 - 09:16:43: [JGHG Trace] {MessageBegin type=TextMsg(77), dest=MSG_ONE(1), classname=player netname=JGHG
	L 02/27/2004 - 09:16:43: [JGHG Trace] WriteByte byte=4
	L 02/27/2004 - 09:16:43: [JGHG Trace] WriteString string=#Already_Have_One
	L 02/27/2004 - 09:16:43: [JGHG Trace] MessageEnd}*/

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_model(AMX *amx, cell *params) // cs_get_user_model(index, model[], len); = 3 params
{
	// Get model a player has.
	// params[1] = user index
	// params[2] = model
	// params[3] = max length to set

	// Valid player index should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t *pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return SET_AMXSTRING(amx, params[2], GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model"), params[3]);
}

static cell AMX_NATIVE_CALL cs_set_user_model(AMX *amx, cell *params) // cs_set_user_model(index, const model[]); = 2 params
{
	// Set model on player.
	// params[1] = user index
	// params[2] = model

	// Valid player index should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t* pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (params[2] == -1) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	char model[32];
	int len;

	strcpy(model, GET_AMXSTRING(amx, params[2], 0, len));
	
	g_players[params[1]].SetModel(model);
	g_players[params[1]].SetModelled(true);

	SETCLIENTKEYVALUE(params[1], GETINFOKEYBUFFER(pPlayer), "model", (char*)g_players[params[1]].GetModel());

	return 1;
}

static cell AMX_NATIVE_CALL cs_reset_user_model(AMX *amx, cell *params) // cs_reset_user_model(index); = 1 param
{
	// Reset model on player.
	// params[1] = user index

	// Valid player index should be within range
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t* pPlayer = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pPlayer)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	g_players[params[1]].SetModelled(false);

	MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer));

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_follow(AMX *amx, cell *params) // cs_get_hostage_follow(index); = 1 param
{
	// What index is the hostage following? (this doesn't have to be a player)
	// params[1] = hostage index

	// Valid index should be within range
	if (params[1] < gpGlobals->maxClients + 1 || params[1] > gpGlobals->maxEntities) // highest player index on a 10 player server is 10 :-)!
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pHostage)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	int following = *((int *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW);

	if (following == 0)
		return following;

	// Else this is probably a pointer to an entity's edict.
	edict_t* pEntity = (edict_t*)following;

	if (FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return ENTINDEX(pEntity);
}

static cell AMX_NATIVE_CALL cs_set_hostage_follow(AMX *amx, cell *params) // cs_set_hostage_follow(index, followedindex = 0); = 2 params
{
	// What index should the hostage be following? (this doesn't have to be a player)
	// params[1] = hostage index
	// params[2] = index to follow, if -1 then set hostage to not follow anything

	// Valid index should be within range
	if (params[1] < gpGlobals->maxClients + 1 || params[1] > gpGlobals->maxEntities) // highest player index on a 10 player server is 10 :-)!
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Check entity validity
	if (FNullEnt(pHostage)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	if (params[2] == -1) {
		*((int *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW) = 0;
		return 1;
	}

	// Valid index should be within range
	if (params[2] < 1 || params[2] > gpGlobals->maxEntities)
	{
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	// Make into edict pointer
	edict_t* pEntity = INDEXENT(params[2]);

	// Check entity validity
	if (FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	*((int *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW) = (int)pEntity;

	return 1;
}

AMX_NATIVE_INFO cstrike_Exports[] = {
	{"cs_set_user_money",			cs_set_user_money},
	{"cs_get_user_money",			cs_get_user_money},
	{"cs_get_user_deaths",			cs_get_user_deaths},
	{"cs_set_user_deaths",			cs_set_user_deaths},
	{"cs_get_hostage_id",			cs_get_hostage_id},
	{"cs_get_weapon_silenced",		cs_get_weapon_silenced},
	{"cs_set_weapon_silenced",		cs_set_weapon_silenced},
	{"cs_get_weapon_burstmode",		cs_get_weapon_burstmode},
	{"cs_set_weapon_burstmode",		cs_set_weapon_burstmode},
	{"cs_get_user_vip",				cs_get_user_vip},
	{"cs_set_user_vip",				cs_set_user_vip},
	{"cs_get_user_team",			cs_get_user_team},
	{"cs_set_user_team",			cs_set_user_team},
	{"cs_get_user_inside_buyzone",	cs_get_user_inside_buyzone},
	{"cs_get_user_plant",			cs_get_user_plant},
	{"cs_set_user_plant",			cs_set_user_plant},
	{"cs_get_user_defusekit",		cs_get_user_defusekit},
	{"cs_set_user_defusekit",		cs_set_user_defusekit},
	{"cs_get_user_backpackammo",	cs_get_user_backpackammo},
	{"cs_set_user_backpackammo",	cs_set_user_backpackammo},
	{"cs_get_user_nvg",		cs_get_user_nvg},
	{"cs_set_user_nvg",		cs_set_user_nvg},
	{"cs_get_hostage_follow",		cs_get_hostage_follow},
	{"cs_set_hostage_follow",		cs_set_hostage_follow},

	{"cs_get_user_model",			cs_get_user_model},
	{"cs_set_user_model",			cs_set_user_model},
	{"cs_reset_user_model",			cs_reset_user_model},
	{NULL,							NULL}
};

/***GetEngineFunctions******************/
void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	// Reset player model a short while (MODELRESETTIME) after this if they are using an edited model.
	if(msg_type == GET_USER_MSG_ID(PLID, "ResetHUD", NULL)) {
		int entityIndex = ENTINDEX(ed);
		if(g_players[entityIndex].GetModelled())
			g_players[entityIndex].SetInspectModel(true);
			//g_players[ENTINDEX(ed)].SetTime(gpGlobals->time + MODELRESETTIME);
	}

	RETURN_META(MRES_IGNORED);
}

C_DLLEXPORT int GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion) {
	if(!pengfuncsFromEngine)
		return(FALSE);
	else if(*interfaceVersion != ENGINE_INTERFACE_VERSION) {
		*interfaceVersion = ENGINE_INTERFACE_VERSION;
		return(FALSE);
	}

	//meta_engfuncs.pfnSetModel = SetModel;
	meta_engfuncs.pfnMessageBegin = MessageBegin;

	memcpy(pengfuncsFromEngine, &meta_engfuncs, sizeof(enginefuncs_t));

	return TRUE;
}

/***GetEntityAPI2******************/
void ClientDisconnect(edict_t *pEntity) {
	g_players[ENTINDEX(pEntity)].SetModelled(false);

	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged(edict_t *pEntity, char *infobuffer) {
	if(g_players[ENTINDEX(pEntity)].GetModelled() && pEntity->v.deadflag == DEAD_NO) {
		RETURN_META(MRES_SUPERCEDE);
	} else {
		RETURN_META(MRES_IGNORED);
	}
}

void PlayerPostThink(edict_t* pPlayer) {
	int entityIndex = ENTINDEX(pPlayer);

	if(g_players[entityIndex].GetModelled()) {
		if (g_players[entityIndex].GetInspectModel() && strcmp(g_players[entityIndex].GetModel(), GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model")) != 0) {
			LOG_CONSOLE(PLID, "%s should have model %s and currently has %s", STRING(pPlayer->v.netname), (char*)g_players[entityIndex].GetModel(), GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model"));
			SETCLIENTKEYVALUE(entityIndex, GETINFOKEYBUFFER(pPlayer), "model", (char*)g_players[entityIndex].GetModel());
			g_players[entityIndex].SetInspectModel(false);
		}
	}
	RETURN_META(MRES_IGNORED);
}

C_DLLEXPORT int GetEntityAPI2(DLL_FUNCTIONS *pFunctionTable, int *interfaceVersion) {
	gFunctionTable.pfnClientDisconnect = ClientDisconnect;
	gFunctionTable.pfnClientUserInfoChanged = ClientUserInfoChanged;
	gFunctionTable.pfnPlayerPostThink = PlayerPostThink;

	memcpy(pFunctionTable, &gFunctionTable, sizeof(DLL_FUNCTIONS));

	return(TRUE);
}

/******************************************************************************************/
C_DLLEXPORT int Meta_Query(char *ifvers, plugin_info_t **pPlugInfo, mutil_funcs_t *pMetaUtilFuncs) {
	*pPlugInfo = &Plugin_info;
	gpMetaUtilFuncs = pMetaUtilFuncs;

	return(TRUE);
}

C_DLLEXPORT int Meta_Attach(PLUG_LOADTIME now, META_FUNCTIONS *pFunctionTable, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs) {
	if(!pMGlobals) {
		LOG_ERROR(PLID, "Meta_Attach called with null pMGlobals");
		return(FALSE);
	}

	gpMetaGlobals = pMGlobals;

	if(!pFunctionTable) {
		LOG_ERROR(PLID, "Meta_Attach called with null pFunctionTable");
		return(FALSE);
	}

	gMetaFunctionTable.pfnGetEntityAPI2 = GetEntityAPI2;
	gMetaFunctionTable.pfnGetEngineFunctions = GetEngineFunctions;

	memcpy(pFunctionTable, &gMetaFunctionTable, sizeof(META_FUNCTIONS));
	gpGamedllFuncs = pGamedllFuncs;

	// Init stuff here

	return(TRUE);
}

C_DLLEXPORT int Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason) {
	if(now && reason) {
		return(TRUE);
	} else {
		return(FALSE);
	}
}

#ifdef __linux__
C_DLLEXPORT void GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals ) {
#else
void WINAPI GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals ) {
#endif
	memcpy(&g_engfuncs, pengfuncsFromEngine, sizeof(enginefuncs_t));
	gpGlobals = pGlobals;
}

C_DLLEXPORT int AMX_Query(module_info_s** info) {
	*info = &module_info;
	return 1;
}

C_DLLEXPORT int AMX_Attach(pfnamx_engine_g* amxeng,pfnmodule_engine_g* meng) {
	g_engAmxFunc = amxeng;
	g_engModuleFunc = meng;

	if(!gpMetaGlobals) REPORT_ERROR(1, "[AMX] %s is not attached to metamod (module \"%s\")\n", NAME, LOGTAG);

	ADD_AMXNATIVES(&module_info, cstrike_Exports);

	return(1);
}

C_DLLEXPORT int AMX_Detach() {
	return(1);
}
