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

CCstrikePlayer g_players[33];
int g_zooming[33] = {0};
bool g_precachedknife = false;
bool g_noknives = false;

// Utils first

bool UTIL_IsPlayer(AMX* amx, edict_t* pPlayer) {
	bool player = false;

	if (strcmp(STRING(pPlayer->v.classname), "player") == 0)
		player = true;

	return player;
}

void UTIL_TextMsg_Generic(edict_t* pPlayer, const char* message)
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
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

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
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	// Return money
	return *((int *)pPlayer->pvPrivateData + OFFSET_CSMONEY);
}

static cell AMX_NATIVE_CALL cs_get_user_deaths(AMX *amx, cell *params) // cs_get_user_deaths(index); = 1 param
{
	// Gets user deaths in cs.
	// params[1] = user

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return *((int *)pPlayer->pvPrivateData + OFFSET_CSDEATHS);
}

static cell AMX_NATIVE_CALL cs_set_user_deaths(AMX *amx, cell *params) // cs_set_user_deaths(index, newdeaths); = 2 arguments
{
	// Sets user deaths in cs.
	// params[1] = user
	// params[2] = new deaths

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	// Set deaths
	*((int *)pPlayer->pvPrivateData + OFFSET_CSDEATHS) = params[2];

	// Update scoreboard here..?
	MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreInfo", NULL));
	WRITE_BYTE(params[1]);
	WRITE_SHORT((int)pPlayer->v.frags); // should these be byte?
	WRITE_SHORT(params[2]); // should these be byte?
	WRITE_SHORT(0); // dunno what this parameter is (doesn't seem to be vip) // should these be byte?
	WRITE_SHORT(*((int *)pPlayer->pvPrivateData + OFFSET_TEAM)); // should these be byte?
	MESSAGE_END();

	*static_cast<int *>(MF_PlayerPropAddr(params[1], Player_Deaths)) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_id(AMX *amx, cell *params) // cs_get_hostage_id(index) = 1 param
{
	// Gets unique id of a CS hostage.
	// params[1] = hostage entity index

	// Valid entity should be within range
	CHECK_ENTITY(params[1]);

	// Make into class pointer
	edict_t *pEdict = GETEDICT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pEdict->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pEdict->v.classname));
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
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	int weapontype = *((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
	int *silencemode = ((int *)pWeapon->pvPrivateData + OFFSET_SILENCER_FIREMODE);
	switch (weapontype) {
		case CSW_M4A1:
			if (*silencemode & M4A1_SILENCED)
				return 1;
		case CSW_USP:
			if (*silencemode & USP_SILENCED)
				return 1;
	}

	// All else return 0.
	return 0;
}

static cell AMX_NATIVE_CALL cs_get_weapon_id(AMX *amx, cell *params) // cs_get_weapon_id(index); = 1 param
{
	// Get weapon type. Corresponds to CSW_*
	// params[1] = weapon index

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	return *((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
}

static cell AMX_NATIVE_CALL cs_set_weapon_silenced(AMX *amx, cell *params) // cs_set_weapon_silenced(index, silence = 1); = 2 params
{
	// Silence/unsilence gun. Does only work on M4A1 and USP.
	// params[1] = weapon index
	// params[2] = 1, and we silence the gun, 0 and we unsilence gun.

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	bool draw_animation = true;

	if ((params[0] / sizeof(cell)) >= 3)
	{
		draw_animation = params[3] ? true : false;
	}

	int weapontype = (int)*((int *)pWeapon->pvPrivateData + OFFSET_WEAPONTYPE);
	int *silencemode = ((int *)pWeapon->pvPrivateData + OFFSET_SILENCER_FIREMODE);

	switch (weapontype)
	{
		case CSW_M4A1:
			if (params[2] == 1)
			{
				if (!(*silencemode & M4A1_SILENCED)) 
				{ // want to silence - can't already be silenced
					*silencemode |= M4A1_SILENCED;
					// If this weapon has an owner that is a player, play animation for that player.
					if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
					{
						pWeapon->v.owner->v.weaponanim = M4A1_ATTACHSILENCEANIM;
					}
				}
			} else if (*silencemode & M4A1_SILENCED) { // want to unsilence - can't already be unsilenced
				*silencemode &= ~M4A1_SILENCED;
				// If this weapon has an owner that is a player, play animation for that player.
				if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
				{
					pWeapon->v.owner->v.weaponanim = M4A1_DETACHSILENCEANIM;
				}
			}
			break;
		case CSW_USP:
			if (params[2] == 1)
			{
				if (!(*silencemode & USP_SILENCED))
				{ // want to silence - can't already be silenced
					*silencemode |= USP_SILENCED;
					// If this weapon has an owner that is a player, play animation for that player.
					if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
					{
						pWeapon->v.owner->v.weaponanim = USP_ATTACHSILENCEANIM;
					}
				}
			} else if (*silencemode & USP_SILENCED) { // want to unsilence - can't already be unsilenced
				*silencemode &= ~USP_SILENCED;
				// If this weapon has an owner that is a player, play animation for that player.
				if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
				{
					pWeapon->v.owner->v.weaponanim = USP_DETACHSILENCEANIM;
				}
			}
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
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

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
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

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

static cell AMX_NATIVE_CALL cs_get_user_armor(AMX *amx, cell *params) // cs_get_user_armor(index, &CsArmorType:armortype); = 2 params
{
	// Return how much armor and set reference of what type...
	// params[1] = user index
	// params[2] = byref, set armor type here (no armor/vest/vest+helmet)

	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

// #if 0 (Removed this to make the function work)
	cell *armorTypeByRef = MF_GetAmxAddr(amx, params[2]);
	*armorTypeByRef = *((int *)pPlayer->pvPrivateData + OFFSET_ARMORTYPE);
// #endif

	return (cell)pPlayer->v.armorvalue;
}

static cell AMX_NATIVE_CALL cs_set_user_armor(AMX *amx, cell *params) // cs_set_user_armor(index, armorvalue, CsArmorType:armortype); = 3 params
{
	// Set armor and set what type and send a message to client...
	// params[1] = user index
	// params[2] = armor value
	// params[3] = armor type (no armor/vest/vest+helmet)

	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	// Set armor value
	pPlayer->v.armorvalue = params[2];

	// Set armor type
	*((int *)pPlayer->pvPrivateData + OFFSET_ARMORTYPE) = params[3];
	
	if (params[3] == CS_ARMOR_KEVLAR || params[3] == CS_ARMOR_ASSAULTSUIT)
	{
		// And send appropriate message
		MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "ArmorType", NULL), NULL, pPlayer);
		WRITE_BYTE(params[3] == CS_ARMOR_ASSAULTSUIT ? 1 : 0);
		MESSAGE_END();
	}

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_vip(AMX *amx, cell *params) // cs_get_user_vip(index); = 1 param
{
	// Is user vip?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if ( *((int *)pPlayer->pvPrivateData + OFFSET_VIP) & PLAYER_IS_VIP )
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_vip(AMX *amx, cell *params) // cs_set_user_vip(index, vip = 1, model = 1, scoreboard = 1); = 4 params
{
	// Set user vip
	// params[1] = user index
	// params[2] = if 1, activate vip, else deactivate vip.
	// params[3] = if 1, update model
	// params[4] = if 1, update scoreboard with vip information

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	bool updateModel, updateScoreboard;

	// Backwards compatibility with older version of native that only took two params
	if (params[0] / sizeof(cell) == 2)
	{
		updateModel = true;
		updateScoreboard = true;
	}
	else
	{
		updateModel = (params[3] == 1);
		updateScoreboard = (params[4] == 1);
	}

	if (params[2] == 1)
	{
		// Set to "be" vip.
		*((int *)pPlayer->pvPrivateData + OFFSET_VIP) |= PLAYER_IS_VIP;

		if (updateModel)
		{
			// Set vip model
			*((int *)pPlayer->pvPrivateData + OFFSET_INTERNALMODEL) = CS_CT_VIP;
			// This makes the model get updated right away.
			MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer)); //  If this causes any problems for WON, do this line only in STEAM builds.
		}

		if (updateScoreboard)
		{
			// Set VIP on scoreboard. Probably doesn't work for terrorist team.
			MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreAttrib", NULL));
			WRITE_BYTE(params[1]);
			WRITE_BYTE(SCOREATTRIB_VIP);
			MESSAGE_END();
		}
	}
	else
	{
		// Set to not be vip.
		*((int *)pPlayer->pvPrivateData + OFFSET_VIP) &= ~PLAYER_IS_VIP;

		if (updateModel)
		{
			// Set a random CT model.
			CS_Internal_Models CTmodels[5] = {CS_CT_URBAN, CS_CT_GSG9, CS_CT_GIGN, CS_CT_SAS, CZ_CT_SPETSNAZ};
			CS_Internal_Models ct_model = CTmodels[RANDOM_LONG(0, 4)];
			*((int *)pPlayer->pvPrivateData + OFFSET_INTERNALMODEL) = ct_model;
			// This makes the model get updated right away.
			MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer)); //  If this causes any problems for WON, do this line only in STEAM builds.
		}

		if (updateScoreboard)
		{
			// Set nothing/dead on scoreboard.
			int scoreattrib;

			if (pPlayer->v.deadflag == DEAD_NO && pPlayer->v.health > 0)
				scoreattrib = SCOREATTRIB_NOTHING; // cts can't have bombs anyway
			else
				scoreattrib = SCOREATTRIB_DEAD;

			MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreAttrib", NULL));
			WRITE_BYTE(params[1]);
			WRITE_BYTE(scoreattrib);
			MESSAGE_END();
		}
	}

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_team(AMX *amx, cell *params) // cs_get_user_team(index); = 1 param
{
	// Get user team
	// params[1] = user index

	// Valid entity should be within range
	cell *model;
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if ((params[0] / sizeof(cell)) >= 2)
	{
		model = MF_GetAmxAddr(amx, params[2]);
		*model = *((int *)pPlayer->pvPrivateData + OFFSET_INTERNALMODEL);
	}

	return *((int *)pPlayer->pvPrivateData + OFFSET_TEAM);
}

static cell AMX_NATIVE_CALL cs_set_user_team(AMX *amx, cell *params) // cs_set_user_team(index, team, model = 0); = 3 params
{
	// Set user team
	// params[1] = user index
	// params[2] = team
	// params[3] = model = 0

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	int model = params[3];

	// Just set team. Removed check of 1-2-3, because maybe scripters want to create new teams, 4, 5 etc?
	*((int *)pPlayer->pvPrivateData + OFFSET_TEAM) = params[2];
	if (model != 0)
		*((int *)pPlayer->pvPrivateData + OFFSET_INTERNALMODEL) = model;
	
	// This makes the model get updated right away.
	MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer)); //  If this causes any problems for WON, do this line only in STEAM builds.

	// And update scoreboard by sending TeamInfo msg.
	char teaminfo[32];
	switch (params[2]) {
		case TEAM_UNASSIGNED:
			strcpy(teaminfo, "UNASSIGNED");
			break;
		case TEAM_T:
			strcpy(teaminfo, "TERRORIST");
			break;
		case TEAM_CT:
			strcpy(teaminfo, "CT");
			break;
		case TEAM_SPECTATOR:
			strcpy(teaminfo, "SPECTATOR");
			break;
		default:
			int team_nr = (int)params[2];
			sprintf(teaminfo, "TEAM_%i", team_nr);
	}
	MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "TeamInfo", NULL));
	WRITE_BYTE(params[1]);
	WRITE_STRING(teaminfo);
	MESSAGE_END();
	
	if (params[2] == 1)
		MF_SetPlayerTeamInfo(params[1], params[2], "TERRORIST");
	else if (params[2] == 2)
		MF_SetPlayerTeamInfo(params[1], params[2], "CT");
	else
		MF_SetPlayerTeamInfo(params[1], params[2], NULL);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_inside_buyzone(AMX *amx, cell *params) // cs_get_user_inside_buyzone(index); = 1 param
{
	// Is user inside buy zone?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	
	// This offset is 0 when outside, 1 when inside.
	if ((int)*((int *)pPlayer->pvPrivateData + OFFSET_MAPZONE) & PLAYER_IN_BUYZONE)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_get_user_mapzones(AMX *amx, cell *params) // cs_get_user_mapzones(index); = 1 param
{
	// Is user inside a special zone?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return (int)*((int *)pPlayer->pvPrivateData + OFFSET_MAPZONE);
}


static cell AMX_NATIVE_CALL cs_get_user_plant(AMX *amx, cell *params) // cs_get_user_plant(index); = 1 param
{
	// Can user plant a bomb if he has one?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	
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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	
	int* defusekit = ((int *)pPlayer->pvPrivateData + OFFSET_DEFUSE_PLANT);

	if (params[2])
	{
		int colour[3] = {DEFUSER_COLOUR_R, DEFUSER_COLOUR_G, DEFUSER_COLOUR_B};
		for (int i = 0; i < 3; i++)
		{
			if (params[i + 3] != -1)
			{
				colour[i] = params[i + 3];
			}
		}

		pPlayer->v.body = 1;

		const char* icon;
		if (params[6] != -1)
		{
			int len;
			icon = MF_GetAmxString(amx, params[6], 1, &len);
		} else {
			icon = "defuser";
		}

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
		pPlayer->v.body = 0;
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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

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
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", params[2]);
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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

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
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", params[2]);
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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	
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
	CHECK_PLAYER(params[1]);
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return MF_SetAmxString(amx, params[2], GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model"), params[3]);
}

static cell AMX_NATIVE_CALL cs_set_user_model(AMX *amx, cell *params) // cs_set_user_model(index, const model[]); = 2 params
{
	// Set model on player.
	// params[1] = user index
	// params[2] = model

	// Valid player index should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t* pPlayer = MF_GetPlayerEdict(params[1]);

	if (params[2] == -1) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid model %d", params[2]);
		return 0;
	}

	char model[32];
	int len;

	strcpy(model, MF_GetAmxString(amx, params[2], 0, &len));
	
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
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t* pPlayer = MF_GetPlayerEdict(params[1]);

	g_players[params[1]].SetModelled(false);

	MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer));

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_follow(AMX *amx, cell *params) // cs_get_hostage_follow(index); = 1 param
{
	// What index is the hostage following? (this doesn't have to be a player)
	// params[1] = hostage index

	// Valid index should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}

#if !defined __amd64__
	int following = *((int *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW);
#else
	long following = *((long *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW);
#endif
	if (following == 0)
		return following;

	// Else this is probably a pointer to an entity's edict.
	edict_t* pEntity = (edict_t*)following;

	if (FNullEnt(pEntity)) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Unknown error finding hostage parameter");
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
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}

	// Set to not follow anything?
	if (params[2] == 0) {
#if !defined __amd64__
		*((int *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW) = 0;
#else
		*((long *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW) = 0;
#endif

		return 1;
	}

	// Valid index should be within range
	CHECK_ENTITY(params[2]);

	// Make into edict pointer
	edict_t* pEntity = GETEDICT(params[2]);

#if !defined __amd64__
	*((int *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW) = (int)pEntity;
#else
	*((long *)pHostage->pvPrivateData + OFFSET_HOSTAGEFOLLOW) = (long)pEntity;
#endif
	return 1;
}

static cell AMX_NATIVE_CALL cs_get_weapon_ammo(AMX *amx, cell *params) // cs_get_weapon_ammo(index); = 1 param
{
	// Get amount of ammo in weapon's clip
	// params[1] = weapon index

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	return *((int *)pWeapon->pvPrivateData + OFFSET_CLIPAMMO);
}

static cell AMX_NATIVE_CALL cs_set_weapon_ammo(AMX *amx, cell *params) // cs_set_weapon_ammo(index, newammo); = 2 params
{
	// Set amount of ammo in weapon's clip
	// params[1] = weapon index
	// params[2] = newammo

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	*((int *)pWeapon->pvPrivateData + OFFSET_CLIPAMMO) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_hasprimary(AMX *amx, cell *params) // cs_get_user_hasprimary(index); = 1 param
{
	// Return 1 if user has a primary or shield (actually just return the value at the offset)
	// params[1] = user index

	// Check player
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return *((int *)pPlayer->pvPrivateData + OFFSET_PRIMARYWEAPON);
}

static cell AMX_NATIVE_CALL cs_get_no_knives(AMX *amx, cell *params) // cs_get_no_knives(); = 0 params
{
	// Returns 1 when g_noknives is true, else 0
	return g_noknives ? 1 : 0;
}

static cell AMX_NATIVE_CALL cs_set_no_knives(AMX *amx, cell *params) // cs_set_no_knives(noknives = 0); = 1 param
{
	// Sets noknives mode on/off. When params[1] is 1, g_noknives goes true and no weapon_knife:s will from there be created until switch off again.
	g_noknives = params[1] == 0 ? false : true;

	return 1;
}

// Damaged Soul
static cell AMX_NATIVE_CALL cs_get_user_tked(AMX *amx, cell *params) // cs_get_user_tked(index); = 1 param 
{ 
	// Return 1 if user has committed a team killing) 
	// params[1] = user index 

	// Check player 
	CHECK_PLAYER(params[1]);

	// Make into edict pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	return *((int *)pPlayer->pvPrivateData + OFFSET_TK); 
}

// Damaged Soul
static cell AMX_NATIVE_CALL cs_set_user_tked(AMX *amx, cell *params) // cs_set_user_tked(index, tk = 1, subtract = 1); = 2 arguments 
{ 
	// Sets whether or not player has committed a TK. 
	// params[1] = user 
	// params[2] = 1: player has TKed, 0: player hasn't TKed 
	// params[3] = number of frags to subtract 

	// Check index 
	CHECK_PLAYER(params[1]);

	// Fetch player pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	if (params[2]) { 
		*((int *)pPlayer->pvPrivateData + OFFSET_TK) = 1; 
	} else { 
		*((int *)pPlayer->pvPrivateData + OFFSET_TK) = 0; 
	} 

	if (params[3]) { 
		pPlayer->v.frags = pPlayer->v.frags - params[3]; 

		MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreInfo", NULL)); 
		WRITE_BYTE(params[1]); // user index 
		WRITE_SHORT((int)pPlayer->v.frags); // frags 
		WRITE_SHORT(*((int *)pPlayer->pvPrivateData + OFFSET_CSDEATHS)); // deaths 
		WRITE_SHORT(0); // ? 
		WRITE_SHORT(*((int *)pPlayer->pvPrivateData + OFFSET_TEAM)); // team 
		MESSAGE_END(); 
	} 

	return 1; 
}

static cell AMX_NATIVE_CALL cs_get_user_driving(AMX *amx, cell *params) // cs_get_user_driving(index); = 1 param 
{ 
	// Returns different values depending on if user is driving a value - and if so at what speed.
	// 0: no driving
	// 1: driving, but standing still
	// 2-4: different positive speeds
	// 5: negative speed (backing)
	// params[1] = user index 

	// Check player 
	CHECK_PLAYER(params[1]);

	// Make into edict pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	// If player driving, return 1, if not, return 0 
	return *((int *)pPlayer->pvPrivateData + OFFSET_ISDRIVING); 
}

static cell AMX_NATIVE_CALL cs_get_user_stationary(AMX *amx, cell *params) // cs_get_user_stationary(index); = 1 param 
{ 
	// Returns 1 if client is using a stationary guns (maybe also other stuff)

	// Check player 
	CHECK_PLAYER(params[1]);

	// Make into edict pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	// If player driving, return 1, if not, return 0
#if !defined __amd64__
	return *((int *)pPlayer->pvPrivateData + OFFSET_STATIONARY); 
#else
	// The 32 bit server return 0 and 1 by itself from this offset, but the amd64 server has 2 and 3 respectively
	// Doing a simple checking of these defined constants here, and mapping to 0 and 1, to keep our plugin authors sane.
	// If an unexpected value is encountered, this will be logged.
	if (AMD64_STATIONARY_NO == *((int *)pPlayer->pvPrivateData + OFFSET_STATIONARY))
		return 0;
	else if (AMD64_STATIONARY_YES == *((int *)pPlayer->pvPrivateData + OFFSET_STATIONARY))
		return 1;
	else {
		MF_LogError(amx, AMX_ERR_NATIVE, "Unexpected value at offset. Please report this to development team @ www.amxmodx.org!");

		return 0; 
	}
#endif
}

static cell AMX_NATIVE_CALL cs_get_user_shield(AMX *amx, cell *params)
{
	//Return 1 if user has a shield.
	//params[1] = user id
   
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
   
	if ((int)*((int *)pPlayer->pvPrivateData + OFFSET_SHIELD) & HAS_SHIELD)
		return 1;

	return 0;   
}

static cell AMX_NATIVE_CALL cs_user_spawn(AMX *amx, cell *params)
{
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	pPlayer->v.deadflag = DEAD_RESPAWNABLE;
	MDLL_Spawn(pPlayer);
	pPlayer->v.iuser1 = 0;

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_armoury_type(AMX *amx, cell *params)
{
	// Return CSW_* constants of specified armoury_entity.
	// params[1] = entity

	// Valid entity should be within range.
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer.
	edict_t *pArmoury = INDEXENT(params[1]);

	// Make sure this is an armoury_entity.
	if (strcmp(STRING(pArmoury->v.classname), "armoury_entity")) {
		// Error out here.
		MF_LogError(amx, AMX_ERR_NATIVE, "Not an armoury_entity! (%d)", params[1]);
		return 0;
	}

#if PAWN_CELL_SIZE == 32
	int weapontype = *((int *)pArmoury->pvPrivateData + OFFSET_ARMOURY_TYPE);

	// We do a switch instead of a mapped array because this way we can nicely catch unexpected values, and we don't get array out of bounds thingies.
	int weapontype_out;
	switch (weapontype) {
		case CSA_MP5NAVY:		weapontype_out = CSW_MP5NAVY; break;
		case CSA_TMP:			weapontype_out = CSW_TMP; break;
		case CSA_P90:			weapontype_out = CSW_P90; break;
		case CSA_MAC10:			weapontype_out = CSW_MAC10; break;
		case CSA_AK47:			weapontype_out = CSW_AK47; break;
		case CSA_SG552:			weapontype_out = CSW_SG552; break;
		case CSA_M4A1:			weapontype_out = CSW_M4A1; break;
		case CSA_AUG:			weapontype_out = CSW_AUG; break;
		case CSA_SCOUT:			weapontype_out = CSW_SCOUT; break;
		case CSA_G3SG1:			weapontype_out = CSW_G3SG1; break;
		case CSA_AWP:			weapontype_out = CSW_AWP; break;
		case CSA_M3:			weapontype_out = CSW_M3; break;
		case CSA_XM1014:		weapontype_out = CSW_XM1014; break;
		case CSA_M249:			weapontype_out = CSW_M249; break;
		case CSA_FLASHBANG:		weapontype_out = CSW_FLASHBANG; break;
		case CSA_HEGRENADE:		weapontype_out = CSW_HEGRENADE; break;
		case CSA_VEST:			weapontype_out = CSW_VEST; break;
		case CSA_VESTHELM:		weapontype_out = CSW_VESTHELM; break;
		case CSA_SMOKEGRENADE:	weapontype_out = CSW_SMOKEGRENADE; break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Unexpected weapon type of %d!", params[1]);
			return 0;
	}

	return weapontype_out;   
#else
	MF_LogError(amx, AMX_ERR_NATIVE, "This function not implemented on AMD64.");
	return 0;
#endif
}

static cell AMX_NATIVE_CALL cs_set_armoury_type(AMX *amx, cell *params)
{
	// Set CSW->CSA mapped weapon type to entity.
	// params[1] = entity
	// params[2] = CSW_* constant

	// Valid entity should be within range.
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer.
	edict_t *pArmoury = INDEXENT(params[1]);

	// Make sure this is an armoury_entity.
	if (strcmp(STRING(pArmoury->v.classname), "armoury_entity")) {
		// Error out here.
		MF_LogError(amx, AMX_ERR_NATIVE, "Not an armoury_entity! (%d)", params[1]);
		return 0;
	}

#if PAWN_CELL_SIZE == 32

	// We do a switch instead of a mapped array because this way we can nicely catch unexpected values, and we don't get array out of bounds thingies.
	int weapontype;
	switch (params[2]) {
		case CSW_MP5NAVY:		weapontype = CSA_MP5NAVY; break;
		case CSW_TMP:			weapontype = CSA_TMP; break;
		case CSW_P90:			weapontype = CSA_P90; break;
		case CSW_MAC10:			weapontype = CSA_MAC10; break;
		case CSW_AK47:			weapontype = CSA_AK47; break;
		case CSW_SG552:			weapontype = CSA_SG552; break;
		case CSW_M4A1:			weapontype = CSA_M4A1; break;
		case CSW_AUG:			weapontype = CSA_AUG; break;
		case CSW_SCOUT:			weapontype = CSA_SCOUT; break;
		case CSW_G3SG1:			weapontype = CSA_G3SG1; break;
		case CSW_AWP:			weapontype = CSA_AWP; break;
		case CSW_M3:			weapontype = CSA_M3; break;
		case CSW_XM1014:		weapontype = CSA_XM1014; break;
		case CSW_M249:			weapontype = CSA_M249; break;
		case CSW_FLASHBANG:		weapontype = CSA_FLASHBANG; break;
		case CSW_HEGRENADE:		weapontype = CSA_HEGRENADE; break;
		case CSW_VEST:			weapontype = CSA_VEST; break;
		case CSW_VESTHELM:		weapontype = CSA_VESTHELM; break;
		case CSW_SMOKEGRENADE:	weapontype = CSA_SMOKEGRENADE; break;
		default:
			MF_LogError(amx, AMX_ERR_NATIVE, "Unsupported weapon type! (%d)", params[2]);
			return 0;
	}

	*((int *)pArmoury->pvPrivateData + OFFSET_ARMOURY_TYPE) = weapontype;

	return 1;   
#else
	MF_LogError(amx, AMX_ERR_NATIVE, "This function not implemented on AMD64.");
	return 0;
#endif
}

static cell AMX_NATIVE_CALL cs_set_user_zoom(AMX *amx, cell *params)
{
	// Set the weapon zoom type of a user
	// params[1] = user index
	// params[2] = zoom type
	// params[3] = mode (0=blocking mode, 1=player will loose the zoom set by changing weapon)

	int index = params[1];
	// Check index 
	CHECK_PLAYER(index);

	int value, type = params[2];
	int curweap = *static_cast<int *>(MF_PlayerPropAddr(index, Player_CurrentWeapon));
	
	// Fetch player pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(index);
	// Reset any previous zooming
	g_zooming[index] = 0;

	if (type == CS_RESET_ZOOM)
	{
		*((int *)pPlayer->pvPrivateData + OFFSET_ZOOMTYPE) = CS_NO_ZOOM;
		return 1;
	}

	switch (type)
	{
	case CS_SET_NO_ZOOM:
		value = CS_NO_ZOOM;
		break;
	case CS_SET_FIRST_ZOOM:
		value = CS_FIRST_ZOOM;
		break;
	case CS_SET_SECOND_ZOOM:
		if (curweap == CSW_G3SG1 || curweap == CSW_SG550 || curweap == CSW_SCOUT)
			value = CS_SECOND_NONAWP_ZOOM;
		else
			value = CS_SECOND_AWP_ZOOM;
		break;
	case CS_SET_AUGSG552_ZOOM:
		value = CS_AUGSG552_ZOOM;
		break;
	default:
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid zoom type %d", type);
		return 0;
	}

	if (!params[3])
		g_zooming[index] = value;
	*((int *)pPlayer->pvPrivateData + OFFSET_ZOOMTYPE) = value;

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_zoom(AMX *amx, cell *params)
{
	// Returns the zoom type of a player
	// params[1] = user id

	// Check Player
	CHECK_PLAYER(params[1]);
	// Fetch player pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	int value = *((int *)pPlayer->pvPrivateData + OFFSET_ZOOMTYPE);

	switch (value)
	{
	case CS_NO_ZOOM:
		return CS_SET_NO_ZOOM;
	case CS_FIRST_ZOOM:
		return CS_SET_FIRST_ZOOM;
	case CS_SECOND_AWP_ZOOM:
	case CS_SECOND_NONAWP_ZOOM:
		return CS_SET_SECOND_ZOOM;
	case CS_AUGSG552_ZOOM:
		return CS_SET_AUGSG552_ZOOM;
	}

	return 0;
}
// Returns whether the player has a thighpack or backpack model on

static cell AMX_NATIVE_CALL cs_get_user_submodel(AMX* amx, cell* params)
{
	// Check Player
	CHECK_PLAYER(params[1]);
	// Fetch player pointer 
	edict_t* pPlayer = MF_GetPlayerEdict(params[1]);

	return pPlayer->v.body;
}
static cell AMX_NATIVE_CALL cs_set_user_submodel(AMX* amx, cell* params)
{
	// Check Player
	CHECK_PLAYER(params[1]);
	// Fetch player pointer 
	edict_t* pPlayer = MF_GetPlayerEdict(params[1]);

	pPlayer->v.body = params[2];

	return 1;
}
#if PAWN_CELL_SIZE == 32
static cell AMX_NATIVE_CALL cs_get_user_lastactivity(AMX *amx, cell *params)
{
	//Return time that the user last did activity
   
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
   
	return amx_ftoc(*((REAL*)pPlayer->pvPrivateData + OFFSET_LASTACTIVITY));
}

static cell AMX_NATIVE_CALL cs_set_user_lastactivity(AMX *amx, cell *params)
{
	//set time that the user last did activity
   
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
   
	*((REAL*)pPlayer->pvPrivateData + OFFSET_LASTACTIVITY) = amx_ctof(params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_hostagekills(AMX *amx, cell *params)
{
	//Return number of hostages that user has killed

	//Check player
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return *((int *)pPlayer->pvPrivateData + OFFSET_HOSTAGEKILLS);
}

static cell AMX_NATIVE_CALL cs_set_user_hostagekills(AMX *amx, cell *params)
{
	//Set number of hostages that user has killed

	//Check player
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	*((int *)pPlayer->pvPrivateData + OFFSET_HOSTAGEKILLS) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_lastuse(AMX *amx, cell *params)
{
	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	return amx_ftoc(*((REAL*)pHostage->pvPrivateData + OFFSET_HOSTAGE_LASTUSE));
}
static cell AMX_NATIVE_CALL cs_set_hostage_lastuse(AMX *amx, cell *params)
{
	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	*((REAL*)pHostage->pvPrivateData + OFFSET_HOSTAGE_LASTUSE) = amx_ctof(params[2]);

	return 1;
}
static cell AMX_NATIVE_CALL cs_get_hostage_nextuse(AMX* amx, cell* params)
{
	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	return amx_ftoc(*((REAL*)pHostage->pvPrivateData + OFFSET_HOSTAGE_NEXTUSE));
}
static cell AMX_NATIVE_CALL cs_set_hostage_nextuse(AMX* amx, cell* params)
{
	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	*((REAL*)pHostage->pvPrivateData + OFFSET_HOSTAGE_NEXTUSE) = amx_ctof(params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_c4_explode_time(AMX* amx, cell* params)
{
	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	return amx_ftoc(*((REAL*)pC4->pvPrivateData + OFFSET_C4_EXPLODE_TIME));
}
static cell AMX_NATIVE_CALL cs_set_c4_explode_time(AMX* amx, cell* params)
{
	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	*((REAL*)pC4->pvPrivateData + OFFSET_C4_EXPLODE_TIME) = amx_ctof(params[2]);

	return 1;
}
static cell AMX_NATIVE_CALL cs_get_c4_defusing(AMX* amx, cell* params)
{
	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	return *(bool *)((char *)(pC4->pvPrivateData) + OFFSET_C4_DEFUSING) ? 1 : 0;
}

static cell AMX_NATIVE_CALL cs_set_c4_defusing(AMX* amx, cell* params)
{
	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	*(bool *)((char *)(pC4->pvPrivateData) + OFFSET_C4_DEFUSING) = params[2] ? true : false;

	return 1;
}

#else

static cell AMX_NATIVE_CALL not_on_64(AMX* amx, cell* params)
{
	MF_LogError(amx, AMX_ERR_NATIVE, "This function is not implemented on AMD64");

	return 0;
}
#define cs_get_user_lastactivity not_on_64
#define cs_set_user_lastactivity not_on_64
#define cs_get_user_hostagekills not_on_64
#define cs_set_user_hostagekills not_on_64
#define cs_get_hostage_lastuse not_on_64
#define cs_set_hostage_lastuse not_on_64
#define cs_get_hostage_nextuse not_on_64
#define cs_set_hostage_nextuse not_on_64
#define cs_get_c4_explode_time not_on_64
#define cs_set_c4_explode_time not_on_64
#define cs_get_c4_defusing not_on_64
#define cs_set_c4_defusing not_on_64
#endif


AMX_NATIVE_INFO cstrike_Exports[] = {
	{"cs_set_user_money",			cs_set_user_money},
	{"cs_get_user_money",			cs_get_user_money},
	{"cs_get_user_deaths",			cs_get_user_deaths},
	{"cs_set_user_deaths",			cs_set_user_deaths},
	{"cs_get_hostage_id",			cs_get_hostage_id},
	{"cs_get_weapon_silen",			cs_get_weapon_silenced},
	{"cs_set_weapon_silen",			cs_set_weapon_silenced},
	{"cs_get_weapon_burst",			cs_get_weapon_burstmode},
	{"cs_set_weapon_burst",			cs_set_weapon_burstmode},
	{"cs_get_user_vip",				cs_get_user_vip},
	{"cs_set_user_vip",				cs_set_user_vip},
	{"cs_get_user_team",			cs_get_user_team},
	{"cs_set_user_team",			cs_set_user_team},
	{"cs_get_user_buyzone",			cs_get_user_inside_buyzone},
	{"cs_get_user_mapzones",		cs_get_user_mapzones},
	{"cs_get_user_plant",			cs_get_user_plant},
	{"cs_set_user_plant",			cs_set_user_plant},
	{"cs_get_user_defuse",			cs_get_user_defusekit},
	{"cs_set_user_defuse",			cs_set_user_defusekit},
	{"cs_get_user_bpammo",			cs_get_user_backpackammo},
	{"cs_set_user_bpammo",			cs_set_user_backpackammo},
	{"cs_get_user_nvg",				cs_get_user_nvg},
	{"cs_set_user_nvg",				cs_set_user_nvg},
	{"cs_get_hostage_foll",			cs_get_hostage_follow},
	{"cs_set_hostage_foll",			cs_set_hostage_follow},
	{"cs_get_user_model",			cs_get_user_model},
	{"cs_set_user_model",			cs_set_user_model},
	{"cs_reset_user_model",			cs_reset_user_model},
	{"cs_set_weapon_ammo",			cs_set_weapon_ammo},
	{"cs_get_weapon_ammo",			cs_get_weapon_ammo},
	{"cs_get_user_hasprim",			cs_get_user_hasprimary},
	{"cs_get_no_knives",			cs_get_no_knives},
	{"cs_set_no_knives",			cs_set_no_knives},
	{"cs_get_weapon_id",			cs_get_weapon_id},
	{"cs_get_user_tked",			cs_get_user_tked},
	{"cs_set_user_tked",			cs_set_user_tked},
	{"cs_get_user_driving",			cs_get_user_driving},
	{"cs_get_user_stationary",		cs_get_user_stationary},
	{"cs_get_user_armor",			cs_get_user_armor},
	{"cs_set_user_armor",			cs_set_user_armor},
	{"cs_get_user_shield",			cs_get_user_shield},
	{"cs_user_spawn",				cs_user_spawn},
	{"cs_get_armoury_type",			cs_get_armoury_type},
	{"cs_set_armoury_type",			cs_set_armoury_type},
	{"cs_get_user_zoom",			cs_get_user_zoom},
	{"cs_set_user_zoom",			cs_set_user_zoom},
	{"cs_get_user_submodel",		cs_get_user_submodel},
	{"cs_set_user_submodel",		cs_set_user_submodel},
	{"cs_get_user_lastactivity",	cs_get_user_lastactivity},
	{"cs_set_user_lastactivity",	cs_set_user_lastactivity},
	{"cs_get_user_hostagekills",	cs_get_user_hostagekills},
	{"cs_set_user_hostagekills",	cs_set_user_hostagekills},
	{"cs_get_hostage_lastuse",		cs_get_hostage_lastuse},
	{"cs_set_hostage_lastuse",		cs_set_hostage_lastuse},
	{"cs_get_hostage_nextuse",		cs_get_hostage_nextuse},
	{"cs_set_hostage_nextuse",		cs_set_hostage_nextuse},
	{"cs_get_c4_explode_time",		cs_get_c4_explode_time},
	{"cs_set_c4_explode_time",		cs_set_c4_explode_time},
	{"cs_get_c4_defusing",			cs_get_c4_defusing},
	{"cs_set_c4_defusing",			cs_set_c4_defusing},

	{NULL,							NULL}
};

edict_s* FN_CreateNamedEntity(int classname) {
	if (g_noknives && !strcmp(STRING(classname), "weapon_knife")) {
		if (g_precachedknife) {
			// Knife is creating
			RETURN_META_VALUE(MRES_SUPERCEDE, NULL);
		}
		// Let it create a knife first time; this seems to keep it precached properly in case anyone give_items a knife later.
		g_precachedknife = true;
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

void FN_ServerDeactivate() {
	g_precachedknife = false;

	RETURN_META(MRES_IGNORED);
}

/***GetEngineFunctions******************/
void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {
	// Reset player model a short while (MODELRESETTIME) after this if they are using an edited model.
	if(msg_type == GET_USER_MSG_ID(PLID, "ResetHUD", NULL)) {
		int entityIndex = ENTINDEX(ed);
		if (g_zooming[entityIndex])
			g_zooming[entityIndex] = 0;
		if(g_players[entityIndex].GetModelled())
			g_players[entityIndex].SetInspectModel(true);
			//g_players[ENTINDEX(ed)].SetTime(gpGlobals->time + MODELRESETTIME);
	}

	RETURN_META(MRES_IGNORED);
}


/***GetEntityAPI2******************/
void ClientDisconnect(edict_t *pEntity) {
	int index = ENTINDEX(pEntity);
	g_players[index].SetModelled(false);
	g_zooming[index] = 0;

	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged(edict_t *pEntity, char *infobuffer) {
	int index = ENTINDEX(pEntity);

	if(g_players[index].GetModelled() && pEntity->v.deadflag == DEAD_NO) {
		RETURN_META(MRES_SUPERCEDE);
	} else {
		RETURN_META(MRES_IGNORED);
	}
}

void PlayerPostThink(edict_t* pPlayer) {
	int entityIndex = ENTINDEX(pPlayer);

	if(g_players[entityIndex].GetModelled()) {
		if (g_players[entityIndex].GetInspectModel() && strcmp(g_players[entityIndex].GetModel(), GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model")) != 0) {
			//LOG_CONSOLE(PLID, "%s should have model %s and currently has %s", STRING(pPlayer->v.netname), (char*)g_players[entityIndex].GetModel(), GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model"));
			SETCLIENTKEYVALUE(entityIndex, GETINFOKEYBUFFER(pPlayer), "model", (char*)g_players[entityIndex].GetModel());
			g_players[entityIndex].SetInspectModel(false);
		}
	}
	RETURN_META(MRES_IGNORED);
}

void PlayerPreThink(edict_t *pPlayer)
{
	int entityIndex = ENTINDEX(pPlayer);
	if (g_zooming[entityIndex])
	{
		*((int *)pPlayer->pvPrivateData + OFFSET_ZOOMTYPE) = g_zooming[entityIndex];
	}

	RETURN_META(MRES_IGNORED);
}

int AmxxCheckGame(const char *game)
{
	if (strcasecmp(game, "cstrike") == 0 ||
		strcasecmp(game, "czero") == 0)
	{
		return AMXX_GAME_OK;
	}
	return AMXX_GAME_BAD;
}
void OnAmxxAttach()
{
	MF_AddNatives(cstrike_Exports);
	InitializeHacks();
}

void OnAmxxDetach()
{
	ShutdownHacks();
}
