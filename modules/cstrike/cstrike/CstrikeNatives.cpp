// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Counter-Strike Module
//

#include "CstrikeDatas.h"
#include "CstrikePlayer.h"
#include "CstrikeUtils.h"
#include "CstrikeHLTypeConversion.h"

CCstrikePlayer g_players[33];
int g_zooming[33] = {0};
bool g_precachedknife = false;
bool g_noknives = false;

static cell AMX_NATIVE_CALL cs_set_user_money(AMX *amx, cell *params) // cs_set_user_money(index, money, flash = 1); = 3 arguments
{
	GET_OFFSET("CBasePlayer", m_iAccount);

	// Give money to user
	// params[1] = user
	// params[2] = money
	// params[3] = flash money

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	// Give money
	set_pdata<int>(pPlayer, m_iAccount, params[2]);

	// Update display
	MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "Money", NULL), NULL, pPlayer);
	WRITE_LONG(params[2]);
	WRITE_BYTE(params[3] ? 1 : 0); // if params[3] is 0, there will be no +/- flash of money in display...
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_money(AMX *amx, cell *params) // cs_get_user_money(index); = 1 argument
{
	GET_OFFSET("CBasePlayer", m_iAccount);

	// Give money to user
	// params[1] = user

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	// Return money
	return get_pdata<int>(pPlayer, m_iAccount);
}

static cell AMX_NATIVE_CALL cs_get_user_deaths(AMX *amx, cell *params) // cs_get_user_deaths(index); = 1 param
{
	GET_OFFSET("CBasePlayer", m_iDeaths);

	// Gets user deaths in cs.
	// params[1] = user

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return get_pdata<int>(pPlayer, m_iDeaths);
}

static cell AMX_NATIVE_CALL cs_set_user_deaths(AMX *amx, cell *params) // cs_set_user_deaths(index, newdeaths); = 2 arguments
{
	GET_OFFSET("CBasePlayer", m_iDeaths);
	GET_OFFSET("CBasePlayer", m_iTeam );

	// Sets user deaths in cs.
	// params[1] = user
	// params[2] = new deaths

	// Check index
	CHECK_PLAYER(params[1]);

	// Fetch player pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	// Set deaths
	set_pdata<int>(pPlayer, m_iDeaths, params[2]);

	// Update scoreboard here..?
	MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreInfo", NULL));
	WRITE_BYTE(params[1]);
	WRITE_SHORT((int)pPlayer->v.frags); // should these be byte?
	WRITE_SHORT(params[2]); // should these be byte?
	WRITE_SHORT(0); // dunno what this parameter is (doesn't seem to be vip) // should these be byte?
	WRITE_SHORT(get_pdata<int>(pPlayer, m_iTeam)); // should these be byte?
	MESSAGE_END();

	*static_cast<int *>(MF_PlayerPropAddr(params[1], Player_Deaths)) = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_id(AMX *amx, cell *params) // cs_get_hostage_id(index) = 1 param
{
	GET_OFFSET("CHostage", m_iHostageIndex);

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
	return get_pdata<int>(pEdict, m_iHostageIndex);
}

static cell AMX_NATIVE_CALL cs_get_weapon_silenced(AMX *amx, cell *params) // cs_get_weapon_silenced(index); = 1 param
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	// Is weapon silenced? Does only work on M4A1 and USP.
	// params[1] = weapon index

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_M4A1:
		{
			if (get_pdata<int>(pWeapon, m_iWeaponState) & WPNSTATE_M4A1_SILENCED)
			{
				return 1;
			}
		}
		case CSW_USP:
		{
			if (get_pdata<int>(pWeapon, m_iWeaponState) & WPNSTATE_USP_SILENCED)
			{
				return 1;
			}
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL cs_get_weapon_id(AMX *amx, cell *params) // cs_get_weapon_id(index); = 1 param
{
	GET_OFFSET("CBasePlayerItem", m_iId);

	// Get weapon type. Corresponds to CSW_*
	// params[1] = weapon index

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	return get_pdata<int>(pWeapon, m_iId);
}

static cell AMX_NATIVE_CALL cs_set_weapon_silenced(AMX *amx, cell *params) // cs_set_weapon_silenced(index, silence = 1); = 2 params
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

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

	int weaponState = get_pdata<int>(pWeapon, m_iWeaponState);

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_M4A1:
		{
			if (params[2] == 1)
			{
				if (!(weaponState & WPNSTATE_M4A1_SILENCED))
				{ 
					set_pdata<int>(pWeapon, m_iWeaponState, weaponState |= WPNSTATE_M4A1_SILENCED);
					
					if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
					{
						pWeapon->v.owner->v.weaponanim =  M4A1_ATTACH_SILENCER;
					}
				}
			}
			else if (weaponState & WPNSTATE_M4A1_SILENCED)
			{ 
				set_pdata<int>(pWeapon, m_iWeaponState, weaponState &= ~WPNSTATE_M4A1_SILENCED);

				if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
				{
					pWeapon->v.owner->v.weaponanim = M4A1_DETACH_SILENCER;
				}
			}
			return 1;
		}
		case CSW_USP:
		{
			if (params[2] == 1)
			{
				if (!(weaponState & WPNSTATE_USP_SILENCED))
				{
					set_pdata<int>(pWeapon, m_iWeaponState, weaponState |= WPNSTATE_USP_SILENCED);
					
					if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
					{
						pWeapon->v.owner->v.weaponanim = USP_ATTACH_SILENCER;
					}
				}
			}
			else if (weaponState & WPNSTATE_USP_SILENCED)
			{
				set_pdata<int>(pWeapon, m_iWeaponState, weaponState &= ~WPNSTATE_USP_SILENCED);

				if (draw_animation && UTIL_IsPlayer(amx, pWeapon->v.owner))
				{
					pWeapon->v.owner->v.weaponanim = USP_DETACH_SILENCER;
				}
			}
			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL cs_get_weapon_burstmode(AMX *amx, cell *params) // cs_get_weapon_burstmode(index); = 1 param
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	// Is weapon in burst mode? Does only work with FAMAS and GLOCK.
	// params[1] = weapon index

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_GLOCK18:
		{
			if (get_pdata<int>(pWeapon, m_iWeaponState) & WPNSTATE_GLOCK18_BURST_MODE)
			{
				return 1;
			}
		}
		case CSW_FAMAS:
		{
			if (get_pdata<int>(pWeapon, m_iWeaponState) & WPNSTATE_FAMAS_BURST_MODE)
			{
				return 1;
			}
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_weapon_burstmode(AMX *amx, cell *params) // cs_set_weapon_burstmode(index, burstmode = 1); = 2 params
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	// Set/unset burstmode. Does only work with FAMAS and GLOCK.
	// params[1] = weapon index
	// params[2] = 1, and we set burstmode, 0 and we unset it.

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	int weaponState = get_pdata<int>(pWeapon, m_iWeaponState);

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_GLOCK18:
		{
			if (params[2])
			{
				if (!(weaponState & WPNSTATE_GLOCK18_BURST_MODE))
				{
					set_pdata<int>(pWeapon, m_iWeaponState, weaponState |= WPNSTATE_GLOCK18_BURST_MODE);

					if (UTIL_IsPlayer(amx, pWeapon->v.owner))
					{
						UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_BurstFire");
					}
				}
			}
			else if (weaponState & WPNSTATE_GLOCK18_BURST_MODE)
			{
				set_pdata<int>(pWeapon, m_iWeaponState, weaponState &= ~WPNSTATE_GLOCK18_BURST_MODE);

				if (UTIL_IsPlayer(amx, pWeapon->v.owner))
				{
					UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_SemiAuto");
				}
			}
			return 1;
		}
		case CSW_FAMAS:
		{
			if (params[2])
			{
				if (!(weaponState & WPNSTATE_FAMAS_BURST_MODE))
				{
					set_pdata<int>(pWeapon, m_iWeaponState, weaponState |= WPNSTATE_FAMAS_BURST_MODE);

					if (UTIL_IsPlayer(amx, pWeapon->v.owner))
					{
						UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_BurstFire");
					}
				}
			}
			else if (weaponState & WPNSTATE_FAMAS_BURST_MODE)
			{
				set_pdata<int>(pWeapon, m_iWeaponState, weaponState &= ~WPNSTATE_FAMAS_BURST_MODE);

				if (UTIL_IsPlayer(amx, pWeapon->v.owner))
				{
					UTIL_TextMsg_Generic(pWeapon->v.owner, "#Switch_To_FullAuto");
				}
			}
			return 1;
		}
	}

	return 0;
}

static cell AMX_NATIVE_CALL cs_get_user_armor(AMX *amx, cell *params) // cs_get_user_armor(index, &CsArmorType:armortype); = 2 params
{
	GET_OFFSET("CBasePlayer", m_iKevlar);

	// Return how much armor and set reference of what type...
	// params[1] = user index
	// params[2] = byref, set armor type here (no armor/vest/vest+helmet)

	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	cell *armorTypeByRef = MF_GetAmxAddr(amx, params[2]);

	*armorTypeByRef = get_pdata<int>(pPlayer, m_iKevlar);

	return static_cast<cell>(pPlayer->v.armorvalue);
}

static cell AMX_NATIVE_CALL cs_set_user_armor(AMX *amx, cell *params) // cs_set_user_armor(index, armorvalue, CsArmorType:armortype); = 3 params
{
	GET_OFFSET("CBasePlayer", m_iKevlar);

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
	set_pdata<int>(pPlayer, m_iKevlar, params[3]);
	
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
	GET_OFFSET("CBasePlayer", m_bIsVIP);

	// Is user vip?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if (get_pdata<bool>(pPlayer, m_bIsVIP))
	{
		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_vip(AMX *amx, cell *params) // cs_set_user_vip(index, vip = 1, model = 1, scoreboard = 1); = 4 params
{
	GET_OFFSET("CBasePlayer", m_bIsVIP);
	GET_OFFSET("CBasePlayer", m_iModelName);

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
		set_pdata<bool>(pPlayer, m_bIsVIP, true);

		if (updateModel)
		{
			// Set vip model
			set_pdata<int>(pPlayer, m_iModelName, CS_CT_VIP);

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
		set_pdata<bool>(pPlayer, m_bIsVIP, false);

		if (updateModel)
		{
			// Set a random CT model.
			CS_Internal_Models CTmodels[5] = {CS_CT_URBAN, CS_CT_GSG9, CS_CT_GIGN, CS_CT_SAS, CZ_CT_SPETSNAZ};
			CS_Internal_Models ct_model = CTmodels[RANDOM_LONG(0, 4)];

			set_pdata<int>(pPlayer, m_iModelName, ct_model);

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
	GET_OFFSET("CBasePlayer", m_iModelName);
	GET_OFFSET("CBasePlayer", m_iTeam);

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
		*model = get_pdata<int>(pPlayer, m_iModelName);
	}

	return get_pdata<int>(pPlayer, m_iTeam);;
}

static cell AMX_NATIVE_CALL cs_set_user_team(AMX *amx, cell *params) // cs_set_user_team(index, team, model = 0); = 3 params
{
	GET_OFFSET("CBasePlayer", m_iModelName);
	GET_OFFSET("CBasePlayer", m_iTeam);

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
	set_pdata<int>(pPlayer, m_iTeam, params[2]);

	if (model != 0)
		set_pdata<int>(pPlayer, m_iModelName, model);
	
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
	
	switch (params[2]) 
	{
		case TEAM_T:
			MF_SetPlayerTeamInfo(params[1], params[2], "TERRORIST");
			break;
		case TEAM_CT:
			MF_SetPlayerTeamInfo(params[1], params[2], "CT");
			break;
		case TEAM_SPECTATOR:
			MF_SetPlayerTeamInfo(params[1], params[2], "SPECTATOR");
			break;
		default:
			MF_SetPlayerTeamInfo(params[1], params[2], NULL);
			break;
	}

	return 1;
}

struct CUnifiedSignals
{
	int m_flSignal;
	int m_flState;
};

static cell AMX_NATIVE_CALL cs_get_user_inside_buyzone(AMX *amx, cell *params) // cs_get_user_inside_buyzone(index); = 1 param
{
	GET_OFFSET("CBasePlayer", m_signals);

	// Is user inside buy zone?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	
	// This offset is 0 when outside, 1 when inside.
	if (get_pdata<CUnifiedSignals>(pPlayer, m_signals).m_flState & SIGNAL_BUY)
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_get_user_mapzones(AMX *amx, cell *params) // cs_get_user_mapzones(index); = 1 param
{
	GET_OFFSET("CBasePlayer", m_signals);

	// Is user inside a special zone?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return get_pdata<CUnifiedSignals>(pPlayer, m_signals).m_flSignal;
}


static cell AMX_NATIVE_CALL cs_get_user_plant(AMX *amx, cell *params) // cs_get_user_plant(index); = 1 param
{
	GET_OFFSET("CBasePlayer", m_bHasC4);

	// Can user plant a bomb if he has one?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if (get_pdata<bool>(pPlayer, m_bHasC4))
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_plant(AMX *amx, cell *params) // cs_set_user_plant(index, plant = 1, showbombicon = 1); = 1 param
{
	GET_OFFSET("CBasePlayer", m_bHasC4);

	// Set user plant "skill".
	// params[1] = user index
	// params[2] = 1 = able to
	// params[3] = show bomb icon?

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if (params[2]) {
		set_pdata<bool>(pPlayer, m_bHasC4, true);
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
		set_pdata<bool>(pPlayer, m_bHasC4, false);
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
	GET_OFFSET("CBasePlayer", m_bHasDefuser);

	// Does user have defusekit?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if (get_pdata<bool>(pPlayer, m_bHasDefuser))
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_defusekit(AMX *amx, cell *params) // cs_set_user_defusekit(index, defusekit = 1, r = 0, g = 160, b = 0, icon[] = "defuser", flash = 0); = 7 params
{
	GET_OFFSET("CBasePlayer", m_bHasDefuser);

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

		set_pdata<bool>(pPlayer, m_bHasDefuser, false);
		MESSAGE_BEGIN(MSG_ONE, GET_USER_MSG_ID(PLID, "StatusIcon", NULL), NULL, pPlayer);
		WRITE_BYTE(params[7] == 1 ? 2 : 1); // show (if params[7] == 1, then this should flash, so we should set two here, else just 1 to show normally)
		WRITE_STRING(icon);
		WRITE_BYTE(colour[0]);
		WRITE_BYTE(colour[1]);
		WRITE_BYTE(colour[2]);
		MESSAGE_END();
	}
	else {
		set_pdata<bool>(pPlayer, m_bHasDefuser, false);
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
	GET_OFFSET("CBasePlayer"      , m_rgpPlayerItems  );
	GET_OFFSET("CBasePlayer"      , m_rgAmmo          );
	GET_OFFSET("CBasePlayerItem"  , m_pNext           );
	GET_OFFSET("CBasePlayerItem"  , m_iId             );
	GET_OFFSET("CBasePlayerWeapon", m_iPrimaryAmmoType);

	// Get amount of ammo in a user's backpack for a specific weapon type.
	// params[1] = user index
	// params[2] = weapon, as in CSW_*

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	int targetWeaponId = params[2];

	if (targetWeaponId < CSW_P228 || targetWeaponId > CSW_P90 || targetWeaponId == CSW_KNIFE)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", params[2]);
		return 0;
	}

	for (size_t i = 0; i < MAX_WEAPON_SLOTS; ++i)
	{
		uintptr_t *pItem = get_pdata<uintptr_t*>(pPlayer, m_rgpPlayerItems, i);

		while (pItem)
		{
			if (targetWeaponId == get_pdata<int>(pItem, m_iId))
			{
				return get_pdata<int>(pPlayer, m_rgAmmo, get_pdata<int>(pItem, m_iPrimaryAmmoType));
			}

			pItem = get_pdata<uintptr_t*>(pItem, m_pNext);
		}
	}
		
	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_backpackammo(AMX *amx, cell *params) // cs_set_user_backpackammo(index, weapon, amount); = 3 params
{
	GET_OFFSET("CBasePlayer"      , m_rgpPlayerItems  );
	GET_OFFSET("CBasePlayer"      , m_rgAmmo          );
	GET_OFFSET("CBasePlayerItem"  , m_pNext           );
	GET_OFFSET("CBasePlayerItem"  , m_iId             );
	GET_OFFSET("CBasePlayerWeapon", m_iPrimaryAmmoType);

	// Set amount of ammo in a user's backpack for a specific weapon type.
	// params[1] = user index
	// params[2] = weapon, as in CSW_*
	// params[3] = new amount

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	int targetWeaponId = params[2];

	if (targetWeaponId < CSW_P228 || targetWeaponId > CSW_P90 || targetWeaponId == CSW_KNIFE)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", params[2]);
		return 0;
	}

	for (size_t i = 0; i < MAX_WEAPON_SLOTS; ++i)
	{
		uintptr_t *pItem = get_pdata<uintptr_t*>(pPlayer, m_rgpPlayerItems, i);

		while (pItem)
		{
			if (targetWeaponId == get_pdata<int>(pItem, m_iId))
			{
				set_pdata<int>(pPlayer, m_rgAmmo, params[3], get_pdata<int>(pItem, m_iPrimaryAmmoType));
				return 1;
			}

			pItem = get_pdata<uintptr_t*>(pItem, m_pNext);
		}
	}
	
	return 0;
}

static cell AMX_NATIVE_CALL cs_get_user_nvg(AMX *amx, cell *params) // cs_get_user_nvg(index); = 1 param
{
	GET_OFFSET("CBasePlayer", m_bHasNightVision);

	// Does user have night vision goggles?
	// params[1] = user index

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	if (get_pdata<bool>(pPlayer, m_bHasNightVision))
		return 1;

	return 0;
}

static cell AMX_NATIVE_CALL cs_set_user_nvg(AMX *amx, cell *params) // cs_set_user_nvg(index, nvgoggles = 1); = 2 params
{
	GET_OFFSET("CBasePlayer", m_bHasNightVision);

	// Give/take nvgoggles..
	// params[1] = user index
	// params[2] = 1 = give, 0 = remove

	// Valid entity should be within range
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
	
	if (params[2])
	{
		if (get_pdata<bool>(pPlayer, m_bHasNightVision))
			UTIL_TextMsg_Generic(pPlayer, "#Already_Have_One");
		else
			set_pdata<bool>(pPlayer, m_bHasNightVision, true);
	}
	else
		set_pdata<bool>(pPlayer, m_bHasNightVision, false);

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
	GET_OFFSET("CBaseMonster"  , m_hTargetEnt);

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

	edict_t *pEntity = get_pdata<EHANDLE>(pHostage, m_hTargetEnt).Get();

	return pEntity ? ENTINDEX(pEntity) : 0;
}

static cell AMX_NATIVE_CALL cs_set_hostage_follow(AMX *amx, cell *params) // cs_set_hostage_follow(index, followedindex = 0); = 2 params
{
	GET_OFFSET("CBaseMonster", m_hTargetEnt);

	// What index should the hostage be following? (this doesn't have to be a player)
	// params[1] = hostage index
	// params[2] = index to follow, if -1 then set hostage to not follow anything

	cell hostage = params[1];
	cell target  = params[2];

	// Valid index should be within range
	CHECK_NONPLAYER(hostage);

	if (target != 0)
	{
		CHECK_ENTITY(target);
	}

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(hostage);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", hostage, STRING(pHostage->v.classname));
		return 0;
	}

	get_pdata<EHANDLE>(pHostage, m_hTargetEnt).Set(target ? GETEDICT(target) : nullptr);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_weapon_ammo(AMX *amx, cell *params) // cs_get_weapon_ammo(index); = 1 param
{
	GET_OFFSET("CBasePlayerWeapon", m_iClip);

	// Get amount of ammo in weapon's clip
	// params[1] = weapon index

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	return get_pdata<int>(pWeapon, m_iClip);
}

static cell AMX_NATIVE_CALL cs_set_weapon_ammo(AMX *amx, cell *params) // cs_set_weapon_ammo(index, newammo); = 2 params
{
	GET_OFFSET("CBasePlayerWeapon", m_iClip);

	// Set amount of ammo in weapon's clip
	// params[1] = weapon index
	// params[2] = newammo

	// Valid entity should be within range
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t *pWeapon = INDEXENT(params[1]);

	set_pdata<int>(pWeapon, m_iClip, params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_hasprimary(AMX *amx, cell *params) // cs_get_user_hasprimary(index); = 1 param
{
	GET_OFFSET("CBasePlayer", m_bHasPrimary);

	// Return 1 if user has a primary or shield (actually just return the value at the offset)
	// params[1] = user index

	// Check player
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return get_pdata<bool>(pPlayer, m_bHasPrimary) ? 1 : 0;
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
	GET_OFFSET("CBasePlayer", m_bJustKilledTeammate);

	// Return 1 if user has committed a team killing) 
	// params[1] = user index 

	// Check player 
	CHECK_PLAYER(params[1]);

	// Make into edict pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	return get_pdata<bool>(pPlayer, m_bJustKilledTeammate) ? 1 : 0;
}

// Damaged Soul
static cell AMX_NATIVE_CALL cs_set_user_tked(AMX *amx, cell *params) // cs_set_user_tked(index, tk = 1, subtract = 1); = 2 arguments 
{ 
	GET_OFFSET("CBasePlayer", m_bJustKilledTeammate);
	GET_OFFSET("CBasePlayer", m_iTeam);
	GET_OFFSET("CBasePlayer", m_iDeaths);

	// Sets whether or not player has committed a TK. 
	// params[1] = user 
	// params[2] = 1: player has TKed, 0: player hasn't TKed 
	// params[3] = number of frags to subtract 

	// Check index 
	CHECK_PLAYER(params[1]);

	// Fetch player pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	set_pdata<bool>(pPlayer, m_bJustKilledTeammate, params[2] != 0);

	if (params[3]) { 
		pPlayer->v.frags = pPlayer->v.frags - params[3]; 

		MESSAGE_BEGIN(MSG_ALL, GET_USER_MSG_ID(PLID, "ScoreInfo", NULL)); 
		WRITE_BYTE(params[1]); // user index 
		WRITE_SHORT((int)pPlayer->v.frags); // frags 
		WRITE_SHORT(get_pdata<int>(pPlayer, m_iDeaths)); // deaths 
		WRITE_SHORT(0); // ? 
		WRITE_SHORT(get_pdata<int>(pPlayer, m_iTeam)); // team 
		MESSAGE_END(); 
	} 

	return 1; 
}

static cell AMX_NATIVE_CALL cs_get_user_driving(AMX *amx, cell *params) // cs_get_user_driving(index); = 1 param 
{ 
	GET_OFFSET("CBasePlayer", m_iTrain);

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
	return get_pdata<int>(pPlayer, m_iTrain);
}

static cell AMX_NATIVE_CALL cs_get_user_stationary(AMX *amx, cell *params) // cs_get_user_stationary(index); = 1 param 
{ 
	GET_OFFSET("CBasePlayer", m_iClientHideHUD);

	// Returns 1 if client is using a stationary guns (maybe also other stuff)

	// Check player 
	CHECK_PLAYER(params[1]);

	// Make into edict pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]); 

	// If player driving, return 1, if not, return 0

	return get_pdata<int>(pPlayer, m_iClientHideHUD);
}

static cell AMX_NATIVE_CALL cs_get_user_shield(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bOwnsShield);

	//Return 1 if user has a shield.
	//params[1] = user id
   
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
   
	if (get_pdata<bool>(pPlayer, m_bOwnsShield))
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
	MDLL_Think(pPlayer);

	const char *auth = GETPLAYERAUTHID(pPlayer);
	if (((pPlayer->v.flags & FL_FAKECLIENT) == FL_FAKECLIENT || (auth && (strcmp(auth, "BOT") == 0))) && pPlayer->v.deadflag == DEAD_RESPAWNABLE) {
		MDLL_Spawn(pPlayer);
	}

	// pPlayer->v.iuser1 = 0;

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_armoury_type(AMX *amx, cell *params)
{
	GET_OFFSET("CArmoury", m_iItem);

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
	int weapontype = get_pdata<int>(pArmoury, m_iItem);;

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
	GET_OFFSET("CArmoury", m_iItem);

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

	set_pdata<int>(pArmoury, m_iItem, weapontype);

	return 1;   
#else
	MF_LogError(amx, AMX_ERR_NATIVE, "This function not implemented on AMD64.");
	return 0;
#endif
}

static cell AMX_NATIVE_CALL cs_set_user_zoom(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iFOV);

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
		set_pdata<int>(pPlayer, m_iFOV, CS_NO_ZOOM);
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

	set_pdata<int>(pPlayer, m_iFOV, value);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_zoom(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iFOV);

	// Returns the zoom type of a player
	// params[1] = user id

	// Check Player
	CHECK_PLAYER(params[1]);
	// Fetch player pointer 
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	switch (get_pdata<int>(pPlayer, m_iFOV))
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
	GET_OFFSET("CBasePlayer", m_fLastMovement);

	//Return time that the user last did activity
   
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
   
	return amx_ftoc(get_pdata<float>(pPlayer, m_fLastMovement));
}

static cell AMX_NATIVE_CALL cs_set_user_lastactivity(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_fLastMovement);

	//set time that the user last did activity
   
	//Check player
	CHECK_PLAYER(params[1]);
   
	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);
   
	set_pdata<float>(pPlayer, m_fLastMovement, amx_ctof(params[2]));

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_user_hostagekills(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iHostagesKilled);

	//Return number of hostages that user has killed

	//Check player
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	return get_pdata<int>(pPlayer, m_iHostagesKilled);
}

static cell AMX_NATIVE_CALL cs_set_user_hostagekills(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iHostagesKilled);

	//Set number of hostages that user has killed

	//Check player
	CHECK_PLAYER(params[1]);

	// Make into edict pointer
	edict_t *pPlayer = MF_GetPlayerEdict(params[1]);

	set_pdata<int>(pPlayer, m_iHostagesKilled, params[2]);

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_hostage_lastuse(AMX *amx, cell *params)
{
	GET_OFFSET("CHostage", m_flPathAcquired);

	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	return amx_ftoc(get_pdata<float>(pHostage, m_flPathAcquired));
}
static cell AMX_NATIVE_CALL cs_set_hostage_lastuse(AMX *amx, cell *params)
{
	GET_OFFSET("CHostage", m_flPathAcquired);

	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	set_pdata<float>(pHostage, m_flPathAcquired, amx_ctof(params[2]));

	return 1;
}
static cell AMX_NATIVE_CALL cs_get_hostage_nextuse(AMX* amx, cell* params)
{
	GET_OFFSET("CHostage", m_flNextChange);

	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}
   
	return amx_ftoc(get_pdata<float>(pHostage, m_flNextChange));
}
static cell AMX_NATIVE_CALL cs_set_hostage_nextuse(AMX* amx, cell* params)
{
	GET_OFFSET("CHostage", m_flNextChange);

	//Return time that the hostage was last used
   
	CHECK_NONPLAYER(params[1]);

	// Make into edict pointer
	edict_t* pHostage = INDEXENT(params[1]);

	// Make sure this is a hostage.
	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) {
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", params[1], STRING(pHostage->v.classname));
		return 0;
	}

	set_pdata<float>(pHostage, m_flNextChange, amx_ctof(params[2]));

	return 1;
}

static cell AMX_NATIVE_CALL cs_get_c4_explode_time(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_flC4Blow);

	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	return amx_ftoc(get_pdata<float>(pC4, m_flC4Blow));
}
static cell AMX_NATIVE_CALL cs_set_c4_explode_time(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_flC4Blow);

	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	set_pdata<float>(pC4, m_flC4Blow, amx_ctof(params[2]));

	return 1;
}
static cell AMX_NATIVE_CALL cs_get_c4_defusing(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_bStartDefuse);

	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	return get_pdata<bool>(pC4, m_bStartDefuse) ? 1 : 0;
}

static cell AMX_NATIVE_CALL cs_set_c4_defusing(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_bStartDefuse);

	CHECK_NONPLAYER(params[1]);
	edict_t* pC4 = INDEXENT(params[1]);

	// Make sure it's a c4
	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", params[1], STRING(pC4->v.classname));
		return 0;
	}

	set_pdata<bool>(pC4, m_bStartDefuse, params[2] != 0);

	return 1;
}

extern CreateNamedEntityFunc CS_CreateNamedEntity;
extern UTIL_FindEntityByStringFunc CS_UTIL_FindEntityByString;

// cs_create_entity(const classname[])
static cell AMX_NATIVE_CALL cs_create_entity(AMX* amx, cell* params)
{
	if (CS_CreateNamedEntity <= 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Native cs_create_entity() is disabled");
		return 0;
	}

	int len;
	int iszClass = ALLOC_STRING(MF_GetAmxString(amx, params[1], 0, &len));

	edict_t *pEnt = CS_CreateNamedEntity(iszClass);

	if (!FNullEnt(pEnt))
	{
		return ENTINDEX(pEnt);
	}

	return 0;
}

// cs_find_ent_by_class(start_index, const classname[])
static cell AMX_NATIVE_CALL cs_find_ent_by_class(AMX* amx, cell* params)
{
	if (CS_UTIL_FindEntityByString <= 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Native cs_find_ent_by_class() is disabled");
		return 0;
	}

	int len;
	void* pEntity = G_HL_TypeConversion.id_to_cbase(params[1]);
	const char* value = MF_GetAmxString(amx, params[2], 0, &len);

	int index = G_HL_TypeConversion.cbase_to_id(CS_UTIL_FindEntityByString(pEntity, "classname", value));

	if (index != -1)
	{
		return index;
	}

	return 0;
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


AMX_NATIVE_INFO CstrikeNatives[] = {
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
	{"cs_create_entity",			cs_create_entity },	
	{"cs_find_ent_by_class",		cs_find_ent_by_class},	

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
	GET_OFFSET_NO_ERROR("CBasePlayer", m_iFOV);

	int entityIndex = ENTINDEX(pPlayer);
	if (g_zooming[entityIndex])
	{
		set_pdata<int>(pPlayer, m_iFOV, g_zooming[entityIndex]);
	}

	RETURN_META(MRES_IGNORED);
}
