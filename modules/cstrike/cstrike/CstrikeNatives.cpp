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
#include "CstrikeHacks.h"
#include "CstrikeHLTypeConversion.h"
#include <CDetour/detours.h>

CCstrikePlayer g_players[33];
int g_zooming[33] = {0};

bool NoKifesMode = false;

int MessageIdArmorType;
int MessageIdMoney;
int MessageIdResetHUD;
int MessageIdScoreAttrib;
int MessageIdScoreInfo;
int MessageIdStatusIcon;
int MessageIdTeamInfo;
int MessageIdTextMsg;

// native cs_set_user_money(index, money, flash = 1);
static cell AMX_NATIVE_CALL cs_set_user_money(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iAccount);

	int index = params[1];
	int money = params[2];
	int flash = static_cast<int>(params[3] != 0);

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	set_pdata<int>(pPlayer, m_iAccount, money);

	MESSAGE_BEGIN(MSG_ONE, MessageIdMoney, nullptr, pPlayer);
		WRITE_LONG(money);
		WRITE_BYTE(flash);
	MESSAGE_END();

	return 1;
}

// native cs_get_user_money(index);
static cell AMX_NATIVE_CALL cs_get_user_money(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iAccount);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return get_pdata<int>(pPlayer, m_iAccount);
}

// native cs_get_user_deaths(index);
static cell AMX_NATIVE_CALL cs_get_user_deaths(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iDeaths);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return get_pdata<int>(pPlayer, m_iDeaths);
}

// native cs_set_user_deaths(index, newdeaths, bool:scoreboard = true);
static cell AMX_NATIVE_CALL cs_set_user_deaths(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iDeaths);
	GET_OFFSET("CBasePlayer", m_iTeam );

	int index  = params[1];
	int deaths = params[2];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	set_pdata<int>(pPlayer, m_iDeaths, deaths);

	bool updateScoreboard = true;

	if (*params / sizeof(cell) >= 3)
	{
		updateScoreboard = params[3] != 0;
	}

	if (updateScoreboard)
	{
		MESSAGE_BEGIN(MSG_ALL, MessageIdScoreInfo);
			WRITE_BYTE(index);
			WRITE_SHORT(static_cast<int>(pPlayer->v.frags));
			WRITE_SHORT(deaths);
			WRITE_SHORT(0);
			WRITE_SHORT(get_pdata<int>(pPlayer, m_iTeam));
		MESSAGE_END();
	}

	*static_cast<int *>(MF_PlayerPropAddr(index, Player_Deaths)) = deaths;

	return 1;
}

// native cs_get_hostage_id(index);
static cell AMX_NATIVE_CALL cs_get_hostage_id(AMX *amx, cell *params)
{
	GET_OFFSET("CHostage", m_iHostageIndex);

	int index = params[1];

	CHECK_ENTITY(index);
	edict_t *pHostage = GETEDICT(index);

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}

	return get_pdata<int>(pHostage, m_iHostageIndex);
}

// native cs_get_weapon_silen(index);
static cell AMX_NATIVE_CALL cs_get_weapon_silenced(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

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

// native cs_get_weapon_id(index);
static cell AMX_NATIVE_CALL cs_get_weapon_id(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerItem", m_iId);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

	return get_pdata<int>(pWeapon, m_iId);
}

// native cs_set_weapon_silen(index, silence = 1, draw_animation = 1);
static cell AMX_NATIVE_CALL cs_set_weapon_silenced(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	int index    = params[1];
	int silence  = params[2];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

	bool draw_animation = true;

	if ((params[0] / sizeof(cell)) >= 3)
	{
		draw_animation = params[3] != 0;
	}

	int weaponState = get_pdata<int>(pWeapon, m_iWeaponState);
	int weaponNewState = weaponState;
	int animation = 0;

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_M4A1:
		{
			if (silence)
			{
				weaponNewState |= WPNSTATE_M4A1_SILENCED;
				animation = M4A1_ATTACH_SILENCER;
			}
			else
			{
				weaponNewState &= ~WPNSTATE_M4A1_SILENCED;
				animation = M4A1_DETACH_SILENCER;
			}
			break;
		}
		case CSW_USP:
		{
			if (silence)
			{
				weaponNewState |= WPNSTATE_USP_SILENCED;
				animation = USP_ATTACH_SILENCER;
			}
			else
			{
				weaponNewState &= ~WPNSTATE_USP_SILENCED;
				animation = USP_DETACH_SILENCER;
			}
			break;
		}
	}

	if (weaponState != weaponNewState)
	{
		set_pdata<int>(pWeapon, m_iWeaponState, weaponNewState);

		if (draw_animation && UTIL_IsPlayer(pWeapon->v.owner))
		{
			pWeapon->v.owner->v.weaponanim = animation;
		}

		return 1;
	}

	return 0;
}

// native cs_get_weapon_burst(index);
static cell AMX_NATIVE_CALL cs_get_weapon_burstmode(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

	int flag = 0;

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_GLOCK18: flag = WPNSTATE_GLOCK18_BURST_MODE; break;
		case CSW_FAMAS:   flag = WPNSTATE_FAMAS_BURST_MODE;   break;
	}

	if (flag && get_pdata<int>(pWeapon, m_iWeaponState) & flag)
	{
		return 1;
	}

	return 0;
}

// native cs_set_weapon_burst(index, burstmode = 1);
static cell AMX_NATIVE_CALL cs_set_weapon_burstmode(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerItem"  , m_iId);
	GET_OFFSET("CBasePlayerWeapon", m_iWeaponState);

	int index = params[1];
	int burst = params[2];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

	int weaponState = get_pdata<int>(pWeapon, m_iWeaponState);
	int weaponNewState = weaponState;

	switch (get_pdata<int>(pWeapon, m_iId))
	{
		case CSW_GLOCK18:
		{
			if (burst)
			{
				weaponNewState |= WPNSTATE_GLOCK18_BURST_MODE;
			}
			else
			{
				weaponNewState &= ~WPNSTATE_GLOCK18_BURST_MODE;
			}
			break;
		}
		case CSW_FAMAS:
		{
			if (burst)
			{
				weaponNewState |= WPNSTATE_FAMAS_BURST_MODE;
			}
			else
			{
				weaponNewState &= ~WPNSTATE_FAMAS_BURST_MODE;
			}
			break;
		}
	}

	if (weaponState != weaponNewState)
	{
		set_pdata<int>(pWeapon, m_iWeaponState, weaponNewState);

		if (UTIL_IsPlayer(pWeapon->v.owner))
		{
			UTIL_TextMsg_Generic(pWeapon->v.owner, burst ? "#Switch_To_BurstFire" : "#Switch_To_FullAuto");
		}

		return 1;
	}

	return 0;
}

// native cs_get_user_armor(index, &CsArmorType:armortype);
static cell AMX_NATIVE_CALL cs_get_user_armor(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iKevlar);

	int index = params[1];

	CHECK_PLAYER(index);

	edict_t *pPlayer = MF_GetPlayerEdict(index);
	cell *armorByRef = MF_GetAmxAddr(amx, params[2]);

	*armorByRef = get_pdata<int>(pPlayer, m_iKevlar);

	return static_cast<cell>(pPlayer->v.armorvalue);
}

// native cs_set_user_armor(index, armorvalue, CsArmorType:armortype);
static cell AMX_NATIVE_CALL cs_set_user_armor(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iKevlar);

	int index = params[1];
	int armor = params[2];
	int type  = params[3];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	pPlayer->v.armorvalue = armor;
	set_pdata<int>(pPlayer, m_iKevlar, type);
	
	if (type == CS_ARMOR_KEVLAR || type == CS_ARMOR_ASSAULTSUIT)
	{
		MESSAGE_BEGIN(MSG_ONE, MessageIdArmorType, nullptr, pPlayer);
			WRITE_BYTE(type == CS_ARMOR_ASSAULTSUIT ? 1 : 0);
		MESSAGE_END();
	}

	return 1;
}

// native cs_get_user_vip(index);
static cell AMX_NATIVE_CALL cs_get_user_vip(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bIsVIP);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (get_pdata<bool>(pPlayer, m_bIsVIP))
	{
		return 1;
	}

	return 0;
}

// native cs_set_user_vip(index, vip = 1, model = 1, scoreboard = 1);
static cell AMX_NATIVE_CALL cs_set_user_vip(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bIsVIP);
	GET_OFFSET("CBasePlayer", m_iModelName);

	int index = params[1];
	bool vip  = params[2] != 0;

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	bool updateModel, updateScoreboard;

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

	set_pdata<bool>(pPlayer, m_bIsVIP, vip);

	if (updateModel)
	{
		CS_Internal_Models modelName = CS_CT_VIP;

		if (!vip)
		{
			CS_Internal_Models CTmodels[5] = { CS_CT_URBAN, CS_CT_GSG9, CS_CT_GIGN, CS_CT_SAS, CZ_CT_SPETSNAZ };
			modelName = CTmodels[RANDOM_LONG(0, 4)];
		}

		set_pdata<int>(pPlayer, m_iModelName, modelName);
		MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer));
	}

	if (updateScoreboard)
	{
		int scoreattrib = SCOREATTRIB_VIP;

		if (!vip)
		{
			scoreattrib = (pPlayer->v.deadflag == DEAD_NO && pPlayer->v.health > 0) ? SCOREATTRIB_NOTHING : SCOREATTRIB_DEAD;
		}

		MESSAGE_BEGIN(MSG_ALL, MessageIdScoreAttrib);
			WRITE_BYTE(index);
			WRITE_BYTE(scoreattrib);
		MESSAGE_END();
	}	

	return 1;
}

// native CsTeams:cs_get_user_team(index, &any:model = CS_DONTCHANGE);
static cell AMX_NATIVE_CALL cs_get_user_team(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iModelName);
	GET_OFFSET("CBasePlayer", m_iTeam);

	int index = params[1];
	
	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if ((params[0] / sizeof(cell)) >= 2)
	{
		cell *modelByRef = MF_GetAmxAddr(amx, params[2]);
		*modelByRef = get_pdata<int>(pPlayer, m_iModelName);
	}

	return get_pdata<int>(pPlayer, m_iTeam);
}

// native cs_set_user_team(index, any:team, any:model = CS_DONTCHANGE);
static cell AMX_NATIVE_CALL cs_set_user_team(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iModelName);
	GET_OFFSET("CBasePlayer", m_iTeam);

	int index = params[1];
	int team  = params[2];
	int model = params[3];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	set_pdata<int>(pPlayer, m_iTeam, team);

	if (model > 0)
	{
		set_pdata<int>(pPlayer, m_iModelName, model);
	}

	MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer));

	char teaminfo[32];

	switch (team)
	{
		case TEAM_UNASSIGNED: strcpy(teaminfo, "UNASSIGNED"); break;
		case TEAM_T:          strcpy(teaminfo, "TERRORIST");  break;
		case TEAM_CT:         strcpy(teaminfo, "CT");         break;
		case TEAM_SPECTATOR:  strcpy(teaminfo, "SPECTATOR");  break;
		default:              sprintf(teaminfo, "TEAM_%i", team);
	}

	MESSAGE_BEGIN(MSG_ALL, MessageIdTeamInfo);
		WRITE_BYTE(index);
		WRITE_STRING(teaminfo);
	MESSAGE_END();
	
	MF_SetPlayerTeamInfo(index, team, team <= TEAM_SPECTATOR ? teaminfo : nullptr);

	return 1;
}

// native cs_get_user_buyzone(index);
static cell AMX_NATIVE_CALL cs_get_user_inside_buyzone(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_signals);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);
		
	if (get_pdata<CUnifiedSignals>(pPlayer, m_signals).GetState() & SIGNAL_BUY)
	{
		return 1;
	}

	return 0;
}

// native cs_get_user_mapzones(index);
static cell AMX_NATIVE_CALL cs_get_user_mapzones(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_signals);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return get_pdata<CUnifiedSignals>(pPlayer, m_signals).GetSignal();
}

// native cs_get_user_plant(index);
static cell AMX_NATIVE_CALL cs_get_user_plant(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasC4);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (get_pdata<bool>(pPlayer, m_bHasC4))
	{
		return 1;
	}

	return 0;
}

// native cs_set_user_plant(index, plant = 1, showbombicon = 1);
static cell AMX_NATIVE_CALL cs_set_user_plant(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasC4);

	int index  = params[1];
	bool plant = params[2] != 0;
	bool icon  = params[3] != 0;

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	set_pdata<bool>(pPlayer, m_bHasC4, plant);

	if (plant)
	{
		if (icon) 
		{
			MESSAGE_BEGIN(MSG_ONE, MessageIdStatusIcon, nullptr, pPlayer);
				WRITE_BYTE(1);
				WRITE_STRING("c4");
				WRITE_BYTE(DEFUSER_COLOUR_R);
				WRITE_BYTE(DEFUSER_COLOUR_G);
				WRITE_BYTE(DEFUSER_COLOUR_B);
			MESSAGE_END();
		}
	}
	else 
	{
		MESSAGE_BEGIN(MSG_ONE, MessageIdStatusIcon, nullptr, pPlayer);
			WRITE_BYTE(0);
			WRITE_STRING("c4");
		MESSAGE_END();
	}

	return 1;
}

// native cs_get_user_defuse(index);
static cell AMX_NATIVE_CALL cs_get_user_defusekit(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasDefuser);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (get_pdata<bool>(pPlayer, m_bHasDefuser))
	{
		return 1;
	}

	return 0;
}

// native cs_set_user_defuse(index, defusekit = 1, r = 0, g = 160, b = 0, icon[] = "defuser", flash = 0);
static cell AMX_NATIVE_CALL cs_set_user_defusekit(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasDefuser);

	int index = params[1];
	bool kit  = params[2] != 0;

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);
	
	set_pdata<bool>(pPlayer, m_bHasDefuser, kit);
	pPlayer->v.body = kit ? 1 : 0;

	if (kit)
	{
		int colour[3] = {DEFUSER_COLOUR_R, DEFUSER_COLOUR_G, DEFUSER_COLOUR_B};
		
		for (int i = 0; i < 3; i++)
		{
			if (params[i + 3] != -1)
			{
				colour[i] = params[i + 3];
			}
		}

		const char* icon = "defuser";

		if (params[6] != -1)
		{
			int length;
			icon = MF_GetAmxString(amx, params[6], 1, &length);
		}
	
		MESSAGE_BEGIN(MSG_ONE, MessageIdStatusIcon, nullptr, pPlayer);
			WRITE_BYTE(params[7] == 1 ? 2 : 1);
			WRITE_STRING(icon);
			WRITE_BYTE(colour[0]);
			WRITE_BYTE(colour[1]);
			WRITE_BYTE(colour[2]);
		MESSAGE_END();
	}
	else 
	{
		MESSAGE_BEGIN(MSG_ONE, MessageIdStatusIcon, nullptr, pPlayer);
			WRITE_BYTE(0);
			WRITE_STRING("defuser");
		MESSAGE_END();
	}

	return 1;
}

// native cs_get_user_bpammo(index, weapon);
static cell AMX_NATIVE_CALL cs_get_user_backpackammo(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer"      , m_rgpPlayerItems  );
	GET_OFFSET("CBasePlayer"      , m_rgAmmo          );
	GET_OFFSET("CBasePlayerItem"  , m_pNext           );
	GET_OFFSET("CBasePlayerItem"  , m_iId             );
	GET_OFFSET("CBasePlayerWeapon", m_iPrimaryAmmoType);

	int index = params[1];
	int weaponId = params[2];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (weaponId < CSW_P228 || weaponId > CSW_P90 || weaponId == CSW_KNIFE)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", params[2]);
		return 0;
	}

	for (size_t i = 0; i < MAX_WEAPON_SLOTS; ++i)
	{
		uintptr_t *pItem = get_pdata<uintptr_t*>(pPlayer, m_rgpPlayerItems, i);

		while (pItem)
		{
			if (weaponId == get_pdata<int>(pItem, m_iId))
			{
				return get_pdata<int>(pPlayer, m_rgAmmo, get_pdata<int>(pItem, m_iPrimaryAmmoType));
			}

			pItem = get_pdata<uintptr_t*>(pItem, m_pNext);
		}
	}
		
	return 0;
}

// native cs_set_user_bpammo(index, weapon, amount);
static cell AMX_NATIVE_CALL cs_set_user_backpackammo(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer"      , m_rgpPlayerItems  );
	GET_OFFSET("CBasePlayer"      , m_rgAmmo          );
	GET_OFFSET("CBasePlayerItem"  , m_pNext           );
	GET_OFFSET("CBasePlayerItem"  , m_iId             );
	GET_OFFSET("CBasePlayerWeapon", m_iPrimaryAmmoType);

	int index    = params[1];
	int weaponId = params[2];
	int amount   = params[3];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (weaponId < CSW_P228 || weaponId > CSW_P90 || weaponId == CSW_KNIFE)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid weapon id %d", params[2]);
		return 0;
	}

	for (size_t i = 0; i < MAX_WEAPON_SLOTS; ++i)
	{
		uintptr_t *pItem = get_pdata<uintptr_t*>(pPlayer, m_rgpPlayerItems, i);

		while (pItem)
		{
			if (weaponId == get_pdata<int>(pItem, m_iId))
			{
				set_pdata<int>(pPlayer, m_rgAmmo, amount, get_pdata<int>(pItem, m_iPrimaryAmmoType));
				return 1;
			}

			pItem = get_pdata<uintptr_t*>(pItem, m_pNext);
		}
	}
	
	return 0;
}

// native cs_get_user_nvg(index);
static cell AMX_NATIVE_CALL cs_get_user_nvg(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasNightVision);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (get_pdata<bool>(pPlayer, m_bHasNightVision))
	{
		return 1;
	}

	return 0;
}

// native cs_set_user_nvg(index, nvgoggles = 1);
static cell AMX_NATIVE_CALL cs_set_user_nvg(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasNightVision);

	int index = params[1];
	bool nvg  = params[2] != 0;

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (nvg && get_pdata<bool>(pPlayer, m_bHasNightVision))
	{
		UTIL_TextMsg_Generic(pPlayer, "#Already_Have_One");
		return 1;
	}

	set_pdata<bool>(pPlayer, m_bHasNightVision, nvg);

	return 1;
}

// native cs_get_user_model(index, model[], len);
static cell AMX_NATIVE_CALL cs_get_user_model(AMX *amx, cell *params)
{
	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return MF_SetAmxString(amx, params[2], GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model"), params[3]);
}

// native cs_set_user_model(index, const model[]);
static cell AMX_NATIVE_CALL cs_set_user_model(AMX *amx, cell *params)
{
	int index = params[1];
	int model = params[2];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (model == -1) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid model %d", params[2]);
		return 0;
	}

	char modelName[32];
	int length;

	strcpy(modelName, MF_GetAmxString(amx, params[2], 0, &length));
	
	g_players[index].SetModel(modelName);
	g_players[index].SetModelled(true);

	SETCLIENTKEYVALUE(index, GETINFOKEYBUFFER(pPlayer), "model", const_cast<char*>(g_players[index].GetModel()));

	return 1;
}

// native cs_reset_user_model(index);
static cell AMX_NATIVE_CALL cs_reset_user_model(AMX *amx, cell *params)
{
	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	g_players[index].SetModelled(false);

	MDLL_ClientUserInfoChanged(pPlayer, GETINFOKEYBUFFER(pPlayer));

	return 1;
}

// native cs_get_hostage_foll(index);
static cell AMX_NATIVE_CALL cs_get_hostage_follow(AMX *amx, cell *params)
{
	GET_OFFSET("CBaseMonster"  , m_hTargetEnt);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t* pHostage = INDEXENT(index);

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}

	edict_t *pEntity = get_pdata<EHANDLE>(pHostage, m_hTargetEnt).Get();

	return pEntity ? ENTINDEX(pEntity) : 0;
}

// native cs_set_hostage_foll(index, followedindex = 0);
static cell AMX_NATIVE_CALL cs_set_hostage_follow(AMX *amx, cell *params)
{
	GET_OFFSET("CBaseMonster", m_hTargetEnt);

	int index  = params[1];
	int target = params[2];

	CHECK_NONPLAYER(index);
	edict_t* pHostage = INDEXENT(index);

	if (target != 0)
	{
		CHECK_ENTITY(target);
	}

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}

	get_pdata<EHANDLE>(pHostage, m_hTargetEnt).Set(target ? GETEDICT(target) : nullptr);

	return 1;
}

// native cs_get_weapon_ammo(index);
static cell AMX_NATIVE_CALL cs_get_weapon_ammo(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerWeapon", m_iClip);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

	return get_pdata<int>(pWeapon, m_iClip);
}

// native cs_set_weapon_ammo(index, newammo);
static cell AMX_NATIVE_CALL cs_set_weapon_ammo(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayerWeapon", m_iClip);

	int index = params[1];
	int ammo  = params[2];

	CHECK_NONPLAYER(index);
	edict_t *pWeapon = INDEXENT(index);

	set_pdata<int>(pWeapon, m_iClip, ammo);

	return 1;
}

// native cs_get_user_hasprim(index);
static cell AMX_NATIVE_CALL cs_get_user_hasprimary(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bHasPrimary);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (get_pdata<bool>(pPlayer, m_bHasPrimary))
	{
		return 1;
	}

	return 0;
}

// native cs_get_no_knives();
static cell AMX_NATIVE_CALL cs_get_no_knives(AMX *amx, cell *params)
{
	return NoKifesMode ? 1 : 0;
}

// native cs_set_no_knives(noknives = 0);
static cell AMX_NATIVE_CALL cs_set_no_knives(AMX *amx, cell *params)
{
	if (!GiveDefaultItemsDetour)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Native cs_set_no_knives() is disabled");
		return 0;
	}

	NoKifesMode = params[1] != 0;

	if (NoKifesMode)
	{
		GiveDefaultItemsDetour->EnableDetour();
	}
	else
	{
		GiveDefaultItemsDetour->DisableDetour();
	}

	return 1;
}

// native cs_get_user_tked(index);
static cell AMX_NATIVE_CALL cs_get_user_tked(AMX *amx, cell *params)
{ 
	GET_OFFSET("CBasePlayer", m_bJustKilledTeammate);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	if (get_pdata<bool>(pPlayer, m_bJustKilledTeammate))
	{
		return 1;
	}

	return 0;
}

// native cs_set_user_tked(index, tk = 1, subtract = 1);
static cell AMX_NATIVE_CALL cs_set_user_tked(AMX *amx, cell *params)
{ 
	GET_OFFSET("CBasePlayer", m_bJustKilledTeammate);
	GET_OFFSET("CBasePlayer", m_iTeam);
	GET_OFFSET("CBasePlayer", m_iDeaths);

	int index    = params[1];
	int tk       = params[2];
	int subtract = params[3];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	set_pdata<bool>(pPlayer, m_bJustKilledTeammate, tk != 0);

	if (subtract > 0)
	{ 
		pPlayer->v.frags -= subtract;

		MESSAGE_BEGIN(MSG_ALL, MessageIdScoreInfo);
			WRITE_BYTE(index);
			WRITE_SHORT(static_cast<int>(pPlayer->v.frags));
			WRITE_SHORT(get_pdata<int>(pPlayer, m_iDeaths));
			WRITE_SHORT(0);
			WRITE_SHORT(get_pdata<int>(pPlayer, m_iTeam));
		MESSAGE_END(); 
	} 

	return 1; 
}

// native cs_get_user_driving(index);
static cell AMX_NATIVE_CALL cs_get_user_driving(AMX *amx, cell *params)
{ 
	GET_OFFSET("CBasePlayer", m_iTrain);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return get_pdata<int>(pPlayer, m_iTrain);
}

// native cs_get_user_stationary(index);
static cell AMX_NATIVE_CALL cs_get_user_stationary(AMX *amx, cell *params) 
{ 
	GET_OFFSET("CBasePlayer", m_iClientHideHUD);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return get_pdata<int>(pPlayer, m_iClientHideHUD);
}

// native cs_get_user_shield(index);
static cell AMX_NATIVE_CALL cs_get_user_shield(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_bOwnsShield);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);
   
	if (get_pdata<bool>(pPlayer, m_bOwnsShield))
	{
		return 1;
	}

	return 0;   
}

// native cs_user_spawn(player);
static cell AMX_NATIVE_CALL cs_user_spawn(AMX *amx, cell *params)
{
	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	pPlayer->v.deadflag = DEAD_RESPAWNABLE;
	MDLL_Think(pPlayer);

	if (MF_IsPlayerBot(index) && pPlayer->v.deadflag == DEAD_RESPAWNABLE) 
	{
		MDLL_Spawn(pPlayer);
	}

	return 1;
}

// native cs_get_armoury_type(index);
static cell AMX_NATIVE_CALL cs_get_armoury_type(AMX *amx, cell *params)
{
	GET_OFFSET("CArmoury", m_iItem);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pArmoury = INDEXENT(index);

	if (strcmp(STRING(pArmoury->v.classname), "armoury_entity")) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Not an armoury_entity! (%d)", index);
		return 0;
	}

	int weapontype = get_pdata<int>(pArmoury, m_iItem);;
	int weapontype_out = 0;

	switch (weapontype) 
	{
		case CSA_MP5NAVY:		weapontype_out = CSW_MP5NAVY;      break;
		case CSA_TMP:			weapontype_out = CSW_TMP;          break;
		case CSA_P90:			weapontype_out = CSW_P90;          break;
		case CSA_MAC10:			weapontype_out = CSW_MAC10;        break;
		case CSA_AK47:			weapontype_out = CSW_AK47;         break;
		case CSA_SG552:			weapontype_out = CSW_SG552;        break;
		case CSA_M4A1:			weapontype_out = CSW_M4A1;         break;
		case CSA_AUG:			weapontype_out = CSW_AUG;          break;
		case CSA_SCOUT:			weapontype_out = CSW_SCOUT;        break;
		case CSA_G3SG1:			weapontype_out = CSW_G3SG1;        break;
		case CSA_AWP:			weapontype_out = CSW_AWP;          break;
		case CSA_M3:			weapontype_out = CSW_M3;           break;
		case CSA_XM1014:		weapontype_out = CSW_XM1014;       break;
		case CSA_M249:			weapontype_out = CSW_M249;         break;
		case CSA_FLASHBANG:		weapontype_out = CSW_FLASHBANG;    break;
		case CSA_HEGRENADE:		weapontype_out = CSW_HEGRENADE;    break;
		case CSA_VEST:			weapontype_out = CSW_VEST;         break;
		case CSA_VESTHELM:		weapontype_out = CSW_VESTHELM;     break;
		case CSA_SMOKEGRENADE:	weapontype_out = CSW_SMOKEGRENADE; break;
		default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Unexpected weapon type of %d!", index);
			return 0;
		}
	}

	return weapontype_out;   
}

// native cs_set_armoury_type(index, type);
static cell AMX_NATIVE_CALL cs_set_armoury_type(AMX *amx, cell *params)
{
	GET_OFFSET("CArmoury", m_iItem);

	int index = params[1];
	int type  = params[2];

	CHECK_NONPLAYER(index);
	edict_t *pArmoury = INDEXENT(index);

	if (strcmp(STRING(pArmoury->v.classname), "armoury_entity"))
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Not an armoury_entity! (%d)", index);
		return 0;
	}

	int weapontype;

	switch (type)
	{
		case CSW_MP5NAVY:		weapontype = CSA_MP5NAVY;      break;
		case CSW_TMP:			weapontype = CSA_TMP;          break;
		case CSW_P90:			weapontype = CSA_P90;          break;
		case CSW_MAC10:			weapontype = CSA_MAC10;        break;
		case CSW_AK47:			weapontype = CSA_AK47;         break;
		case CSW_SG552:			weapontype = CSA_SG552;        break;
		case CSW_M4A1:			weapontype = CSA_M4A1;         break;
		case CSW_AUG:			weapontype = CSA_AUG;          break;
		case CSW_SCOUT:			weapontype = CSA_SCOUT;        break;
		case CSW_G3SG1:			weapontype = CSA_G3SG1;        break;
		case CSW_AWP:			weapontype = CSA_AWP;          break;
		case CSW_M3:			weapontype = CSA_M3;           break;
		case CSW_XM1014:		weapontype = CSA_XM1014;       break;
		case CSW_M249:			weapontype = CSA_M249;         break;
		case CSW_FLASHBANG:		weapontype = CSA_FLASHBANG;    break;
		case CSW_HEGRENADE:		weapontype = CSA_HEGRENADE;    break;
		case CSW_VEST:			weapontype = CSA_VEST;         break;
		case CSW_VESTHELM:		weapontype = CSA_VESTHELM;     break;
		case CSW_SMOKEGRENADE:	weapontype = CSA_SMOKEGRENADE; break;
		default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Unsupported weapon type! (%d)", type);
			return 0;
		}
	}

	set_pdata<int>(pArmoury, m_iItem, weapontype);

	return 1;
}

// native cs_set_user_zoom(index, type, mode);
static cell AMX_NATIVE_CALL cs_set_user_zoom(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iFOV);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	int type = params[2];
	int mode = params[3];
	int weapon = *static_cast<int *>(MF_PlayerPropAddr(index, Player_CurrentWeapon));
	
	g_zooming[index] = 0;

	if (type == CS_RESET_ZOOM)
	{
		set_pdata<int>(pPlayer, m_iFOV, CS_NO_ZOOM);
		return 1;
	}

	int value;

	switch (type)
	{
		case CS_SET_NO_ZOOM:
		{
			value = CS_NO_ZOOM;
			break;
		}
		case CS_SET_FIRST_ZOOM:
		{
			value = CS_FIRST_ZOOM;
			break;
		}
		case CS_SET_SECOND_ZOOM:
		{
			if (weapon == CSW_G3SG1 || weapon == CSW_SG550 || weapon == CSW_SCOUT)
			{
				value = CS_SECOND_NONAWP_ZOOM;
			}
			else
			{
				value = CS_SECOND_AWP_ZOOM;
			}
			break;
		}
		case CS_SET_AUGSG552_ZOOM:
		{
			value = CS_AUGSG552_ZOOM;
			break;
		}
		default:
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid zoom type %d", type);
			return 0;
		}
	}

	if (!mode)
	{
		g_zooming[index] = value;
	}

	set_pdata<int>(pPlayer, m_iFOV, value);

	return 1;
}

// native cs_get_user_zoom(index);
static cell AMX_NATIVE_CALL cs_get_user_zoom(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iFOV);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	switch (get_pdata<int>(pPlayer, m_iFOV))
	{
		case CS_NO_ZOOM:
		{
			return CS_SET_NO_ZOOM;
		}
		case CS_FIRST_ZOOM:
		{
			return CS_SET_FIRST_ZOOM;
		}
		case CS_SECOND_AWP_ZOOM:
		case CS_SECOND_NONAWP_ZOOM:
		{
			return CS_SET_SECOND_ZOOM;
		}
		case CS_AUGSG552_ZOOM:
		{
			return CS_SET_AUGSG552_ZOOM;
		}
	}

	return 0;
}

// native cs_get_user_submodel(index);
static cell AMX_NATIVE_CALL cs_get_user_submodel(AMX* amx, cell* params)
{
	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return pPlayer->v.body;
}

// native cs_set_user_submodel(index, value);
static cell AMX_NATIVE_CALL cs_set_user_submodel(AMX* amx, cell* params)
{
	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	pPlayer->v.body = params[2];

	return 1;
}

// native Float:cs_get_user_lastactivity(index);
static cell AMX_NATIVE_CALL cs_get_user_lastactivity(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_fLastMovement);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);
   
	return amx_ftoc(get_pdata<float>(pPlayer, m_fLastMovement));
}

// native cs_set_user_lastactivity(index, Float:value);
static cell AMX_NATIVE_CALL cs_set_user_lastactivity(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_fLastMovement);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);
   
	set_pdata<float>(pPlayer, m_fLastMovement, amx_ctof(params[2]));

	return 1;
}

// native cs_get_user_hostagekills(index);
static cell AMX_NATIVE_CALL cs_get_user_hostagekills(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iHostagesKilled);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	return get_pdata<int>(pPlayer, m_iHostagesKilled);
}

// native cs_set_user_hostagekills(index, value);
static cell AMX_NATIVE_CALL cs_set_user_hostagekills(AMX *amx, cell *params)
{
	GET_OFFSET("CBasePlayer", m_iHostagesKilled);

	int index = params[1];

	CHECK_PLAYER(index);
	edict_t *pPlayer = MF_GetPlayerEdict(index);

	set_pdata<int>(pPlayer, m_iHostagesKilled, params[2]);

	return 1;
}

// native Float:cs_get_hostage_lastuse(index);
static cell AMX_NATIVE_CALL cs_get_hostage_lastuse(AMX *amx, cell *params)
{
	GET_OFFSET("CHostage", m_flPathAcquired);

	int index = params[1];
   
	CHECK_NONPLAYER(index);
	edict_t *pHostage = INDEXENT(index);

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}
   
	return amx_ftoc(get_pdata<float>(pHostage, m_flPathAcquired));
}

// native cs_set_hostage_lastuse(index, Float:value);
static cell AMX_NATIVE_CALL cs_set_hostage_lastuse(AMX *amx, cell *params)
{
	GET_OFFSET("CHostage", m_flPathAcquired);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pHostage = INDEXENT(index);

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}
   
	set_pdata<float>(pHostage, m_flPathAcquired, amx_ctof(params[2]));

	return 1;
}

// native Float:cs_get_hostage_nextuse(index);
static cell AMX_NATIVE_CALL cs_get_hostage_nextuse(AMX* amx, cell* params)
{
	GET_OFFSET("CHostage", m_flNextChange);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pHostage = INDEXENT(index);

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}
   
	return amx_ftoc(get_pdata<float>(pHostage, m_flNextChange));
}

// native cs_set_hostage_nextuse(index, Float:value);
static cell AMX_NATIVE_CALL cs_set_hostage_nextuse(AMX* amx, cell* params)
{
	GET_OFFSET("CHostage", m_flNextChange);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pHostage = INDEXENT(index);

	if (strcmp(STRING(pHostage->v.classname), "hostage_entity") != 0) 
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not a hostage", index, STRING(pHostage->v.classname));
		return 0;
	}

	set_pdata<float>(pHostage, m_flNextChange, amx_ctof(params[2]));

	return 1;
}

// native Float:cs_get_c4_explode_time(index);
static cell AMX_NATIVE_CALL cs_get_c4_explode_time(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_flC4Blow);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pC4 = INDEXENT(index);

	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", index, STRING(pC4->v.classname));
		return 0;
	}

	return amx_ftoc(get_pdata<float>(pC4, m_flC4Blow));
}

// native cs_set_c4_explode_time(index, Float:value);
static cell AMX_NATIVE_CALL cs_set_c4_explode_time(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_flC4Blow);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pC4 = INDEXENT(index);

	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", index, STRING(pC4->v.classname));
		return 0;
	}

	set_pdata<float>(pC4, m_flC4Blow, amx_ctof(params[2]));

	return 1;
}

// native bool:cs_get_c4_defusing(c4index);
static cell AMX_NATIVE_CALL cs_get_c4_defusing(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_bStartDefuse);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pC4 = INDEXENT(index);

	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", index, STRING(pC4->v.classname));
		return 0;
	}

	return get_pdata<bool>(pC4, m_bStartDefuse) ? 1 : 0;
}

// native cs_set_c4_defusing(c4index, bool:defusing);
static cell AMX_NATIVE_CALL cs_set_c4_defusing(AMX* amx, cell* params)
{
	GET_OFFSET("CGrenade", m_bStartDefuse);

	int index = params[1];

	CHECK_NONPLAYER(index);
	edict_t *pC4 = INDEXENT(index);

	if (strcmp(STRING(pC4->v.classname), "grenade") != 0)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d (\"%s\") is not C4!", index, STRING(pC4->v.classname));
		return 0;
	}

	set_pdata<bool>(pC4, m_bStartDefuse, params[2] != 0);

	return 1;
}

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

AMX_NATIVE_INFO CstrikeNatives[] = 
{
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

	{nullptr,						nullptr}
};

void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) 
{
	// Reset player model a short while (MODELRESETTIME) after this if they are using an edited model.
	if(msg_type == MessageIdResetHUD) 
	{
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
void ClientDisconnect(edict_t *pEntity) 
{
	int index = ENTINDEX(pEntity);
	g_players[index].SetModelled(false);
	g_zooming[index] = 0;

	RETURN_META(MRES_IGNORED);
}

void ClientUserInfoChanged(edict_t *pEntity, char *infobuffer) 
{
	int index = ENTINDEX(pEntity);

	if(g_players[index].GetModelled() && pEntity->v.deadflag == DEAD_NO) 
	{
		RETURN_META(MRES_SUPERCEDE);
	} 
	else 
	{
		RETURN_META(MRES_IGNORED);
	}
}

void PlayerPostThink(edict_t* pPlayer) 
{
	int entityIndex = ENTINDEX(pPlayer);

	if(g_players[entityIndex].GetModelled()) 
	{
		if (g_players[entityIndex].GetInspectModel() && strcmp(g_players[entityIndex].GetModel(), GETCLIENTKEYVALUE(GETINFOKEYBUFFER(pPlayer), "model")) != 0) 
		{
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
