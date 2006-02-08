#include "svencoop.h"

static cell AMX_NATIVE_CALL sc_get_frags(AMX *amx, cell *params)
{
	return GetFrags(params[1]);
}

static cell AMX_NATIVE_CALL sc_set_frags(AMX *amx, cell *params)
{
	SetFrags(params[1], params[2]);
	return 1;
}

static cell AMX_NATIVE_CALL sc_get_user_deaths(AMX *amx, cell *params)
{
	return GetDeaths(params[1]);
}

static cell AMX_NATIVE_CALL sc_set_user_deaths(AMX *amx, cell *params)
{
	SetDeaths(params[1], params[2] );
	return 1;
}

static cell AMX_NATIVE_CALL sc_is_player_ally(AMX *amx, cell *params)
{
	return IsEntAlly(params[1]);
}

static cell AMX_NATIVE_CALL sc_get_weapon_ammo(AMX *amx, cell *params)
{
	switch(params[2])
	{
		case SVEN_WEP_9MM:		return GetSvenWeapon(params[1], sven_9mm);
		case SVEN_WEP_SHOTGUN:	return GetSvenWeapon(params[1], sven_shotgun);
		case SVEN_WEP_RPG:		return GetSvenWeapon(params[1], sven_rpg);

		case SVEN_WEP_RADIO:	return GetSvenWeapon(params[1], sven_radio);
		case SVEN_WEP_SNARK:	return GetSvenWeapon(params[1], sven_snark);

		default: MF_LogError(amx, AMX_ERR_NATIVE,"Incorrect weapon specified in SvenCoop Get Weapon Native");
	}
	return 0;
}

static cell AMX_NATIVE_CALL sc_set_weapon_ammo(AMX *amx, cell *params)
{
	switch(params[3])
	{
		case SVEN_WEP_9MM:		return SetSvenWeapon(params[1], params[2], sven_9mm);
		case SVEN_WEP_SHOTGUN:	return SetSvenWeapon(params[1], params[2], sven_shotgun);
		case SVEN_WEP_RPG:		return SetSvenWeapon(params[1], params[2], sven_rpg);

		case SVEN_WEP_RADIO:	return SetSvenWeapon(params[1], params[2], sven_radio);
		case SVEN_WEP_SNARK:	return SetSvenWeapon(params[1], params[2], sven_snark);

		default: MF_LogError(amx, AMX_ERR_NATIVE,"Incorrect weapon specified in SvenCoop Set Weapon Native");
	}
	return 0;
}

AMX_NATIVE_INFO sven_Natives[] = {
  { "sc_get_frags",			sc_get_frags },
  { "sc_set_frags",			sc_set_frags },
  { "sc_get_user_deaths",	sc_get_user_deaths },
  { "sc_set_user_deaths",	sc_set_user_deaths },
  { "sc_is_player_ally",	sc_is_player_ally },

   { "sc_get_weapon_ammo",	sc_get_weapon_ammo },
  { "sc_set_weapon_ammo",	sc_set_weapon_ammo },

  { NULL, NULL }
};

void OnAmxxAttach()
{
	MF_AddNatives(sven_Natives);
}