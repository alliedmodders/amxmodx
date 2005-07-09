#include <extdll.h>
#include <meta_api.h>
#include <eiface.h>
#include <edict.h>
#include "esforces.h"

int g_PowerupMsg = 0;
int g_StopPowerupMsg = 0;
int g_ExplosionMsg = 0;

static cell AMX_NATIVE_CALL esf_start_powerup(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	if (!g_PowerupMsg)
	{
		g_PowerupMsg = GET_USER_MSG_ID(PLID, "Powerup", NULL);
		if (!g_PowerupMsg)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Could not find Powerup message!");
			return 0;
		}
	}

	MESSAGE_BEGIN(MSG_BROADCAST, g_PowerupMsg);
		WRITE_BYTE(index);
		WRITE_BYTE(params[2]);
		WRITE_BYTE(params[3]);
		WRITE_BYTE(params[4]);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL esf_stop_powerup(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	if (!g_StopPowerupMsg)
	{
		g_StopPowerupMsg = GET_USER_MSG_ID(PLID, "StopPowerup", NULL);
		if (!g_StopPowerupMsg)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Could not find StopPowerup message!");
			return 0;
		}
	}

	MESSAGE_BEGIN(MSG_ONE, g_StopPowerupMsg);
		WRITE_BYTE(index);
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL esf_explosion(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	if (params[3] < 0 || params[3] >= Explosions_Total)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid explosion id %d", params[3]);
		return 0;
	}

	if (!g_ExplosionMsg)
	{
		g_ExplosionMsg = GET_USER_MSG_ID(PLID, "Explosion", NULL);
		if (!g_ExplosionMsg)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Could not find Explosion message!");
			return 0;
		}
	}

	cell *cp = MF_GetAmxAddr(amx, params[1]);

	MESSAGE_BEGIN(MSG_BROADCAST, g_ExplosionMsg);
		WRITE_COORD( cp[0] );
		WRITE_COORD( cp[1] );
		WRITE_COORD( cp[2] );
		WRITE_LONG( params[2] );
		WRITE_BYTE( params[3] );
	MESSAGE_END();

	return 1;
}

AMX_NATIVE_INFO g_EffectsNatives[] = {
	{"esf_start_powerup",	esf_start_powerup},
	{"esf_stop_powerup",	esf_stop_powerup},
	{"esf_explosion",		esf_explosion},
	{NULL,					NULL},
};
