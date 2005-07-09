#include <extdll.h>
#include <meta_api.h>
#include <eiface.h>
#include <edict.h>
#include "esforces.h"

int g_MaxHealthMsg = 0;
int g_ChargeMsg = 0;

static cell AMX_NATIVE_CALL esf_get_class(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	return pEdict->v.playerclass;
}

static cell AMX_NATIVE_CALL esf_set_class(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	pEdict->v.playerclass = params[2];

	return 1;
}

static cell AMX_NATIVE_CALL esf_get_ki(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	return amx_ftoc((REAL)pEdict->v.fuser4);
}

static cell AMX_NATIVE_CALL esf_set_ki(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	REAL ki = amx_ctof(params[2]);

	if (ki < 0.0f || ki > 1000.0f)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "You cannot set ki above 1000.0!");
		return 0;
	}

	pEdict->v.fuser4 = (float)ki;

	return 1;
}

static cell AMX_NATIVE_CALL esf_get_swoopspeed(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	return amx_ftoc((REAL)pEdict->v.fuser1);
}

static cell AMX_NATIVE_CALL esf_set_swoopspeed(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	REAL speed = amx_ctof(params[2]);

	pEdict->v.fuser1 = (float)speed;

	return 1;
}

static cell AMX_NATIVE_CALL esf_set_powerlevel(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	int health = params[2];
	pEdict->v.health = (float)health;

	if (!g_MaxHealthMsg)
	{
		g_MaxHealthMsg = GET_USER_MSG_ID(PLID, "MaxHealth", NULL);
		if (!g_MaxHealthMsg)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Could not find MaxHealth message!");
			return 0;
		}
	}

	float vecOrigin[3] = {0.0f,0.0f,0.0f};

	MESSAGE_BEGIN(MSG_ONE, g_MaxHealthMsg, vecOrigin, pEdict);
		WRITE_BYTE( health );
	MESSAGE_END();

	return 1;
}

static cell AMX_NATIVE_CALL esf_set_chargebar(AMX *amx, cell *params)
{
	int index = params[1];
	CHECKPLAYER(index);

	edict_t *pEdict = INDEXENT(index);

	if (!g_ChargeMsg)
	{
		g_ChargeMsg = GET_USER_MSG_ID(PLID, "Charge", NULL);
		if (!g_ChargeMsg)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Could not find Charge message!");
			return 0;
		}
	}

	int value = params[2];

	float vecOrigin[3] = {0.0f,0.0f,0.0f};

	MESSAGE_BEGIN(MSG_ONE, g_ChargeMsg, vecOrigin, pEdict);
		WRITE_BYTE(value);
	MESSAGE_END();

	return 1;
}

AMX_NATIVE_INFO g_BaseNatives[] = {
 {"esf_get_class",		esf_get_class},
 {"esf_set_class",		esf_set_class},
 {"esf_get_ki",			esf_get_ki},
 {"esf_set_ki",			esf_set_ki},
 {"esf_get_swoopspeed",	esf_get_swoopspeed},
 {"esf_set_swoopspeed",	esf_set_swoopspeed},
 {"esf_set_powerlevel",	esf_set_powerlevel},
 {"esf_set_chargebar",	esf_set_chargebar},
 {NULL,					NULL},
};
