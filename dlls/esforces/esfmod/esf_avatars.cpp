#include <extdll.h>
#include <meta_api.h>
#include <eiface.h>
#include <edict.h>
#include "esforces.h"

int g_Avatars[33] = {0};

static cell AMX_NATIVE_CALL esf_create_avatar(AMX *amx, cell *params)
{
	int player = params[1];
	CHECKPLAYER(player);

	if (g_Avatars[player] == 0)
	{
		edict_t *pEntity = CREATE_NAMED_ENTITY(MAKE_STRING("env_model"));
		if (!pEntity || FNullEnt(pEntity))
			return 0;
		pEntity->v.movetype = MOVETYPE_FOLLOW;
		pEntity->v.aiment = MF_GetPlayerEdict(player);

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_remove_avatar(AMX *amx, cell *params)
{
	int player = params[1];
	CHECKPLAYER(player);

	if (g_Avatars[player])
	{
		edict_t *pEntity = INDEXENT(g_Avatars[player]);
		if (!pEntity || FNullEnt(pEntity))
		{
			g_Avatars[player] = 0;
			return 0;
		}
		REMOVE_ENTITY(pEntity);
		g_Avatars[player] = 0;

		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_has_avatar(AMX *amx, cell *params)
{
	int player = params[1];
	CHECKPLAYER(player);

	if (g_Avatars[player])
	{
		edict_t *pEntity = INDEXENT(g_Avatars[player]);
		if (!pEntity || FNullEnt(pEntity))
		{
			g_Avatars[player] = 0;
			return 0;
		}
		return 1;
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_avatar_getent(AMX *amx, cell *params)
{
	int player = params[1];
	CHECKPLAYER(player);

	if (g_Avatars[player])
	{
		edict_t *pEntity = INDEXENT(g_Avatars[player]);
		if (!pEntity || FNullEnt(pEntity))
		{
			g_Avatars[player] = 0;
			return 0;
		}
		return g_Avatars[player];
	}

	return 0;
}

static cell AMX_NATIVE_CALL esf_avatar_setmodel(AMX *amx, cell *params)
{
	int player = params[1];
	CHECKPLAYER(player);

	if (g_Avatars[player])
	{
		edict_t *pEntity = INDEXENT(g_Avatars[player]);
		if (!pEntity || FNullEnt(pEntity))
		{
			g_Avatars[player] = 0;
			return 0;
		}
		int len;
		const char *str = MF_GetAmxString(amx, params[2], 0, &len);
		SET_MODEL(pEntity, STRING(ALLOC_STRING(str)));

		return 1;
	}

	return 0;
}

AMX_NATIVE_INFO g_AvatarNatives[] = {
	{"esf_create_avatar",	esf_create_avatar},
	{"esf_remove_avatar",	esf_remove_avatar},
	{"esf_has_avatar",		esf_has_avatar},
	{"esf_avatar_getent",	esf_avatar_getent},
	{"esf_avatar_setmodel",	esf_avatar_setmodel},

	{NULL,					NULL},
};