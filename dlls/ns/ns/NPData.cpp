#include "ns.h"

int get_private(edict_t *pEntity, int woffset,int loffset)
{
#ifdef __linux__
	return *((int *)pEntity->pvPrivateData + loffset);
#else
	return *((int *)pEntity->pvPrivateData + woffset);
#endif
}
REAL get_private_f(edict_t *pEntity, int woffset, int loffset)
{
#ifdef __linux__
	return *((REAL *)pEntity->pvPrivateData + loffset);
#else
	return *((REAL *)pEntity->pvPrivateData + woffset);
#endif
}
void set_private(edict_t *pEntity, int woffset, int loffset, int value)
{
#ifdef __linux__
	*((int *)pEntity->pvPrivateData + loffset) = value;
#else
	*((int *)pEntity->pvPrivateData + woffset) = value;
#endif
}
void set_private(edict_t *pEntity, int woffset, int loffset, REAL value)
{
#ifdef __linux__
	*((REAL *)pEntity->pvPrivateData + loffset) = value;
#else
	*((REAL *)pEntity->pvPrivateData + woffset) = value;
#endif
}

static cell AMX_NATIVE_CALL ns_get_res(AMX *amx, cell *params)
{
	if (iscombat)
		return 0;
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;

	CPlayer *player = GET_PLAYER_I(id);

	if (!player->connected)
		return 0;
	if (player->edict->pvPrivateData == NULL) // Worth a shot to make sure it's initialized.
		return 0;
	REAL res = get_private_f(player->edict,OFFSET_WIN_RESOURCES,OFFSET_LIN_RESOURCES);
	return FLOAT_TO_CELL(res);
}

static cell AMX_NATIVE_CALL ns_set_res(AMX *amx, cell *params)
{
	if (iscombat)
		return 0;
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	REAL res = CELL_TO_FLOAT(params[2]);
	set_private(player->edict,OFFSET_WIN_RESOURCES,OFFSET_LIN_RESOURCES,res);
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_exp(AMX *amx, cell *params)
{
	if (!iscombat)
		return 0;
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;

	CPlayer *player = GET_PLAYER_I(id);

	if (!player->connected)
		return 0;
	if (player->edict->pvPrivateData == NULL) // Worth a shot to make sure it's initialized.
		return 0;
	REAL res = get_private_f(player->edict,OFFSET_WIN_EXP,OFFSET_LIN_EXP);
	return FLOAT_TO_CELL(res);
}

static cell AMX_NATIVE_CALL ns_set_exp(AMX *amx, cell *params)
{
	if (!iscombat)
		return 0;
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	REAL res = CELL_TO_FLOAT(params[2]);
	set_private(player->edict,OFFSET_WIN_EXP,OFFSET_LIN_EXP,res);
	return 1;
}

static cell AMX_NATIVE_CALL ns_get_points(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	return get_private(player->edict,OFFSET_WIN_POINTS,OFFSET_LIN_POINTS);
}
static cell AMX_NATIVE_CALL ns_set_points(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	set_private(player->edict,OFFSET_WIN_POINTS,OFFSET_LIN_POINTS,(int)params[2]);
	return 1;
}
static cell AMX_NATIVE_CALL ns_set_weapon_dmg(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	REAL dmg = CELL_TO_FLOAT(params[2]);
	set_private(pEntity,OFFSET_WIN_WEAPDMG,OFFSET_LIN_WEAPDMG,dmg);
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_weapon_dmg(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	return FLOAT_TO_CELL(get_private_f(pEntity,OFFSET_WIN_WEAPDMG,OFFSET_LIN_WEAPDMG));
}

static cell AMX_NATIVE_CALL ns_set_weapon_range(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	REAL dmg = CELL_TO_FLOAT(params[2]);
	set_private(pEntity,OFFSET_WIN_WEAPRANGE,OFFSET_LIN_WEAPRANGE,dmg);
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_weapon_range(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	return FLOAT_TO_CELL(get_private_f(pEntity,OFFSET_WIN_WEAPRANGE,OFFSET_LIN_WEAPRANGE));
}
static cell AMX_NATIVE_CALL ns_set_weapon_ammo(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	set_private(pEntity,OFFSET_WIN_WEAPCLIP,OFFSET_LIN_WEAPCLIP,(int)params[2]);
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_weapon_ammo(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	return get_private(pEntity,OFFSET_WIN_WEAPCLIP,OFFSET_LIN_WEAPCLIP);
}
static cell AMX_NATIVE_CALL ns_get_weap_reserve(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	switch (params[2])
	{
	case WEAPON_PISTOL:
		return get_private(player->edict,OFFSET_WIN_AMMO_PISTOL,OFFSET_LIN_AMMO_PISTOL);
	case WEAPON_LMG:
		return get_private(player->edict,OFFSET_WIN_AMMO_LMG,OFFSET_LIN_AMMO_LMG);
	case WEAPON_HMG:
		return get_private(player->edict,OFFSET_WIN_AMMO_HMG,OFFSET_LIN_AMMO_HMG);
	case WEAPON_GRENADE_GUN:
		return get_private(player->edict,OFFSET_WIN_AMMO_GL,OFFSET_LIN_AMMO_GL);
	case WEAPON_GRENADE:
		return get_private(player->edict,OFFSET_WIN_AMMO_HG,OFFSET_LIN_AMMO_HG);
	default:
		return 0;
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_set_weap_reserve(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	switch (params[2])
	{
	case WEAPON_PISTOL:
		set_private(player->edict,OFFSET_WIN_AMMO_PISTOL,OFFSET_LIN_AMMO_PISTOL,(int)params[3]);
		return 1;
	case WEAPON_LMG:
		set_private(player->edict,OFFSET_WIN_AMMO_LMG,OFFSET_LIN_AMMO_LMG,(int)params[3]);
		return 1;
	case WEAPON_HMG:
		set_private(player->edict,OFFSET_WIN_AMMO_HMG,OFFSET_LIN_AMMO_HMG,(int)params[3]);
		return 1;
	case WEAPON_GRENADE_GUN:
		set_private(player->edict,OFFSET_WIN_AMMO_GL,OFFSET_LIN_AMMO_GL,(int)params[3]);
		return 1;
	case WEAPON_GRENADE:
		set_private(player->edict,OFFSET_WIN_AMMO_HG,OFFSET_LIN_AMMO_HG,(int)params[3]);
		return 1;
	default:
		return 0;
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_get_score(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	return get_private(player->edict,OFFSET_WIN_SCORE,OFFSET_LIN_SCORE);
}
static cell AMX_NATIVE_CALL ns_set_score(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	if (!player->connected || player->edict->pvPrivateData == NULL)
		return 0;
	set_private(player->edict,OFFSET_WIN_SCORE,OFFSET_LIN_SCORE,(int)params[2]);
	return 1;
}
static cell AMX_NATIVE_CALL ns_set_hive_trait(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	set_private(pEntity,OFFSET_WIN_HIVE_TRAIT,OFFSET_LIN_HIVE_TRAIT,(int)params[2]);
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_hive_trait(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
		return 0;
	edict_t *pEntity = INDEXENT2(id);
	if (pEntity->pvPrivateData == NULL)
		return 0;
	return get_private(pEntity,OFFSET_WIN_HIVE_TRAIT,OFFSET_LIN_HIVE_TRAIT);
}
static cell AMX_NATIVE_CALL ns_get_deaths(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	return get_private(player->edict,OFFSET_WIN_DEATHS,OFFSET_LIN_DEATHS);
}
static cell AMX_NATIVE_CALL ns_set_deaths(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	set_private(player->edict,OFFSET_WIN_DEATHS,OFFSET_LIN_DEATHS,(int)params[2]);
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_icon(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	return get_private(player->edict,OFFSET_WIN_ICON,OFFSET_LIN_ICON);
}
static cell AMX_NATIVE_CALL ns_set_icon(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(id);
	set_private(player->edict,OFFSET_WIN_ICON,OFFSET_LIN_ICON,(int)params[2]);
	return 1;
}

AMX_NATIVE_INFO ns_pdata_natives[] = {
	   /*****************/
	{ "ns_get_res",				ns_get_res },
	{ "ns_set_res",				ns_set_res },

	{ "ns_get_exp",				ns_get_exp },
	{ "ns_set_exp",				ns_set_exp },

	{ "ns_get_points",			ns_get_points },
	{ "ns_set_points",			ns_set_points },

	{ "ns_set_weap_dmg",		ns_set_weapon_dmg },
	{ "ns_get_weap_dmg",		ns_get_weapon_dmg },

	{ "ns_set_weap_range",		ns_set_weapon_range },
	{ "ns_get_weap_range",		ns_get_weapon_range },

	{ "ns_set_weap_clip",		ns_set_weapon_ammo },
	{ "ns_get_weap_clip",		ns_get_weapon_ammo },

	{ "ns_set_weap_reserve",	ns_set_weap_reserve },
	{ "ns_get_weap_reserve",	ns_get_weap_reserve },

	{ "ns_set_score",			ns_set_score },
	{ "ns_get_score",			ns_get_score },

	{ "ns_get_deaths",			ns_get_deaths },
	{ "ns_set_deaths",			ns_set_deaths },

	{ "ns_get_icon",			ns_get_icon },
	{ "ns_set_icon",			ns_set_icon },

	{ "ns_get_hive_trait",		ns_get_hive_trait },
	{ "ns_set_hive_trait",		ns_set_hive_trait },

	{ NULL, NULL }
};

