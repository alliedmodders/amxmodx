#include "ns.h"

// These are natives directly from NS2AMX


static cell AMX_NATIVE_CALL ns_has_weapon(AMX *amx,cell *params)
{
	CHECK_ENTITY(params[1]);
	edict_t *pEntity = NULL;
	pEntity = INDEXENT2(params[1]);
	if (params[3] == -1)
	{
		if ((pEntity->v.weapons & (1<<params[2])) > 0)
		{
			return 1;
		}
	}
	else
	{
		if ((pEntity->v.weapons & (1<<params[2])) > 0)
		{
			if (params[3] == 0)
			{
				pEntity->v.weapons &= ~(1<<params[2]);
				return 1;
			}
			return 0;
		}
		else
		{
			if (params[3] == 1)
			{
				pEntity->v.weapons |= (1<<params[2]);
				return 1;
			}
		}
		return 0;
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_get_spawnpoints(AMX *amx, cell *params)
{
	vec3_t vRet;
	if (params[2] == 0)
	{
		return ns_spawnpoints.getnum(params[1]);
	}
	else
	{
		vRet=ns_spawnpoints.getpoint(params[1],params[2]);
		cell *vCell = MF_GetAmxAddr(amx,params[3]);
		vCell[0] = FLOAT_TO_CELL(vRet.x);
		vCell[1] = FLOAT_TO_CELL(vRet.y);
		vCell[2] = FLOAT_TO_CELL(vRet.z);
	}
	return 1;
}

#define MASK_ELECTRICITY	8192
static cell AMX_NATIVE_CALL ns_get_build(AMX *amx, cell *params)
{
	int iLength;
	char *buildtype = MF_GetAmxString(amx,params[1],0,&iLength);
	int iBuiltOnly = params[2];
	int iNumber = params[3];
	edict_t* pBuild = NULL;
	int iCount=0;

	while ((pBuild = UTIL_FindEntityByString(pBuild,"classname",buildtype)) != NULL)
	{
		if (iBuiltOnly > 0)
		{
			if (FStrEq("team_advarmory",buildtype) || FStrEq("team_advturretfactory",buildtype))
			{
				iCount++;
			}
			else
			{
				if (pBuild->v.fuser1 >= 1000 || pBuild->v.iuser4 & MASK_ELECTRICITY)
				{
					iCount++;
				}
			}
		}
		else
		{
			iCount++;
		}
		if (iNumber > 0 && iCount == iNumber)
			return ENTINDEX(pBuild);
	}
	return iCount++;
}

static cell AMX_NATIVE_CALL ns_get_speedchange(AMX *amx, cell *params)
{
	// Params: get_speedchange(index)
	int index=params[1];
	if (!(params[1]>0 && params[1]<=gpGlobals->maxClients))
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);
	return player->speedchange;
}
static cell AMX_NATIVE_CALL ns_set_speedchange(AMX *amx, cell *params)
{
	// Params: set_speedchange(index,speedchange=0)
	if (!(params[1]>0&&params[1]<=32))
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);
	player->speedchange=params[2];
	return 1;
}
static cell AMX_NATIVE_CALL ns_get_maxspeed(AMX *amx, cell *params)
{
	// Params: get_maxspeed(index) (returns the max speed of the player BEFORE speed change is factored in.)
	if (!(params[1]>0 && params[1]<=32))
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);
	return player->maxspeed;
}
static cell AMX_NATIVE_CALL ns_set_player_model(AMX *amx, cell *params)
{
	// Params: set_player_model(id,szModel[])
	if (!(params[1] > 0 && params[1] <= gpGlobals->maxClients))
	{
		MF_Log("Can't set player model for a non-player entity.",MODULE_LOGTAG);
		return 0;
	}
	int len;
	char *temp = MF_GetAmxString(amx,params[2],0,&len);
	CPlayer *player = GET_PLAYER_I(params[1]);
	if (!FStrEq(temp,""))
	{
		PRECACHE_MODEL(temp);
		snprintf(player->model,127,"%s",temp);
		player->custommodel=true;

	}
	else
		player->custommodel=false;
	return 1;
}
static cell AMX_NATIVE_CALL ns_set_player_skin(AMX *amx, cell *params)
{
	// Params: set_player_skin(id,skin=-1)
	if (!(params[1] > 0 && params[1] <= gpGlobals->maxClients))
	{
		LOG_ERROR(PLID,"[%s] Can't set player skin for a non-player entity.",MODULE_LOGTAG);
		return 0;
	}
	CPlayer *player = GET_PLAYER_I(params[1]);
	if (params[2] < 0)
	{
		// Reset skin.
		player->skin=0;
		player->customskin=false;
	}
	else
	{
		player->customskin=true;
		player->skin=params[2];
	}
	return 1;
}
static cell AMX_NATIVE_CALL ns_set_player_body(AMX *amx, cell *params)
{
	// Params: set_player_body(id,body=-1)
	if (!(params[1] > 0 && params[1] <= gpGlobals->maxClients))
	{
		LOG_ERROR(PLID,"[%s] Can't set player body for a non-player entity.",MODULE_LOGTAG);
		return 0;
	}
	CPlayer *player = GET_PLAYER_I(params[1]);
	if (params[2] < 0)
	{
		// Reset body.
		player->body=0;
		player->custombody=false;
	}
	else
	{
		player->custombody=true;
		player->body=params[2];
	}
	return 1;
}

static cell AMX_NATIVE_CALL ns_get_class(AMX *amx, cell *params)
{
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);

	return player->GetClass();
}
static cell AMX_NATIVE_CALL ns_get_jpfuel(AMX *amx, cell *params)
{
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);
	return FLOAT_TO_CELL(player->pev->fuser3 / 10.0);
}
static cell AMX_NATIVE_CALL ns_set_jpfuel(AMX *amx, cell *params)
{
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);
	REAL fuel = CELL_TO_FLOAT(params[2]);
	if (fuel > 100.0)
		fuel = 100.0;
	if (fuel < 0.0)
		fuel = 0.0;
	player->pev->fuser3 = fuel * 10.0;
	return 1;
}
static cell AMX_NATIVE_CALL ns_is_combat(AMX *amx, cell *params)
{
	return iscombat;
}

static cell AMX_NATIVE_CALL ns_get_mask(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxEntities)
		return -1;
	edict_t *pEntity = INDEXENT2(params[1]);
	if (pEntity->v.iuser4 & params[2])
		return 1;
	return 0;
}
static cell AMX_NATIVE_CALL ns_set_mask(AMX *amx, cell *params)
{
	int id = params[1];
	if (id < 1 || id > gpGlobals->maxEntities)
		return -1;
	edict_t *pEntity = INDEXENT2(params[1]);
	if (params[3] > 0)
	{
		if (pEntity->v.iuser4 & params[2])
			return 0;
		pEntity->v.iuser4 |= params[2];
		return 1;
	}
	if (pEntity->v.iuser4 & params[2])
	{
		pEntity->v.iuser4 &= ~params[2];
		return 1;
	}
	return 0;
}

static cell AMX_NATIVE_CALL ns_popup(AMX *amx, cell *params)
{
	if (!gmsgHudText2)
		gmsgHudText2 = GET_USER_MSG_ID(PLID, "HudText2", NULL);
	if (params[1])
	{
		if (params[1] > gpGlobals->maxClients)
			return 0;
		CPlayer *player = GET_PLAYER_I(params[1]);
		MESSAGE_BEGIN(MSG_ONE,gmsgHudText2,NULL,player->edict);
	}
	else
		MESSAGE_BEGIN(MSG_ALL,gmsgHudText2);
	int len;
	char *blah = MF_GetAmxString(amx,params[2],0,&len);
	char msg[190];
	strncpy(msg,blah,188);
	WRITE_STRING(msg);
	WRITE_BYTE(params[3]);
	MESSAGE_END();
	return 1;
}
static cell AMX_NATIVE_CALL ns_set_fov(AMX *amx, cell *params)
{
	if (params[1] < 1 || params[1] > gpGlobals->maxClients)
		return 0;
	CPlayer *player = GET_PLAYER_I(params[1]);
	REAL fov = CELL_TO_FLOAT(params[2]);
	LOG_CONSOLE(PLID,"Got fov.  fov=%f",fov);
	int gmsgSetFov = GET_USER_MSG_ID(PLID, "SetFOV", NULL);
	if (fov == 0.0)
	{
		player->foved=false;
		player->fov=0.0;
		MESSAGE_BEGIN(MSG_ONE,gmsgSetFov,NULL,player->edict);
		WRITE_BYTE(0);
		MESSAGE_END();
		return 1;
	}
	if (fov > 0)
	{
		player->foved=true;
		player->fov=fov;
		MESSAGE_BEGIN(MSG_ONE,gmsgSetFov,NULL,player->edict);
		WRITE_BYTE((int)fov);
		MESSAGE_END();
		return 1;
	}
	return 0;
}
static cell AMX_NATIVE_CALL ns_giveitem(AMX *amx, cell *params)
{
	int index=params[1];
	int len;
	char *classname = MF_GetAmxString(amx,params[2],0,&len);
	if (index<1 || index>gpGlobals->maxClients)
		return 0;
	edict_t *player=INDEXENT2(index);
	if (player->v.deadflag > 0)
		return 0;
	edict_t *object=CREATE_NAMED_ENTITY(ALLOC_STRING(classname));	//create
	if (!object)
	{
		MF_Log("Error creating entity `%s`",classname);
		return 0;
	}
	SET_ORIGIN(object,player->v.origin);							// move to player
	gpGamedllFuncs->dllapi_table->pfnSpawn(object);					// emulate spawn
	object->v.flags |= FL_ONGROUND;									// make it think it's touched the ground
	gpGamedllFuncs->dllapi_table->pfnThink(object);					// 
	gpGamedllFuncs->dllapi_table->pfnTouch(player,object);			// give it to the player

	return 1;
}
static cell AMX_NATIVE_CALL ns_user_kill(AMX *amx, cell *params)
{
	int index = params[1];
	if (index<1||index>gpGlobals->maxClients)
		return 0;

	edict_t *e=INDEXENT2(index);
	if (e->v.iuser3 == 2 /* Commander class*/)
		return 0;

	if (MF_IsPlayerIngame(index) && MF_IsPlayerAlive(index))
	{
		float bef = e->v.frags;
		edict_t *pEntity = CREATE_NAMED_ENTITY(MAKE_STRING("trigger_hurt"));
		if (pEntity)
		{
			KeyValueData kvd;
			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="classname";
			kvd.szValue="trigger_hurt";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);
			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="dmg";
			kvd.szValue="20000.0";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);
			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="damagetype";
			kvd.szValue="1";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);
			kvd.szClassName="trigger_hurt";
			kvd.szKeyName="origin";
			kvd.szValue="8192 8192 8192";
			kvd.fHandled=0;
			MDLL_KeyValue(pEntity,&kvd);
			MDLL_Spawn(pEntity);
			pEntity->v.classname=MAKE_STRING("slay");
			MDLL_Touch(pEntity,e);
			REMOVE_ENTITY(pEntity);
		}
		if (params[2]) e->v.frags = bef;
		return 1;
	}

  return 0;
}
AMX_NATIVE_INFO ns_misc_natives[] = {
	   ///////////////////
	{ "user_kill",				ns_user_kill },


	{ "ns_get_build",			ns_get_build },

	{ "ns_set_player_model",	ns_set_player_model },
	{ "ns_set_player_skin",		ns_set_player_skin },
	{ "ns_set_player_body",		ns_set_player_body },

	{ "ns_has_weapon",			ns_has_weapon },

	{ "ns_get_spawn",			ns_get_spawnpoints },

	{ "ns_get_speedchange",		ns_get_speedchange },
	{ "ns_set_speedchange",		ns_set_speedchange },
	{ "ns_get_maxspeed",		ns_get_maxspeed },

	{ "ns_get_class",			ns_get_class },

	{ "ns_get_jpfuel",			ns_get_jpfuel },
	{ "ns_set_jpfuel",			ns_set_jpfuel },

	{ "ns_get_energy",			ns_get_jpfuel },  // They do the same thing...
	{ "ns_set_energy",			ns_set_jpfuel },  // 
	{ "ns_is_combat",			ns_is_combat  },

	{ "ns_get_mask",			ns_get_mask },
	{ "ns_set_mask",			ns_set_mask },

	{ "ns_popup",				ns_popup },

	{ "ns_set_fov",				ns_set_fov },

	{ "ns_give_item",			ns_giveitem },

	   ///////////////////

	{ NULL, NULL } 
};
