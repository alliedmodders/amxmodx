// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Engine Module
//

#include "entity.h"

int is_ent_valid(int iEnt)
{
	if (iEnt < 1 || iEnt > gpGlobals->maxEntities) 
		return 0;

	if (iEnt <= gpGlobals->maxClients)
	{
		if (!MF_IsPlayerIngame(iEnt))
		{
			return 0;
		}
	} else {
		if (FNullEnt(TypeConversion.id_to_edict(iEnt)))
		{
			return 0;
		}
	}

	return 1;
}

/***********
 Basic stuff
 ***********/

static cell AMX_NATIVE_CALL entity_range(AMX *amx, cell *params)
{
	int idxa = params[1];
	int idxb = params[2];

	CHECK_ENTITY(idxa);
	CHECK_ENTITY(idxb);

	edict_t *pEntA = TypeConversion.id_to_edict(idxa);
	edict_t *pEntB = TypeConversion.id_to_edict(idxb);

	REAL fRet = (pEntA->v.origin - pEntB->v.origin).Length();

	return amx_ftoc(fRet);
}

/*********************
  Entity control stuff 
  ********************/

static cell AMX_NATIVE_CALL call_think(AMX *amx, cell *params)
{
	int iEnt = params[1];

	CHECK_ENTITY(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	MDLL_Think(pEnt);

	return 1;
}


static cell AMX_NATIVE_CALL fake_touch(AMX *amx, cell *params)
{
	int iPtr = params[1];
	int iPtd = params[2];

	CHECK_ENTITY(iPtr);
	CHECK_ENTITY(iPtd);

	edict_t *pToucher = TypeConversion.id_to_edict(iPtr);
	edict_t *pTouched = TypeConversion.id_to_edict(iPtd);

	MDLL_Touch(pToucher, pTouched);

	return 1;
}

static cell AMX_NATIVE_CALL force_use(AMX *amx, cell *params)
{
	int iPtr = params[1];
	int iPtd = params[2];

	CHECK_ENTITY(iPtr);
	CHECK_ENTITY(iPtd);

	edict_t *pUser = TypeConversion.id_to_edict(iPtr);
	edict_t *pUsed = TypeConversion.id_to_edict(iPtd);

	MDLL_Use(pUsed, pUser);

	return 1;
}

static cell AMX_NATIVE_CALL create_entity(AMX *amx, cell *params)
{
	int len;
	int iszClass = AmxStringToEngine(amx, params[1], len);

	edict_t *pEnt = CREATE_NAMED_ENTITY(iszClass);

	if (FNullEnt(pEnt))
		return 0;

	return ENTINDEX(pEnt);
}

static cell AMX_NATIVE_CALL remove_entity(AMX *amx, cell *params)
{
	int id = params[1];
	if (id <= gpGlobals->maxClients || id > gpGlobals->maxEntities)
	{
		MF_LogError(amx, AMX_ERR_NATIVE, "Entity %d can not be removed", id);
		return 0;
	}
	
	edict_t *pEnt = TypeConversion.id_to_edict(id);

	if (FNullEnt(pEnt))
		return 0;

	REMOVE_ENTITY(pEnt);

	return 1;
}

static cell AMX_NATIVE_CALL entity_count(AMX *amx, cell *params)
{
	return NUMBER_OF_ENTITIES();
}

static cell AMX_NATIVE_CALL is_valid_ent(AMX *amx, cell *params)
{
	return is_ent_valid(params[1]);
}

//uses mahnsawce's version now
static cell AMX_NATIVE_CALL DispatchKeyValue(AMX *amx, cell *params)
{
	int count = *params / sizeof(cell);

	if (count == 3) 
	{
		cell *cVal = MF_GetAmxAddr(amx, params[1]);
		int iValue = *cVal;

		CHECK_ENTITY_SIMPLE(iValue);

		edict_t *pEntity = TypeConversion.id_to_edict(iValue);
		KeyValueData kvd;
		int iLength=0;
		char *char1 = MF_GetAmxString(amx, params[2], 0, &iLength);
		char *char2 = MF_GetAmxString(amx, params[3], 1, &iLength);

		kvd.szClassName = (char*)STRING(pEntity->v.classname);
		kvd.szKeyName = char1;
		kvd.szValue = char2;
		kvd.fHandled = 0;

		MDLL_KeyValue(pEntity, &kvd);
	} else if (count == 2) {
		if (!g_inKeyValue)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "DispatchKeyValue() with two arguments can only be used inside of pfn_keyvalue()");
			return 0;
		}

		int iLength;
		char *char1 = MF_GetAmxString(amx, params[1], 0, &iLength);
		char *char2 = MF_GetAmxString(amx, params[2], 1, &iLength);
		const char *charA = STRING(ALLOC_STRING(char1));
		const char *charB = STRING(ALLOC_STRING(char2));

		g_pkvd->szKeyName = const_cast<char *>(charA);
		g_pkvd->szValue = const_cast<char *>(charB);
	}
	return 1;
}

static cell AMX_NATIVE_CALL get_keyvalue(AMX *amx, cell *params)
{
	int idx = params[1];
	CHECK_ENTITY(idx);
	edict_t *pEntity = TypeConversion.id_to_edict(idx);
	int iLength=0;
	char *char1 = MF_GetAmxString(amx, params[2], 1, &iLength);
	char *val = INFO_KEY_VALUE(INFO_KEY_BUFFER(pEntity), char1);
	return MF_SetAmxStringUTF8Char(amx, params[3], val, strlen(val), params[4]); 
}

static cell AMX_NATIVE_CALL copy_keyvalue(AMX *amx, cell *params)
{
	if (!g_inKeyValue)
		return 0;

	if (g_pkvd->szClassName)
		MF_SetAmxStringUTF8Char(amx, params[1], g_pkvd->szClassName, strlen(g_pkvd->szClassName), params[2]);
	if (g_pkvd->szKeyName)
		MF_SetAmxStringUTF8Char(amx, params[3], g_pkvd->szKeyName, strlen(g_pkvd->szKeyName), params[4]);
	if (g_pkvd->szValue)
		MF_SetAmxStringUTF8Char(amx, params[5], g_pkvd->szValue, strlen(g_pkvd->szValue), params[6]);

	return 1;
}

static cell AMX_NATIVE_CALL DispatchSpawn(AMX *amx, cell *params)
{
	int iEnt = params[1];

	CHECK_ENTITY(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);
	
	MDLL_Spawn(pEnt);

	return 1;
}

/***************************
	Entity modification stuff
 ***************************/

static cell AMX_NATIVE_CALL entity_get_float(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	REAL fVal = 0;

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case impacttime:
			fVal = pEnt->v.impacttime;
			break;
		case starttime:
			fVal = pEnt->v.starttime;
			break;
		case idealpitch:
			fVal = pEnt->v.idealpitch;
			break;
		case pitch_speed:
			fVal = pEnt->v.pitch_speed;
			break;
		case ideal_yaw:
			fVal = pEnt->v.ideal_yaw;
			break;
		case yaw_speed:
			fVal = pEnt->v.yaw_speed;
			break;
		case ltime:
			fVal = pEnt->v.ltime;
			break;
		case nextthink:
			fVal = pEnt->v.nextthink;
			break;
		case gravity:
			fVal = pEnt->v.gravity;
			break;
		case friction:
			fVal = pEnt->v.friction;
			break;
		case frame:
			fVal = pEnt->v.frame;
			break;
		case animtime:
			fVal = pEnt->v.animtime;
			break;
		case framerate:
			fVal = pEnt->v.framerate;
			break;
		case health:
			fVal = pEnt->v.health;
			break;
		case frags:
			fVal = pEnt->v.frags;
			break;
		case takedamage:
			fVal = pEnt->v.takedamage;
			break;
		case max_health:
			fVal = pEnt->v.max_health;
			break;
		case teleport_time:
			fVal = pEnt->v.teleport_time;
			break;
		case armortype:
			fVal = pEnt->v.armortype;
			break;
		case armorvalue:
			fVal = pEnt->v.armorvalue;
			break;
		case dmg_take:
			fVal = pEnt->v.dmg_take;
			break;
		case dmg_save:
			fVal = pEnt->v.dmg_save;
			break;
		case dmg:
			fVal = pEnt->v.dmg;
			break;
		case dmgtime:
			fVal = pEnt->v.dmgtime;
			break;
		case speed:
			fVal = pEnt->v.speed;
			break;
		case air_finished:
			fVal = pEnt->v.air_finished;
			break;
		case pain_finished:
			fVal = pEnt->v.pain_finished;
			break;
		case radsuit_finished:
			fVal = pEnt->v.radsuit_finished;
			break;
		case scale:
			fVal = pEnt->v.scale;
			break;
		case renderamt:
			fVal = pEnt->v.renderamt;
			break;
		case maxspeed:
			fVal = pEnt->v.maxspeed;
			break;
		case fov:
			fVal = pEnt->v.fov;
			break;
		case flFallVelocity:
			fVal = pEnt->v.flFallVelocity;
			break;
		case fuser1:
			fVal = pEnt->v.fuser1;
			break;
		case fuser2:
			fVal = pEnt->v.fuser2;
			break;
		case fuser3:
			fVal = pEnt->v.fuser3;
			break;
		case fuser4:
			fVal = pEnt->v.fuser4;
			break;
		default:
			return 0;
			break;
	}

	return amx_ftoc(fVal);
}

static cell AMX_NATIVE_CALL entity_set_float(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	REAL fVal = amx_ctof(params[3]);

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case impacttime:
			pEnt->v.impacttime = fVal;
			break;
		case starttime:
			pEnt->v.starttime = fVal;
			break;
		case idealpitch:
			pEnt->v.idealpitch = fVal;
			break;
		case pitch_speed:
			pEnt->v.pitch_speed = fVal;
			break;
		case ideal_yaw:
			pEnt->v.ideal_yaw = fVal;
			break;
		case yaw_speed:
			pEnt->v.yaw_speed = fVal;
			break;
		case ltime:
			pEnt->v.ltime = fVal;
			break;
		case nextthink:
			pEnt->v.nextthink = fVal;
			break;
		case gravity:
			pEnt->v.gravity = fVal;
			break;
		case friction:
			pEnt->v.friction = fVal;
			break;
		case frame:
			pEnt->v.frame = fVal;
			break;
		case animtime:
			pEnt->v.animtime = fVal;
			break;
		case framerate:
			pEnt->v.framerate = fVal;
			break;
		case health:
			pEnt->v.health = fVal;
			break;
		case frags:
			pEnt->v.frags = fVal;
			break;
		case takedamage:
			pEnt->v.takedamage = fVal;
			break;
		case max_health:
			pEnt->v.max_health = fVal;
			break;
		case teleport_time:
			pEnt->v.teleport_time = fVal;
			break;
		case armortype:
			pEnt->v.armortype = fVal;
			break;
		case armorvalue:
			pEnt->v.armorvalue = fVal;
			break;
		case dmg_take:
			pEnt->v.dmg_take = fVal;
			break;
		case dmg_save:
			pEnt->v.dmg_save = fVal;
			break;
		case dmg:
			pEnt->v.dmg = fVal;
			break;
		case dmgtime:
			pEnt->v.dmgtime = fVal;
			break;
		case speed:
			pEnt->v.speed = fVal;
			break;
		case air_finished:
			pEnt->v.air_finished = fVal;
			break;
		case pain_finished:
			pEnt->v.pain_finished = fVal;
			break;
		case radsuit_finished:
			pEnt->v.radsuit_finished = fVal;
			break;
		case scale:
			pEnt->v.scale = fVal;
			break;
		case renderamt:
			pEnt->v.renderamt = fVal;
			break;
		case maxspeed:
			pEnt->v.maxspeed = fVal;
			break;
		case fov:
			pEnt->v.fov = fVal;
			break;
		case flFallVelocity:
			pEnt->v.flFallVelocity = fVal;
			break;
		case fuser1:
			pEnt->v.fuser1 = fVal;
			break;
		case fuser2:
			pEnt->v.fuser2 = fVal;
			break;
		case fuser3:
			pEnt->v.fuser3 = fVal;
			break;
		case fuser4:
			pEnt->v.fuser4 = fVal;
			break;
		default:
			return 0;
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL entity_get_int(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iRetValue = 0;

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case gamestate:
			iRetValue = pEnt->v.gamestate;
			break;
		case oldbuttons:
			iRetValue = pEnt->v.oldbuttons;
			break;
		case groupinfo:
			iRetValue = pEnt->v.groupinfo;
			break;
		case iuser1:
			iRetValue = pEnt->v.iuser1;
			break;
		case iuser2:
			iRetValue = pEnt->v.iuser2;
			break;
		case iuser3:
			iRetValue = pEnt->v.iuser3;
			break;
		case iuser4:
			iRetValue = pEnt->v.iuser4;
			break;
		case weaponanim:
			iRetValue = pEnt->v.weaponanim;
			break;
		case pushmsec:
			iRetValue = pEnt->v.pushmsec;
			break;
		case bInDuck:
			iRetValue = pEnt->v.bInDuck;
			break;
		case flTimeStepSound:
			iRetValue = pEnt->v.flTimeStepSound;
			break;
		case flSwimTime:
			iRetValue = pEnt->v.flSwimTime;
			break;
		case flDuckTime:
			iRetValue = pEnt->v.flDuckTime;
			break;
		case iStepLeft:
			iRetValue = pEnt->v.iStepLeft;
			break;
		case movetype:
			iRetValue = pEnt->v.movetype;
			break;
		case solid:
			iRetValue = pEnt->v.solid;
			break;
		case skin:
			iRetValue = pEnt->v.skin;
			break;
		case body:
			iRetValue = pEnt->v.body;
			break;
		case effects:
			iRetValue = pEnt->v.effects;
			break;
		case light_level:
			iRetValue = pEnt->v.light_level;
			break;
		case sequence:
			iRetValue = pEnt->v.sequence;
			break;
		case gaitsequence:
			iRetValue = pEnt->v.gaitsequence;
			break;
		case modelindex:
			iRetValue = pEnt->v.modelindex;
			break;
		case playerclass:
			iRetValue = pEnt->v.playerclass;
			break;
		case waterlevel:
			iRetValue = pEnt->v.waterlevel;
			break;
		case watertype:
			iRetValue = pEnt->v.watertype;
			break;
		case spawnflags:
			iRetValue = pEnt->v.spawnflags;
			break;
		case flags:
			iRetValue = pEnt->v.flags;
			break;
		case colormap:
			iRetValue = pEnt->v.colormap;
			break;
		case team:
			iRetValue = pEnt->v.team;
			break;
		case fixangle:
			iRetValue = pEnt->v.fixangle;
			break;
		case weapons:
			iRetValue = pEnt->v.weapons;
			break;
		case rendermode:
			iRetValue = pEnt->v.rendermode;
			break;
		case renderfx:
			iRetValue = pEnt->v.renderfx;
			break;
		case button:
			iRetValue = pEnt->v.button;
			break;
		case impulse:
			iRetValue = pEnt->v.impulse;
			break;
		case deadflag:
			iRetValue = pEnt->v.deadflag;
			break;
		default:
			return 0;
			break;
	}

	return iRetValue;
}

static cell AMX_NATIVE_CALL entity_set_int(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iNewValue = params[3];

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case gamestate:
			pEnt->v.gamestate = iNewValue;
			break;
		case oldbuttons:
			pEnt->v.oldbuttons = iNewValue;
			break;
		case groupinfo:
			pEnt->v.groupinfo = iNewValue;
			break;
		case iuser1:
			pEnt->v.iuser1 = iNewValue;
			break;
		case iuser2:
			pEnt->v.iuser2 = iNewValue;
			break;
		case iuser3:
			pEnt->v.iuser3 = iNewValue;
			break;
		case iuser4:
			pEnt->v.iuser4 = iNewValue;
			break;
		case weaponanim:
			pEnt->v.weaponanim = iNewValue;
			break;
		case pushmsec:
			pEnt->v.pushmsec = iNewValue;
			break;
		case bInDuck:
			pEnt->v.bInDuck = iNewValue;
			break;
		case flTimeStepSound:
			pEnt->v.flTimeStepSound = iNewValue;
			break;
		case flSwimTime:
			pEnt->v.flSwimTime = iNewValue;
			break;
		case flDuckTime:
			pEnt->v.flDuckTime = iNewValue;
			break;
		case iStepLeft:
			pEnt->v.iStepLeft = iNewValue;
			break;
		case movetype:
			pEnt->v.movetype = iNewValue;
			break;
		case solid:
			pEnt->v.solid = iNewValue;
			break;
		case skin:
			pEnt->v.skin = iNewValue;
			break;
		case body:
			pEnt->v.body = iNewValue;
			break;
		case effects:
			pEnt->v.effects = iNewValue;
			break;
		case light_level:
			pEnt->v.light_level = iNewValue;
			break;
		case sequence:
			pEnt->v.sequence = iNewValue;
			break;
		case gaitsequence:
			pEnt->v.gaitsequence = iNewValue;
			break;
		case modelindex:
			pEnt->v.modelindex = iNewValue;
			break;
		case playerclass:
			pEnt->v.playerclass = iNewValue;
			break;
		case waterlevel:
			pEnt->v.waterlevel = iNewValue;
			break;
		case watertype:
			pEnt->v.watertype = iNewValue;
			break;
		case spawnflags:
			pEnt->v.spawnflags = iNewValue;
			break;
		case flags:
			pEnt->v.flags = iNewValue;
			break;
		case colormap:
			pEnt->v.colormap = iNewValue;
			break;
		case team:
			pEnt->v.team = iNewValue;
			break;
		case fixangle:
			pEnt->v.fixangle = iNewValue;
			break;
		case weapons:
			pEnt->v.weapons = iNewValue;
			break;
		case rendermode:
			pEnt->v.rendermode = iNewValue;
			break;
		case renderfx:
			pEnt->v.renderfx = iNewValue;
			break;
		case button:
			pEnt->v.button = iNewValue;
			break;
		case impulse:
			pEnt->v.impulse = iNewValue;
			break;
		case deadflag:
			pEnt->v.deadflag = iNewValue;
			break;
		default:
			return 0;
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL entity_get_vector(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	cell *vRet = MF_GetAmxAddr(amx, params[3]);
	Vector vRetValue = Vector(0, 0, 0);

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case origin:
			vRetValue = pEnt->v.origin;
			break;
		case oldorigin:
			vRetValue = pEnt->v.oldorigin;
			break;
		case velocity:
			vRetValue = pEnt->v.velocity;
			break;
		case basevelocity:
			vRetValue = pEnt->v.basevelocity;
			break;
		case clbasevelocity:
			vRetValue = pEnt->v.clbasevelocity;
			break;
		case movedir:
			vRetValue = pEnt->v.movedir;
			break;
		case angles:
			vRetValue = pEnt->v.angles;
			break;
		case avelocity:
			vRetValue = pEnt->v.avelocity;
			break;
		case punchangle:
			vRetValue = pEnt->v.punchangle;
			break;
		case v_angle:
			vRetValue = pEnt->v.v_angle;
			break;
		case endpos:
			vRetValue = pEnt->v.endpos;
			break;
		case startpos:
			vRetValue = pEnt->v.startpos;
			break;
		case absmin:
			vRetValue = pEnt->v.absmin;
			break;
		case absmax:
			vRetValue = pEnt->v.absmax;
			break;
		case mins:
			vRetValue = pEnt->v.mins;
			break;
		case maxs:
			vRetValue = pEnt->v.maxs;
			break;
		case size:
			vRetValue = pEnt->v.size;
			break;
		case rendercolor:
			vRetValue = pEnt->v.rendercolor;
			break;
		case view_ofs:
			vRetValue = pEnt->v.view_ofs;
			break;
		case vuser1:
			vRetValue = pEnt->v.vuser1;
			break;
		case vuser2:
			vRetValue = pEnt->v.vuser2;
			break;
		case vuser3:
			vRetValue = pEnt->v.vuser3;
			break;
		case vuser4:
			vRetValue = pEnt->v.vuser4;
			break;
		default:
			return 0;
			break;
	}

	vRet[0] = amx_ftoc(vRetValue.x);
	vRet[1] = amx_ftoc(vRetValue.y);
	vRet[2] = amx_ftoc(vRetValue.z);

	return 1;
}

static cell AMX_NATIVE_CALL entity_set_vector(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	cell *vAmx = MF_GetAmxAddr(amx, params[3]);

	CHECK_ENTITY_SIMPLE(iEnt);

	REAL fX = amx_ctof(vAmx[0]);
	REAL fY = amx_ctof(vAmx[1]);
	REAL fZ = amx_ctof(vAmx[2]);
	Vector vSet = Vector(fX, fY, fZ);
	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case origin:
			pEnt->v.origin = vSet;
			break;
		case oldorigin:
			pEnt->v.oldorigin = vSet;
			break;
		case velocity:
			pEnt->v.velocity = vSet;
			break;
		case basevelocity:
			pEnt->v.basevelocity = vSet;
			break;
		case clbasevelocity:
			pEnt->v.clbasevelocity = vSet;
			break;
		case movedir:
			pEnt->v.movedir = vSet;
			break;
		case angles:
			pEnt->v.angles = vSet;
			break;
		case avelocity:
			pEnt->v.avelocity = vSet;
			break;
		case punchangle:
			pEnt->v.punchangle = vSet;
			break;
		case v_angle:
			pEnt->v.v_angle = vSet;
			break;
		case endpos:
			pEnt->v.endpos = vSet;
			break;
		case startpos:
			pEnt->v.startpos = vSet;
			break;
		case absmin:
			pEnt->v.absmin = vSet;
			break;
		case absmax:
			pEnt->v.absmax = vSet;
			break;
		case mins:
			pEnt->v.mins = vSet;
			break;
		case maxs:
			pEnt->v.maxs = vSet;
			break;
		case size:
			pEnt->v.size = vSet;
			break;
		case rendercolor:
			pEnt->v.rendercolor = vSet;
			break;
		case view_ofs:
			pEnt->v.view_ofs = vSet;
			break;
		case vuser1:
			pEnt->v.vuser1 = vSet;
			break;
		case vuser2:
			pEnt->v.vuser2 = vSet;
			break;
		case vuser3:
			pEnt->v.vuser3 = vSet;
			break;
		case vuser4:
			pEnt->v.vuser4 = vSet;
			break;
		default:
			return 0;
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL entity_get_string(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iszString = 0;
	const char *szRet = NULL;

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case classname:
			iszString = pEnt->v.classname;
			break;
		case globalname:
			iszString = pEnt->v.globalname;
			break;
		case model:
			iszString = pEnt->v.model;
			break;
		case target:
			iszString = pEnt->v.target;
			break;
		case targetname:
			iszString = pEnt->v.targetname;
			break;
		case netname:
			iszString = pEnt->v.netname;
			break;
		case message:
			iszString = pEnt->v.message;
			break;
		case noise:
			iszString = pEnt->v.noise;
			break;
		case noise1:
			iszString = pEnt->v.noise1;
			break;
		case noise2:
			iszString = pEnt->v.noise2;
			break;
		case noise3:
			iszString = pEnt->v.noise3;
			break;
		case viewmodel:
			iszString = pEnt->v.viewmodel;
			break;
		case weaponmodel:
			iszString = pEnt->v.weaponmodel;
			break;
		default:
			return 0;
			break;
	}

	szRet = STRING(iszString);

	return MF_SetAmxString(amx, params[3], szRet, params[4]);
}

static cell AMX_NATIVE_CALL entity_set_string(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iLen;
	int iszString = AmxStringToEngine(amx, params[3], iLen);

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);
	
	switch (idx)
	{
		case classname:
			pEnt->v.classname = iszString;
			break;
		case globalname:
			pEnt->v.globalname = iszString;
			break;
		case model:
			pEnt->v.model = iszString;
			break;
		case target:
			pEnt->v.target = iszString;
			break;
		case targetname:
			pEnt->v.targetname = iszString;
			break;
		case netname:
			pEnt->v.netname = iszString;
			break;
		case message:
			pEnt->v.message = iszString;
			break;
		case noise:
			pEnt->v.noise = iszString;
			break;
		case noise1:
			pEnt->v.noise1 = iszString;
			break;
		case noise2:
			pEnt->v.noise2 = iszString;
			break;
		case noise3:
			pEnt->v.noise3 = iszString;
			break;
		case viewmodel:
			pEnt->v.viewmodel = iszString;
			break;
		case weaponmodel:
			pEnt->v.weaponmodel = iszString;
			break;
		default:
			return 0;
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL entity_get_edict2(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	edict_t *pRet;

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case chain:
			pRet = pEnt->v.chain;
			break;
		case dmg_inflictor:
			pRet = pEnt->v.dmg_inflictor;
			break;
		case enemy:
			pRet = pEnt->v.enemy;
			break;
		case aiment:
			pRet = pEnt->v.aiment;
			break;
		case owner:
			pRet = pEnt->v.owner;
			break;
		case groundentity:
			pRet = pEnt->v.groundentity;
			break;
		case pContainingEntity:
			pRet = pEnt->v.pContainingEntity;
			break;
		case euser1:
			pRet = pEnt->v.euser1;
			break;
		case euser2:
			pRet = pEnt->v.euser2;
			break;
		case euser3:
			pRet = pEnt->v.euser3;
			break;
		case euser4:
			pRet = pEnt->v.euser4;
			break;
		default:
			return -1;
			break;
	}

	if (FNullEnt(pRet))
		return -1;

	return ENTINDEX(pRet);
}

static cell AMX_NATIVE_CALL entity_get_edict(AMX *amx, cell *params)
{
	cell res = entity_get_edict2(amx, params);

	if (res == -1)
		res = 0;

	return res;
}

static cell AMX_NATIVE_CALL entity_set_edict(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iSetEnt = params[3];

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);
	edict_t *pSetEnt = TypeConversion.id_to_edict(iSetEnt);

	switch (idx)
	{
		case chain:
			pEnt->v.chain = pSetEnt;
			break;
		case dmg_inflictor:
			pEnt->v.dmg_inflictor = pSetEnt;
			break;
		case enemy:
			pEnt->v.enemy = pSetEnt;
			break;
		case aiment:
			pEnt->v.aiment = pSetEnt;
			break;
		case owner:
			pEnt->v.owner = pSetEnt;
			break;
		case groundentity:
			pEnt->v.groundentity = pSetEnt;
			break;
		case pContainingEntity:
			pEnt->v.pContainingEntity = pSetEnt;
			break;
		case euser1:
			pEnt->v.euser1 = pSetEnt;
			break;
		case euser2:
			pEnt->v.euser2 = pSetEnt;
			break;
		case euser3:
			pEnt->v.euser3 = pSetEnt;
			break;
		case euser4:
			pEnt->v.euser4 = pSetEnt;
			break;
		default:
			return 0;
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL entity_get_byte(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iRetValue = 0;

	CHECK_ENTITY_SIMPLE(iEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case controller1:
			iRetValue = pEnt->v.controller[0];
			break;
		case controller2:
			iRetValue = pEnt->v.controller[1];
			break;
		case controller3:
			iRetValue = pEnt->v.controller[2];
			break;
		case controller4:
			iRetValue = pEnt->v.controller[3];
			break;
		case blending1:
			iRetValue = pEnt->v.blending[0];
			break;
		case blending2:
			iRetValue = pEnt->v.blending[1];
			break;
		default:
			return 0;
			break;
	}

	return iRetValue;
}

static cell AMX_NATIVE_CALL entity_set_byte(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int idx = params[2];
	int iNewValue = params[3];

	CHECK_ENTITY_SIMPLE(iEnt);

	if(iNewValue > 255)
			iNewValue = 255;
	if(iNewValue < 0)
			iNewValue = 0;

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	switch (idx)
	{
		case controller1:
			pEnt->v.controller[0] = iNewValue;
			break;
		case controller2:
			pEnt->v.controller[1] = iNewValue;
			break;
		case controller3:
			pEnt->v.controller[2] = iNewValue;
			break;
		case controller4:
			pEnt->v.controller[3] = iNewValue;
			break;
		case blending1:
			pEnt->v.blending[0] = iNewValue;
			break;
		case blending2:
			pEnt->v.blending[1] = iNewValue;
			break;
		default:
			return 0;
			break;
	}

	return 1;
}

static cell AMX_NATIVE_CALL entity_set_origin(AMX *amx, cell *params)
{
	int iEnt = params[1];

	CHECK_ENTITY_SIMPLE(iEnt);
	
	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);
	cell *vVector = MF_GetAmxAddr(amx, params[2]);
	REAL fX = amx_ctof(vVector[0]);
	REAL fY = amx_ctof(vVector[1]);
	REAL fZ = amx_ctof(vVector[2]);
	Vector vOrigin = Vector(fX, fY, fZ);

	SET_SIZE(pEnt, pEnt->v.mins, pEnt->v.maxs);
	SET_ORIGIN(pEnt, vOrigin);

	return 1;
}

static cell AMX_NATIVE_CALL entity_set_model(AMX *amx, cell *params)
{
	int iEnt = params[1];

	CHECK_ENTITY_SIMPLE(iEnt);
	
	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);
	int iLen;
	char *szModel = MF_GetAmxString(amx, params[2], 0, &iLen);
	const char *szStatic = STRING(ALLOC_STRING(szModel));

	SET_MODEL(pEnt, szStatic);

	return 1;
}

static cell AMX_NATIVE_CALL entity_set_size(AMX *amx, cell *params)
{
	int iEnt = params[1];

	CHECK_ENTITY_SIMPLE(iEnt);
	
	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);

	cell *cMin = MF_GetAmxAddr(amx, params[2]);
	REAL x1 = amx_ctof(cMin[0]);
	REAL y1 = amx_ctof(cMin[1]);
	REAL z1 = amx_ctof(cMin[2]);
	Vector vMin = Vector(x1, y1, z1);

	cell *cMax = MF_GetAmxAddr(amx, params[3]);
	REAL x2 = amx_ctof(cMax[0]);
	REAL y2 = amx_ctof(cMax[1]);
	REAL z2 = amx_ctof(cMax[2]);
	Vector vMax = Vector(x2, y2, z2);

	UTIL_SetSize(pEnt, vMin, vMax);

	return 1;
}

static cell AMX_NATIVE_CALL get_entity_pointer(AMX *amx, cell *params) // get_entity_pointer(index, pointer[], len); = 3 params
{
	return 0;
}

/************************
 Entity finding functions
 ************************/

static cell AMX_NATIVE_CALL find_ent_in_sphere(AMX *amx, cell *params)
{
	int idx = params[1];
	if (idx > 0) {
		CHECK_ENTITY_SIMPLE(idx);
	}

	edict_t *pEnt = TypeConversion.id_to_edict(idx);
	cell *cAddr = MF_GetAmxAddr(amx, params[2]);
	float origin[3] = {
		amx_ctof(cAddr[0]),
		amx_ctof(cAddr[1]),
		amx_ctof(cAddr[2])
	};
	REAL radius = amx_ctof(params[3]);

	int returnEnt = ENTINDEX(FIND_ENTITY_IN_SPHERE(pEnt, origin, radius));

	if (FNullEnt(returnEnt))
		return 0;

	return returnEnt;
}

static cell AMX_NATIVE_CALL find_ent_by_class(AMX *amx, cell *params) /* 3 param */
{
	int idx = params[1];
	if (idx > 0) {
		CHECK_ENTITY_SIMPLE(idx);
	}

	edict_t *pEnt = TypeConversion.id_to_edict(idx);

	int len;
	char* sValue = MF_GetAmxString(amx, params[2], 0, &len);

	pEnt = FIND_ENTITY_BY_STRING(pEnt, "classname", sValue);

	if (FNullEnt(pEnt))
		return 0;

	return ENTINDEX(pEnt);
}

static cell AMX_NATIVE_CALL find_sphere_class(AMX *amx, cell *params) // find_sphere_class(aroundent, _lookforclassname[], Float:radius, entlist[], maxents, Float:origin[3] = {0.0, 0.0, 0.0}); // 6 params
{
	// params[1] = index to find around, if this is less than 1 then use around origin in last parameter.
	// params[2] = classname to find
	int len;
	char* classToFind = MF_GetAmxString(amx, params[2], 0, &len);
	// params[3] = radius, float...
	REAL radius =amx_ctof(params[3]);
	// params[4] = store ents in this list
	cell *entList = MF_GetAmxAddr(amx, params[4]);
	// params[5] = maximum ents to store in entlist[] in params[4]
	// params[6] = origin, use this if params[1] is less than 1

	vec3_t vecOrigin;
	if (params[1] > 0) {
		CHECK_ENTITY(params[1]);

		edict_t* pEntity = TypeConversion.id_to_edict(params[1]);
		vecOrigin = pEntity->v.origin;
	} else {
		cell *cAddr = MF_GetAmxAddr(amx, params[6]);
		vecOrigin = Vector(amx_ctof(cAddr[0]), amx_ctof(cAddr[1]), amx_ctof(cAddr[2]));
	}
	
	int entsFound = 0;
	edict_t* pSearchEnt = TypeConversion.id_to_edict(0);

	while (entsFound < params[5]) {
		pSearchEnt = FIND_ENTITY_IN_SPHERE(pSearchEnt, vecOrigin, radius); // takes const float origin
		if (FNullEnt(pSearchEnt))
			break;
		else {
			if (strcmp(STRING(pSearchEnt->v.classname), classToFind) == 0) {
				// Add to entlist (params[4])
				entList[entsFound++] = ENTINDEX(pSearchEnt); // raise entsFound
			}
		}
	}

	return entsFound;
}

static cell AMX_NATIVE_CALL find_ent_by_target(AMX *amx, cell *params)
{
	int iStart = params[1];
	int iLength;
	char *szValue = MF_GetAmxString(amx, params[2], 0, &iLength);

	edict_t *pStart;

	if (iStart == -1) {
		pStart = NULL;
	} else {
		if (!is_ent_valid(iStart))
			pStart = NULL;
		else
			pStart = TypeConversion.id_to_edict(iStart);
	}

	int iReturnEnt = ENTINDEX(FIND_ENTITY_BY_TARGET(pStart, szValue));

	return iReturnEnt;
}

static cell AMX_NATIVE_CALL find_ent_by_model(AMX *amx, cell *params) { 
	int iStart = params[1];
	int iLength, iLength2;
	char *szClass = MF_GetAmxString(amx, params[2], 0, &iLength);
	char *szModel = MF_GetAmxString(amx, params[3], 1, &iLength2);

	edict_t *pStart;

	if (iStart == -1)
	{
		pStart = NULL;
	} else {
		if (!is_ent_valid(iStart))
			pStart = NULL;
		else
			pStart = TypeConversion.id_to_edict(iStart);
	}

	edict_t *pEdict = FIND_ENTITY_BY_STRING(pStart, "classname", szClass);

	const char *check;

	while (pEdict && !FNullEnt(pEdict))
	{
		check = STRING(pEdict->v.model);
		if (!check || strcmp(check, szModel))
			pEdict = FIND_ENTITY_BY_STRING(pEdict, "classname", szClass);
		else
			return ENTINDEX(pEdict);
	}

	return 0;
}

static cell AMX_NATIVE_CALL find_ent_by_tname(AMX *amx, cell *params) {
	int iStart = params[1];
	int iLength;
	char *szValue = MF_GetAmxString(amx, params[2], 0, &iLength);

	edict_t *pStart;

	if (iStart == -1) {
		pStart = NULL;
	} else {
		if (!is_ent_valid(iStart))
			pStart = NULL;
		else
			pStart = TypeConversion.id_to_edict(iStart);
	}

	int iReturnEnt = ENTINDEX(FIND_ENTITY_BY_TARGETNAME(pStart, szValue));

	return iReturnEnt;
}

static cell AMX_NATIVE_CALL find_ent_by_owner(AMX *amx, cell *params)  // native find_ent_by_owner(start_from_ent, classname[], owner_index); = 3 params
{
	int iEnt = params[1];
	int oEnt = params[3];
	if (iEnt > 0) {
		CHECK_ENTITY_SIMPLE(iEnt);
	}
	CHECK_ENTITY_SIMPLE(oEnt);

	edict_t *pEnt = TypeConversion.id_to_edict(iEnt);
	edict_t *entOwner = TypeConversion.id_to_edict(oEnt);

	//optional fourth parameter is for jghg2 compatibility
	const char* sCategory = NULL; 
	switch(params[4]){ 
		case 1: sCategory = "target"; break; 
		case 2: sCategory = "targetname"; break; 
		default: sCategory = "classname"; 
	}

	// No need to check if there is a real ent where entOwner points at since we don't access it anyway.

	int len;
	char* classname = MF_GetAmxString(amx, params[2], 0, &len);

	while (true) {
		pEnt = FIND_ENTITY_BY_STRING(pEnt, sCategory, classname);
		if (FNullEnt(pEnt)) // break and return 0 if bad
			break;
		else if (pEnt->v.owner == entOwner) // compare pointers
			return ENTINDEX(pEnt);
	}

	// If it comes here, the while loop ended because an ent failed (FNullEnt() == true)
	return 0;
}

static cell AMX_NATIVE_CALL get_grenade_id(AMX *amx, cell *params)  /* 4 param */ 
{
	int index = params[1];
	const char *szModel;

	if (params[4] > 0) {
		CHECK_ENTITY_SIMPLE(params[4]);
	}

	CHECK_ENTITY(index);

	edict_t* pentFind = TypeConversion.id_to_edict(params[4]);
	edict_t* pentOwner = TypeConversion.id_to_edict(index);

	pentFind = FIND_ENTITY_BY_CLASSNAME( pentFind, "grenade" );
	while (!FNullEnt(pentFind)) {
		if (pentFind->v.owner == pentOwner) {
			if (params[3]>0) {
				szModel = (char*)STRING(pentFind->v.model);
				MF_SetAmxString(amx, params[2], szModel, params[3]);
				return ENTINDEX(pentFind);
			}
		}
		pentFind = FIND_ENTITY_BY_CLASSNAME( pentFind, "grenade" );
	}
	return 0;
}

static cell AMX_NATIVE_CALL set_ent_rendering(AMX *amx, cell *params) // set_ent_rendering(index, fx = kRenderFxNone, r = 0, g = 0, b = 0, render = kRenderNormal, amount = 0); = 7 arguments
{
	// params[1] = index
	// params[2] = fx
	// params[3] = r
	// params[4] = g
	// params[5] = b
	// params[6] = render
	// params[7] = amount

	CHECK_ENTITY_SIMPLE(params[1]);

	edict_t *pEntity = TypeConversion.id_to_edict(params[1]);

	pEntity->v.renderfx = params[2];
	pEntity->v.rendercolor = Vector(float(params[3]), float(params[4]), float(params[5]));
	pEntity->v.rendermode = params[6];
	pEntity->v.renderamt = float(params[7]);

	return 1;
}

static cell AMX_NATIVE_CALL entity_intersects(AMX *amx, cell *params) // bool:entity_intersects(entity, other); = 2 arguments
{
	// params[1] = entity
	// params[2] = other

	CHECK_ENTITY_SIMPLE(params[1]);
	CHECK_ENTITY_SIMPLE(params[2]);

	entvars_s *pevEntity = VARS(TypeConversion.id_to_edict(params[1]));
	entvars_s *pevOther = VARS(TypeConversion.id_to_edict(params[2]));

	if (pevOther->absmin.x > pevEntity->absmax.x ||
		pevOther->absmin.y > pevEntity->absmax.y ||
		pevOther->absmin.z > pevEntity->absmax.z ||
		pevOther->absmax.x < pevEntity->absmin.x ||
		pevOther->absmax.y < pevEntity->absmin.y ||
		pevOther->absmax.z < pevEntity->absmin.z)
	{
		return 1;
	}

	return 0;
}

AMX_NATIVE_INFO ent_NewNatives[] =
{
	{"DispatchKeyValue",	DispatchKeyValue},
	{"set_ent_rendering",	set_ent_rendering},
	{"entity_intersects",	entity_intersects},
	{NULL,					NULL}
};

AMX_NATIVE_INFO ent_Natives[] = {
	{"create_entity",		create_entity},
	{"remove_entity",		remove_entity},
	{"entity_count",		entity_count},
	{"is_valid_ent",		is_valid_ent},

	{"entity_range",		entity_range},

	{"entity_get_float",	entity_get_float},
	{"entity_set_float",	entity_set_float},
	{"entity_set_int",		entity_set_int},
	{"entity_get_int",		entity_get_int},
	{"entity_get_vector",	entity_get_vector},
	{"entity_set_vector",	entity_set_vector},
	{"entity_get_string",	entity_get_string},
	{"entity_set_string",	entity_set_string},
	{"entity_get_edict",	entity_get_edict},
	{"entity_get_edict2",	entity_get_edict2},
	{"entity_set_edict",	entity_set_edict},
	{"entity_get_byte",		entity_get_byte},
	{"entity_set_byte",		entity_set_byte},
	{"entity_set_origin",	entity_set_origin},
	{"entity_set_model",	entity_set_model},
	{"entity_set_size",		entity_set_size},
	{"DispatchSpawn",		DispatchSpawn},

	{"call_think",			call_think},
	{"fake_touch",			fake_touch},
	{"force_use",			force_use},

	{"get_entity_pointer",	get_entity_pointer},

	{"find_ent_in_sphere",	find_ent_in_sphere},
	{"find_ent_by_class",	find_ent_by_class},
	{"find_sphere_class",	find_sphere_class},
	{"find_ent_by_model",	find_ent_by_model},
	{"find_ent_by_target",	find_ent_by_target},
	{"find_ent_by_tname",	find_ent_by_tname},
	{"find_ent_by_owner",	find_ent_by_owner},
	{"get_grenade_id",		get_grenade_id},

	{"get_keyvalue",		get_keyvalue },

	{"copy_keyvalue",		copy_keyvalue},

	{NULL,					NULL}
	 ///////////////////
};

