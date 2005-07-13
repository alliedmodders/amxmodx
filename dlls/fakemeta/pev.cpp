#include "fakemeta_amxx.h"

// originally by mahnsawce
static cell AMX_NATIVE_CALL amx_pev(AMX *amx,cell *params)
{
	int index=params[1];
#ifdef DONT_TOUCH_THIS_AGAIN_BAIL
	if (index >= 1 && index <= gpGlobals->maxClients)
	{
		if (!MF_IsPlayerIngame(index))
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Player %d is not in game", index);
			return 0;
		}
	} else {
		if (index > gpGlobals->maxEntities || index < 1)
		{
			MF_LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", index);
			return 0;
		}
	}
#endif
	CHECK_ENTITY(index);
	edict_t *pPlayer = INDEXENT2(index);
	int		returntype = *params/sizeof(cell);
	int		valuetype=0;
	int		iReturn=0;
	float	fReturn=0;
	Vector	vReturn=Vector(0,0,0);
	byte	bReturn[4]={0,0,0,0};
	int		iSwitch	= params[2];
	if (iSwitch > pev_int_start && iSwitch < pev_int_end)
		valuetype=VALUETYPE_INT;
	else if (iSwitch > pev_float_start && iSwitch < pev_float_end)
		valuetype=VALUETYPE_FLOAT;
	else if (iSwitch > pev_vecarray_start && iSwitch < pev_vecarray_end)
		valuetype=VALUETYPE_VECTOR;
	else if (iSwitch > pev_byte_start && iSwitch < pev_byte_end)
		valuetype=VALUETYPE_BYTE;
	else if (iSwitch > pev_string_start && iSwitch < pev_string_end)
		valuetype=VALUETYPE_STRING;
	else if (iSwitch > pev_edict_start && iSwitch < pev_edict_end)
		valuetype=VALUETYPE_EDICT;
	if (iSwitch > pev_int_start && iSwitch < pev_int_end)
	{
		valuetype=VALUETYPE_INT;
		switch(iSwitch)
		{
			case fixangle:
				iReturn = pPlayer->v.fixangle;
				break;
			case modelindex:
				iReturn = pPlayer->v.modelindex;
				break;
			case viewmodel:
				iReturn = pPlayer->v.viewmodel;
				break;
			case weaponmodel:
				iReturn = pPlayer->v.weaponmodel;
				break;
			case movetype:
				iReturn = pPlayer->v.movetype;
				break;
			case solid:
				iReturn = pPlayer->v.solid;
				break;
			case skin:
				iReturn = pPlayer->v.skin;
				break;
			case body:
				iReturn = pPlayer->v.body;
				break;
			case effects:
				iReturn = pPlayer->v.effects;
				break;
			case light_level:
				iReturn = pPlayer->v.light_level;
				break;
			case sequence:
				iReturn = pPlayer->v.sequence;
				break;
			case gaitsequence:
				iReturn = pPlayer->v.gaitsequence;
				break;
			case rendermode:
				iReturn = pPlayer->v.rendermode;
				break;
			case renderfx:
				iReturn = pPlayer->v.renderfx;
				break;
			case weapons:
				iReturn = pPlayer->v.weapons;
				break;
			case deadflag:
				iReturn = pPlayer->v.deadflag;
				break;
			case button:
				iReturn = pPlayer->v.button;
				break;
			case impulse:
				iReturn = pPlayer->v.impulse;
				break;
			case spawnflags:
				iReturn = pPlayer->v.spawnflags;
				break;
			case flags:
				iReturn = pPlayer->v.flags;
				break;
			case colormap:
				iReturn = pPlayer->v.colormap;
				break;
			case team:
				iReturn = pPlayer->v.team;
				break;
			case waterlevel:
				iReturn = pPlayer->v.waterlevel;
				break;
			case watertype:
				iReturn = pPlayer->v.watertype;
				break;
			case playerclass:
				iReturn = pPlayer->v.playerclass;
				break;
			case weaponanim:
				iReturn = pPlayer->v.weaponanim;
				break;
			case pushmsec:
				iReturn = pPlayer->v.pushmsec;
				break;
			case bInDuck:
				iReturn = pPlayer->v.bInDuck;
				break;
			case flTimeStepSound:
				iReturn = pPlayer->v.flTimeStepSound;
				break;
			case flSwimTime:
				iReturn = pPlayer->v.flSwimTime;
				break;
			case flDuckTime:
				iReturn = pPlayer->v.flDuckTime;
				break;
			case iStepLeft:
				iReturn = pPlayer->v.iStepLeft;
				break;
			case gamestate:
				iReturn = pPlayer->v.gamestate;
				break;
			case oldbuttons:
				iReturn = pPlayer->v.oldbuttons;
				break;
			case groupinfo:
				iReturn = pPlayer->v.groupinfo;
				break;
			case iuser1:
				iReturn = pPlayer->v.iuser1;
				break;
			case iuser2:
				iReturn = pPlayer->v.iuser2;
				break;
			case iuser3:
				iReturn = pPlayer->v.iuser3;
				break;
			case iuser4:
				iReturn = pPlayer->v.iuser4;
				break;
			default:
				return 0;
		}
	}
	else if (iSwitch > pev_float_start && iSwitch < pev_float_end)
	{
		valuetype=VALUETYPE_FLOAT;
		switch(iSwitch)
		{
			case impacttime:
				fReturn = pPlayer->v.impacttime;
				break;
			case starttime:
				fReturn = pPlayer->v.starttime;
				break;
			case idealpitch:
				fReturn = pPlayer->v.idealpitch;
				break;
			case ideal_yaw:
				fReturn = pPlayer->v.ideal_yaw;
				break;
			case pitch_speed:
				fReturn = pPlayer->v.pitch_speed;
				break;
			case yaw_speed:
				fReturn = pPlayer->v.yaw_speed;
				break;
			case ltime:
				fReturn = pPlayer->v.ltime;
				break;
			case nextthink:
				fReturn = pPlayer->v.nextthink;
				break;
			case gravity:
				fReturn = pPlayer->v.gravity;
				break;
			case friction:
				fReturn = pPlayer->v.friction;
				break;
			case frame:
				fReturn = pPlayer->v.frame;
				break;
			case animtime:
				fReturn = pPlayer->v.animtime;
				break;
			case framerate:
				fReturn = pPlayer->v.framerate;
				break;
			case scale:
				fReturn = pPlayer->v.scale;
				break;
			case renderamt:
				fReturn = pPlayer->v.renderamt;
				break;
			case health:
				fReturn = pPlayer->v.health;
				break;
			case frags:
				fReturn = pPlayer->v.frags;
				break;
			case takedamage:
				fReturn = pPlayer->v.takedamage;
				break;
			case max_health:
				fReturn = pPlayer->v.max_health;
				break;
			case teleport_time:
				fReturn = pPlayer->v.teleport_time;
				break;
			case armortype:
				fReturn = pPlayer->v.armortype;
				break;
			case armorvalue:
				fReturn = pPlayer->v.armorvalue;
				break;
			case dmg_take:
				fReturn = pPlayer->v.dmg_take;
				break;
			case dmg_save:
				fReturn = pPlayer->v.dmg_save;
				break;
			case dmg:
				fReturn = pPlayer->v.dmg;
				break;
			case dmgtime:
				fReturn = pPlayer->v.dmgtime;
				break;
			case speed:
				fReturn = pPlayer->v.speed;
				break;
			case air_finished:
				fReturn = pPlayer->v.air_finished;
				break;
			case pain_finished:
				fReturn = pPlayer->v.pain_finished;
				break;
			case radsuit_finished:
				fReturn = pPlayer->v.radsuit_finished;
				break;
			case maxspeed:
				fReturn = pPlayer->v.maxspeed;
				break;
			case fov:
				fReturn = pPlayer->v.fov;
				break;
			case flFallVelocity:
				fReturn = pPlayer->v.flFallVelocity;
				break;
			case fuser1:
				fReturn = pPlayer->v.fuser1;
				break;
			case fuser2:
				fReturn = pPlayer->v.fuser2;
				break;
			case fuser3:
				fReturn = pPlayer->v.fuser3;
				break;
			case fuser4:
				fReturn = pPlayer->v.fuser4;
				break;
			default:
				return 0;
				break;
		}
	}
	else if (iSwitch > pev_string_start && iSwitch < pev_string_end)
	{
		valuetype=VALUETYPE_STRING;
		switch (iSwitch)
		{
		case classname:
			iReturn = pPlayer->v.classname;
			break;
		case globalname:
			iReturn = pPlayer->v.globalname;
			break;
		case model:
			iReturn = pPlayer->v.model;
			break;
		case target:
			iReturn = pPlayer->v.target;
			break;
		case targetname:
			iReturn = pPlayer->v.targetname;
			break;
		case netname:
			iReturn = pPlayer->v.netname;
			break;
		case message:
			iReturn = pPlayer->v.message;
			break;
		case noise:
			iReturn = pPlayer->v.noise;
			break;
		case noise1:
			iReturn = pPlayer->v.noise1;
			break;
		case noise2:
			iReturn = pPlayer->v.noise2;
			break;
		case noise3:
			iReturn = pPlayer->v.noise3;
			break;
		default:
			return 0;
		}

	}
	else if (iSwitch > pev_edict_start && iSwitch < pev_edict_end)
	{
		valuetype=VALUETYPE_EDICT;
		switch (iSwitch)
		{
		case chain:
			iReturn = ENTINDEX(pPlayer->v.chain);
			break;
		case dmg_inflictor:
			iReturn = ENTINDEX(pPlayer->v.dmg_inflictor);
			break;
		case enemy:
			iReturn = ENTINDEX(pPlayer->v.enemy);
			break;
		case aiment:
			iReturn = ENTINDEX(pPlayer->v.aiment);
			break;
		case owner:
			iReturn = ENTINDEX(pPlayer->v.owner);
			break;
		case groundentity:
			iReturn = ENTINDEX(pPlayer->v.groundentity);
			break;
		case euser1:
			iReturn = ENTINDEX(pPlayer->v.euser1);
			break;
		case euser2:
			iReturn = ENTINDEX(pPlayer->v.euser2);
			break;
		case euser3:
			iReturn = ENTINDEX(pPlayer->v.euser3);
			break;
		case euser4:
			iReturn = ENTINDEX(pPlayer->v.euser4);
			break;
		default:
			return 0;
		}
	}
	else if (iSwitch > pev_vecarray_start && iSwitch < pev_vecarray_end)
	{
		valuetype=VALUETYPE_VECTOR;
		switch(iSwitch)
		{
		case origin:
			vReturn = pPlayer->v.origin;
			break;
		case oldorigin:
			vReturn = pPlayer->v.oldorigin;
			break;
		case velocity:
			vReturn = pPlayer->v.velocity;
			break;
		case basevelocity:
			vReturn = pPlayer->v.basevelocity;
			break;
		case movedir:
			vReturn = pPlayer->v.movedir;
			break;
		case angles:
			vReturn = pPlayer->v.angles;
			break;
		case avelocity:
			vReturn = pPlayer->v.avelocity;
			break;
		case v_angle:
			vReturn = pPlayer->v.v_angle;
			break;
		case endpos:
			vReturn = pPlayer->v.endpos;
			break;
		case startpos:
			vReturn = pPlayer->v.startpos;
			break;
		case absmin:
			vReturn = pPlayer->v.absmin;
			break;
		case absmax:
			vReturn = pPlayer->v.absmax;
			break;
		case mins:
			vReturn = pPlayer->v.mins;
			break;
		case maxs:
			vReturn = pPlayer->v.maxs;
			break;
		case size:
			vReturn = pPlayer->v.size;
			break;
		case rendercolor:
			vReturn = pPlayer->v.rendercolor;
			break;
		case view_ofs:
			vReturn = pPlayer->v.view_ofs;
			break;
		case vuser1:
			vReturn = pPlayer->v.vuser1;
			break;
		case vuser2:
			vReturn = pPlayer->v.vuser2;
			break;
		case vuser3:
			vReturn = pPlayer->v.vuser3;
			break;
		case vuser4:
			vReturn = pPlayer->v.vuser4;
			break;
		case punchangle:
			vReturn = pPlayer->v.punchangle;
			break;
		default:
			return 0;
		}
	}
	else if ((iSwitch > pev_byte_start && iSwitch < pev_byte_end) || (iSwitch > pev_bytearray_start && iSwitch < pev_bytearray_end))
	{
		if (iSwitch > pev_byte_start && iSwitch < pev_byte_end)
			valuetype=VALUETYPE_INT;
		else
			valuetype=VALUETYPE_BYTE;
		switch(iSwitch)
		{
			case controller:
			{
				bReturn[0] = pPlayer->v.controller[0];
				bReturn[1] = pPlayer->v.controller[1];
				bReturn[2] = pPlayer->v.controller[2];
				bReturn[3] = pPlayer->v.controller[3];
				break;
			}
			case controller_0:
				iReturn = pPlayer->v.controller[0];
				break;
			case controller_1:
				iReturn = pPlayer->v.controller[1];
				break;
			case controller_2:
				iReturn = pPlayer->v.controller[2];
				break;
			case controller_3:
				iReturn = pPlayer->v.controller[3];
				break;
			case blending:
			{
				bReturn[0] = pPlayer->v.blending[0];
				bReturn[1] = pPlayer->v.blending[1];
				bReturn[2]=0;
				bReturn[3]=0;
				break;
			}
			case blending_0:
				iReturn = pPlayer->v.blending[0];
				break;
			case blending_1:
				iReturn = pPlayer->v.blending[1];
				break;
			default:
				return 0;
		}
	}
	if (returntype == RETURNTYPE_INT)
	{
		// We are only returning an integer here.
		// If the returned value is a string, return make_string value.
		// If the returned value is a float, round it down.
		// If the returned value is int, just return it.
		// Otherwise, print a warning.
		if (valuetype == VALUETYPE_INT || valuetype == VALUETYPE_EDICT)
		{
			return iReturn;
		}
		else if (valuetype == VALUETYPE_FLOAT)
		{
			return (int)fReturn;
		}
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return valuetype for pev().");
		return 0;
	}
	else if (returntype == RETURNTYPE_FLOAT)
	{
		// We are setting a variable as a float here.
		// If it's a float, just set it.
		// If it's an integer, convert and set it.
		// Otherwise, return an error.
		if (valuetype == VALUETYPE_INT)
		{
			float fTemp = (float)iReturn;
			cell *cRet = MF_GetAmxAddr(amx,params[3]);
			*cRet = amx_ftoc(fTemp);
			return 1;
		}
		else if (valuetype == VALUETYPE_FLOAT)
		{
			cell *cRet = MF_GetAmxAddr(amx,params[3]);
			*cRet = amx_ftoc(fReturn);
			return 1;
		}
		else if (valuetype == VALUETYPE_VECTOR)
		{
			cell *cRet = MF_GetAmxAddr(amx,params[3]);
			cRet[0] = amx_ftoc(vReturn.x);
			cRet[1] = amx_ftoc(vReturn.y);
			cRet[2] = amx_ftoc(vReturn.z);
			return 1;
		}
		else if (valuetype == VALUETYPE_BYTE)
		{
			cell *cRet = MF_GetAmxAddr(amx,params[3]);
			if (iSwitch == blending)
			{
				// Only 2 for blending.
				cRet[0]=bReturn[0];
				cRet[1]=bReturn[1];
				return 1;
			}
			else
			{
				// There's 4 for controller.
				cRet[0]=bReturn[0];
				cRet[1]=bReturn[1];
				cRet[2]=bReturn[2];
				cRet[3]=bReturn[3];
				return 1;
			}
		}
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return valuetype for pev().");
	}
	else if (returntype == RETURNTYPE_STRING)
	{
		// Here is a string value that was requested.
		// If the returned value is an integer or float, then sprintf() it to string.
		// If the returned is a string, then string() it.

		cell *cBlah = MF_GetAmxAddr(amx,params[4]);
		int size = cBlah[0];
		if (valuetype == VALUETYPE_INT || valuetype == VALUETYPE_STRING || valuetype == VALUETYPE_EDICT)
		{
			if (valuetype == VALUETYPE_STRING)
			{
				MF_SetAmxString(amx, params[3], STRING(iReturn), size);
				return 1;
			}
			else
			{
				char blah[64];
				sprintf(blah,"%i",iReturn);
				MF_SetAmxString(amx, params[3], blah, size);
				return 1;
			}

		}
		if (valuetype == VALUETYPE_FLOAT)
		{
			char blah[64];
			sprintf(blah,"%f",fReturn);
			MF_SetAmxString(amx, params[3], blah, size);
			return 1;
		}
		if (valuetype == VALUETYPE_VECTOR)
		{
			char blah[256];
			sprintf(blah,"%f %f %f",vReturn.x,vReturn.y,vReturn.z);
			MF_SetAmxString(amx, params[3], blah, size);
			return 1;
		}
		if (valuetype == VALUETYPE_BYTE)
		{
			if (iSwitch == controller)
			{
				char blah[128];
				sprintf(blah,"%i %i",bReturn[0],bReturn[1]);
				MF_SetAmxString(amx,params[3],blah,size);
				return 1;
			}
			else
			{
				char blah[256];
				sprintf(blah,"%i %i %i %i",bReturn[0],bReturn[1],bReturn[2],bReturn[3]);
				MF_SetAmxString(amx,params[3],blah,size);
				return 1;
			}
		}
		MF_LogError(amx, AMX_ERR_NATIVE, "Invalid return valuetype for pev().");
	}
	return 0;
}

static cell AMX_NATIVE_CALL amx_set_pev(AMX *amx, cell *params)
{
	// index, pevdata
	int index = params[1];
	CHECK_ENTITY(index);
	edict_t *pPlayer = INDEXENT2(index);
	int iSwitch = params[2];
	cell *blah = MF_GetAmxAddr(amx,params[3]);
	if (iSwitch > pev_int_start && iSwitch < pev_int_end)
	{
		// Grrr...
		int iValue = blah[0];
		switch(iSwitch)
		{
			case fixangle:
				pPlayer->v.fixangle = iValue;
				return 1;
			case modelindex:
				pPlayer->v.modelindex = iValue;
				return 1;
			case viewmodel:
				pPlayer->v.viewmodel = iValue;
				return 1;
			case weaponmodel:
				pPlayer->v.weaponmodel = iValue;
				return 1;
			case movetype:
				pPlayer->v.movetype = iValue;
				return 1;
			case solid:
				pPlayer->v.solid = iValue;
				return 1;
			case skin:
				pPlayer->v.skin = iValue;
				return 1;
			case body:
				pPlayer->v.body = iValue;
				return 1;
			case effects:
				pPlayer->v.effects = iValue;
				return 1;
			case light_level:
				pPlayer->v.light_level = iValue;
				return 1;
			case sequence:
				pPlayer->v.sequence = iValue;
				return 1;
			case gaitsequence:
				pPlayer->v.gaitsequence = iValue;
				return 1;
			case rendermode:
				pPlayer->v.rendermode = iValue;
				return 1;
			case renderfx:
				pPlayer->v.renderfx = iValue;
				return 1;
			case weapons:
				pPlayer->v.weapons = iValue;
				return 1;
			case deadflag:
				pPlayer->v.deadflag = iValue;
				return 1;
			case button:
				pPlayer->v.button = iValue;
				return 1;
			case impulse:
				pPlayer->v.impulse = iValue;
				return 1;
			case spawnflags:
				pPlayer->v.spawnflags = iValue;
				return 1;
			case flags:
				pPlayer->v.flags = iValue;
				return 1;
			case colormap:
				pPlayer->v.colormap = iValue;
				return 1;
			case team:
				pPlayer->v.team = iValue;
				return 1;
			case waterlevel:
				pPlayer->v.waterlevel = iValue;
				return 1;
			case watertype:
				pPlayer->v.watertype = iValue;
				return 1;
			case playerclass:
				pPlayer->v.playerclass = iValue;
				return 1;
			case weaponanim:
				pPlayer->v.weaponanim = iValue;
				return 1;
			case pushmsec:
				pPlayer->v.pushmsec = iValue;
				return 1;
			case bInDuck:
				pPlayer->v.bInDuck = iValue;
				return 1;
			case flTimeStepSound:
				pPlayer->v.flTimeStepSound = iValue;
				return 1;
			case flSwimTime:
				pPlayer->v.flSwimTime = iValue;
				return 1;
			case flDuckTime:
				pPlayer->v.flDuckTime = iValue;
				return 1;
			case iStepLeft:
				pPlayer->v.iStepLeft = iValue;
				return 1;
			case gamestate:
				pPlayer->v.gamestate = iValue;
				return 1;
			case oldbuttons:
				pPlayer->v.oldbuttons = iValue;
				return 1;
			case groupinfo:
				pPlayer->v.groupinfo = iValue;
				return 1;
			case iuser1:
				pPlayer->v.iuser1 = iValue;
				return 1;
			case iuser2:
				pPlayer->v.iuser2 = iValue;
				return 1;
			case iuser3:
				pPlayer->v.iuser3 = iValue;
				return 1;
			case iuser4:
				pPlayer->v.iuser4 = iValue;
				return 1;
			default:
				return 0;
		}
	}
	else if (iSwitch > pev_float_start && iSwitch < pev_float_end)
	{
		float fValue = amx_ctof(blah[0]);

		switch(iSwitch)
		{
			case impacttime:
				pPlayer->v.impacttime = fValue;
				return 1;
			case starttime:
				pPlayer->v.starttime = fValue;
				return 1;
			case idealpitch:
				pPlayer->v.idealpitch = fValue;
				return 1;
			case ideal_yaw:
				pPlayer->v.ideal_yaw = fValue;
			case pitch_speed:
				pPlayer->v.pitch_speed = fValue;
				return 1;
			case yaw_speed:
				pPlayer->v.yaw_speed = fValue;
				return 1;
			case ltime:
				pPlayer->v.ltime = fValue;
				return 1;
			case nextthink:
				pPlayer->v.nextthink = fValue;
				return 1;
			case gravity:
				pPlayer->v.gravity = fValue;
				return 1;
			case friction:
				pPlayer->v.friction = fValue;
				return 1;
			case frame:
				pPlayer->v.frame = fValue;
				return 1;
			case animtime:
				pPlayer->v.animtime = fValue;
				return 1;
			case framerate:
				pPlayer->v.framerate = fValue;
				return 1;
			case scale:
				pPlayer->v.scale = fValue;
				return 1;
			case renderamt:
				pPlayer->v.renderamt = fValue;
				return 1;
			case health:
				pPlayer->v.health = fValue;
				return 1;
			case frags:
				pPlayer->v.frags = fValue;
				return 1;
			case takedamage:
				pPlayer->v.takedamage = fValue;
				return 1;
			case max_health:
				pPlayer->v.max_health = fValue;
				return 1;
			case teleport_time:
				pPlayer->v.teleport_time = fValue;
				return 1;
			case armortype:
				pPlayer->v.armortype = fValue;
				return 1;
			case armorvalue:
				pPlayer->v.armorvalue = fValue;
				return 1;
			case dmg_take:
				pPlayer->v.dmg_take = fValue;
				return 1;
			case dmg_save:
				pPlayer->v.dmg_save = fValue;
				return 1;
			case dmg:
				pPlayer->v.dmg = fValue;
				return 1;
			case dmgtime:
				pPlayer->v.dmgtime = fValue;
				return 1;
			case speed:
				pPlayer->v.speed = fValue;
				return 1;
			case air_finished:
				pPlayer->v.air_finished = fValue;
				return 1;
			case pain_finished:
				pPlayer->v.pain_finished = fValue;
				return 1;
			case radsuit_finished:
				pPlayer->v.radsuit_finished = fValue;
				return 1;
			case maxspeed:
				pPlayer->v.maxspeed = fValue;
				return 1;
			case fov:
				pPlayer->v.fov = fValue;
				return 1;
			case flFallVelocity:
				pPlayer->v.flFallVelocity = fValue;
				return 1;
			case fuser1:
				pPlayer->v.fuser1 = fValue;
				return 1;
			case fuser2:
				pPlayer->v.fuser2 = fValue;
				return 1;
			case fuser3:
				pPlayer->v.fuser3 = fValue;
				return 1;
			case fuser4:
				pPlayer->v.fuser4 = fValue;
				return 1;
			default:
				return 0;
		}
	}
	else if (iSwitch > pev_string_start && iSwitch < pev_string_end)
	{

		int iValue = blah[0];
		switch (iSwitch)
		{
		case classname:
			pPlayer->v.classname = iValue;
			return 1;
		case globalname:
			pPlayer->v.globalname = iValue;
			return 1;
		case model:
			pPlayer->v.model = iValue;
			return 1;
		case target:
			pPlayer->v.target = iValue;
			return 1;
		case targetname:
			pPlayer->v.targetname = iValue;
			return 1;
		case netname:
			pPlayer->v.netname = iValue;
			return 1;
		case message:
			pPlayer->v.message = iValue;
			return 1;
		case noise:
			pPlayer->v.noise = iValue;
			return 1;
		case noise1:
			pPlayer->v.noise1 = iValue;
			return 1;
		case noise2:
			pPlayer->v.noise2 = iValue;
			return 1;
		case noise3:
			pPlayer->v.noise3 = iValue;
			return 1;
		default:
			return 0;
		}

	}
	else if (iSwitch > pev_edict_start && iSwitch < pev_edict_end)
	{
		int iValue = blah[0];
		switch (iSwitch)
		{
		case chain:
			pPlayer->v.chain = INDEXENT2(iValue);
			return 1;
		case dmg_inflictor:
			pPlayer->v.dmg_inflictor = INDEXENT2(iValue);
			return 1;
		case enemy:
			pPlayer->v.enemy = INDEXENT2(iValue);
			return 1;
		case aiment:
			pPlayer->v.aiment = INDEXENT2(iValue);
			return 1;
		case owner:
			pPlayer->v.owner = INDEXENT2(iValue);
			return 1;
		case groundentity:
			pPlayer->v.groundentity = INDEXENT2(iValue);
			return 1;
		case euser1:
			pPlayer->v.euser1 = INDEXENT2(iValue);
			return 1;
		case euser2:
			pPlayer->v.euser2 = INDEXENT2(iValue);
			return 1;
		case euser3:
			pPlayer->v.euser3 = INDEXENT2(iValue);
			return 1;
		case euser4:
			pPlayer->v.euser4 = INDEXENT2(iValue);
			return 1;
		default:
			return 0;
		}
	}
	else if (iSwitch > pev_vecarray_start && iSwitch < pev_vecarray_end)
	{
		cell *vCell = MF_GetAmxAddr(amx,params[3]);
		Vector vValue;
		vValue.x = amx_ctof(vCell[0]);
		vValue.y = amx_ctof(vCell[1]);
		vValue.z = amx_ctof(vCell[2]);
		switch(iSwitch)
		{
		case origin:
			pPlayer->v.origin = vValue;
			return 1;
		case oldorigin:
			pPlayer->v.oldorigin = vValue;
			return 1;
		case velocity:
			pPlayer->v.velocity = vValue;
			return 1;
		case basevelocity:
			pPlayer->v.basevelocity = vValue;
			return 1;
		case clbasevelocity:
			pPlayer->v.clbasevelocity = vValue;
			return 1;
		case movedir:
			pPlayer->v.movedir = vValue;
			return 1;
		case angles:
			pPlayer->v.angles = vValue;
			return 1;
		case avelocity:
			pPlayer->v.avelocity = vValue;
			return 1;
		case v_angle:
			pPlayer->v.v_angle = vValue;
			return 1;
		case endpos:
			pPlayer->v.endpos = vValue;
			return 1;
		case startpos:
			pPlayer->v.startpos = vValue;
			return 1;
		case absmin:
			pPlayer->v.absmin = vValue;
			return 1;
		case absmax:
			pPlayer->v.absmax = vValue;
			return 1;
		case mins:
			pPlayer->v.mins = vValue;
			return 1;
		case maxs:
			pPlayer->v.maxs = vValue;
			return 1;
		case size:
			pPlayer->v.size = vValue;
			return 1;
		case rendercolor:
			pPlayer->v.rendercolor = vValue;
			return 1;
		case view_ofs:
			pPlayer->v.view_ofs = vValue;
			return 1;
		case vuser1:
			pPlayer->v.vuser1 = vValue;
			return 1;
		case vuser2:
			pPlayer->v.vuser2 = vValue;
			return 1;
		case vuser3:
			pPlayer->v.vuser3 = vValue;
			return 1;
		case vuser4:
			pPlayer->v.vuser4 = vValue;
			return 1;
		case punchangle:
			pPlayer->v.punchangle = vValue;
			return 1;
		default:
			return 0;
		}
	}
	else if (iSwitch > pev_byte_start && iSwitch < pev_byte_end)
	{
		cell *blah = MF_GetAmxAddr(amx,params[3]);
		int iValue = blah[0];

		switch(iSwitch)
		{
			case controller_0:
				pPlayer->v.controller[0]=iValue;
				return 1;
			case controller_1:
				pPlayer->v.controller[1]=iValue;
				return 1;
			case controller_2:
				pPlayer->v.controller[2]=iValue;
				return 1;
			case controller_3:
				pPlayer->v.controller[3]=iValue;
				return 1;
			case blending_0:
				pPlayer->v.blending[0]=iValue;
				return 1;
			case blending_1:
				pPlayer->v.blending[1]=iValue;
				return 1;
			default:
				return 0;
		}
	}
	else if (iSwitch > pev_bytearray_start && iSwitch < pev_bytearray_end)
	{
		cell *vCell = MF_GetAmxAddr(amx,params[3]);
		switch(iSwitch)
		{
			case controller:
				pPlayer->v.controller[0]=vCell[0];
				pPlayer->v.controller[1]=vCell[1];
				pPlayer->v.controller[2]=vCell[2];
				pPlayer->v.controller[3]=vCell[3];
				return 1;
			case blending:
				pPlayer->v.controller[0]=vCell[0];
				pPlayer->v.controller[1]=vCell[1];
				return 1;
			default:
				return 0;
		}
	}
	return 0;
}

AMX_NATIVE_INFO pev_natives[] = {
	{ "pev",			amx_pev },
	{ "set_pev",		amx_set_pev },
	{NULL,				NULL},
};
