#include "fakemeta_amxx.h"

// originally by mahnsawce
static cell AMX_NATIVE_CALL amx_pev(AMX *amx,cell *params)
{
	int index=params[1];
	if (index >= 1 && index <= 32)
	{
		if (!MF_IsPlayerIngame(index))
		{
			MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
			return 0;
		}
	} else {
		if (index > gpGlobals->maxEntities)
		{
			MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}
	edict_t *pPlayer = INDEXENT(index);
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
		MF_Log("Invalid return valuetype for pev().");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
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
		MF_Log("Invalid return valuetype for pev().");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
	}
	else if (returntype == RETURNTYPE_STRING)
	{
		// Here is a string value that was requested.
		// If the returned value is an integer or float, then sprintf() it to string.
		// If the returned is a string, then string() it.
		if (valuetype == VALUETYPE_INT || valuetype == VALUETYPE_STRING || valuetype == VALUETYPE_EDICT)
		{
			if (valuetype == VALUETYPE_STRING)
			{
				MF_SetAmxString(amx, params[3], STRING(iReturn), params[4]);
				return 1;
			}
			else
			{
				char blah[64];
				sprintf(blah,"%i",iReturn);
				MF_SetAmxString(amx, params[3], blah, params[4]);
				return 1;
			}

		}
		if (valuetype == VALUETYPE_FLOAT)
		{
			char blah[64];
			sprintf(blah,"%f",fReturn);
			MF_SetAmxString(amx, params[3], blah, params[4]);
			return 1;
		}
		if (valuetype == VALUETYPE_VECTOR)
		{
			char blah[256];
			sprintf(blah,"%f %f %f",vReturn.x,vReturn.y,vReturn.z);
			MF_SetAmxString(amx, params[3], blah, params[4]);
			return 1;
		}
		if (valuetype == VALUETYPE_BYTE)
		{
			if (iSwitch == controller)
			{
				char blah[128];
				sprintf(blah,"%i %i",bReturn[0],bReturn[1]);
				MF_SetAmxString(amx,params[3],blah,params[4]);
				return 1;
			}
			else
			{
				char blah[256];
				sprintf(blah,"%i %i %i %i",bReturn[0],bReturn[1],bReturn[2],bReturn[3]);
				MF_SetAmxString(amx,params[3],blah,params[4]);
				return 1;
			}
		}
		MF_Log("Invalid return valuetype for pev().");
		MF_RaiseAmxError(amx, AMX_ERR_NATIVE);
	}
	return 0;
}