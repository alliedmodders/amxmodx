/****************************************************************************
 *  Project: AMX Mod X
 *      Engine module by: BAILOPAN
 *		AMX Mod X Team:  BAILOPAN, JohnnyGotHisGun, Manip, PM, SniperBeamer
 *
 *	Purpose:Engine and entity related natives for AMXX scripting.
 *
 *	Notes:  Many core functions of this module were originally made by VexD.
 *			VexD gets the credit for setting up the framework for this, as it was
 *			Originally VexD Utilities
 *
 *	License:
 *			This program is free software; you can redistribute it and/or modify
 *			it under the terms of the GNU General Public License as published by
 *			the Free Software Foundation; either version 2 of the License, or
 *			(at your option) any later version.
 *
 *			This program is distributed in the hope that it will be useful,
 *			but WITHOUT ANY WARRANTY; without even the implied warranty of
 *			MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *			GNU General Public License for more details.
 *
 *			You should have received a copy of the GNU General Public License
 *			along with this program; if not, write to the Free Software
 *			Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *	Full license can be found at:
 *			http://www.gnu.org/licenses/gpl.txt
 ***************************************************************************/


#include <extdll.h>
#include <meta_api.h>
#include <dllapi.h>
#include "sdk_util.h"
#include <h_export.h>
#include <modules.h>
#include <vector>
#include <limits.h>
#include "engine.h"

extern "C" void destroy(MessageInfo* p) {
    delete p;
}

GlobalInfo GlInfo;
MsgSets Msg[MAX_MESSAGES];
int LastMessage;

cvar_t amxxe_version = {"amxxe_version", VERSION, FCVAR_SERVER, 0};


 
/********************************************************
  vexd's utility funcs
  ******************************************************/
// Finds edict points that are in a sphere, used in RadiusDamage.
edict_t *UTIL_FindEntityInSphere(edict_t *pStart, const Vector &vecCenter, float flRadius) {
	if (!pStart) pStart = NULL;

	pStart = FIND_ENTITY_IN_SPHERE(pStart, vecCenter, flRadius);

	if (!FNullEnt(pStart)) return pStart;
	return NULL;
}

// Makes A Half-Life string (which is just an integer, an index of the string
// half-life uses to refer to the string) out of an AMX cell.
int AMX_MAKE_STRING(AMX *oPlugin, cell tParam, int &iLength) {
	char *szNewValue = GET_AMXSTRING(oPlugin, tParam, 0, iLength);

	char* szCopyValue = new char[iLength + 1]; 
	strncpy(szCopyValue , szNewValue, iLength); 
	*(szCopyValue + iLength) = '\0'; 

	return MAKE_STRING(szCopyValue);
}

// Makes a char pointer out of an AMX cell.
char *AMX_GET_STRING(AMX *oPlugin, cell tParam, int &iLength) {
	char *szNewValue = GET_AMXSTRING(oPlugin, tParam, 0, iLength);

	char* szCopyValue = new char[iLength + 1]; 
	strncpy(szCopyValue , szNewValue, iLength); 
	*(szCopyValue + iLength) = '\0';

	return szCopyValue;
}

/********************************************************
  exported functions
  ******************************************************/

//(BAILOPAN)
//Hooks a register_message()
static cell AMX_NATIVE_CALL register_message(AMX *amx, cell *params)
{
	int iLen;
	int iFunctionIndex;
	int iMessage = params[1];
	char *szFunction = AMX_GET_STRING(amx, params[2], iLen);

	if (iMessage > 0 && iMessage < MAX_MESSAGES) {
		if (AMX_FINDPUBLIC(amx, szFunction, &iFunctionIndex) == AMX_ERR_NONE) {
			Msg[iMessage].isHooked = true;
			Msg[iMessage].type = iMessage;
			Msg[iMessage].msgCalls.put(amx, iFunctionIndex);
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	} else {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}

	return 1;
}

//(BAILOPAN)
//Gets the argument type of a message argument
static cell AMX_NATIVE_CALL get_msg_argtype(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];

	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if ((Msg[msg_type].isHooked) && (Msg[msg_type].msg != NULL)) {
			return Msg[msg_type].msg->ArgType(argn);
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
//Gets the argument count for a message.
static cell AMX_NATIVE_CALL get_msg_args(AMX *amx, cell *params)
{
	int msg_type = params[1];

	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if ((Msg[msg_type].isHooked) && (Msg[msg_type].msg != NULL)) {
			return Msg[msg_type].msg->args();
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}
}

//(BAILOPAN)
//gets a message argument as an integer
static cell AMX_NATIVE_CALL get_msg_arg_int(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];
	
	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if (Msg[msg_type].isHooked && Msg[msg_type].msg!=NULL) {
			if (argn < Msg[msg_type].msg->args() && argn > 0) {
				return Msg[msg_type].msg->RetArg_Int(argn);
			} else {
				AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
				return 0;
			}
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
//gets a message argument as a float
static cell AMX_NATIVE_CALL get_msg_arg_float(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];

	float retVal;
	
	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if (Msg[msg_type].isHooked && Msg[msg_type].msg!=NULL) {
			if (argn < Msg[msg_type].msg->args() && argn > 0) {
				retVal = Msg[msg_type].msg->RetArg_Float(argn);
				return *(cell*)((void *)&retVal);
			} else {
				AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
				return 0;
			}
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
//gets a message argument as an string
static cell AMX_NATIVE_CALL get_msg_arg_string(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];
	char *szValue;
	
	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if (Msg[msg_type].isHooked && Msg[msg_type].msg!=NULL) {
			if (argn < Msg[msg_type].msg->args() && argn > 0) {
				szValue = Msg[msg_type].msg->RetArg_String(argn);
				return SET_AMXSTRING(amx, params[3], szValue, params[4]);
			} else {
				AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
				return 0;
			}
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
static cell AMX_NATIVE_CALL set_msg_arg_string(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];
	int iLen;
	char *szData = AMX_GET_STRING(amx, params[3], iLen);
	
	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if (Msg[msg_type].isHooked && Msg[msg_type].msg!=NULL) {
			if (argn < Msg[msg_type].msg->args() && argn > 0) {
				if (Msg[msg_type].msg->Set(argn, arg_string, szData)) {
					return 1;
				} else {
					return 0;
				}
			} else {
				AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
				return 0;
			}
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
static cell AMX_NATIVE_CALL set_msg_arg_float(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];
	int argtype = params[3];
	
	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if (Msg[msg_type].isHooked && Msg[msg_type].msg!=NULL) {
			if (argn < Msg[msg_type].msg->args() && argn > 0) {
				if (Msg[msg_type].msg->Set(argn, argtype, params[4])) {
					return 1;
				} else {
					return 0;
				}
			} else {
				AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
				return 0;
			}
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
static cell AMX_NATIVE_CALL set_msg_arg_int(AMX *amx, cell *params)
{
	int msg_type = params[1];
	int argn = params[2];
	int argtype = params[3];
	int iData = params[4];
	
	if (msg_type < 0 || msg_type > MAX_MESSAGES) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	} else {
		if (Msg[msg_type].isHooked && Msg[msg_type].msg!=NULL) {
			if (argn < Msg[msg_type].msg->args() && argn > 0) {
				if (Msg[msg_type].msg->Set(argn, argtype, iData)) {
					return 1;
				} else {
					return 0;
				}
			} else {
				AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
				return 0;
			}
		} else {
			AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
			return 0;
		}
	}

	return 1;
}

//(BAILOPAN)
//Sets a pvPrivateData offset for a player (player, offset, val, float=0)
static cell AMX_NATIVE_CALL set_offset_short(AMX *amx, cell *params)
{
	int index = params[1];
	int off = params[2];
	
	if (index < 1 || index > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	edict_t *Player = INDEXENT(index);
	
#ifndef __linux__
       off -= 5;
#endif
	
	*((short *)Player->pvPrivateData + off) = params[3];
	
	return 1;
}

//(BAILOPAN)
//Sets a pvPrivateData offset for a player (player, offset, val)
static cell AMX_NATIVE_CALL set_offset(AMX *amx, cell *params)
{
	int index = params[1];
	int off = params[2];
	
	if (index < 1 || index > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	edict_t *Player = INDEXENT(index);
	
#ifndef __linux__
       off -= 5;
#endif
	
	*((int *)Player->pvPrivateData + off) = params[3];
	
	return 1;
}

//(BAILOPAN)
//Sets a pvPrivateData offset for a player (player, offset, Float:val)
static cell AMX_NATIVE_CALL set_offset_float(AMX *amx, cell *params)
{
	int index = params[1];
	int off = params[2];
	
	if (index < 1 || index > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	edict_t *Player = INDEXENT(index);
	
#ifndef __linux__
       off -= 5;
#endif
	
	*((float *)Player->pvPrivateData + off) = params[3];
	
	return 1;
}

//(BAILOPAN)
//Gets a pvPrivateData offset for a player (player, offset)
static cell AMX_NATIVE_CALL get_offset_short(AMX *amx, cell *params)
{
	int index = params[1];
	int off = params[2];
	
	if (index < 1 || index > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	edict_t *Player = INDEXENT(index);
	
#ifndef __linux__
       off -= 5;
#endif
	
	return (int)*((short *)Player->pvPrivateData + off);
	
}

//(BAILOPAN)
//Gets a pvPrivateData offset for a player (player, offset, float=0)
static cell AMX_NATIVE_CALL get_offset(AMX *amx, cell *params)
{
	int index = params[1];
	int off = params[2];
	
	if (index < 1 || index > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	edict_t *Player = INDEXENT(index);
	
#ifndef __linux__
       off -= 5;
#endif
	
	return (int)*((int *)Player->pvPrivateData + off);
	
}

//(BAILOPAN)
//Gets a pvPrivateData offset for a player (player, offset)
static cell AMX_NATIVE_CALL get_offset_float(AMX *amx, cell *params)
{
	int index = params[1];
	int off = params[2];
	float retVal;
	
	if (index < 1 || index > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx, AMX_ERR_NATIVE);
		return 0;
	}
	
	edict_t *Player = INDEXENT(index);
	
#ifndef __linux__
       off -= 5;
#endif
	
	retVal = ((float)*((float *)Player->pvPrivateData + off));

	return *(cell*)((void *)&retVal);
	
	return 1;
}

//(BAILOPAN)
//return operating system
static cell AMX_NATIVE_CALL get_system_os(AMX *amx, cell *params)
{
	int iLen = params[2];
#ifndef __linux__
	char *szOS = "win32";
#else
	char *szOS = "linux";
#endif
	return SET_AMXSTRING(amx, params[1], szOS, iLen);
}

//(BAILOPAN)
//Allows you to issue a command to the operating system.
static cell AMX_NATIVE_CALL system_cmd(AMX  *amx, cell *params)
{
	int i_apptype = params[1];
	int iLen, retVal, iLen2;
	char *szCommand = AMX_GET_STRING(amx, params[2], iLen);
	char *szDirectory = AMX_GET_STRING(amx, params[3], iLen2);
	if (!iLen2) {
		szDirectory = NULL;
	}
#ifndef __linux__
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	ZeroMemory(&si, sizeof(si));
	if (i_apptype & 2) {
		si.dwFlags = STARTF_USESHOWWINDOW;
		si.wShowWindow = SW_HIDE;
	}
	si.cb = sizeof(si);
	ZeroMemory(&pi, sizeof(pi));

	if (!i_apptype & 1) {
		if (!CreateProcess(NULL, (LPTSTR)szCommand, NULL, NULL, FALSE, 0, NULL, (LPCTSTR)szDirectory, &si, &pi)) {
			return 0;
		} else {
			retVal = 1;
		}
	} else {
		if (!CreateProcess((LPCTSTR)szCommand, NULL, NULL, NULL, FALSE, 0, NULL, (LPCTSTR)szDirectory, &si, &pi)) {
			return 0;
		} else {
			retVal = 1;
		}
	}
#else
	void *stack;
	pid_t pid;
	
	if (!app_type) {
		app_type = 64;
	}

	stack = malloc(app_type * 1024);
	if (stack == 0) {
		return -1;
	}

	pid = clone(&thread_fork, (char *)stack + app_type*1024, 0, szCommand);
	if (pid == -1) {
		return -1;
	}
	pid = waitpid(pid, 0, 0);
	if (pid == -1) {
		return -1;
	}
	free(stack);
	retval = 1;
#endif
	return retVal;
}

// Get an integer from an entities entvars.
// (vexd)
static cell AMX_NATIVE_CALL entity_get_int(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	int iRetValue = 0;

	// Is it a valid entity?
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	// get its edict pointer.
	edict_t* pEntity = INDEXENT(iTargetEntity);

	// Is it a real entity?
	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case gamestate:
			iRetValue = pEntity->v.gamestate;
			break;
		case oldbuttons:
			iRetValue = pEntity->v.oldbuttons;
			break;
		case groupinfo:
			iRetValue = pEntity->v.groupinfo;
			break;
		case iuser1:
			iRetValue = pEntity->v.iuser1;
			break;
		case iuser2:
			iRetValue = pEntity->v.iuser2;
			break;
		case iuser3:
			iRetValue = pEntity->v.iuser3;
			break;
		case iuser4:
			iRetValue = pEntity->v.iuser4;
			break;
		case weaponanim:
			iRetValue = pEntity->v.weaponanim;
			break;
		case pushmsec:
			iRetValue = pEntity->v.pushmsec;
			break;
		case bInDuck:
			iRetValue = pEntity->v.bInDuck;
			break;
		case flTimeStepSound:
			iRetValue = pEntity->v.flTimeStepSound;
			break;
		case flSwimTime:
			iRetValue = pEntity->v.flSwimTime;
			break;
		case flDuckTime:
			iRetValue = pEntity->v.flDuckTime;
			break;
		case iStepLeft:
			iRetValue = pEntity->v.iStepLeft;
			break;
		case movetype:
			iRetValue = pEntity->v.movetype;
			break;
		case solid:
			iRetValue = pEntity->v.solid;
			break;
		case skin:
			iRetValue = pEntity->v.skin;
			break;
		case body:
			iRetValue = pEntity->v.body;
			break;
		case effects:
			iRetValue = pEntity->v.effects;
			break;
		case light_level:
			iRetValue = pEntity->v.light_level;
			break;
		case sequence:
			iRetValue = pEntity->v.sequence;
			break;
		case gaitsequence:
			iRetValue = pEntity->v.gaitsequence;
			break;
		case modelindex:
			iRetValue = pEntity->v.modelindex;
			break;
		case playerclass:
			iRetValue = pEntity->v.playerclass;
			break;
		case waterlevel:
			iRetValue = pEntity->v.waterlevel;
			break;
		case watertype:
			iRetValue = pEntity->v.watertype;
			break;
		case spawnflags:
			iRetValue = pEntity->v.spawnflags;
			break;
		case flags:
			iRetValue = pEntity->v.flags;
			break;
		case colormap:
			iRetValue = pEntity->v.colormap;
			break;
		case team:
			iRetValue = pEntity->v.team;
			break;
		case fixangle:
			iRetValue = pEntity->v.fixangle;
			break;
		case weapons:
			iRetValue = pEntity->v.weapons;
			break;
		case rendermode:
			iRetValue = pEntity->v.rendermode;
			break;
		case renderfx:
			iRetValue = pEntity->v.renderfx;
			break;
		case button:
			iRetValue = pEntity->v.button;
			break;
		case impulse:
			iRetValue = pEntity->v.impulse;
			break;
		case deadflag:
			iRetValue = pEntity->v.deadflag;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return iRetValue;
}

//vexd
static cell AMX_NATIVE_CALL entity_set_int(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	int iNewValue = params[3];
  
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case gamestate:
			pEntity->v.gamestate = iNewValue;
			break;
		case oldbuttons:
			pEntity->v.oldbuttons = iNewValue;
			break;
		case groupinfo:
			pEntity->v.groupinfo = iNewValue;
			break;
		case iuser1:
			pEntity->v.iuser1 = iNewValue;
			break;
		case iuser2:
			pEntity->v.iuser2 = iNewValue;
			break;
		case iuser3:
			pEntity->v.iuser3 = iNewValue;
			break;
		case iuser4:
			pEntity->v.iuser4 = iNewValue;
			break;
		case weaponanim:
			pEntity->v.weaponanim = iNewValue;
			break;
		case pushmsec:
			pEntity->v.pushmsec = iNewValue;
			break;
		case bInDuck:
			pEntity->v.bInDuck = iNewValue;
			break;
		case flTimeStepSound:
			pEntity->v.flTimeStepSound = iNewValue;
			break;
		case flSwimTime:
			pEntity->v.flSwimTime = iNewValue;
			break;
		case flDuckTime:
			pEntity->v.flDuckTime = iNewValue;
			break;
		case iStepLeft:
			pEntity->v.iStepLeft = iNewValue;
			break;
		case movetype:
			pEntity->v.movetype = iNewValue;
			break;
		case solid:
			pEntity->v.solid = iNewValue;
			break;
		case skin:
			pEntity->v.skin = iNewValue;
			break;
		case body:
			pEntity->v.body = iNewValue;
			break;
		case effects:
			pEntity->v.effects = iNewValue;
			break;
		case light_level:
			pEntity->v.light_level = iNewValue;
			break;
		case sequence:
			pEntity->v.sequence = iNewValue;
			break;
		case gaitsequence:
			pEntity->v.gaitsequence = iNewValue;
			break;
		case modelindex:
			pEntity->v.modelindex = iNewValue;
			break;
		case playerclass:
			pEntity->v.playerclass = iNewValue;
			break;
		case waterlevel:
			pEntity->v.waterlevel = iNewValue;
			break;
		case watertype:
			pEntity->v.watertype = iNewValue;
			break;
		case spawnflags:
			pEntity->v.spawnflags = iNewValue;
			break;
		case flags:
			pEntity->v.flags = iNewValue;
			break;
		case colormap:
			pEntity->v.colormap = iNewValue;
			break;
		case team:
			pEntity->v.team = iNewValue;
			break;
		case fixangle:
			pEntity->v.fixangle = iNewValue;
			break;
		case weapons:
			pEntity->v.weapons = iNewValue;
			break;
		case rendermode:
			pEntity->v.rendermode = iNewValue;
			break;
		case renderfx:
			pEntity->v.renderfx = iNewValue;
			break;
		case button:
			pEntity->v.button = iNewValue;
			break;
		case impulse:
			pEntity->v.impulse = iNewValue;
			break;
		case deadflag:
			pEntity->v.deadflag = iNewValue;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return 1;
}

// Gets a float out of entvars.
//(vexd)
static cell AMX_NATIVE_CALL entity_get_float(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	float fRetValue = 0;
  
	// Valid entity?
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	// Get pointer.
	edict_t* pEntity = INDEXENT(iTargetEntity);

	// is it a valid entity?
	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case impacttime:
			fRetValue = pEntity->v.impacttime;
			break;
		case starttime:
			fRetValue = pEntity->v.starttime;
			break;
		case idealpitch:
			fRetValue = pEntity->v.idealpitch;
			break;
		case pitch_speed:
			fRetValue = pEntity->v.pitch_speed;
			break;
		case ideal_yaw:
			fRetValue = pEntity->v.ideal_yaw;
			break;
		case yaw_speed:
			fRetValue = pEntity->v.yaw_speed;
			break;
		case ltime:
			fRetValue = pEntity->v.ltime;
			break;
		case nextthink:
			fRetValue = pEntity->v.nextthink;
			break;
		case gravity:
			fRetValue = pEntity->v.gravity;
			break;
		case friction:
			fRetValue = pEntity->v.friction;
			break;
		case frame:
			fRetValue = pEntity->v.frame;
			break;
		case animtime:
			fRetValue = pEntity->v.animtime;
			break;
		case framerate:
			fRetValue = pEntity->v.framerate;
			break;
		case health:
			fRetValue = pEntity->v.health;
			break;
		case frags:
			fRetValue = pEntity->v.frags;
			break;
		case takedamage:
			fRetValue = pEntity->v.takedamage;
			break;
		case max_health:
			fRetValue = pEntity->v.max_health;
			break;
		case teleport_time:
			fRetValue = pEntity->v.teleport_time;
			break;
		case armortype:
			fRetValue = pEntity->v.armortype;
			break;
		case armorvalue:
			fRetValue = pEntity->v.armorvalue;
			break;
		case dmg_take:
			fRetValue = pEntity->v.dmg_take;
			break;
		case dmg_save:
			fRetValue = pEntity->v.dmg_save;
			break;
		case dmg:
			fRetValue = pEntity->v.dmg;
			break;
		case dmgtime:
			fRetValue = pEntity->v.dmgtime;
			break;
		case speed:
			fRetValue = pEntity->v.speed;
			break;
		case air_finished:
			fRetValue = pEntity->v.air_finished;
			break;
		case pain_finished:
			fRetValue = pEntity->v.pain_finished;
			break;
		case radsuit_finished:
			fRetValue = pEntity->v.radsuit_finished;
			break;
		case scale:
			fRetValue = pEntity->v.scale;
			break;
		case renderamt:
			fRetValue = pEntity->v.renderamt;
			break;
		case maxspeed:
			fRetValue = pEntity->v.maxspeed;
			break;
		case fov:
			fRetValue = pEntity->v.fov;
			break;
		case flFallVelocity:
			fRetValue = pEntity->v.flFallVelocity;
			break;
		case fuser1:
			fRetValue = pEntity->v.fuser1;
			break;
		case fuser2:
			fRetValue = pEntity->v.fuser2;
			break;
		case fuser3:
			fRetValue = pEntity->v.fuser3;
			break;
		case fuser4:
			fRetValue = pEntity->v.fuser4;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return *(cell*)((void *)&fRetValue);
}

// Almost the same as Get_float, look there for comments.
static cell AMX_NATIVE_CALL entity_set_float(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	float fNewValue = *(float *)((void *)&params[3]);
  
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case impacttime:
			pEntity->v.impacttime = fNewValue;
			break;
		case starttime:
			pEntity->v.starttime = fNewValue;
			break;
		case idealpitch:
			pEntity->v.idealpitch = fNewValue;
			break;
		case pitch_speed:
			pEntity->v.pitch_speed = fNewValue;
			break;
		case ideal_yaw:
			pEntity->v.ideal_yaw = fNewValue;
			break;
		case yaw_speed:
			pEntity->v.yaw_speed = fNewValue;
			break;
		case ltime:
			pEntity->v.ltime = fNewValue;
			break;
		case nextthink:
			pEntity->v.nextthink = fNewValue;
			break;
		case gravity:
			pEntity->v.gravity = fNewValue;
			break;
		case friction:
			pEntity->v.friction = fNewValue;
			break;
		case frame:
			pEntity->v.frame = fNewValue;
			break;
		case animtime:
			pEntity->v.animtime = fNewValue;
			break;
		case framerate:
			pEntity->v.framerate = fNewValue;
			break;
		case health:
			pEntity->v.health = fNewValue;
			break;
		case frags:
			pEntity->v.frags = fNewValue;
			break;
		case takedamage:
			pEntity->v.takedamage = fNewValue;
			break;
		case max_health:
			pEntity->v.max_health = fNewValue;
			break;
		case teleport_time:
			pEntity->v.teleport_time = fNewValue;
			break;
		case armortype:
			pEntity->v.armortype = fNewValue;
			break;
		case armorvalue:
			pEntity->v.armorvalue = fNewValue;
			break;
		case dmg_take:
			pEntity->v.dmg_take = fNewValue;
			break;
		case dmg_save:
			pEntity->v.dmg_save = fNewValue;
			break;
		case dmg:
			pEntity->v.dmg = fNewValue;
			break;
		case dmgtime:
			pEntity->v.dmgtime = fNewValue;
			break;
		case speed:
			pEntity->v.speed = fNewValue;
			break;
		case air_finished:
			pEntity->v.air_finished = fNewValue;
			break;
		case pain_finished:
			pEntity->v.pain_finished = fNewValue;
			break;
		case radsuit_finished:
			pEntity->v.radsuit_finished = fNewValue;
			break;
		case scale:
			pEntity->v.scale = fNewValue;
			break;
		case renderamt:
			pEntity->v.renderamt = fNewValue;
			break;
		case maxspeed:
			pEntity->v.maxspeed = fNewValue;
			break;
		case fov:
			pEntity->v.fov = fNewValue;
			break;
		case flFallVelocity:
			pEntity->v.flFallVelocity = fNewValue;
			break;
		case fuser1:
			pEntity->v.fuser1 = fNewValue;
			break;
		case fuser2:
			pEntity->v.fuser2 = fNewValue;
			break;
		case fuser3:
			pEntity->v.fuser3 = fNewValue;
			break;
		case fuser4:
			pEntity->v.fuser4 = fNewValue;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return 1;
}

// Gets a vector from entvars. I use a float[3] to describe half life vectors,
// as this is how half-life handles vectors. also angle vectors and the such are
// less then 1, so you need floats. All functions that i have that work with vectors
// use the float[3], this makes it easier.
//(vexd)
static cell AMX_NATIVE_CALL entity_get_vector(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];

	cell *vReturnTo = GET_AMXADDR(amx,params[3]);

	Vector vRetValue = Vector(0, 0, 0);
  
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case origin:
			vRetValue = pEntity->v.origin;
			break;
		case oldorigin:
			vRetValue = pEntity->v.oldorigin;
			break;
		case velocity:
			vRetValue = pEntity->v.velocity;
			break;
		case basevelocity:
			vRetValue = pEntity->v.basevelocity;
			break;
		case clbasevelocity:
			vRetValue = pEntity->v.clbasevelocity;
			break;
		case movedir:
			vRetValue = pEntity->v.movedir;
			break;
		case angles:
			vRetValue = pEntity->v.angles;
			break;
		case avelocity:
			vRetValue = pEntity->v.avelocity;
			break;
		case punchangle:
			vRetValue = pEntity->v.punchangle;
			break;
		case v_angle:
			vRetValue = pEntity->v.v_angle;
			break;
		case endpos:
			vRetValue = pEntity->v.endpos;
			break;
		case startpos:
			vRetValue = pEntity->v.startpos;
			break;
		case absmin:
			vRetValue = pEntity->v.absmin;
			break;
		case absmax:
			vRetValue = pEntity->v.absmax;
			break;
		case mins:
			vRetValue = pEntity->v.mins;
			break;
		case maxs:
			vRetValue = pEntity->v.maxs;
			break;
		case size:
			vRetValue = pEntity->v.size;
			break;
		case rendercolor:
			vRetValue = pEntity->v.rendercolor;
			break;
		case view_ofs:
			vRetValue = pEntity->v.view_ofs;
			break;
		case vuser1:
			vRetValue = pEntity->v.vuser1;
			break;
		case vuser2:
			vRetValue = pEntity->v.vuser2;
			break;
		case vuser3:
			vRetValue = pEntity->v.vuser3;
			break;
		case vuser4:
			vRetValue = pEntity->v.vuser4;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}


	vReturnTo[0] = *(cell*)((void *)&vRetValue.x); 
	vReturnTo[1] = *(cell*)((void *)&vRetValue.y); 
	vReturnTo[2] = *(cell*)((void *)&vRetValue.z); 

	return 1;
}

// Set is close to get, these functions are pretty self-explanitory.
static cell AMX_NATIVE_CALL entity_set_vector(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	cell *vInput = GET_AMXADDR(amx,params[3]);

	float fNewX = *(float *)((void *)&vInput[0]);
	float fNewY = *(float *)((void *)&vInput[1]);
	float fNewZ = *(float *)((void *)&vInput[2]);

	Vector vNewValue = Vector(fNewX, fNewY, fNewZ);
  
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case origin:
			pEntity->v.origin = vNewValue;
			break;
		case oldorigin:
			pEntity->v.oldorigin = vNewValue;
			break;
		case velocity:
			pEntity->v.velocity = vNewValue;
			break;
		case basevelocity:
			pEntity->v.basevelocity = vNewValue;
			break;
		case clbasevelocity:
			pEntity->v.clbasevelocity = vNewValue;
			break;
		case movedir:
			pEntity->v.movedir = vNewValue;
			break;
		case angles:
			pEntity->v.angles = vNewValue;
			break;
		case avelocity:
			pEntity->v.avelocity = vNewValue;
			break;
		case punchangle:
			pEntity->v.punchangle = vNewValue;
			break;
		case v_angle:
			pEntity->v.v_angle = vNewValue;
			break;
		case endpos:
			pEntity->v.endpos = vNewValue;
			break;
		case startpos:
			pEntity->v.startpos = vNewValue;
			break;
		case absmin:
			pEntity->v.absmin = vNewValue;
			break;
		case absmax:
			pEntity->v.absmax = vNewValue;
			break;
		case mins:
			pEntity->v.mins = vNewValue;
			break;
		case maxs:
			pEntity->v.maxs = vNewValue;
			break;
		case size:
			pEntity->v.size = vNewValue;
			break;
		case rendercolor:
			pEntity->v.rendercolor = vNewValue;
			break;
		case view_ofs:
			pEntity->v.view_ofs = vNewValue;
			break;
		case vuser1:
			pEntity->v.vuser1 = vNewValue;
			break;
		case vuser2:
			pEntity->v.vuser2 = vNewValue;
			break;
		case vuser3:
			pEntity->v.vuser3 = vNewValue;
			break;
		case vuser4:
			pEntity->v.vuser4 = vNewValue;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return 1;
}

// Get an edict pointer (or index, amx does not have pointers) from entvars.
//(vexd)
static cell AMX_NATIVE_CALL entity_get_edict(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];

	// Return edict structure.
	edict_t *pRetValue;
  
	// Valid entity?
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	// get edict pointer of target entity
	edict_t* pEntity = INDEXENT(iTargetEntity);

	// is it valid?
	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case chain:
			pRetValue = pEntity->v.chain;
			break;
		case dmg_inflictor:
			pRetValue = pEntity->v.dmg_inflictor;
			break;
		case enemy:
			pRetValue = pEntity->v.enemy;
			break;
		case aiment:
			pRetValue = pEntity->v.aiment;
			break;
		case owner:
			pRetValue = pEntity->v.owner;
			break;
		case groundentity:
			pRetValue = pEntity->v.groundentity;
			break;
		case pContainingEntity:
			pRetValue = pEntity->v.pContainingEntity;
			break;
		case euser1:
			pRetValue = pEntity->v.euser1;
			break;
		case euser2:
			pRetValue = pEntity->v.euser2;
			break;
		case euser3:
			pRetValue = pEntity->v.euser3;
			break;
		case euser4:
			pRetValue = pEntity->v.euser4;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	if(!FNullEnt(pRetValue)) {
		return ENTINDEX(pRetValue);
	} else {
		return 0;
	}
}

// Set edict is almost the same as get, look there for comments.
//(vexd)
static cell AMX_NATIVE_CALL entity_set_edict(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	edict_t *pNewValue = INDEXENT(params[3]);
  
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	if(FNullEnt(pNewValue)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case chain:
			pEntity->v.chain = pNewValue;
			break;
		case dmg_inflictor:
			pEntity->v.dmg_inflictor = pNewValue;
			break;
		case enemy:
			pEntity->v.enemy = pNewValue;
			break;
		case aiment:
			pEntity->v.aiment = pNewValue;
			break;
		case owner:
			pEntity->v.owner = pNewValue;
			break;
		case groundentity:
			pEntity->v.groundentity = pNewValue;
			break;
		case pContainingEntity:
			pEntity->v.pContainingEntity = pNewValue;
			break;
		case euser1:
			pEntity->v.euser1 = pNewValue;
			break;
		case euser2:
			pEntity->v.euser2 = pNewValue;
			break;
		case euser3:
			pEntity->v.euser3 = pNewValue;
			break;
		case euser4:
			pEntity->v.euser4 = pNewValue;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return 1;
}

// Strings. I remember now that weaponmodel and viewmodel are strings as well,
// even though half-life declares them as integers. (a half-life string is just a
// typedefed integer.).
//(vexd)
static cell AMX_NATIVE_CALL entity_get_string(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	int iszRetValue = 0;
  
	// Valid entity?
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	// Get pointer of entity.
	edict_t* pEntity = INDEXENT(iTargetEntity);

	// Is entity valid again?
	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case classname:
			iszRetValue = pEntity->v.classname;
			break;
		case globalname:
			iszRetValue = pEntity->v.globalname;
			break;
		case model:
			iszRetValue = pEntity->v.model;
			break;
		case target:
			iszRetValue = pEntity->v.target;
			break;
		case targetname:
			iszRetValue = pEntity->v.targetname;
			break;
		case netname:
			iszRetValue = pEntity->v.netname;
			break;
		case message:
			iszRetValue = pEntity->v.message;
			break;
		case noise:
			iszRetValue = pEntity->v.noise;
			break;
		case noise1:
			iszRetValue = pEntity->v.noise1;
			break;
		case noise2:
			iszRetValue = pEntity->v.noise2;
			break;
		case noise3:
			iszRetValue = pEntity->v.noise3;
			break;
		case viewmodel:
			iszRetValue = pEntity->v.viewmodel;
			break;
		case weaponmodel:
			iszRetValue = pEntity->v.weaponmodel;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return SET_AMXSTRING(amx, params[3], STRING(iszRetValue), params[4]); 
}

// Almost the same as Get_String, look there for comments.
//(vexd)
static cell AMX_NATIVE_CALL entity_set_string(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	int iLength;
	int iszNewValue = AMX_MAKE_STRING(amx, params[3], iLength);

	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case classname:
			pEntity->v.classname = iszNewValue;
			break;
		case globalname:
			pEntity->v.globalname = iszNewValue;
			break;
		case model:
			pEntity->v.model = iszNewValue;
			break;
		case target:
			pEntity->v.target = iszNewValue;
			break;
		case targetname:
			pEntity->v.targetname = iszNewValue;
			break;
		case netname:
			pEntity->v.netname = iszNewValue;
			break;
		case message:
			pEntity->v.message = iszNewValue;
			break;
		case noise:
			pEntity->v.noise = iszNewValue;
			break;
		case noise1:
			pEntity->v.noise1 = iszNewValue;
			break;
		case noise2:
			pEntity->v.noise2 = iszNewValue;
			break;
		case noise3:
			pEntity->v.noise3 = iszNewValue;
			break;
		case viewmodel:
			pEntity->v.viewmodel = iszNewValue;
			break;
		case weaponmodel:
			pEntity->v.weaponmodel = iszNewValue;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return 1;
}

// Bytes, these were used for some things, mostly useless, but might be useful.
// They are arrays, but we just use numbers in naming of the enum variables to
// let us pass get different positions in the array.
//(vexd)
static cell AMX_NATIVE_CALL entity_get_byte(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	int iRetValue = 0;

	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	switch(iValueSet) {
		case controller1:
			iRetValue = pEntity->v.controller[1];
			break;
		case controller2:
			iRetValue = pEntity->v.controller[2];
			break;
		case controller3:
			iRetValue = pEntity->v.controller[3];
			break;
		case controller4:
			iRetValue = pEntity->v.controller[4];
			break;
		case blending1:
			iRetValue = pEntity->v.blending[1];
			break;
		case blending2:
			iRetValue = pEntity->v.blending[2];
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return iRetValue;
}

// Like Get_Byte, but sets.
//(vexd)
static cell AMX_NATIVE_CALL entity_set_byte(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iValueSet = params[2];
	int iNewValue = params[3];

	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	if(iNewValue > 255) iNewValue = 255;
	if(iNewValue < 0) iNewValue = 0;

	switch(iValueSet) {
		case controller1:
			pEntity->v.controller[1] = iNewValue;
			break;
		case controller2:
			pEntity->v.controller[2] = iNewValue;
			break;
		case controller3:
			pEntity->v.controller[3] = iNewValue;
			break;
		case controller4:
			pEntity->v.controller[4] = iNewValue;
			break;
		case blending1:
			pEntity->v.blending[1] = iNewValue;
			break;
		case blending2:
			pEntity->v.blending[2] = iNewValue;
			break;
		default:
			AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
			return 0;
			break;
	}

	return 1;
}

// VelocityByAim, this function will take the aimvectors of an entity, and create a velocity
// of iVelocity in the direction of the aimvec.
//(vexd)
static cell AMX_NATIVE_CALL VelocityByAim(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	int iVelocity = params[2];
	cell *vReturnTo = GET_AMXADDR(amx,params[3]);

	Vector vRetValue = Vector(0, 0, 0);
  
	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pEntity = INDEXENT(iTargetEntity);

	if(FNullEnt(pEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	MAKE_VECTORS(pEntity->v.v_angle);
	vRetValue = gpGlobals->v_forward * iVelocity;

	vReturnTo[0] = *(cell*)((void *)&vRetValue.x); 
	vReturnTo[1] = *(cell*)((void *)&vRetValue.y); 
	vReturnTo[2] = *(cell*)((void *)&vRetValue.z); 

	return 1;
}

// RadiusDamage. Damages players within a certain radius. ToDo: add the
// damage messaging so players know where the damage is coming from
// (the red arrow-like things on the screen).
//(vexd)
static cell AMX_NATIVE_CALL RadiusDamage(AMX *amx, cell *params) {
	cell *vInput = GET_AMXADDR(amx,params[1]);

	float fCurrentX = *(float *)((void *)&vInput[0]);
	float fCurrentY = *(float *)((void *)&vInput[1]);
	float fCurrentZ = *(float *)((void *)&vInput[2]);
	int iDamageMultiplier = params[2];
	int iRadiusMultiplier = params[3];

	Vector vOrigin = Vector(fCurrentX, fCurrentY, fCurrentZ);

	edict_t *pSearchEnt = NULL;
	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 5 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) {
				pSearchEnt->v.health -= 10 + RANDOM_FLOAT(0,1 * iDamageMultiplier);
				if(pSearchEnt->v.health < 1) {
					pSearchEnt->v.health = 1;
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 4 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) {
				pSearchEnt->v.health -= 25 + RANDOM_FLOAT(0,2 * iDamageMultiplier);
				if(pSearchEnt->v.health < 1) {
					pSearchEnt->v.health = 1;
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 3 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) {
				pSearchEnt->v.health -= 50 + RANDOM_FLOAT(0,3 * iDamageMultiplier);
				if(pSearchEnt->v.health < 1) {
					pSearchEnt->v.health = 1;
					MDLL_ClientKill(pSearchEnt);
				}
			}
		}
	}

	pSearchEnt = NULL;

	while((pSearchEnt = UTIL_FindEntityInSphere(pSearchEnt, vOrigin, 2 * iRadiusMultiplier)) != NULL) {
		if(FStrEq(STRING(pSearchEnt->v.classname), "player")) {
			if(pSearchEnt->v.takedamage != DAMAGE_NO) MDLL_ClientKill(pSearchEnt);
		}
	}

	return 1;
}

// Gets the contents of a point. Return values for this are probably in const.h.
//(vexd)
static cell AMX_NATIVE_CALL PointContents(AMX *amx, cell *params) {
	cell *vInput = GET_AMXADDR(amx,params[1]);

	float fCurrentX = *(float *)((void *)&vInput[0]);
	float fCurrentY = *(float *)((void *)&vInput[1]);
	float fCurrentZ = *(float *)((void *)&vInput[2]);

	Vector vTestValue = Vector(fCurrentX, fCurrentY, fCurrentZ);

	return POINT_CONTENTS(vTestValue);
}


// CreateEntity. Makes an entity.
//(vexd)
static cell AMX_NATIVE_CALL create_entity(AMX *amx, cell *params) { 
	int iLength;
	int iszNewClassName = AMX_MAKE_STRING(amx, params[1], iLength);

	edict_t* pNewEntity = CREATE_NAMED_ENTITY(iszNewClassName);

	if(FNullEnt(pNewEntity)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	return ENTINDEX(pNewEntity);
}

// FindEntity, use this in a while loop. will return -1 when theres no entities left.
// It searches by classname.
//(vexd)
static cell AMX_NATIVE_CALL find_entity(AMX *amx, cell *params) {
	int iStartEnt = params[1];

	int iLengthSearchStrn;
	char *szValue = AMX_GET_STRING(amx, params[2], iLengthSearchStrn);

	edict_t *pStartEnt;

	if(iStartEnt == -1) {
		pStartEnt = NULL;
	} else {
		pStartEnt = INDEXENT(iStartEnt);

		if(FNullEnt(pStartEnt)) {
			return -1;
		}
	}

	int iReturnEnt = ENTINDEX(FIND_ENTITY_BY_STRING(pStartEnt, "classname", szValue));

	if(!iReturnEnt || FNullEnt(iReturnEnt)) {
		return -1;
	}

	return iReturnEnt;
}

// DispatchKeyValue, sets a key-value pair for a newly created entity.
// Use DispatchSpawn after doing ALL DispatchKeyValues on an entity.
//(vexd)
static cell AMX_NATIVE_CALL DispatchKeyValue(AMX *amx, cell *params) { 
	edict_t* pTarget = INDEXENT(params[1]);
	int iKeyLength;
	int iValueLength;
	char *szKey = AMX_GET_STRING(amx, params[2], iKeyLength);
	char *szValue = AMX_GET_STRING(amx, params[3], iValueLength);


	if(FNullEnt(pTarget)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	KeyValueData pkvd;
	pkvd.szClassName = (char *)STRING(pTarget->v.classname);
	pkvd.szKeyName = szKey;
	pkvd.szValue = szValue;
	pkvd.fHandled = false;

	MDLL_KeyValue(pTarget, &pkvd);
	return 1;
}

// DispatchSpawn. Call this after doing any DispatchKeyValues you are
// doing on an entity your creating.
//(vexd)
static cell AMX_NATIVE_CALL DispatchSpawn(AMX *amx, cell *params) { 
	edict_t* pTarget = INDEXENT(params[1]);

	if(FNullEnt(pTarget)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	MDLL_Spawn(pTarget);

	return 1;
}

// Set origin for entities. Use this instead of entvars, as it updates the engine.
// It also does a SetSize, this way we can set the size at the same time as origin.
//(vexd)
static cell AMX_NATIVE_CALL entity_set_origin(AMX *amx, cell *params) { 
	int iTargetEntity = params[1];
	cell *vInput = GET_AMXADDR(amx,params[2]);

	float fNewX = *(float *)((void *)&vInput[0]);
	float fNewY = *(float *)((void *)&vInput[1]);
	float fNewZ = *(float *)((void *)&vInput[2]);

	Vector vNewValue = Vector(fNewX, fNewY, fNewZ);

	if (iTargetEntity < 1 || iTargetEntity > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t* pTarget = INDEXENT(iTargetEntity);

	if(FNullEnt(pTarget)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	SET_SIZE(pTarget, pTarget->v.mins, pTarget->v.maxs);
	SET_ORIGIN(pTarget, vNewValue);

	return 1;
}


// Sets a model on an entity. Dont set models in entvars, it wont
// update the engine of the model change.
//(vexd)
static cell AMX_NATIVE_CALL entity_set_model(AMX *amx, cell *params) { 
	edict_t* pTarget = INDEXENT(params[1]);

	int iLength;
	char *szNewValue = AMX_GET_STRING(amx, params[2], iLength);

	if(FNullEnt(pTarget)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	SET_MODEL(pTarget, szNewValue);

	return 1;
}

//FindEntityByTarget (BAILOPAN)
// Works like FindEntity except finds by Target
static cell AMX_NATIVE_CALL find_ent_by_target(AMX *amx, cell *params)
{
	int iStart = params[1];
	int iLength;
	char *szValue = AMX_GET_STRING(amx, params[2], iLength);

	edict_t *pStart;

	if (iStart == -1) {
		pStart = NULL;
	} else {
		pStart = INDEXENT(iStart);
		if (FNullEnt(pStart)) {
			return -1;
		}
	}

	int iReturnEnt = ENTINDEX(FIND_ENTITY_BY_TARGET(pStart, szValue));

	if (!iReturnEnt || FNullEnt(iReturnEnt)) {
		return -1;
	}

	return iReturnEnt;
}

//FindEntityByModel (BAILOPAN)
//Takes in a classname and model...
static cell AMX_NATIVE_CALL find_ent_by_model(AMX *amx, cell *params) { 
	int iStart = params[1];
	int iLength, iLength2;
	char *szClass = AMX_GET_STRING(amx, params[2], iLength);
	char *szModel = AMX_GET_STRING(amx, params[3], iLength2);
	int iModel = MAKE_STRING(szModel);

	edict_t *pStart;

	if (iStart == -1) {
		pStart = NULL;
	} else {
		pStart = INDEXENT(iStart);
		if (FNullEnt(pStart)) {
			return -1;
		}
	}

	int checkEnt = ENTINDEX(FIND_ENTITY_BY_STRING(pStart, "classname", szClass));
	int iCheckModel = 0;

	while ((checkEnt && FNullEnt(checkEnt))) {
		iCheckModel = pStart->v.model;
		if (iCheckModel == iModel) {
			return checkEnt;
		} else {
			pStart = INDEXENT(checkEnt);
			checkEnt = ENTINDEX(FIND_ENTITY_BY_STRING(pStart, "classname", szClass));
		}
	}

	if(!checkEnt || FNullEnt(checkEnt)) {
		return -1;
	}

	return checkEnt;
}

//FindEntityByTName (BAILOPAN)
// Works like FindEntity except finds by TargetName
static cell AMX_NATIVE_CALL find_ent_by_tname(AMX *amx, cell *params) {
	int iStart = params[1];
	int iLength;
	char *szValue = AMX_GET_STRING(amx, params[2], iLength);

	edict_t *pStart;

	if (iStart == -1) {
		pStart = NULL;
	} else {
		pStart = INDEXENT(iStart);
		if (FNullEnt(pStart)) {
			return -1;
		}
	}

	int iReturnEnt = ENTINDEX(FIND_ENTITY_BY_TARGETNAME(pStart, szValue));

	if (!iReturnEnt || FNullEnt(iReturnEnt)) {
		return -1;
	}

	return iReturnEnt;
}

// FindEntityByOwner (BAILOPAN)
// Works like FindEntity except only returns by owner.
// Searches by classname.
static cell AMX_NATIVE_CALL find_ent_by_owner(AMX *amx, cell *params) { 
	int iStartEnt = params[1];
	int iEntOwner = params[3];
	int iLengthSearchStrn;
	char *szValue = AMX_GET_STRING(amx, params[2], iLengthSearchStrn);

	edict_t *pStartEnt;

	if(iStartEnt == -1) {
		pStartEnt = NULL;
	} else {
		pStartEnt = INDEXENT(iStartEnt);

		if(FNullEnt(pStartEnt)) {
			return -1;
		}
	}

	int checkEnt = ENTINDEX(FIND_ENTITY_BY_STRING(pStartEnt, "classname", szValue));
	int iOwner = -1;

	while ((checkEnt && FNullEnt(checkEnt)) && (iOwner!=-1)) {
		iOwner = ENTINDEX(pStartEnt->v.owner);
		if (iOwner == iEntOwner) {
			return checkEnt;
		} else {
			pStartEnt = INDEXENT(checkEnt);
			checkEnt = ENTINDEX(FIND_ENTITY_BY_STRING(pStartEnt, "classname", szValue));
		}
	}

	if(!checkEnt || FNullEnt(checkEnt) || (iOwner == -1)) {
		return -1;
	}

	return checkEnt;
}

//returns current number of entities in game (BAILOPAN)
static cell AMX_NATIVE_CALL entity_count(AMX *amx, cell *params)
{ 
	return NUMBER_OF_ENTITIES(); 
}

// RemoveEntity, this removes an entity from the world.
// Could also be done setting the flag FL_KILLME in entvars.
//(vexd)
static cell AMX_NATIVE_CALL remove_entity(AMX *amx, cell *params) { 
	int iTarget = params[1];

	edict_t* pTarget = INDEXENT(iTarget);

	if(FNullEnt(pTarget)) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	REMOVE_ENTITY(pTarget);

	return 1;
}

// VecToAngles, this is a half-life function for making a vector out of
// angles.
//(vexd)
static cell AMX_NATIVE_CALL vector_to_angle(AMX *amx, cell *params) { 
	cell *vInput = GET_AMXADDR(amx,params[1]);
	float fInX = *(float *)((void *)&vInput[0]);
	float fInY = *(float *)((void *)&vInput[1]);
	float fInZ = *(float *)((void *)&vInput[2]);

	Vector vVector = Vector(fInX, fInY, fInZ);
	Vector vAngle = Vector(0, 0, 0);
	VEC_TO_ANGLES(vVector, vAngle);

	cell *vReturnTo = GET_AMXADDR(amx,params[2]);
	vReturnTo[0] = *(cell*)((void *)&vAngle.x);
	vReturnTo[1] = *(cell*)((void *)&vAngle.y);
	vReturnTo[2] = *(cell*)((void *)&vAngle.z);

	return 1;
}

// VecLength, this gives you the length of a vector (float[3] type).
//(vexd)
static cell AMX_NATIVE_CALL vector_length(AMX *amx, cell *params) { 
	cell *vInput = GET_AMXADDR(amx,params[1]);
	float fInX = *(float *)((void *)&vInput[0]);
	float fInY = *(float *)((void *)&vInput[1]);
	float fInZ = *(float *)((void *)&vInput[2]);

	Vector vVector = Vector(fInX, fInY, fInZ);

	float flLength = vVector.Length();

	return *(cell*)((void *)&flLength);
}

// VecDist, this gives you the distance between 2 vectors (float[3] type).
//(vexd)
static cell AMX_NATIVE_CALL vector_distance(AMX *amx, cell *params) { 
	cell *vInput = GET_AMXADDR(amx,params[1]);
	float fInX = *(float *)((void *)&vInput[0]);
	float fInY = *(float *)((void *)&vInput[1]);
	float fInZ = *(float *)((void *)&vInput[2]);

	cell *vInput2 = GET_AMXADDR(amx,params[2]);
	float fInX2 = *(float *)((void *)&vInput2[0]);
	float fInY2 = *(float *)((void *)&vInput2[1]);
	float fInZ2 = *(float *)((void *)&vInput2[2]);

	Vector vVector = Vector(fInX, fInY, fInZ);
	Vector vVector2 = Vector(fInX2, fInY2, fInZ2);

	float flLength = (vVector - vVector2).Length();

	return *(cell*)((void *)&flLength);
}

// TraceNormal. This is just like TraceLine, but it gives back the
// Normal of whatever plane it hit. Use with TraceLine and you can have
// A point on a plane, with its normal, good for creating things that attach
// to walls (like tripmines).
//(vexd)
static cell AMX_NATIVE_CALL trace_normal(AMX *amx, cell *params) { 
	int iIgnoreEnt = params[1];

	cell *fpStart = GET_AMXADDR(amx,params[2]);
	float fStartX = *(float *)((void *)&fpStart[0]);
	float fStartY = *(float *)((void *)&fpStart[1]);
	float fStartZ = *(float *)((void *)&fpStart[2]);

	cell *fpEnd = GET_AMXADDR(amx,params[3]);
	float fEndX = *(float *)((void *)&fpEnd[0]);
	float fEndY = *(float *)((void *)&fpEnd[1]);
	float fEndZ = *(float *)((void *)&fpEnd[2]);


	cell *vReturnTo = GET_AMXADDR(amx,params[4]);

	Vector vStart = Vector(fStartX, fStartY, fStartZ);
	Vector vEnd = Vector(fEndX, fEndY, fEndZ);

	TraceResult tr;
	TRACE_LINE(vStart, vEnd, dont_ignore_monsters, INDEXENT(iIgnoreEnt), &tr);

	vReturnTo[0] = *(cell*)((void *)&tr.vecPlaneNormal.x); 
	vReturnTo[1] = *(cell*)((void *)&tr.vecPlaneNormal.y); 
	vReturnTo[2] = *(cell*)((void *)&tr.vecPlaneNormal.z); 

	if (tr.flFraction >= 1.0) return 0;

	return 1;
}

// TraceLn, Traces a Line in space. This is useful for making beams, or checking if
// An entity is at the end of a line.
// The return value is either the end of the line (where it crashed into an object,
// or the end you supplied.)
//(vexd)
static cell AMX_NATIVE_CALL trace_line(AMX *amx, cell *params) { 
	int iIgnoreEnt = params[1];

	cell *fpStart = GET_AMXADDR(amx,params[2]);
	float fStartX = *(float *)((void *)&fpStart[0]);
	float fStartY = *(float *)((void *)&fpStart[1]);
	float fStartZ = *(float *)((void *)&fpStart[2]);

	cell *fpEnd = GET_AMXADDR(amx,params[3]);
	float fEndX = *(float *)((void *)&fpEnd[0]);
	float fEndY = *(float *)((void *)&fpEnd[1]);
	float fEndZ = *(float *)((void *)&fpEnd[2]);


	cell *vReturnTo = GET_AMXADDR(amx,params[4]);

	Vector vStart = Vector(fStartX, fStartY, fStartZ);
	Vector vEnd = Vector(fEndX, fEndY, fEndZ);

	TraceResult tr;

	if(iIgnoreEnt == -1) {
		TRACE_LINE(vStart, vEnd, ignore_monsters, NULL, &tr);
	} else {
		TRACE_LINE(vStart, vEnd, dont_ignore_monsters, INDEXENT(iIgnoreEnt), &tr);
	}

	edict_t *pHit = tr.pHit;

	vReturnTo[0] = *(cell*)((void *)&tr.vecEndPos.x); 
	vReturnTo[1] = *(cell*)((void *)&tr.vecEndPos.y); 
	vReturnTo[2] = *(cell*)((void *)&tr.vecEndPos.z); 

	if(FNullEnt(pHit)) {
		return -1;
	}

	return ENTINDEX(pHit);
}

// Get gpGlobals->time, this is useful for timing things.
//(vexd)
static cell AMX_NATIVE_CALL halflife_time(AMX *amx, cell *params) {
	float fRetValue = gpGlobals->time;
	return *(cell*)((void *)&fRetValue);
}

//simulate two objects coliding
//(vexd)
static cell AMX_NATIVE_CALL fake_touch(AMX *amx, cell *params) {
	int iToucher = params[1];
	int iTouched = params[2];

	if(iToucher < 1 || iTouched < 1) return 0;
	if(iToucher > gpGlobals->maxEntities || iTouched > gpGlobals->maxEntities) return 0;

	edict_t *pToucher = INDEXENT(iToucher);
	edict_t *pTouched = INDEXENT(iTouched);

	if(FNullEnt(pToucher) || FNullEnt(pTouched)) return 0;

	MDLL_Touch(pToucher, pTouched);

	return 1;
}

//(SpaceDude)
static cell AMX_NATIVE_CALL get_grenade_id(AMX *amx, cell *params)  /* 4 param */ 
{
	int index = params[1];
	char* szModel;

	if (index<1||index>gpGlobals->maxClients)
	{
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE); 
		return 0; 
	}

	edict_t* pentFind = INDEXENT(params[4]);
	edict_t* pentOwner = INDEXENT(index);

	pentFind = FIND_ENTITY_BY_CLASSNAME( pentFind, "grenade" );
	while ( !FNullEnt( pentFind ) )
	{
		if (pentFind->v.owner == pentOwner)
		{
			szModel = new char[params[3]];
			szModel = (char*)STRING(pentFind->v.model);
			SET_AMXSTRING(amx,params[2],szModel,params[3]);
			delete [] szModel;
			return ENTINDEX(pentFind);
		}
		pentFind = FIND_ENTITY_BY_CLASSNAME( pentFind, "grenade" );
	}
	return 0;
}

// Sets a message block.
static cell AMX_NATIVE_CALL set_msg_block(AMX *amx, cell *params) { 
	int iMessage = params[1];
	int iMessageFlags = params[2];

	if (iMessage < 1 || iMessage > MAX_MESSAGES) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	GlInfo.iMessageBlock[iMessage] = iMessageFlags;

	return 1;
}

// Gets a message block.
static cell AMX_NATIVE_CALL get_msg_block(AMX *amx, cell *params) { 
	int iMessage = params[1];

	if (iMessage < 1 || iMessage > MAX_MESSAGES) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	return GlInfo.iMessageBlock[iMessage];
}

// SetLights, this sets the lights for the map.
//(vexd)
static cell AMX_NATIVE_CALL set_lights(AMX *amx, cell *params) { 
	int iLength;

	char *szLights = AMX_GET_STRING(amx, params[1], iLength);

	if(FStrEq(szLights, "#OFF")) {
		GlInfo.bLights = false;
		memset(GlInfo.szLastLights, 0x0, 128);
		(g_engfuncs.pfnLightStyle)(0, (char *)GlInfo.szRealLights);
		return 1;
	}

	GlInfo.bLights = true;

	//Reset LastLights
	memset(GlInfo.szLastLights, 0x0, 128);
	//Store the previous lighting.
	memcpy(GlInfo.szLastLights, szLights, strlen(szLights));

	(g_engfuncs.pfnLightStyle)(0, szLights);

	// These make it so that players/weaponmodels look like whatever the lighting is
	// at. otherwise it would color players under the skybox to these values.
	SERVER_COMMAND("sv_skycolor_r 0\n");
	SERVER_COMMAND("sv_skycolor_g 0\n");
	SERVER_COMMAND("sv_skycolor_b 0\n");

	return 1;
}


// SetView, this sets the view of a player. This is done by
// Creating a camera entity, which follows the player.
//(vexd)
static cell AMX_NATIVE_CALL set_view(AMX *amx, cell *params) { 
	int iIndex = params[1];
	int iCameraType = params[2];

	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	edict_t *pPlayer = INDEXENT(iIndex);
	edict_t *pNewCamera;

	switch(iCameraType) {
		case CAMERA_NONE:
			SET_VIEW(pPlayer, pPlayer);
			if(PlInfo[ENTINDEX(pPlayer)].pViewEnt) {
				REMOVE_ENTITY(PlInfo[ENTINDEX(pPlayer)].pViewEnt);
			}
			PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_NONE;
			PlInfo[ENTINDEX(pPlayer)].pViewEnt = NULL;

			pPlayer->v.rendermode = PlInfo[ENTINDEX(pPlayer)].iRenderMode;
			pPlayer->v.renderamt = PlInfo[ENTINDEX(pPlayer)].fRenderAmt;

			PlInfo[ENTINDEX(pPlayer)].iRenderMode = 0;
			PlInfo[ENTINDEX(pPlayer)].fRenderAmt = 0;

			return 1;
			break;
		case CAMERA_3RDPERSON:
			if(PlInfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_3RDPERSON;
				return 1;
			}

			PlInfo[ENTINDEX(pPlayer)].iRenderMode = pPlayer->v.rendermode;
			PlInfo[ENTINDEX(pPlayer)].fRenderAmt = pPlayer->v.renderamt;

			PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_3RDPERSON;
			pNewCamera = CREATE_NAMED_ENTITY(MAKE_STRING("info_target"));
			pNewCamera->v.classname = MAKE_STRING("VexdCam");

			SET_MODEL(pNewCamera, "models/rpgrocket.mdl");
			SET_SIZE(pNewCamera, Vector(0, 0, 0), Vector(0, 0, 0));

			pNewCamera->v.movetype = MOVETYPE_FLY;
			pNewCamera->v.solid = SOLID_BBOX;
			pNewCamera->v.takedamage = DAMAGE_NO;
			pNewCamera->v.gravity = 0;
			pNewCamera->v.owner = pPlayer;
			pNewCamera->v.rendermode = kRenderTransColor;
			pNewCamera->v.renderamt = 0;
			pNewCamera->v.renderfx = kRenderFxNone;

			SET_VIEW(pPlayer, pNewCamera);

			PlInfo[ENTINDEX(pPlayer)].pViewEnt = pNewCamera;
			break;
		case CAMERA_UPLEFT:
			if(PlInfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_UPLEFT;
				return 1;
			}

			PlInfo[ENTINDEX(pPlayer)].iRenderMode = pPlayer->v.rendermode;
			PlInfo[ENTINDEX(pPlayer)].fRenderAmt = pPlayer->v.renderamt;

			PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_3RDPERSON;
			pNewCamera = CREATE_NAMED_ENTITY(MAKE_STRING("info_target"));
			pNewCamera->v.classname = MAKE_STRING("VexdCam");

			SET_MODEL(pNewCamera, "models/rpgrocket.mdl");
			SET_SIZE(pNewCamera, Vector(0, 0, 0), Vector(0, 0, 0));

			pNewCamera->v.movetype = MOVETYPE_FLY;
			pNewCamera->v.solid = SOLID_BBOX;
			pNewCamera->v.takedamage = DAMAGE_NO;
			pNewCamera->v.gravity = 0;
			pNewCamera->v.owner = pPlayer;
			pNewCamera->v.rendermode = kRenderTransColor;
			pNewCamera->v.renderamt = 0;
			pNewCamera->v.renderfx = kRenderFxNone;

			SET_VIEW(pPlayer, pNewCamera);

			PlInfo[ENTINDEX(pPlayer)].pViewEnt = pNewCamera;
			break;
		case CAMERA_TOPDOWN:
			if(PlInfo[ENTINDEX(pPlayer)].iViewType != CAMERA_NONE) {
				PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_TOPDOWN;
				return 1;
			}

			PlInfo[ENTINDEX(pPlayer)].iRenderMode = pPlayer->v.rendermode;
			PlInfo[ENTINDEX(pPlayer)].fRenderAmt = pPlayer->v.renderamt;

			PlInfo[ENTINDEX(pPlayer)].iViewType = CAMERA_TOPDOWN;
			pNewCamera = CREATE_NAMED_ENTITY(MAKE_STRING("info_target"));
			pNewCamera->v.classname = MAKE_STRING("VexdCam");

			SET_MODEL(pNewCamera, "models/rpgrocket.mdl");
			SET_SIZE(pNewCamera, Vector(0, 0, 0), Vector(0, 0, 0));

			pNewCamera->v.movetype = MOVETYPE_FLY;
			pNewCamera->v.solid = SOLID_BBOX;
			pNewCamera->v.takedamage = DAMAGE_NO;
			pNewCamera->v.gravity = 0;
			pNewCamera->v.owner = pPlayer;
			pNewCamera->v.rendermode = kRenderTransColor;
			pNewCamera->v.renderamt = 0;
			pNewCamera->v.renderfx = kRenderFxNone;

			SET_VIEW(pPlayer, pNewCamera);

			PlInfo[ENTINDEX(pPlayer)].pViewEnt = pNewCamera;
			break;
		default:
			break;
	}

	return 1;
}

// Attachview, this allows you to attach a player's view to an entity.
// use AttachView(player, player) to reset view.
//(vexd)
static cell AMX_NATIVE_CALL attach_view(AMX *amx, cell *params) { 
	int iIndex = params[1];
	int iTargetIndex = params[2];

	if (iIndex < 1 || iIndex > gpGlobals->maxClients) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	if(iTargetIndex < 1 || iTargetIndex > gpGlobals->maxEntities) {
		AMX_RAISEERROR(amx,AMX_ERR_NATIVE);
		return 0;
	}

	SET_VIEW(INDEXENT(iIndex), INDEXENT(iTargetIndex));

	return 1;
}

static cell AMX_NATIVE_CALL precache_generic(AMX *amx, cell *params)
{
	int len;
	char* szPreCache = GET_AMXSTRING(amx,params[1],0,len);
	PRECACHE_GENERIC((char*)STRING(ALLOC_STRING(szPreCache)));
	return 1;
}

/********************************************
   METAMOD HOOKED FUNCTIONS
   *****************************************/

//Added by BAILOPAN.  ClientKill() forward.
void ClientKill(edict_t *pEntity)
{
	cell iRetVal = 0;
	META_RES result = MRES_IGNORED;
	
	for (AmxCallList::AmxCall* i = clientKill.head; i; i = i->next) {
		AMX_EXEC(i->amx, &iRetVal, i->iFunctionIdx, 1, ENTINDEX(pEntity));
		if (iRetVal & 2) {
			RETURN_META(MRES_SUPERCEDE);
		} else if (iRetVal & 1) {
			result = MRES_SUPERCEDE;
		}
	}

	RETURN_META(result);
}

// This allows us to make the player transparent when in third person (but visable to others).
//(vexd)
int AddToFullPack(struct entity_state_s *state, int e, edict_t *ent, edict_t *host, int hostflags, int player, unsigned char *pSet) {
	if(ent == host) {
		if(FStrEq(STRING(ent->v.classname), "player")) {
			if(PlInfo[ENTINDEX(ent)].iViewType != CAMERA_NONE) {
				ent->v.rendermode = kRenderTransTexture;
				ent->v.renderamt = 100;
				RETURN_META_VALUE(MRES_IGNORED, 0);
			}
		}
	}

	if(FStrEq(STRING(ent->v.classname), "player")) {
		if(PlInfo[ENTINDEX(ent)].iViewType != CAMERA_NONE) {
			ent->v.rendermode = PlInfo[ENTINDEX(ent)].iRenderMode;
			ent->v.renderamt = PlInfo[ENTINDEX(ent)].fRenderAmt;
		}
	}

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

// Check if they are using a model, if so, don't let CS know.
// HACKHACK: this might mess up some stuff with other infobuffers,
// ie you may not see when a modeled player changes thier name, etc.
//(vexd)
void ClientUserInfoChanged(edict_t *pEntity, char *infobuffer) {
	if(PlInfo[ENTINDEX(pEntity)].bModeled && pEntity->v.deadflag == DEAD_NO) {
		RETURN_META(MRES_SUPERCEDE);
	} else {
		RETURN_META(MRES_IGNORED);
	}
}

// This code is to set the model at a specified time. the second part of the code updates the
// SetView camera. (vexd)
void PlayerPostThink(edict_t *pEntity) {
	if((PlInfo[ENTINDEX(pEntity)].bModeled) && (PlInfo[ENTINDEX(pEntity)].fModelSet != 0) && (PlInfo[ENTINDEX(pEntity)].fModelSet < gpGlobals->time)) {
		(g_engfuncs.pfnSetClientKeyValue)(ENTINDEX(pEntity), (g_engfuncs.pfnGetInfoKeyBuffer)(pEntity), "model", PlInfo[ENTINDEX(pEntity)].szModel);
	}

	if(PlInfo[ENTINDEX(pEntity)].pViewEnt) {
		edict_t *pCamEnt = PlInfo[ENTINDEX(pEntity)].pViewEnt;

		MAKE_VECTORS(pEntity->v.v_angle + pEntity->v.punchangle);
		Vector vecSrc	 = pEntity->v.origin + pEntity->v.view_ofs;
		Vector vecAiming = gpGlobals->v_forward;
		TraceResult tr;

		switch(PlInfo[ENTINDEX(pEntity)].iViewType) {
			case CAMERA_3RDPERSON:
				TRACE_LINE(vecSrc, vecSrc - (vecAiming * 128), ignore_monsters, ENT(pEntity), &tr);
				SET_VIEW(pEntity, pCamEnt);
				pCamEnt->v.origin = tr.vecEndPos;
				pCamEnt->v.angles = pEntity->v.v_angle;
				break;
			case CAMERA_UPLEFT:
				TRACE_LINE(vecSrc, vecSrc - ((vecAiming * 32) - ((gpGlobals->v_right * 15) + (gpGlobals->v_up * 15))), ignore_monsters, ENT(pEntity), &tr);
				SET_VIEW(pEntity, pCamEnt);
				pCamEnt->v.origin = tr.vecEndPos;
				pCamEnt->v.angles = pEntity->v.v_angle;
				break;
			case CAMERA_TOPDOWN:
				TRACE_LINE(vecSrc, vecSrc + Vector(0,0,2048), dont_ignore_monsters, ENT(pEntity), &tr);
				SET_VIEW(pEntity, pCamEnt);
				pCamEnt->v.origin = tr.vecEndPos;
				pCamEnt->v.origin.z -= 40;
				pCamEnt->v.angles = Vector(90,pEntity->v.v_angle.y,0);
				break;
			default:
				SET_VIEW(pEntity, pEntity);
				REMOVE_ENTITY(PlInfo[ENTINDEX(pEntity)].pViewEnt);
				PlInfo[ENTINDEX(pEntity)].iViewType = CAMERA_NONE;
				PlInfo[ENTINDEX(pEntity)].pViewEnt = NULL;
				break;
		}
	}
	for (AmxCallList::AmxCall* i = postThink.head; i; i = i->next) {
		AMX_EXEC(i->amx, NULL, i->iFunctionIdx, 1, ENTINDEX(pEntity));
	}
	
	RETURN_META(MRES_IGNORED);
}

// This is called once every server frame. This code resets the lights once every second.
// this is so joining players will see ther correct lighting.
// Also forward, may cause lag, but it is good for checking things.
//(vexd)
void StartFrame() {
	if(!FStrEq((const char *)GlInfo.szLastLights, "")) {
		if(GlInfo.fNextLights < gpGlobals->time) {
			(g_engfuncs.pfnLightStyle)(0, (char *)GlInfo.szLastLights);
			GlInfo.fNextLights = gpGlobals->time + 1;
		}
	}

	for (AmxCallList::AmxCall* i = serverFrame.head; i; i = i->next) {
		AMX_EXEC(i->amx, NULL, i->iFunctionIdx, 0);
	}

	RETURN_META(MRES_IGNORED);
}

//(BAILOPAN) - forward this

void PlayerPreThink(edict_t *pEntity) {
	for (AmxCallList::AmxCall* i = preThink.head; i; i = i->next) {
		AMX_EXEC(i->amx, NULL, i->iFunctionIdx, 1, ENTINDEX(pEntity));
	}
	RETURN_META(MRES_IGNORED);
}

// ClientDisconnect. Reinitialize the PlayerInfo struct for that player.
//(vexd)
void ClientDisconnect(edict_t *pEntity) {
	memset(PlInfo[ENTINDEX(pEntity)].szModel, 0x0, sizeof(PlInfo[ENTINDEX(pEntity)].szModel));
	PlInfo[ENTINDEX(pEntity)].bModeled = false;
	PlInfo[ENTINDEX(pEntity)].fModelSet = 0;
	PlInfo[ENTINDEX(pEntity)].iSpeakFlags = SPEAK_NORMAL;
	PlInfo[ENTINDEX(pEntity)].iViewType = CAMERA_NONE;
	PlInfo[ENTINDEX(pEntity)].iRenderMode = 0;
	PlInfo[ENTINDEX(pEntity)].fRenderAmt = 0;

	RETURN_META(MRES_IGNORED);
}


// pfnTouch, this is a forward that is called whenever 2 entities collide.
void Touch(edict_t *pToucher, edict_t *pTouched) {
	cell iResult;
	META_RES result = MRES_IGNORED;
	for (AmxCallList::AmxCall* i = pfnTouch.head; i; i = i->next) {
		AMX_EXEC(i->amx, &iResult, i->iFunctionIdx, 2, pToucher, pTouched);
		if (iResult & 2) {
			RETURN_META(MRES_SUPERCEDE);
		} else if (iResult & 1) {
			result = MRES_SUPERCEDE;
		}
	}

	RETURN_META(result);
}

// ClientConnect, reinitialize player info here as well.
// Also gives small message that its using the model.
//(vexd)
BOOL ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char szRejectReason[128]) {
	memset(PlInfo[ENTINDEX(pEntity)].szModel, 0x0, sizeof(PlInfo[ENTINDEX(pEntity)].szModel));
	PlInfo[ENTINDEX(pEntity)].bModeled = false;
	PlInfo[ENTINDEX(pEntity)].fModelSet = 0;
	PlInfo[ENTINDEX(pEntity)].iSpeakFlags = SPEAK_NORMAL;
	PlInfo[ENTINDEX(pEntity)].iViewType = CAMERA_NONE;
	PlInfo[ENTINDEX(pEntity)].pViewEnt = NULL;
	PlInfo[ENTINDEX(pEntity)].iRenderMode = 0;
	PlInfo[ENTINDEX(pEntity)].fRenderAmt = 0;

	//CLIENT_PRINTF(pEntity, print_console, "*** This server is using Vexd's Utility Module.\n");

	RETURN_META_VALUE(MRES_IGNORED, 0);
}

//(vexd)
void GameInit(void) {
	CVAR_REGISTER(&amxxe_version);
}

// make sure that if we currently have an edited light value, to use it.
//(vexd)
void LightStyle(int style, char *val) {
	if(style == 0) {
		memset(GlInfo.szRealLights, 0x0, 128);
		memcpy(GlInfo.szRealLights, val, strlen(val));
	}

	RETURN_META(MRES_IGNORED);
}

// Engine message functions. (vexd)
void MessageBegin(int msg_dest, int msg_type, const float *pOrigin, edict_t *ed) {

	// Reset player model a couple milliseconds after this if they are using an edited model.
	if(msg_type == REG_USER_MSG("ResetHUD", 1)) {
		if(PlInfo[ENTINDEX(ed)].bModeled) {
			PlInfo[ENTINDEX(ed)].fModelSet = gpGlobals->time + AMS_OFFSET;
		}
	}

	// If the message is a blocked one, block it.
	if(GlInfo.iMessageBlock[msg_type]) {
		GlInfo.bBlocking = true;

		if(GlInfo.iMessageBlock[msg_type] == BLOCK_ONCE) {
			GlInfo.iMessageBlock[msg_type] = BLOCK_NOT;
		}

		RETURN_META(MRES_SUPERCEDE);
	} else {
		if (Msg[msg_type].isHooked && !Msg[msg_type].isCalled) {
			LastMessage = msg_type;
			Msg[msg_type].msg = new MessageInfo(msg_dest, msg_type, pOrigin, ed);
			Msg[msg_type].isCalled = true;
			RETURN_META(MRES_SUPERCEDE);
		}
	}

	RETURN_META(MRES_IGNORED);
}

void MessageEnd(void) {
	cell iResult;
	META_RES result = MRES_IGNORED;

	if(GlInfo.bBlocking) {
		GlInfo.bBlocking = false;
		RETURN_META(MRES_SUPERCEDE);
	}

	int msg_type = LastMessage;

	if (Msg[msg_type].isHooked && Msg[msg_type].isCalled) {
		for (AmxCallList::AmxCall* i = Msg[msg_type].msgCalls.head; i; i = i->next) {
			AMX_EXEC(i->amx, &iResult, i->iFunctionIdx, 1, msg_type);
			if (iResult & 2) {
				RETURN_META(MRES_SUPERCEDE);
			} else if (iResult & 1) {
				result = MRES_SUPERCEDE;
			}
		}
		Msg[msg_type].isCalled = false;
		if (result != MRES_SUPERCEDE) {	//supercede the message ANYWAY
			Msg[msg_type].msg->SendMsg();
			result = MRES_SUPERCEDE;
		}
		//destroy(Msg[msg_type].msg);
	}

	RETURN_META(result);
}

void WriteByte(int iValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_byte, iValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteChar(int iValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_char, iValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteShort(int iValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_short, iValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteLong(int iValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_long, iValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteAngle(float flValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_angle, flValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteCoord(float flValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_coord, flValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteString(const char *sz) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_string, (char*)sz);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void WriteEntity(int iValue) {
	if(GlInfo.bBlocking) {
		RETURN_META(MRES_SUPERCEDE);
	}
	int msg_type = LastMessage;
	if (msg_type && Msg[msg_type].isCalled && Msg[msg_type].isHooked) {
		Msg[msg_type].msg->AddArg(arg_entity, iValue);
		RETURN_META(MRES_SUPERCEDE);
	}
	RETURN_META(MRES_IGNORED);
}

void ServerActivate( edict_t *pEdictList, int edictCount, int clientMax ){
 	AMX* amx;
	void* code;
	const char* filename;
	int iFunctionIndex;
	int i=0;
	while ((amx = GET_AMXSCRIPT(i++, &code, &filename)) != 0) {
		if (AMX_FINDPUBLIC(amx, "vexd_pfntouch", &iFunctionIndex) == AMX_ERR_NONE) {
			pfnTouch.put(amx, iFunctionIndex);
		}
		if (AMX_FINDPUBLIC(amx, "pfn_touch", &iFunctionIndex) == AMX_ERR_NONE) {
			pfnTouch.put(amx, iFunctionIndex);
		}
		if (AMX_FINDPUBLIC(amx, "server_frame", &iFunctionIndex) == AMX_ERR_NONE) {
			serverFrame.put(amx, iFunctionIndex);
		}
		if (AMX_FINDPUBLIC(amx, "ServerFrame", &iFunctionIndex) == AMX_ERR_NONE) {
			serverFrame.put(amx, iFunctionIndex);
		}
		if (AMX_FINDPUBLIC(amx, "client_PreThink", &iFunctionIndex) == AMX_ERR_NONE) {
			preThink.put(amx, iFunctionIndex);
		}
		if (AMX_FINDPUBLIC(amx, "client_PostThink", &iFunctionIndex) == AMX_ERR_NONE) {
			postThink.put(amx, iFunctionIndex);
		}
		if (AMX_FINDPUBLIC(amx, "client_kill", &iFunctionIndex) == AMX_ERR_NONE) {
			clientKill.put(amx, iFunctionIndex);
		}
	}

	for (i=0; i<MAX_MESSAGES; i++) {
		Msg[i].isHooked = false;
		Msg[i].isCalled = false;
		Msg[i].msg = NULL;
		Msg[i].type = 0;
	}

	RETURN_META(MRES_IGNORED);
}

void ServerDeactivate() {
	memset(GlInfo.szLastLights, 0x0, 128);
	memset(GlInfo.szRealLights, 0x0, 128);
	GlInfo.bLights = false;
	GlInfo.fNextLights = 0;

	pfnTouch.clear();
	serverFrame.clear();
	postThink.clear();
	preThink.clear();
	clientKill.clear();

	int i;
	// Reset message blocks.
	for(i = 0; i < MAX_MESSAGES; i++) {
		GlInfo.iMessageBlock[i] = BLOCK_NOT;
		if (Msg[i].msg != NULL) {
			destroy(Msg[i].msg);
			Msg[i].msgCalls.clear();
		}
	}

	RETURN_META(MRES_IGNORED);

}

C_DLLEXPORT int Meta_Query(char *ifvers, plugin_info_t **pPlugInfo, mutil_funcs_t *pMetaUtilFuncs) {

	gpMetaUtilFuncs=pMetaUtilFuncs;
	*pPlugInfo=&Plugin_info;

	if(strcmp(ifvers, Plugin_info.ifvers)) {

		int mmajor=0, mminor=0, pmajor=0, pminor=0;
		LOG_MESSAGE(PLID, "WARNING: meta-interface version mismatch; requested=%s ours=%s", Plugin_info.logtag, ifvers);
		sscanf(ifvers, "%d:%d", &mmajor, &mminor);
		sscanf(META_INTERFACE_VERSION, "%d:%d", &pmajor, &pminor);

		if(pmajor > mmajor || (pmajor==mmajor && pminor > mminor)) {
			LOG_ERROR(PLID, "metamod version is too old for this plugin; update metamod");
			return(FALSE);

		}

		else if(pmajor < mmajor) {
			LOG_ERROR(PLID, "metamod version is incompatible with this plugin; please find a newer version of this plugin");
			return(FALSE);

		}

		else if(pmajor==mmajor && pminor < mminor)
			LOG_MESSAGE(PLID, "WARNING: metamod version is newer than expected; consider finding a newer version of this plugin");
		else
			LOG_ERROR(PLID, "unexpected version comparison; metavers=%s, mmajor=%d, mminor=%d; plugvers=%s, pmajor=%d, pminor=%d", ifvers, mmajor, mminor, META_INTERFACE_VERSION, pmajor, pminor);

	}

	return(TRUE);

}

static META_FUNCTIONS gMetaFunctionTable;

C_DLLEXPORT int Meta_Attach(PLUG_LOADTIME now, META_FUNCTIONS *pFunctionTable, meta_globals_t *pMGlobals, gamedll_funcs_t *pGamedllFuncs) {

	if(now > Plugin_info.loadable) {

		LOG_ERROR(PLID, "Can't load plugin right now");

		return(FALSE);

	}

	gpMetaGlobals=pMGlobals;
	gMetaFunctionTable.pfnGetEntityAPI2 = GetEntityAPI2;
	gMetaFunctionTable.pfnGetEngineFunctions = GetEngineFunctions;

	memcpy(pFunctionTable, &gMetaFunctionTable, sizeof(META_FUNCTIONS));

	gpGamedllFuncs=pGamedllFuncs;

	return(TRUE);

}



C_DLLEXPORT int Meta_Detach(PLUG_LOADTIME now, PL_UNLOAD_REASON reason) {

	if(now > Plugin_info.unloadable && reason != PNL_CMD_FORCED) {
		LOG_ERROR(PLID, "Can't unload plugin right now");
		return(FALSE);

	}

	return(TRUE);

}



void WINAPI GiveFnptrsToDll( enginefuncs_t* pengfuncsFromEngine, globalvars_t *pGlobals ) {

	memcpy(&g_engfuncs, pengfuncsFromEngine, sizeof(enginefuncs_t));
	gpGlobals = pGlobals;

}



DLL_FUNCTIONS gFunctionTable;

C_DLLEXPORT int GetEntityAPI2( DLL_FUNCTIONS *pFunctionTable, int *interfaceVersion ){

	gFunctionTable.pfnGameInit = GameInit;
	gFunctionTable.pfnStartFrame = StartFrame;
	gFunctionTable.pfnTouch = Touch;
	gFunctionTable.pfnServerDeactivate = ServerDeactivate;
	gFunctionTable.pfnClientDisconnect = ClientDisconnect;
	gFunctionTable.pfnServerActivate = ServerActivate;
	gFunctionTable.pfnClientConnect = ClientConnect;
	gFunctionTable.pfnClientDisconnect = ClientDisconnect;
	gFunctionTable.pfnPlayerPostThink = PlayerPostThink;
	gFunctionTable.pfnPlayerPreThink = PlayerPreThink;
	gFunctionTable.pfnClientUserInfoChanged = ClientUserInfoChanged;
	gFunctionTable.pfnAddToFullPack = AddToFullPack;
	gFunctionTable.pfnClientKill = ClientKill;

	if(*interfaceVersion!=INTERFACE_VERSION) {
		LOG_ERROR(PLID, "GetEntityAPI2 version mismatch; requested=%d ours=%d", *interfaceVersion, INTERFACE_VERSION);
		*interfaceVersion = INTERFACE_VERSION;
		return(FALSE);

	}
	memcpy( pFunctionTable, &gFunctionTable, sizeof( DLL_FUNCTIONS ) );

	return(TRUE);

}

enginefuncs_t meta_engfuncs;

C_DLLEXPORT int GetEngineFunctions(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion ) {

	meta_engfuncs.pfnMessageBegin = MessageBegin;
	meta_engfuncs.pfnMessageEnd = MessageEnd;
	meta_engfuncs.pfnWriteByte = WriteByte;
	meta_engfuncs.pfnWriteChar = WriteChar;
	meta_engfuncs.pfnWriteShort = WriteShort;
	meta_engfuncs.pfnWriteLong = WriteLong;
	meta_engfuncs.pfnWriteAngle = WriteAngle;
	meta_engfuncs.pfnWriteCoord = WriteCoord;
	meta_engfuncs.pfnWriteString = WriteString;
	meta_engfuncs.pfnWriteEntity = WriteEntity;
	meta_engfuncs.pfnLightStyle = LightStyle;

	if(*interfaceVersion!=ENGINE_INTERFACE_VERSION) {
		LOG_ERROR(PLID, "GetEngineFunctions version mismatch; requested=%d ours=%d", *interfaceVersion, ENGINE_INTERFACE_VERSION);
		*interfaceVersion = ENGINE_INTERFACE_VERSION;
		return(FALSE);

	}
	memcpy(pengfuncsFromEngine, &meta_engfuncs, sizeof(enginefuncs_t));

	return(TRUE);

}



enginefuncs_t meta_engfuncs_post;

C_DLLEXPORT int GetEngineFunctions_Post(enginefuncs_t *pengfuncsFromEngine, int *interfaceVersion ) {


	if(*interfaceVersion!=ENGINE_INTERFACE_VERSION) {
		LOG_ERROR(PLID, "GetEngineFunctions_Post version mismatch; requested=%d ours=%d", *interfaceVersion, ENGINE_INTERFACE_VERSION);
		*interfaceVersion = ENGINE_INTERFACE_VERSION;
		return(FALSE);

	}
	memcpy(pengfuncsFromEngine, &meta_engfuncs_post, sizeof(enginefuncs_t));

	return(TRUE);

}

C_DLLEXPORT int AMX_Query(module_info_s** info) {

	*info = &module_info;

	return 1;

}



C_DLLEXPORT int AMX_Attach(pfnamx_engine_g* amxeng,pfnmodule_engine_g* meng) {

	g_engAmxFunc = amxeng;
	g_engModuleFunc = meng;

	if (!gpMetaGlobals)
		REPORT_ERROR( 1 , "[CS STATS] Module is not attached to MetaMod\n");

	ADD_AMXNATIVES( &module_info , Engine_Natives);

	return 1;

}



C_DLLEXPORT int AMX_Detach() {

	return 1;

}

AMX_NATIVE_INFO Engine_Natives[] = {
	{"set_offset_float",	set_offset_float},
	{"set_offset_short",	set_offset_short},
	{"set_offset",			set_offset},
	{"get_offset_float",	get_offset_float},
	{"get_offset_short",	get_offset_short},
	{"get_offset",			get_offset},

	{"entity_get_float",	entity_get_float},
	{"entity_set_float",	entity_set_float},
	{"entity_set_int",		entity_set_int},
	{"entity_get_int",		entity_get_int},
	{"entity_get_vector",	entity_get_vector},
	{"entity_get_string",	entity_get_string},
	{"entity_get_edict",	entity_get_edict},
	{"entity_get_byte",		entity_get_byte},
	{"entity_set_vector",	entity_set_vector},
	{"entity_set_string",	entity_set_string},
	{"entity_set_edict",	entity_set_edict},
	{"entity_set_byte",		entity_set_byte},
	{"entity_set_origin",	entity_set_origin},
	{"entity_set_model",	entity_set_model},

	{"PointContents",		PointContents},
	{"RadiusDamage",		RadiusDamage},
	{"VelocityByAim",		VelocityByAim},
	{"vector_length",		vector_length},
	{"vector_distance",		vector_distance},
	{"vector_to_angle",		vector_to_angle},
	{"trace_line",			trace_line},
	{"trace_normal",		trace_normal},
	{"halflife_time",		halflife_time},
	{"fake_touch",			fake_touch},
	{"get_grenade_id",		get_grenade_id},

	{"create_entity",		create_entity},
	{"remove_entity",		remove_entity},
	{"find_entity",			find_entity},
	{"find_ent_by_owner",	find_ent_by_owner},
	{"find_ent_by_target",	find_ent_by_target},
	{"find_ent_by_tname",	find_ent_by_tname},
	{"find_ent_by_model",	find_ent_by_model},
	{"entity_count",		entity_count},
	{"DispatchKeyValue",	DispatchKeyValue},
	{"DispatchSpawn",		DispatchSpawn},

	{"set_msg_block",		set_msg_block},
	{"get_msg_block",		get_msg_block},
	{"set_lights",			set_lights},
	{"set_view",			set_view},
	{"attach_view",			attach_view},

	{"precache_generic",	precache_generic},
	{"register_message",	register_message},
	{"set_msg_arg_float",	set_msg_arg_float},
	{"set_msg_arg_int",		set_msg_arg_int},
	{"set_msg_arg_string",	set_msg_arg_string},
	{"get_msg_arg_float",	get_msg_arg_float},
	{"get_msg_arg_int",		get_msg_arg_int},
	{"get_msg_arg_string",	get_msg_arg_string},
	{"get_msg_args",		get_msg_args},
	{"get_msg_argtype",		get_msg_argtype},

	{"get_system_os",		get_system_os},
	{"system_cmd",			system_cmd},

	{ NULL, NULL }

};