/* AMX Mod X
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"

#define ANGLEVECTORS_FORWARD	1
#define ANGLEVECTORS_RIGHT		2
#define ANGLEVECTORS_UP			3

static cell AMX_NATIVE_CALL get_distance(AMX *amx, cell *params)
{
	cell *cpVec1 = get_amxaddr(amx, params[1]);
	cell *cpVec2 = get_amxaddr(amx, params[2]);

	Vector vec1 = Vector((float)cpVec1[0], (float)cpVec1[1], (float)cpVec1[2]);
	Vector vec2 = Vector((float)cpVec2[0], (float)cpVec2[1], (float)cpVec2[2]);

	int iDist = (int)((vec1 - vec2).Length());

	return iDist;
}

static cell AMX_NATIVE_CALL get_distance_f(AMX *amx, cell *params)
{
	cell *cpVec1 = get_amxaddr(amx, params[1]);
	cell *cpVec2 = get_amxaddr(amx, params[2]);

	Vector vec1 = Vector((float)amx_ctof(cpVec1[0]), (float)amx_ctof(cpVec1[1]), (float)amx_ctof(cpVec1[2]));
	Vector vec2 = Vector((float)amx_ctof(cpVec2[0]), (float)amx_ctof(cpVec2[1]), (float)amx_ctof(cpVec2[2]));

	REAL fDist = (REAL) (vec1 - vec2).Length();

	return amx_ftoc(fDist);
}

static cell AMX_NATIVE_CALL VelocityByAim(AMX *amx, cell *params)
{
	int iEnt = params[1];
	int iVelocity = params[2];
	cell *vRet = get_amxaddr(amx, params[3]);
	Vector vVector = Vector(0, 0, 0);

	if (iEnt < 0 || iEnt > gpGlobals->maxEntities)
	{
		LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", iEnt);
		return 0;
	}
	else
	{
		if (iEnt <= gpGlobals->maxClients && !GET_PLAYER_POINTER_I(iEnt)->ingame)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not in-game)", iEnt);
			return 0;
		}
		else if (iEnt != 0 && FNullEnt(INDEXENT(iEnt)))
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d", iEnt);
			return 0;
		}
	}

	edict_t *pEnt;

	if (iEnt >= 1 || iEnt <= gpGlobals->maxClients)
		pEnt = GET_PLAYER_POINTER_I(iEnt)->pEdict;
	else
		pEnt = INDEXENT(iEnt);

	MAKE_VECTORS(pEnt->v.v_angle);
	vVector = gpGlobals->v_forward * iVelocity;

	vRet[0] = FloatToCell(vVector.x);
	vRet[1] = FloatToCell(vVector.y);
	vRet[2] = FloatToCell(vVector.z);

	return 1;
}

static cell AMX_NATIVE_CALL vector_to_angle(AMX *amx, cell *params)
{
	cell *cAddr = get_amxaddr(amx, params[1]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);

	Vector vVector = Vector(fX, fY, fZ);
	Vector vAngle = Vector(0, 0, 0);
	VEC_TO_ANGLES(vVector, vAngle);

	cell *vRet = get_amxaddr(amx, params[2]);

	vRet[0] = FloatToCell(vAngle.x);
	vRet[1] = FloatToCell(vAngle.y);
	vRet[2] = FloatToCell(vAngle.z);

	return 1;
}

static cell AMX_NATIVE_CALL angle_vector(AMX *amx, cell *params)
{
	Vector v_angles, v_forward, v_right, v_up, v_return;

	cell *vCell = get_amxaddr(amx, params[1]);
	v_angles.x = amx_ctof(vCell[0]);
	v_angles.y = amx_ctof(vCell[1]);
	v_angles.z = amx_ctof(vCell[2]);

	g_engfuncs.pfnAngleVectors(v_angles, v_forward, v_right, v_up);

	switch (params[2])
	{
	case ANGLEVECTORS_FORWARD:
		v_return = v_forward;
	case ANGLEVECTORS_RIGHT:
		v_return = v_right;
	case ANGLEVECTORS_UP:
		v_return = v_up;
	}

	vCell = get_amxaddr(amx,params[3]);
	vCell[0] = FloatToCell(v_return.x);
	vCell[1] = FloatToCell(v_return.y);
	vCell[2] = FloatToCell(v_return.z);

	return 1;
}

static cell AMX_NATIVE_CALL vector_length(AMX *amx, cell *params)
{
	cell *cAddr = get_amxaddr(amx, params[1]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);

	Vector vVector = Vector(fX, fY, fZ);

	REAL fLength = vVector.Length();

	return amx_ftoc(fLength);
}

static cell AMX_NATIVE_CALL vector_distance(AMX *amx, cell *params)
{
	cell *cAddr = get_amxaddr(amx, params[1]);
	cell *cAddr2 = get_amxaddr(amx, params[2]);

	REAL fX = amx_ctof(cAddr[0]);
	REAL fY = amx_ctof(cAddr[1]);
	REAL fZ = amx_ctof(cAddr[2]);
	REAL fX2 = amx_ctof(cAddr2[0]);
	REAL fY2 = amx_ctof(cAddr2[1]);
	REAL fZ2 = amx_ctof(cAddr2[2]);

	Vector vVector = Vector(fX, fY, fZ);
	Vector vVector2 = Vector(fX2, fY2, fZ2);

	REAL fLength = (vVector - vVector2).Length();

	return amx_ftoc(fLength);
}

AMX_NATIVE_INFO vector_Natives[] = {
	{"get_distance",		get_distance},
	{"get_distance_f",		get_distance_f},
	{"velocity_by_aim",		VelocityByAim},
	{"vector_to_angle",		vector_to_angle},
	{"angle_vector",		angle_vector},
	{"vector_length",		vector_length},
	{"vector_distance",		vector_distance},
	{NULL,					NULL},
};
