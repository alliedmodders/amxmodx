// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

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
	edict_t *pEnt = NULL;

	if (iEnt < 0 || iEnt > gpGlobals->maxEntities)
	{
		LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", iEnt);
		return 0;
	}
	else
	{
		if (iEnt > 0 && iEnt <= gpGlobals->maxClients)
		{
			if (!GET_PLAYER_POINTER_I(iEnt)->ingame)
			{
				LogError(amx, AMX_ERR_NATIVE, "Invalid player %d (not ingame)", iEnt);
				return 0;
			}
			pEnt = GET_PLAYER_POINTER_I(iEnt)->pEdict;
		} else {
			pEnt = TypeConversion.id_to_edict(iEnt);
		}
	}

	if (!pEnt)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid entity %d (nullent)", iEnt);
		return 0;
	}

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
		break;
	case ANGLEVECTORS_RIGHT:
		v_return = v_right;
		break;
	case ANGLEVECTORS_UP:
		v_return = v_up;
		break;
	}

	vCell = get_amxaddr(amx, params[3]);
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
