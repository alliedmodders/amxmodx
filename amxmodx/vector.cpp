// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"

#define ANGLEVECTORS_FORWARD 1
#define ANGLEVECTORS_RIGHT 2
#define ANGLEVECTORS_UP 3

/**
 * Sets vector to address.
 */
#define SET_VECTOR(Addr, Vec) Addr[0] = amx_ftoc(Vec.x), Addr[1] = amx_ftoc(Vec.y), Addr[2] = amx_ftoc(Vec.z);

/**
 * Returns vector's length if pVecB is null.
 * Returns the distance between vectors otherwise.
 */
REAL ComputeVectorLength(Vector & VecA, Vector * pVecB = NULL /* Optional */, bool VecLen3D = true)
{
	if (VecLen3D) return pVecB ? (VecA - *pVecB).Length() : VecA.Length();
	else return pVecB ? (VecA - *pVecB).Length2D() : VecA.Length2D();
}

static cell AMX_NATIVE_CALL get_distance(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(pVecA[0], pVecA[1], pVecA[2]);
	Vector VecB(pVecB[0], pVecB[1], pVecB[2]);

	return (cell)ComputeVectorLength(VecA, & VecB);
}

static cell AMX_NATIVE_CALL get_distance2d(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(pVecA[0], pVecA[1], pVecA[2]);
	Vector VecB(pVecB[0], pVecB[1], pVecB[2]);

	return (cell)ComputeVectorLength(VecA, & VecB, false);
}

static cell AMX_NATIVE_CALL get_distance_f(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(amx_ctof(pVecA[0]), amx_ctof(pVecA[1]), amx_ctof(pVecA[2]));
	Vector VecB(amx_ctof(pVecB[0]), amx_ctof(pVecB[1]), amx_ctof(pVecB[2]));

	REAL Length = ComputeVectorLength(VecA, & VecB);
	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL get_distance2d_f(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(amx_ctof(pVecA[0]), amx_ctof(pVecA[1]), amx_ctof(pVecA[2]));
	Vector VecB(amx_ctof(pVecB[0]), amx_ctof(pVecB[1]), amx_ctof(pVecB[2]));

	REAL Length = ComputeVectorLength(VecA, & VecB, false);
	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL VelocityByAim(AMX *amx, cell *params)
{
	int Entity = params[1], Velocity = params[2];
	edict_t * pEntity = NULL;

	if (Entity < 0 || Entity > gpGlobals->maxEntities)
	{
		LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", Entity);
		return 0;
	}
	else
	{
		if (Entity > 0 && Entity <= gpGlobals->maxClients)
		{
			if (!GET_PLAYER_POINTER_I(Entity)->ingame)
			{
				LogError(amx, AMX_ERR_NATIVE, "Player not in-game (%d)", Entity);
				return 0;
			}
			pEntity = GET_PLAYER_POINTER_I(Entity)->pEdict;
		}
		else
			pEntity = INDEXENT(Entity);
	}
	if (!pEntity)
	{
		LogError(amx, AMX_ERR_NATIVE, "Null entity (%d)", Entity);
		return 0;
	}

	MAKE_VECTORS(pEntity->v.v_angle);
	Vector Set = gpGlobals->v_forward * Velocity;

	cell * pSet = get_amxaddr(amx, params[3]);
	SET_VECTOR(pSet, Set)

	return 1;
}

static cell AMX_NATIVE_CALL vector_to_angle(AMX *amx, cell *params)
{
	cell * pAddress = get_amxaddr(amx, params[1]);
	Vector Source(amx_ctof(pSource[0]), amx_ctof(pSource[1]), amx_ctof(pSource[2]));

	Vector Set;
	VEC_TO_ANGLES(Source, Set);

	pAddress = get_amxaddr(amx, params[2]);
	SET_VECTOR(pAddress, Set)

	return 1;
}

static cell AMX_NATIVE_CALL angle_vector(AMX *amx, cell *params)
{
	cell * pAddress = get_amxaddr(amx, params[1]);
	Vector Source(amx_ctof(pSource[0]), amx_ctof(pSource[1]), amx_ctof(pSource[2]));

	Vector Set, Forward, Right, Up;
	g_engfuncs.pfnAngleVectors(Source, Forward, Right, Up);

	switch (params[2])
	{
		case ANGLEVECTORS_FORWARD: Set = Forward; break;
		case ANGLEVECTORS_RIGHT: Set = Right; break;
		case ANGLEVECTORS_UP: Set = Up; break;
	}

	pAddress = get_amxaddr(amx, params[3]);
	SET_VECTOR(pAddress, Set)

	return 1;
}

static cell AMX_NATIVE_CALL vector_length(AMX *amx, cell *params)
{
	cell * pSource = get_amxaddr(amx, params[1]);
	Vector Source(amx_ctof(pSource[0]), amx_ctof(pSource[1]), amx_ctof(pSource[2]));

	REAL Length = ComputeVectorLength(Source, NULL);
	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL vector_length2d(AMX *amx, cell *params)
{
	cell * pSource = get_amxaddr(amx, params[1]);
	Vector Source(amx_ctof(pSource[0]), amx_ctof(pSource[1]), amx_ctof(pSource[2]));

	REAL Length = ComputeVectorLength(Source, NULL, false);
	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL vector_distance(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(amx_ctof(pVecA[0]), amx_ctof(pVecA[1]), amx_ctof(pVecA[2]));
	Vector VecB(amx_ctof(pVecB[0]), amx_ctof(pVecB[1]), amx_ctof(pVecB[2]));

	REAL Length = ComputeVectorLength(VecA, & VecB);
	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL vector_distance2d(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(amx_ctof(pVecA[0]), amx_ctof(pVecA[1]), amx_ctof(pVecA[2]));
	Vector VecB(amx_ctof(pVecB[0]), amx_ctof(pVecB[1]), amx_ctof(pVecB[2]));

	REAL Length = ComputeVectorLength(VecA, & VecB, false);
	return amx_ftoc(Length);
}

AMX_NATIVE_INFO vector_Natives[] = {
	{"get_distance",	get_distance},
	{"get_distance2d",	get_distance2d},
	{"get_distance_f",	get_distance_f},
	{"get_distance2d_f",	get_distance2d_f},
	{"velocity_by_aim",	VelocityByAim},
	{"vector_to_angle",	vector_to_angle},
	{"angle_vector",	angle_vector},
	{"vector_length",	vector_length},
	{"vector_length2d",	vector_length2d},
	{"vector_distance",	vector_distance},
	{"vector_distance2d",	vector_distance2d},
	{NULL,			NULL},
};
