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
#define ANGLEVECTORS_RIGHT	2
#define ANGLEVECTORS_UP		3

/**
 * Returns vector's length if pVecB is null.
 * Returns the distance between vectors otherwise.
 */
REAL ComputeVectorLength(Vector & VecA, Vector * pVecB = NULL /* Optional */, VecLenType Type = VecLen3D)
{
	REAL Length = 0.0f;

	switch (Type)
	{
	case VecLen3D:
		Length = pVecB ? (REAL)(VecA - *pVecB).Length() : (REAL)VecA.Length();
		break;

	case VecLen2D:
		Length = pVecB ? (REAL)(VecA - *pVecB).Length2D() : (REAL)VecA.Length2D();
		break;
	}

	return Length;
}

static cell AMX_NATIVE_CALL get_distance(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA((REAL)pVecA[0], (REAL)pVecA[1], (REAL)pVecA[2]);
	Vector VecB((REAL)pVecB[0], (REAL)pVecB[1], (REAL)pVecB[2]);

	return (cell)ComputeVectorLength(VecA, & VecB, (VecLenType)params[3]);
}

static cell AMX_NATIVE_CALL get_distance_f(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA((REAL)amx_ctof(pVecA[0]), (REAL)amx_ctof(pVecA[1]), (REAL)amx_ctof(pVecA[2]));
	Vector VecB((REAL)amx_ctof(pVecB[0]), (REAL)amx_ctof(pVecB[1]), (REAL)amx_ctof(pVecB[2]));

	REAL Length = ComputeVectorLength(VecA, & VecB, (VecLenType)params[3]);

	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL VelocityByAim(AMX *amx, cell *params)
{
	int iEntity = params[1], iVelocity = params[2];
	edict_t * pEntity = NULL;

	if (iEntity < 0 || iEntity > gpGlobals->maxEntities)
	{
		LogError(amx, AMX_ERR_NATIVE, "Entity out of range (%d)", iEntity);
		return 0;
	}
	else
	{
		if (iEntity > 0 && iEntity <= gpGlobals->maxClients)
		{
			if (!GET_PLAYER_POINTER_I(iEntity)->ingame)
			{
				LogError(amx, AMX_ERR_NATIVE, "Player not in-game (%d)", iEntity);
				return 0;
			}

			pEntity = GET_PLAYER_POINTER_I(iEntity)->pEdict;
		}
		else
			pEntity = INDEXENT(iEntity);
	}

	if (!pEntity)
	{
		LogError(amx, AMX_ERR_NATIVE, "Null entity (%d)", iEntity);
		return 0;
	}

	MAKE_VECTORS(pEntity->v.v_angle);
	Vector Result = gpGlobals->v_forward * iVelocity;

	cell * pSet = get_amxaddr(amx, params[3]);
	pSet[0] = FloatToCell(Result.x);
	pSet[1] = FloatToCell(Result.y);
	pSet[2] = FloatToCell(Result.z);

	return 1;
}

static cell AMX_NATIVE_CALL vector_to_angle(AMX *amx, cell *params)
{
	cell * pAddress = get_amxaddr(amx, params[1]);
	Vector Source(amx_ctof(pAddress[0]), amx_ctof(pAddress[1]), amx_ctof(pAddress[2]));

	Vector Angles;
	VEC_TO_ANGLES(Source, Angles);

	cell * pSet = get_amxaddr(amx, params[2]);
	pSet[0] = FloatToCell(Angles.x);
	pSet[1] = FloatToCell(Angles.y);
	pSet[2] = FloatToCell(Angles.z);

	return 1;
}

static cell AMX_NATIVE_CALL angle_vector(AMX *amx, cell *params)
{
	cell * pAngles = get_amxaddr(amx, params[1]);
	Vector Angles(amx_ctof(pAngles[0]), amx_ctof(pAngles[1]), amx_ctof(pAngles[2]));

	Vector Result, Forward, Right, Up;
	g_engfuncs.pfnAngleVectors(Angles, Forward, Right, Up);

	switch (params[2])
	{
	case ANGLEVECTORS_FORWARD:
		Result = Forward;
		break;

	case ANGLEVECTORS_RIGHT:
		Result = Right;
		break;

	case ANGLEVECTORS_UP:
		Result = Up;
		break;
	}

	pAngles = get_amxaddr(amx, params[3]);
	pAngles[0] = FloatToCell(Result.x);
	pAngles[1] = FloatToCell(Result.y);
	pAngles[2] = FloatToCell(Result.z);

	return 1;
}

static cell AMX_NATIVE_CALL vector_length(AMX *amx, cell *params)
{
	cell * pSource = get_amxaddr(amx, params[1]);
	Vector Source(amx_ctof(pSource[0]), amx_ctof(pSource[1]), amx_ctof(pSource[2]));

	REAL Length = ComputeVectorLength(Source, NULL, (VecLenType)params[2]);

	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL vector_distance(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	Vector VecA(amx_ctof(pVecA[0]), amx_ctof(pVecA[1]), amx_ctof(pVecA[2]));
	Vector VecB(amx_ctof(pVecB[0]), amx_ctof(pVecB[1]), amx_ctof(pVecB[2]));

	REAL Length = ComputeVectorLength(VecA, & VecB, (VecLenType)params[3]);

	return amx_ftoc(Length);
}

static cell AMX_NATIVE_CALL GetVectorDotProduct(AMX *amx, cell *params)
{
	cell * pVecA = get_amxaddr(amx, params[1]);
	cell * pVecB = get_amxaddr(amx, params[2]);

	VecLenType Type = (VecLenType)params[3];

	REAL Product = 0.0f;

	switch (Type)
	{
		case VecLen3D:
		{
			Vector VecA((float)amx_ctof(pVecA[0]), (float)amx_ctof(pVecA[1]), (float)amx_ctof(pVecA[2]));
			Vector VecB((float)amx_ctof(pVecB[0]), (float)amx_ctof(pVecB[1]), (float)amx_ctof(pVecB[2]));

			Product = (REAL)DotProduct(VecA, VecB);

			break;
		}

		case VecLen2D:
		{
			Vector2D VecA2D((float)amx_ctof(pVecA[0]), (float)amx_ctof(pVecA[1]));
			Vector2D VecB2D((float)amx_ctof(pVecB[0]), (float)amx_ctof(pVecB[1]));

			Product = (REAL)DotProduct(VecA2D, VecB2D);

			break;
		}
	}

	return amx_ftoc(Product);
}

static cell AMX_NATIVE_CALL GetVectorCrossProduct(AMX *amx, cell *params)
{
	cell *pVecA = get_amxaddr(amx, params[1]);
	cell *pVecB = get_amxaddr(amx, params[2]);
	cell *pVecC = get_amxaddr(amx, params[3]);

	Vector VecA((float)amx_ctof(pVecA[0]), (float)amx_ctof(pVecA[1]), (float)amx_ctof(pVecA[2]));
	Vector VecB((float)amx_ctof(pVecB[0]), (float)amx_ctof(pVecB[1]), (float)amx_ctof(pVecB[2]));

	Vector VecC = CrossProduct(VecA, VecB);

	pVecC[0] = amx_ftoc(VecC.x);
	pVecC[1] = amx_ftoc(VecC.y);
	pVecC[2] = amx_ftoc(VecC.z);

	return 1;
}

static cell AMX_NATIVE_CALL NormalizeVector(AMX *amx, cell *params)
{
	cell * pSource = get_amxaddr(amx, params[1]);
	cell * pSet = get_amxaddr(amx, params[2]);

	Vector Source((float)amx_ctof(pSource[0]), (float)amx_ctof(pSource[1]), (float)amx_ctof(pSource[2]));

	VecLenType Type = (VecLenType)params[3];

	switch (Type)
	{
		case VecLen3D:
		{
			Vector Normalized = Source.Normalize();

			pSet[0] = amx_ftoc(Normalized.x);
			pSet[1] = amx_ftoc(Normalized.y);
			pSet[2] = amx_ftoc(Normalized.z);

			break;
		}

		case VecLen2D:
		{
			Vector2D Source2D = Source.Make2D();
			Vector2D Normalized2D = Source2D.Normalize();

			pSet[0] = amx_ftoc(Normalized2D.x);
			pSet[1] = amx_ftoc(Normalized2D.y);

			break;
		}
	}

	return 1;
}

AMX_NATIVE_INFO vector_Natives[] = {
	{"get_distance",		get_distance},
	{"get_distance_f",		get_distance_f},
	{"velocity_by_aim",		VelocityByAim},
	{"vector_to_angle",		vector_to_angle},
	{"angle_vector",		angle_vector},
	{"vector_length",		vector_length},
	{"vector_distance",		vector_distance},
	{"GetVectorDotProduct",		GetVectorDotProduct},
	{"GetVectorCrossProduct",	GetVectorCrossProduct},
	{"NormalizeVector",		NormalizeVector},
	{NULL,					NULL},
};
