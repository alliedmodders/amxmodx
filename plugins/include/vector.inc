// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Vector Functions
//

#if defined _corevector_included
  #endinput
#endif
#define _corevector_included

/**
 * Used for angle_vector function.
 */
#define ANGLEVECTOR_FORWARD	1
#define ANGLEVECTOR_RIGHT	2
#define ANGLEVECTOR_UP		3

/**
 * Returns the Int. value of the distance between two Int. vectors.
 *
 * @param _In_ OriginA			The first origin.
 * @param _In_ OriginB			The second origin.
 *
 * @return				Int. value of the distance.
 */
native get_distance(const OriginA[3], const OriginB[3]);

/**
 * Returns the Int. value of the 2d distance between two Int. vectors.
 *
 * @param _In_ OriginA			The first origin.
 * @param _In_ OriginB			The second origin.
 *
 * @return				Int. value of the 2d distance.
 */
native get_distance2d(const OriginA[3], const OriginB[3]);

/**
 * Returns the Fl. value of the distance between two Fl. vectors.
 *
 * @param _In_ OriginA			The first origin.
 * @param _In_ OriginB			The second origin.
 *
 * @return				Fl. value of the distance.
 */
native Float:get_distance_f(const Float:OriginA[3], const Float:OriginB[3]);

/**
 * Returns the Fl. value of the 2d distance between two Fl. vectors.
 *
 * @param _In_ OriginA			The first origin.
 * @param _In_ OriginB			The second origin.
 *
 * @return				Fl. value of the 2d distance.
 */
native Float:get_distance2d_f(const Float:OriginA[3], const Float:OriginB[3]);

/**
 * Prepares a velocity in the direction the player is looking, multiplied by Velocity parameter.
 *
 * @param _In_ Id			The player ID.
 * @param _In_ Velocity			The velocity multiplier.
 * @param _Out_ Output			The output vector of the velocity obtained.
 *
 * @return				Zero for fails, positive otherwise.
 */
native velocity_by_aim(const Id, const Velocity, Float:Output[3]);

/**
 * Changes a vector into an angle vector.
 *
 * @param _In_ Vector			The source vector.
 * @param _Out_ Output			The obtained vector.
 *
 * @return				Positive.
 */
native vector_to_angle(const Float:Vector[3], Float:Output[3]);

/**
 * Changes an angle vector into a vector.
 *
 * @param _In_ Vector			The source vector.
 * @param _In_ FRU			Forward, right or up index.
 * @param _Out_ Output			The obtained vector.
 *
 * @return				Positive.
 */
native angle_vector(const Float:Vector[3], FRU, Float:Output[3]);

/**
 * Gets the Fl. value of the length of a vector.
 *
 * @param _In_ Vector			The source vector.
 *
 * @return				The Fl. value of the vector's length.
 */
native Float:vector_length(const Float:Vector[3]);

/**
 * Gets the Fl. value of the 2d length of a vector.
 *
 * @param _In_ Vector			The source vector.
 *
 * @return				The Fl. value of the vector's 2d length.
 */
native Float:vector_length2d(const Float:Vector[3]);

/**
 * Gets the Fl. value of the distance between two vectors.
 *
 * @param _In_ VectorA			The first vector.
 * @param _In_ VectorB			The second vector.
 *
 * @return				The Fl. value of the distance between the vectors.
 */
native Float:vector_distance(const Float:VectorA[3], const Float:VectorB[3]);

/**
 * Gets the Fl. value of the 2d distance between two vectors.
 *
 * @param _In_ VectorA			The first vector.
 * @param _In_ VectorB			The second vector.
 *
 * @return				The Fl. value of the 2d distance between the vectors.
 */
native Float:vector_distance2d(const Float:VectorA[3], const Float:VectorB[3]);

/** Changes an Int. vector to a Fl. vector.
 * This is not a 'for' loop because that's slower.
 *
 * @param _In_ IVec			The source Int. vector.
 * @param _Out_ FVec			The output Fl. vector.
 *
 * @return				Positive.
 */
stock IVecFVec(const IVec[3], Float:FVec[3])
{
	FVec[0] = float(IVec[0]);
	FVec[1] = float(IVec[1]);
	FVec[2] = float(IVec[2]);

	return 1;
}

/** Changes a Fl. vector to an Int. vector.
 *
 * @param _In_ FVec			The source Fl. vector.
 * @param _Out_ IVec			The output Int. vector.
 *
 * @return				Positive.
 */
stock FVecIVec(const Float:FVec[3], IVec[3])
{
	IVec[0] = floatround(FVec[0]);
	IVec[1] = floatround(FVec[1]);
	IVec[2] = floatround(FVec[2]);
	
	return 1;
}
