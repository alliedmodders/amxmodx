/* Vector functions (now part of Core)
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */

#if defined _corevector_included
  #endinput
#endif
#define _corevector_included

/* Used for angle_vector() */
#define ANGLEVECTOR_FORWARD      1
#define ANGLEVECTOR_RIGHT        2
#define ANGLEVECTOR_UP           3

/* Returns distance between two vectors. */
native get_distance(const origin1[3], const origin2[3]);

/* Gets distance between two origins (float). */
native Float:get_distance_f(const Float:Origin1[3], const Float:Origin2[3]);

/* Gives you a velocity in the direction a player is looking, iVelocity is the multiplier. */
native velocity_by_aim(iIndex, iVelocity, Float:vRetValue[3]);

/* Changes a vector into an angle vector. */
native vector_to_angle(const Float:fVector[3], Float:vReturn[3]);

/* Changes an angle vector into a vector. */
native angle_vector(const Float:vector[3], FRU, Float:ret[3]);

/* Gets the length of a vector (float[3]). */
native Float:vector_length(const Float:vVector[3]);

/* Gets the distance between 2 vectors (float[3]). */
native Float:vector_distance(const Float:vVector[3], const Float:vVector2[3]);

/* Changes an integer vec to a floating vec
 * This is not a for loop because that's slower
 */
stock IVecFVec(const IVec[3], Float:FVec[3])
{
	FVec[0] = float(IVec[0]);
	FVec[1] = float(IVec[1]);
	FVec[2] = float(IVec[2]);

	return 1;
}

/* Changes a float vec to an integer vec */
stock FVecIVec(const Float:FVec[3], IVec[3])
{
	IVec[0] = floatround(FVec[0]);
	IVec[1] = floatround(FVec[1]);
	IVec[2] = floatround(FVec[2]);
	
	return 1;
}
