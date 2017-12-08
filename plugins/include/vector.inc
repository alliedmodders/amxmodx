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
 * Used for angle_vector()
 */
#define ANGLEVECTOR_FORWARD      1
#define ANGLEVECTOR_RIGHT        2
#define ANGLEVECTOR_UP           3

/**
 * Calculates the distance between two input vectors.
 * 
 * @param origin1       The first vector
 * @param origin2       The second vector
 * 	
 * @return              The distance between two input vectors
 */
native get_distance(const origin1[3], const origin2[3]);

/**
 * Calculates the distance between two input float vectors.
 * 
 * @param origin1       The first vector
 * @param origin2       The second vector
 * 	
 * @return              The distance between two input vectors
 */
native Float:get_distance_f(const Float:Origin1[3], const Float:Origin2[3]);

/**
 * Calculates velocity in the direction player is looking.
 * 
 * @param iIndex        Client index
 * @param iVelocity     Multiply vRetValue length by this much
 * @param vRetValue     Store the calculated velocity in this vector.
 * 	
 * @noreturn
 * @error               If client is not connected or client index is not
 *                      within the range of 1 to MaxClients.
 */
native velocity_by_aim(iIndex, iVelocity, Float:vRetValue[3]);

/**
 * Changes a vector into an angle vector.
 * 
 * @param fVector       Input vector
 * @param vReturn       Output angle vector
 * 	
 * @noreturn
 */
native vector_to_angle(const Float:fVector[3], Float:vReturn[3]);

/**
 * Changes an angle vector into a vector.
 * 
 * @param vector        Input angle vector
 * @param FRU           One of the ANGLEVECTOR_* constants
 * @param ret           Output vector
 * 	
 * @noreturn
 */
native angle_vector(const Float:vector[3], FRU, Float:ret[3]);

/**
 * Calculates the length of a vector.
 * 
 * @param vVector       Input vector
 * 	
 * @return              Length of the input vector
 */
native Float:vector_length(const Float:vVector[3]);

/**
 * Calculates the distance between two vectors.
 * 
 * @param vVector       The first vector
 * @param vVector2      The second vector
 * 	
 * @return              Distance between two input vectors
 */
native Float:vector_distance(const Float:vVector[3], const Float:vVector2[3]);

/**
 * Converts an integer vector to a floating point vector.
 * 
 * @param IVec          Input integer vector
 * @param FVec          Output float vector
 * 	
 * @noreturn
 */
stock IVecFVec(const IVec[3], Float:FVec[3])
{
	FVec[0] = float(IVec[0]);
	FVec[1] = float(IVec[1]);
	FVec[2] = float(IVec[2]);

	return 1;
}

/**
 * Converts a floating point vector into an integer vector.
 * 
 * @param FVec          Input float vector
 * @param IVec          Output integer vector
 * 	
 * @noreturn
 */
stock FVecIVec(const Float:FVec[3], IVec[3])
{
	IVec[0] = floatround(FVec[0]);
	IVec[1] = floatround(FVec[1]);
	IVec[2] = floatround(FVec[2]);
	
	return 1;
}
