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
native get_distance(origin1[3],origin2[3]);

/* Gets distance between two origins (float). */
native Float:get_distance_f(Float:Origin1[3], Float:Origin2[3]);

/* Changes a vector into an angle vector. */
native vector_to_angle(Float:fVector[3], Float:vReturn[3]);

/* Changes an angle vector into a vector. */
native angle_vector(Float:vector[3], FRU, Float:ret[3]);

/* Gets the length of a vector (float[3]). */
native Float:vector_length(Float:vVector[3]);

/* Gets the distance between 2 vectors (float[3]). */
native Float:vector_distance(Float:vVector[3], Float:vVector2[3]);
