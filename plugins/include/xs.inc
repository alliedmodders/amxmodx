// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
// Copyright (C) 2004 Pavol "PM" Marko
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

/**
 *  XS Library
 *  Version 0.1
 *
 * MACROS THAT YOU CAN DEFINE BEFORE INCLUDING XS.INC:
 * XS_FLEQ_TOLERANCE:
 *  Tolerance that is used for XS_FLEQ float nearly-equal comparisions
 *  DEFAULT: 0.000005
 * XS_DEBUG
 *  Turn debug logging on
 *  DEFAULT: 0
 * XS_LOGBUFFER_SIZE
 *  Buffer size for logging
 *  DEFAULT: 512
 * XS_TASK_MAXPARAMS
 *  Maximal parameter count for managed tasks
 *  DEFAULT: 8
 * XS_TASK_MAXPARAMSIZE
 *  Maximal size of string parameter for tasks
 *  Has to be power of 2 and has to be >= 8
 *  DEFAULT: 512
 * XS_TASK_MANAGEDIDS
 *  Number of managed IDs for tasks.
 *  DEFAULT: 2048
 * XS_REPLACEBUF_SIZE
 *  DEFAULT: 3072
 *
 * NOTES:
 *  On AMX, VexdUM is required for some math functions
 *  xs__ / XS__ (2 underscores) stuff is meant to be intern
 *  untested: never tested
 *  half-tested: succesfully used in other applications; not extensively tested in xs though
 *  tested: fully tested
 *  If you have any useful functions / ideas for functions, please tell me.
*/

#if defined _xs_included
  #endinput
#endif
#define _xs_included

// **** CONFIG CHECK

#if !defined XS_FLEQ_TOLERANCE
	#define XS_FLEQ_TOLERANCE 0.000005
#endif

#if !defined XS_DEBUG
	#define XS_DEBUG 0
#endif

#if !defined XS_LOGBUFFER_SIZE
	#define XS_LOGBUFFER_SIZE 512
#endif

#if !defined XS_TASK_MAXPARAMS
	#define XS_TASK_MAXPARAMS 8
#endif

#if !defined XS_TASK_MAXPARAMSIZE
	#define XS_TASK_MAXPARAMSIZE 512
#endif

#if !defined XS_TASK_MANAGEDIDS
	#define XS_TASK_MANAGEDIDS 2048
#endif

#if !defined XS_REPLACEBUF_SIZE
	#define XS_REPLACEBUF_SIZE 3072
#endif


/****** DEBUGGING / LOGING FUNCTIONS ******/
enum xs_logtypes
{
	xs_debug,
	xs_message,
	xs_warning,
	xs_error,
	xs_fatalerror,
	xs__assertionfailed,

	// must come last
	xs_logtypes_count
}

stock const xs__logtypenames[xs_logtypes_count][] = {"DEBUG", "", "WARNING", "ERROR", "FATAL ERROR", "DEBUG ASSERTION FAILED"};

// tested
stock xs_log(xs_logtypes:logtype, any:...)
{
	// WARNING: Don't try to use assert in here; it uses this func

	// Don't log debug if not in debug mode
	#if !XS_DEBUG
		if (logtype == xs_debug)
			return;
	#endif

	new buffer[XS_LOGBUFFER_SIZE+1];
	buffer[XS_LOGBUFFER_SIZE]=0;
	format_args(buffer, XS_LOGBUFFER_SIZE, 1 /* go from SECOND argument*/);
	new bool:addLogTypeName = strlen(xs__logtypenames[logtype]) ? true : false;

	// Use AMXX's logging system
	log_amx("%s%s%s", addLogTypeName ? xs__logtypenames[logtype] : "",
		addLogTypeName ? ": " : "", buffer);
}

// Assertion
// tested
stock xs_assertfunc(any:exp, const desc[])
{
	// Check exp
	if (exp)
		return 1; // ok

	// not ok

	// print info
	xs_log(xs__assertionfailed, "%s", desc);

	return 0;
}
#define xs_assert(%1,%2) if (!xs_assertfunc(%1,%2)) xs__global_null /= xs__global_null


// Assertion; only in debug mode
// untested; logical flow says it should work
#if XS_DEBUG
	#define xs_assert_dbg(%1,%2) if (!xs_assertfunc(%1,%2)) xs__global_null /= xs__global_null
#else
	#define xs_assert_dbg(%1,%2)
#endif

new xs__global_null = 0;

/****** MATH FUNCTIONS ******/

/****** BASIC STUFF ******/

/**
 * Gets the sign of a value.
 *
 * @param num           Number to get the sign from.
 *
 * @return              -1 if the number is negative,
 *                      0 if the number is equal to 0,
 *                      1 if the number is positive.
 */
stock xs_sign(num)
{
	return (num < 0) ? -1 : ((num == 0) ? 0 : 1);
}

/**
 * Gets the sign of a float value.
 *
 * @param num           Number to get the sign from.
 *
 * @return              -1 if the number is negative,
 *                      0 if the number is equal to 0,
 *                      1 if the number is positive.
 */
stock xs_fsign(Float:num)
{
	return (num < 0.0) ? -1 : ((num == 0.0) ? 0 : 1);
}

/**
 * Gets the absolute value of a number.
 *
 * @param num			Number to get the absolute value from.
 *
 * @return              Absolute value of the input number.
 */
stock xs_abs(num)
{
	return (num < 0) ? -num : num;
}

/**
 * Checks if the number is a power of 2.
 *
 * @param x             Number to check.
 *
 * @return              1 if it is a power of 2, 0 otherwise.
 */
stock xs_is_2power(x)
{
	return (x!=0) && ((x&(x-1))==0);
}

/**
 * Converts degrees to radians.
 *
 * @param x             Input degrees.
 *
 * @return              Degrees converted to radians.
 */
stock Float:xs_deg2rad(Float:x)
{
	return x * 0.017453292519943;
}

/**
 * Converts radians to degrees.
 *
 * @param x             Input radians.
 *
 * @return              Radians converted to degrees.
 */
stock Float:xs_rad2deg(Float:x)
{
	return x * 57.29577951308232;
}

/**
 * Converts gradians to radians.
 *
 * @param x             Input gradians.
 *
 * @return              Gradians converted to radians.
 */
stock Float:xs_gra2rad(Float:x)
{
	return x * 0.015707963267948;
}

/**
 * Converts radians to gradians.
 *
 * @param x             Input radians.
 *
 * @return              Radians converted to gradians.
 */
stock Float:xs_rad2gra(Float:x)
{
	return x * 63.66197723675813;
}

/**
 * Checks if two floating point values are nearly equal.
 *
 * @param %1            The first value to compare.
 * @param %2            The second value to compare.
 *
 * @return              1 if they are nearly equal, 0 otherwise.
 */
#define XS_FLEQ(%1,%2) (((%1) <= ((%2) + XS_FLEQ_TOLERANCE)) && ((%1) >= ((%2) - XS_FLEQ_TOLERANCE)))

/**
 * Calculates the reciprocal of the square root of the input value.
 *
 * @param x             The input value.
 *
 * @return              The reciprocal of the square root of the input value.
 */
stock Float:xs_rsqrt(Float:x)
{
	return 1.0 / floatsqroot(x);
}

/**
 * Calculates the square root of the input value.
 *
 * @note This is an alias for floatsqroot().
 *
 * @param x             The input value.
 *
 * @return              The square root of the input value.
 */
stock Float:xs_sqrt(Float:x)
{
	return floatsqroot(x);
}

// These functions generate errors if you use the macros with wrong parameter count.
stock Float:xs_fabs(Float:pa)
{
	#pragma unused pa
	new rawr = you_need_one_param_for_fabs;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}
stock Float:xs_asin(Float:pa,Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_asin;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}
stock Float:xs_sin(Float:pa,Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_sin;
	#pragma unused rawr
}
stock Float:xs_acos(Float:pa,Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_acos;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}
stock Float:xs_cos(Float:pa,Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_cos;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}
stock Float:xs_atan(Float:pa,Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_atan;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}
stock Float:xs_atan2(Float:pa,Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_atan2;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}
stock Float:xs_tan(Float:pa, Float:pb)
{
	#pragma unused pa,pb
	new rawr = you_need_two_params_for_tan;
	rawr = warning_below_shows_line_number;
	#pragma unused rawr
}

#define xs_fabs(%1) floatabs(%1)
#define xs_asin(%1,%2) floatasin(%1, %2)
#define xs_sin(%1,%2) floatsin(%1, %2)
#define xs_acos(%1,%2) floatacos(%1, %2)
#define xs_cos(%1,%2) floatcos(%1, %2)
#define xs_atan(%1,%2) floatatan(%1, %2)
#define xs_atan2(%1,%2) floatatan2(%1, %2)
#define xs_tan(%1,%2) floattan(%1, %2)

/****** RANDOM NUMBERS ******/
// This routine comes from the book "Inner Loops" by Rick Booth, Addison-Wesley
// (ISBN 0-201-47960-5). This is a "multiplicative congruential random number
// generator" that has been extended to 31-bits

stock xs__internalseed=0x546875;

#define XS__IL_RMULT 1103515245

/**
 * Sets the seed for the random number generation.
 *
 * @param x             The seed to set.
 *
 * @noreturn
 */
stock xs_seed(seed)
{
	xs__internalseed = seed;
}

/**
 * Retrieves a random integer.
 *
 * @return              A random integer.
 */
stock xs_irand()
{
	new lo, hi, ll, lh, hh, hl;
	new result;

	lo = xs__internalseed & 0xffff;
	hi = xs__internalseed >> 16;
	xs__internalseed = xs__internalseed * XS__IL_RMULT + 12345;
	ll = lo * (XS__IL_RMULT  & 0xffff);
	lh = lo * (XS__IL_RMULT >> 16    );
	hl = hi * (XS__IL_RMULT  & 0xffff);
	hh = hi * (XS__IL_RMULT >> 16    );
	result = xs_abs(((ll + 12345) >> 16) + lh + hl + (hh << 16));
	return result;
}

/**
 * Retrieves a random float.
 *
 * @return              A random float.
 */
stock Float:xs_frand()
{
	return float(xs_irand()) / float(xs_get_maxnum());		// -1/2 should be the biggest possible positive number
}

/**
 * Retrieves a random integer between the specified values.
 *
 * @note @pmax has to be greater than @pmin!
 *
 * @param pmin          The minimum value.
 * @param pmax          The maximum value.
 *
 * @return              A random integer.
 */
stock xs_irand_range(pmin, pmax)
{
	xs_assert_dbg(pmax - pmin >= 0, "xs_irand_range: pmin > pmax");
	new i = pmin + floatround(xs_frand() * float(pmax - pmin));
	if (i > pmax)
		i = pmax;
	return i;
}

/****** VECTORS & PLANES ******/

// *** vectors

/**
 * Sets vector's components to specified values.
 *
 * @param vec           The vector to set values to.
 * @param x             The X component to be set.
 * @param y             The Y component to be set.
 * @param z             The Z component to be set.
 *
 * @noreturn
 */
stock xs_vec_set(Float:vec[], Float:x, Float:y, Float:z)
{
	vec[0] = x;
	vec[1] = y;
	vec[2] = z;
}

/**
 * Adds two vectors.
 *
 * @param in1           The first vector to add.
 * @param in2           The second vector to add.
 * @param out           The output vector. Can be one of the input vectors.
 *
 * @noreturn
 */
stock xs_vec_add(const Float:in1[], const Float:in2[], Float:out[])
{
	out[0] = in1[0] + in2[0];
	out[1] = in1[1] + in2[1];
	out[2] = in1[2] + in2[2];
}

/**
 * Subtracts one vector from another one.
 *
 * @param in1           Vector to subtract from.
 * @param in2           Vector to subtract from the first one.
 * @param out           The output vector. Can be one of the input vectors.
 *
 * @noreturn
 */
stock xs_vec_sub(const Float:in1[], const Float:in2[], Float:out[])
{
	out[0] = in1[0] - in2[0];
	out[1] = in1[1] - in2[1];
	out[2] = in1[2] - in2[2];
}

/**
 * Adds the second vector scaled by a scalar to the first.
 *
 * @param in1           Vector to add to.
 * @param in2           Vector to scale and add.
 * @param scalar        Scalar to scale the second vector with.
 * @param out           The output vector. Can be one of the input vectors.
 *
 * @noreturn
 */
stock xs_vec_add_scaled(const Float:in1[], const Float:in2[], Float:scalar, Float:out[])
{
	out[0] = in1[0] + in2[0] * scalar;
	out[1] = in1[1] + in2[1] * scalar;
	out[2] = in1[2] + in2[2] * scalar;
}

/**
 * Subtracts the second vector scaled by a scalar from the first one.
 *
 * @param in1           Vector to subtract from.
 * @param in2           Vector to scale and subtract.
 * @param scalar        Scalar to scale the second vector with.
 * @param out           The output vector. Can be one of the input vectors.
 *
 * @noreturn
 */
stock xs_vec_sub_scaled(const Float:in1[], const Float:in2[], Float:scalar, Float:out[])
{
	out[0] = in1[0] - in2[0] * scalar;
	out[1] = in1[1] - in2[1] * scalar;
	out[2] = in1[2] - in2[2] * scalar;
}

/**
 * Checks if two vectors are equal.
 *
 * @note If you need to check if two vectors are nearly equal,
 * 		 take a look at xs_vec_nearlyequal().
 *
 * @param vec1          The first input vector to check.
 * @param vec2          The second input vector to check.
 *
 * @return              1 if vectors are equal, 0 otherwise.
 */
stock bool:xs_vec_equal(const Float:vec1[], const Float:vec2[])
{
	return (vec1[0] == vec2[0]) && (vec1[1] == vec2[1]) && (vec1[2] == vec2[2]);
}

/**
 * Checks if two vectors are nearly equal.
 *
 * @note If you need to check if two vectors are exactly equal,
 * 		 take a look at xs_vec_equal().
 *
 * @param vec1          The first input vector to check.
 * @param vec2          The second input vector to check.
 *
 * @return              1 if vectors are equal, 0 otherwise.
 */
stock bool:xs_vec_nearlyequal(const Float:vec1[], const Float:vec2[])
{
	return XS_FLEQ(vec1[0], vec2[0]) && XS_FLEQ(vec1[1], vec2[1]) && XS_FLEQ(vec1[2], vec2[2]);
}

/**
 * Multiply a vector by a scalar value.
 *
 * @param vec           The vector to be multiplied.
 * @param scalar        The scalar value to multiply the vector with.
 * @param out           The output vector. Can be the same as the input vector.
 *
 * @noreturn
 */
stock xs_vec_mul_scalar(const Float:vec[], Float:scalar, Float:out[])
{
	out[0] = vec[0] * scalar;
	out[1] = vec[1] * scalar;
	out[2] = vec[2] * scalar;
}

/**
 * Divide a vector by a scalar value.
 *
 * @param vec           The vector to be divided.
 * @param scalar        The scalar value to divide the vector with.
 * @param out           The output vector. Can be the same as the input vector.
 *
 * @noreturn
 */
stock xs_vec_div_scalar(const Float:vec[], Float:scalar, Float:out[])
{
	new Float:__tmp = 1.0 / scalar;
	out[0] = vec[0] * __tmp;
	out[1] = vec[1] * __tmp;
	out[2] = vec[2] * __tmp;
}

/**
 * Computes the length of a vector.
 *
 * @param vec           The vector to compute the length of.
 *
 * @return              The length of the input vector.
 */
stock Float:xs_vec_len(const Float:vec[])
{
	return xs_sqrt(vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2]);
}

/**
 * Computes the length of a 2D vector.
 *
 * @param vec           The vector to compute the length of.
 *
 * @return              The length of the input vector.
 */
stock Float:xs_vec_len_2d(const Float:vec[])
{
	return xs_sqrt(vec[0]*vec[0] + vec[1]*vec[1]);
}

/**
 * Computes the distance between two vectors (points).
 *
 * @param vec1          First vector.
 * @param vec2          Second vector.
 *
 * @return              The distance between two vectors.
 */
stock Float:xs_vec_distance(const Float:vec1[], const Float:vec2[])
{
	return xs_sqrt((vec1[0]-vec2[0]) * (vec1[0]-vec2[0]) +
	               (vec1[1]-vec2[1]) * (vec1[1]-vec2[1]) +
	               (vec1[2]-vec2[2]) * (vec1[2]-vec2[2]));
}

/**
 * Computes the distance between two 2D vectors (points).
 *
 * @param vec1          First vector.
 * @param vec2          Second vector.
 *
 * @return              The distance between two vectors.
 */
stock Float:xs_vec_distance_2d(const Float:vec1[], const Float:vec2[])
{
	return xs_sqrt((vec1[0]-vec2[0]) * (vec1[0]-vec2[0]) +
	               (vec1[1]-vec2[1]) * (vec1[1]-vec2[1]));
}

/**
 * Computes the square of the distance between two vectors (points).
 * This is faster than the distance.
 *
 * @param vec1          First vector.
 * @param vec2          Second vector.
 *
 * @return              The square of the distance between two vectors.
 */
stock Float:xs_vec_sqdistance(const Float:vec1[], const Float:vec2[])
{
	return (vec1[0]-vec2[0]) * (vec1[0]-vec2[0]) +
	               (vec1[1]-vec2[1]) * (vec1[1]-vec2[1]) +
	               (vec1[2]-vec2[2]) * (vec1[2]-vec2[2]);
}

/**
 * Computes the square of the distance between two 2D vectors (points).
 * This is faster than the distance.
 *
 * @param vec1          First vector.
 * @param vec2          Second vector.
 *
 * @return              The square of the distance between two vectors.
 */
stock Float:xs_vec_sqdistance_2d(const Float:vec1[], const Float:vec2[])
{
	return (vec1[0]-vec2[0]) * (vec1[0]-vec2[0]) +
	               (vec1[1]-vec2[1]) * (vec1[1]-vec2[1]);
}

/**
 * Normalizes a vector. Normalized vector is a vector with the length of 1 unit,
 * but with the same direction as the original vector.
 *
 * @param vec           The vector to be normalized.
 * @param out           The output vector. Can be the same as the input vector.
 *
 * @noreturn
 */
stock xs_vec_normalize(const Float:vec[], Float:out[])
{
	new Float:invlen = xs_rsqrt(vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2]);
	out[0] = vec[0] * invlen;
	out[1] = vec[1] * invlen;
	out[2] = vec[2] * invlen;
}

/**
 * Computes the cross product of two vectors.
 *
 * @param vec1          The first vector operand of the cross operation.
 * @param vec2          The second vector operand of the cross operation.
 * @param out           The output vector. *Can't* be one of the input vectors.
 *
 * @noreturn
 */
stock xs_vec_cross(const Float:vec1[], const Float:vec2[], Float:out[])
{
	out[0] = vec1[1]*vec2[2] - vec1[2]*vec2[1];
	out[1] = vec1[2]*vec2[0] - vec1[0]*vec2[2];
	out[2] = vec1[0]*vec2[1] - vec1[1]*vec2[0];
}

/**
 * Computes the dot product of two vectors.
 *
 * @param vec1          The first vector operand of the dot operation.
 * @param vec2          The second vector operand of the dot operation.
 *
 * @return              The dot product of two input vectors.
 */
stock Float:xs_vec_dot(const Float:vec1[], const Float:vec2[])
{
	return vec1[0]*vec2[0] + vec1[1]*vec2[1] + vec1[2]*vec2[2];
}

/**
 * Negates a vector.
 *
 * @param vec           The vector to negate.
 * @param out           The output vector. Can be the same as the input vector.
 *
 * @noreturn
 */
stock xs_vec_neg(const Float:vec[], Float:out[])
{
	out[0] = -vec[0];
	out[1] = -vec[1];
	out[2] = -vec[2];
}

/**
 * Copies a vector into another one.
 *
 * @param vecIn         The vector to copy.
 * @param vecOut        The output vector where to copy the input vector.
 *
 * @noreturn
 */
stock xs_vec_copy(const Float:vecIn[], Float:vecOut[])
{
	vecOut[0] = vecIn[0];
	vecOut[1] = vecIn[1];
	vecOut[2] = vecIn[2];
}

/**
 * Computes the angle between two vectors.
 *
 * @param vec1          The first vector.
 * @param vec2          The second vector.
 *
 * @return              The angle between two input vectors in degrees.
 */
stock Float:xs_vec_angle(const Float:vec1[], const Float:vec2[])
{
	return xs_rad2deg(xs_acos(xs_vec_dot(vec1, vec2), radian));
}

/**
 * Reflects a vector about a normal.
 *
 * @param vec           The vector to be reflected.
 * @param normal        The normal vector about which to reflect.
 * @param out           The output reflected vector.
 *
 * @noreturn
 */
stock xs_vec_reflect(const Float:vec[], const Float:normal[], Float:out[])
{
	// normalize(vec) - (normal * 2.0 * (tmp . normal)) * length(vec)

	new Float:tmp1[3];
	xs_vec_normalize(vec, tmp1);

	// tmp1 - (normal * 2.0 * (tmp . normal)) * length(vec)

	new Float:tmp2[3];
	xs_vec_mul_scalar(normal, 2.0, tmp2);
	xs_vec_mul_scalar(tmp2, xs_vec_dot(tmp1, normal), tmp2);

	// tmp1 - tmp2 * length(vec)
	xs_vec_mul_scalar(tmp2, xs_vec_len(vec), tmp2);

	// tmp1 - tmp2
	xs_vec_sub(tmp1, tmp2, out);
}

/**
 * Turns a 3D vector into a 2D vector.
 *
 * @note This function just ignores the Z (3rd) component of a 3D vector.
 *
 * @param vec           A 3D vector to turn into a 2D vector.
 * @param out           The output 2D vector.
 *
 * @noreturn
 */
stock xs_vec_make2d(const Float:vec[3], Float:out[2])
{
	out[0] = vec[0];
	out[1] = vec[1];
}

// *** planes

// normal
#define XS_PLANE_A 0
#define XS_PLANE_B 1
#define XS_PLANE_C 2
// plane shift distance
#define XS_PLANE_D 3


/**
 * Sets a plane to the specified values.
 *
 * @param plane         The plane to set the values to. It's a 4D vector.
 * @param a             The first component of a plane to be set.
 * @param b             The second component of a plane to be set.
 * @param c             The third component of a plane to be set.
 * @param d             The fouth component of a plane to be set.
 *
 * @noreturn
 */
stock xs_plane_set(Float:plane[], Float:a, Float:b, Float:c, Float:d)
{
	plane[XS_PLANE_A] = a;
	plane[XS_PLANE_B] = b;
	plane[XS_PLANE_C] = c;
	plane[XS_PLANE_D] = d;
}

/**
 * Constructs a plane out of 4 points in space.
 *
 * @param plane         The output plane to store the newly created plane.
 * @param p1            The first point of a plane.
 * @param p2            The second point of a plane.
 * @param p3            The third point of a plane.
 *
 * @noreturn
 */
stock xs_plane_3p(Float:plane[], const Float:p1[], const Float:p2[], const Float:p3[])
{
	new Float:normalA[3], Float:normalB[3];

	// normalA = Normalize(p3 - p1);
	normalA[0] = p3[0] - p1[0];
	normalA[1] = p3[1] - p1[1];
	normalA[2] = p3[2] - p1[2];
	xs_vec_normalize(normalA, normalA);

	// normalB = Normalize(p3 - p2);
	normalB[0] = p3[0] - p2[0];
	normalB[1] = p3[1] - p2[1];
	normalB[2] = p3[2] - p2[2];
	xs_vec_normalize(normalB, normalB);

	// plane normal = Normalize(normalA cross normalB)
	xs_vec_cross(normalA, normalB, plane);
	xs_vec_normalize(plane, plane);

	// plane shift distance = (-p1) dot plane normal
	new Float:__tmp[3];
	xs_vec_neg(plane, __tmp);
	plane[XS_PLANE_D] = xs_vec_dot(__tmp, p1);

}

/**
 * Checks if two planes are equal.
 *
 * @note If you have to check if two planes are just nearly equal,
 *       take a look at xs_plane_nearlyequal().
 *
 * @param plane1        The first plane to check.
 * @param plane2        The second plane to check.
 *
 * @return              1 if planes are equal, 0 otherwise.
 */
stock bool:xs_plane_equal(const Float:plane1[], const Float:plane2[])
{
	if (	(plane1[0] == plane2[0]) &&
		(plane1[1] == plane2[1]) &&
		(plane1[2] == plane2[2]) &&
		(plane1[3] == plane2[3]))
		return true;
	return false;
}

/**
 * Checks if two planes are nearly equal.
 *
 * @note If you have to check if two planes are exactly equal,
 *       take a look at xs_plane_equal().
 *
 * @param plane1        The first plane to check.
 * @param plane2        The second plane to check.
 *
 * @return              1 if planes are nearly equal, 0 otherwise.
 */
stock bool:xs_plane_nearlyequal(const Float:plane1[], const Float:plane2[])
{
	if (	XS_FLEQ(plane1[0], plane2[0]) &&
		XS_FLEQ(plane1[1], plane2[1]) &&
		XS_FLEQ(plane1[2], plane2[2]) &&
		XS_FLEQ(plane1[3], plane2[3]))
		return true;
	return false;
}

/**
 * Computes the distance between a plane and a point.
 *
 * @param plane         The plane to check the distance from.
 * @param point         The point to check the distance to.
 *
 * @return              The distance between the input plane and point.
 */
stock Float:xs_plane_dst2point(const Float:plane[], const Float:point[])
{
	// return normal dot point + D
	return xs_vec_dot(plane, point) + plane[XS_PLANE_D];
}

/**
 * Checks whether a plane intersects with the ray starting at @rayStart and
 * going to @rayDir direction.
 * If it does intersect, outputs the intersection point in @out.
 *
 * @param plane         The plane to check intersection with.
 * @param rayStart      The starting point of the ray.
 * @param rayDir        Direction in which the ray is going.
 * @param out           The vector to copy the intersection point to, if it exists.
 *
 * @return              true if they intersect, false otherwise.
 */
stock bool:xs_plane_rayintersect(const Float:plane[], const Float:rayStart[], const Float:rayDir[], Float:out[])
{
	new Float:a = xs_vec_dot(plane, rayDir);

	if (a == 0.0)
		return false;		// ray is parallel to plane

	// if (distance plane<->(rayStart + rayDir) > distance plane<->rayStart) and both have the same sign, the ray
	// goes away from the plane
	new Float:rsplusrd[3];
	xs_vec_add(rayStart, rayDir, rsplusrd);
	new Float:dst1 = xs_plane_dst2point(plane, rsplusrd);
	new Float:dst2 = xs_plane_dst2point(plane, rayStart);
	if (xs_fabs(dst1) > xs_fabs(dst2) && xs_fsign(dst1) == xs_fsign(dst2))
		return false;


	// out = rayStart - rayDir * ((distance plane<->rayStart) / a)
	new Float:__tmp[3];
	xs_vec_mul_scalar(rayDir, xs_plane_dst2point(plane, rayStart) / a, __tmp);
	// out = rayStart - tmp
	xs_vec_sub(rayStart, __tmp, out);

	return true;
}

/**
 * Checks if a point is on a specified plane.
 *
 * @param plane         The plane to check.
 * @param point         The point to check.
 *
 * @return              true if the point is on the plane, false otherwise.
 */
stock bool:xs_point_onplane(const Float:plane[], const Float:point[])
{
	return XS_FLEQ(xs_plane_dst2point(plane, point), 0.0);
}

/**
 * Projects a point on the plane. Stores the projected point in @out.
 *
 * @param plane         The plane to project the point onto.
 * @param point         The point to project onto the plane.
 * @param out           The vector to copy the projected point into.
 *
 * @noreturn
 */
stock xs_projpoint_onplane(const Float:plane[], const Float:point[], Float:out[])
{
	new Float:__tmp[3];
	// out = point - (plane normal * distance point<->plane)
	xs_vec_copy(plane, __tmp);
	xs_vec_mul_scalar(__tmp, xs_plane_dst2point(plane, point), __tmp);
	xs_vec_sub(point, __tmp, out);
}

/**
 * Copies a plane.
 *
 * @param planeIn       The plane to copy.
 * @param planeOut      The plane to store the copy into.
 *
 * @noreturn
 */
stock xs_plane_copy(const Float:planeIn[], Float:planeOut[])
{
	planeOut[0] = planeIn[0];
	planeOut[1] = planeIn[1];
	planeOut[2] = planeIn[2];
	planeOut[3] = planeIn[3];
}

/****** HL ENGINE SPECIFIC STUFF ******/

// angle indexes
#define	XS_PITCH				0		// up / down
#define	XS_YAW					1		// left / right
#define	XS_ROLL					2		// fall over

/**
 * Computes forward, right and up vectors from given angles.
 *
 * @param angles        Angles to compute vectors from.
 * @param fwd           The vector to store the forward vector into.
 * @param right         The vector to store the right vector into.
 * @param up            The vector to store the up vector into.
 *
 * @noreturn
 */
stock xs_anglevectors(const Float:angles[3], Float:fwd[3], Float:right[3], Float:up[3])
{
	// sin (s) and cos (c) for yaw (y), pitch (p) and roll (r)
	new Float:sr, Float:sp, Float:sy, Float:cr, Float:cp, Float:cy;

	sy = xs_sin(angles[XS_YAW], degrees);
	cy = xs_cos(angles[XS_YAW], degrees);
	sp = xs_sin(angles[XS_PITCH], degrees);
	cp = xs_cos(angles[XS_PITCH], degrees);
	sr = xs_sin(angles[XS_ROLL], degrees);
	cr = xs_cos(angles[XS_ROLL], degrees);

	fwd[0] = cp*cy;
	fwd[1] = cp*sy;
	fwd[2] = -sp;

	right[0] = (-1*sr*sp*cy + -1*cr*-sy);
	right[1] = (-1*sr*sp*sy + -1*cr*cy);
	right[2] = -1*sr*cp;

	up[0] = (cr*sp*cy + -sr*-sy);
	up[1] = (cr*sp*sy + -sr*cy);
	up[2] = cr*cp;
}
/****** STRING FUNCS *******/

/**
 * Finds a character in a string and returns its position in the string.
 *
 * @param str           The string to search in.
 * @param chr           The character to search for in the string.
 *
 * @return              The character position if found, -1 otherwise.
 */
stock xs_strchr(const str[], chr)
{
	for (new i = 0; str[i] != 0; ++i)
	{
		if (str[i] == chr)
			return i;
	}
	return -1;
}

/**
 * Remove @charstotrim number of characters from @stringtotrim,
 * either from the beginning or the end of the string.
 *
 * @param stringtotrim  The string to be trimmed.
 * @param charstostrim  The number of characters to trim.
 * @param fromleft      If set to true, the string will be trimmer from the left.
 *                      If false, it will be trimmed from the right.
 *
 * @noreturn
 */
stock xs_strtrim(stringtotrim[], charstotrim, bool:fromleft = true)
{
	if (charstotrim <= 0)
    		return;

	if (fromleft)
	{
		new maxlen = strlen(stringtotrim);
		if (charstotrim > maxlen)
			charstotrim = maxlen;

		// In format, input and output regions can overlap
		format(stringtotrim, maxlen, "%s", stringtotrim[charstotrim]);
	}
	else
	{
		new maxlen = strlen(stringtotrim) - charstotrim;
		if (maxlen < 0)
			maxlen = 0;

		// In format, input and output regions can overlap
		format(stringtotrim, maxlen, "%s", stringtotrim);
	}
}

/**
 * Copies characters from @oldmsg to @newmsg, starting at @start and ending
 * at @end (includes the end character).
 *
 * @param oldmsg        The string to copy from.
 * @param newmsg        The string to copy to.
 * @param start         Starting position of the @oldmsg string to copy from.
 * @param end           Ending position of the @oldmsg string to copy from.
 * @param outlen        If positive, specifies the maximum number of characters
 *                      to be copied. Otherwise, the function assumes that
 *                      newmsg is at least @end - @start + 1 characters long.
 *
 * @noreturn
 */
stock xs_strmid(const oldmsg[], newmsg[], start, end, outlen=-1)
{
	new len = strlen(oldmsg);

	if(start < 0)
		start = 0;

	++end;		// Include end

	if(end <= start || end > len)
		end = len;

	new j = 0, i = start;
	for(; (i < end) && (outlen--);)
		newmsg[j++] = oldmsg[i++];

	newmsg[j] = 0;
}

/**
 * "Explodes" a string, breaking it at the @delimeter character and putting
 * each exploded part into the @output array.
 *
 * @param input         The input string to be exploded.
 * @param output        The output array of string where exploded string will be stored.
 * @param delimeter     The character to break the string at.
 * @param maxelems      Maximum amount of elements in @output.
 * @param elemsize      Maximum size of each string in the @output array.
 *
 * @return              The number of strings successfully exploded.
 */
stock xs_explode(const input[], output[][], delimiter, maxelems, elemsize)
{
	new nIdx = 0;
	new nLen = 0;

	new copied = 0;
	while(nLen < strlen(input) && nIdx < maxelems)
	{
		copied = copyc(output[nIdx++], elemsize, input[nLen], delimiter);
		if (copied == elemsize)
		{
			// maybe it got force-stopped because of maxsize
			// so check whether we have to skip something
			if (input[nLen + copied] != delimiter && input[nLen + copied] != 0)
			{
				new found = xs_strchr(input[nLen + copied], delimiter);
				if (found == -1)
					break;
				copied += found;
			}
		}

		nLen += copied + 1;	// +1: skip delimiter
	}
	return nIdx;
}

/**
 * The opposite of xs_explode(). Takes an array of strings and puts them together
 * in a single string, delimeted by the @delimeter character.
 *
 * @param output        The string to store the impoded string into.
 * @param outsize       The size of the output buffer.
 * @param delimeter     The character to put between imploded strings.
 * @param input         The array of strings to implode.
 * @param elemsnum      The number of strings in the input array.
 *
 * @return              The number of characters in the final output buffer.
 */
stock xs_implode(output[], outsize, delimiter, const input[][], elemsnum)
{
	new pos = 0;
	new copied;
	for (new i = 0; i < elemsnum; ++i)
	{
		copied = copy(output[pos], outsize - pos, input[i]);
		pos += copied;
		if (pos >= outsize)
			return outsize;
		// append delimiter
		output[pos] = delimiter;
		++pos;
		// last check
		if (pos >= outsize)
			return outsize;
	}

	output[--pos] = 0;		// The last char would be delimiter, so skip it.
	return pos;
}


stock xs__replace_buf[XS_REPLACEBUF_SIZE];

/**
 * Replaces all occurencies of @what in @text with @with.
 *
 * @param text          The text to search in.
 * @param len           The maximum size of the @text buffer.
 * @param what          What to search for.
 * @param with          What to replace occurencies with.
 *
 * @return              Returns the number of replaced items.
 */
stock xs_replace(text[], len, const what[], const with[])
{
	new occur = 0;
	new i = 0;
	new bufPos = 0;
	new replaceLen = strlen(with);
	new whatLen = strlen(what);
	for (; text[i]; ++i)
	{
		if (text[i] == what[0])
		{
			new posInWhat=0;
			new j;
			for (j = i; j-i < replaceLen && text[j]; ++j, ++posInWhat)
			{
				if (text[j] != what[posInWhat])
					break;
			}
			if (whatLen == posInWhat)
			{
				for (new i2 = 0; i2 < replaceLen && bufPos < XS_REPLACEBUF_SIZE; ++i2)
					xs__replace_buf[bufPos++] = with[i2];
				i = j - 1;
				++occur;
				if (bufPos >= XS_REPLACEBUF_SIZE)
					return occur;
				continue;
			}
		}
		if (bufPos >= XS_REPLACEBUF_SIZE)
			return occur;
		xs__replace_buf[bufPos++] = text[i];
	}
	xs__replace_buf[bufPos] = 0;
	copy(text, len, xs__replace_buf);
	return occur;
}

/**
 * Replaces all occurencies of @what character in @text with @with character.
 *
 * @param text          The text to search in.
 * @param len           The maximum size of the @text buffer.
 * @param what          What character to search for.
 * @param with          What charactear to replace occurencies with.
 *
 * @return              The number of replaced characters.
 */
stock xs_replace_char(text[], len, what, with)
{
	// let the xs_replace function do the work
	new arr[4];
	arr[0] = what;
	arr[1] = 0;
	arr[2] = with;
	arr[3] = 0;

	return xs_replace(text, len, arr[0], arr[2]);
}

/****** MISC FUNCS *******/

/**
 * Retrieves the name of a command identified by its ID.
 *
 * @param cid           The command ID.
 * @param namestr       The buffer where to store command's name.
 * @param namelen       The maximum size of the output buffer.
 *
 * @noreturn
 */
stock xs_concmd_name(cid, namestr[], namelen)
{
	new dummy1;
	new dummy2[1];
	get_concmd(cid, namestr, namelen, dummy1, dummy2, 0, 0);
}

/**
 * Checks whether there are at least @num free visible slots.
 *
 * @param num           The number of slots to check.
 *
 * @return              true if there are at least that many free, false otherwise.
 */
stock bool:xs_freevisibleslots(num)
{
	new maxplayers = get_cvar_num("sv_visiblemaxplayers");
	if (maxplayers <= 0)
		maxplayers = MaxClients;

	return (get_playersnum(1) <= maxplayers-num) ? true : false;
}

stock xs__maxnum = 0;

/**
 * Returns the biggest possible positive number.
 *
 * @return              The biggest possible positive number.
 */
stock xs_get_maxnum()
{
	if (!xs__maxnum)
	{
		// build it
		xs__maxnum = ((1 << (cellbits - 2)) - 1 ) | (1 << (cellbits - 2));
		/*
		new bits = get_cellsize() * 8 - 1;
		for (new i = 0; i < bits; ++i)
			xs__maxnum |= 1 << i;
		*/
	}
	return xs__maxnum;
}

/**
 * Returns the smallest possible negative number.
 *
 * @return              The smallest possible negative number.
 */
stock xs_get_minnum()
{
	return xs_get_maxnum() + 1;
}


// *** The following two functions were created by Damaged Soul.

// Max messages reserved by engine (DO NOT MODIFY)
#define XS__MAX_ENGINE_MESSAGES 63
// Max possible messages for mod, is 255 really the limit?
#define XS__MAX_POSSIBLE_MESSAGES 255

// Returns max number of messages for mod
stock xs_get_maxmessages()
{
	new name[2];

	for (new i = XS__MAX_ENGINE_MESSAGES + 1; i <= XS__MAX_POSSIBLE_MESSAGES; i++)
		if (!get_user_msgname(i, name, 1))
			return i - 1;

	return XS__MAX_POSSIBLE_MESSAGES;
}

// Returns true if msgid is a valid message
stock bool:xs_is_msg_valid(msgid)
{
	new name[2];
	new retval = get_user_msgname(msgid, name, 1);

	if (msgid < 1 || (msgid > XS__MAX_ENGINE_MESSAGES && !retval))
		return false;

	return true;
}

/****** MANAGED TASKS ******/

// ***** managed task ids
stock xs_find_freetaskid()
{
	for (new i = 1; i <= XS_TASK_MANAGEDIDS; ++i)
	{
		if (!task_exists(i))
			return i;
	}
	return -1;
}

// ***** managed tasks
enum xs_paramtypes
{
	xs_invalid = 0,
	xs_int,
	xs_float,
	xs_string
}

// new task
stock xs__TaskParam[	1 +						// number of parameters
			XS_TASK_MAXPARAMS +						// parameter types
			(XS_TASK_MAXPARAMSIZE char) * XS_TASK_MAXPARAMS];		// space for len + value

stock Float:xs__TaskInterval = 0.0;
stock xs__TaskFlags[5];
stock xs__TaskFunc[48];
stock xs__TaskId;
stock xs__TaskRepeat;

#define xs__TaskParamCount xs__TaskParam[0]
#define xs__TaskParamType[%1] xs__TaskParam[1 + %1]

#define xs__TaskParamValue[%1] xs__TaskParam[1 + XS_TASK_MAXPARAMS + (%1 * (XS_TASK_MAXPARAMSIZE char))]


// incoming task
stock xs__ITaskParam[	1 +						// number of parameters
			XS_TASK_MAXPARAMS +						// parameter types
			(XS_TASK_MAXPARAMSIZE char) * XS_TASK_MAXPARAMS];		// space for len + value
stock xs__ITaskId;

#define xs__ITaskParamCount xs__ITaskParam[0]
#define xs__ITaskParamType[%1] xs__ITaskParam[1 + %1]

#define xs__ITaskParamValue[%1] xs__ITaskParam[1 + XS_TASK_MAXPARAMS + (%1 * (XS_TASK_MAXPARAMSIZE char))]

// tested
stock xs_task_begin(Float:interval, const func[], id = 0, const flags[] = "", repeat = 0)
{
	xs_assert(xs__TaskInterval == 0.0, "New xs_task_begin called before xs_task_end");

	xs__TaskInterval = interval;
	if (xs__TaskInterval < 0.1)
		xs__TaskInterval = 0.1;

	copy(xs__TaskFunc, 47, func);
	xs__TaskId = id;
	copy(xs__TaskFlags, 4, flags);
	xs__TaskRepeat = repeat;

	xs__TaskParamCount = 0;
}

// tested
stock xs_task_pushint(value, bool:__isfl=false /*internal use only*/)
{
	xs_assert(xs__TaskInterval, "xs_task_push* called without xs_task_begin");
	if (xs__TaskParamCount >= XS_TASK_MAXPARAMS)
		return 0;

	xs__TaskParamType[xs__TaskParamCount] = __isfl ? xs_float : xs_int;
	xs__TaskParamValue[xs__TaskParamCount] = value;

	++xs__TaskParamCount;
	return 1;
}

// tested
stock xs_task_pushfl(Float:value)
{
	return xs_task_pushint(_:value, true);
}

// tested
stock xs_task_pushstr(const value[])
{
	xs_assert(xs__TaskInterval, "xs_task_push* called without xs_task_begin");
	if (xs__TaskParamCount >= XS_TASK_MAXPARAMS)
		return 0;

	xs__TaskParamType[xs__TaskParamCount] = xs_string;
	strpack(xs__TaskParamValue[xs__TaskParamCount], value);
	++xs__TaskParamCount;
	return 1;
}

// tested
stock xs_task_end()
{
	xs_assert(xs__TaskInterval, "xs_task_end called without xs_task_begin");

	// find a task id if needed
	if (xs__TaskId == -1)
	{
		xs__TaskId = xs_find_freetaskid();
		if (xs__TaskId == -1)
		{
			// not found
			xs__TaskInterval = 0.0;
			return -1;
		}
	}

	set_task(xs__TaskInterval, xs__TaskFunc, xs__TaskId, xs__TaskParam,
		1 + xs__TaskParamCount * (XS_TASK_MAXPARAMSIZE char), xs__TaskFlags, xs__TaskRepeat);

	xs__TaskInterval = 0.0;

	return xs__TaskId;
}


// tested
#define XS_MAKE_TASKFUNC(%1) public %1(const _xs__taskparam[], _xs__taskid) if(xs__task_setup(_xs__taskparam, _xs__taskid))

// tested
stock xs__task_setup(const param[], taskid)
{
	xs__ITaskId = taskid;
	new len = 1 + param[0] * (XS_TASK_MAXPARAMSIZE char);
	for (new i = 0; i < len; ++i)
		xs__ITaskParam[i] = param[i];
	return 1;
}

// tested
stock xs_task_readid()
{
	return xs__ITaskId;
}

// tested
stock xs_task_paramcount()
{
	return xs__ITaskParamCount;
}

// tested
stock xs_paramtypes:xs_task_paramtype(paramid)
{
	if (paramid < 0 || paramid >= xs__ITaskParamCount)
		return xs_invalid;

	return xs_paramtypes:xs__ITaskParamType[paramid];
}

// tested
stock xs_task_paramint(paramid)
{
	if (paramid < 0 || paramid >= xs__ITaskParamCount)
		return 0;
	if (xs__ITaskParamType[paramid] != _:xs_int)
		return 0;

	return xs__ITaskParamValue[paramid];
}

// tested
stock Float:xs_task_paramfl(paramid)
{
	if (paramid < 0 || paramid >= xs__ITaskParamCount)
		return 0.0;
	if (xs__ITaskParamType[paramid] != _:xs_float)
		return 0.0;

	return Float:xs__ITaskParamValue[paramid];
}

// tested
stock xs_task_paramstr(paramid, out[], maxlen)
{
	#pragma unused maxlen

	if (paramid < 0 || paramid >= xs__ITaskParamCount)
		return 0;
	if (xs__ITaskParamType[paramid] != _:xs_string)
		return 0;

	strunpack(out, xs__ITaskParamValue[paramid]);
	return 1;
}
