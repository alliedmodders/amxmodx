/* Float arithmetic
*
* (c) Copyright 1999, Artran, Inc.
*  Written by Greg Garner (gmg@artran.com)
*  Modified in March 2001 to include user defined
*  operators for the floating point functions.
*
* This file is provided as is (no warranties).
*/

#if defined _float_included
  #endinput
#endif
#define _float_included

#pragma rational Float

/**
 * Different methods of rounding
 */
enum floatround_method {
	floatround_round = 0,
	floatround_floor,
	floatround_ceil,
	floatround_tozero
};

/**
 * Different units of measurement for angles 
 */
enum anglemode {
	radian = 0,
	degrees,
	grades
};

/**
 * Converts an integer into a floating point value.
 *
 * @param value         Value to be converted
 *
 * @return              Converted value
 */
native Float:float(value);

/**
 * Converts a string into a floating point value.
 *
 * @param string        Input string to be converted
 *
 * @return              Converted value
 */
native Float:floatstr(const string[]);

/**
 * Returns the fractional part of a floating point value
 *
 * @param string        Floating point value to get the fractional part from
 *
 * @return              The fractional part
 */
native Float:floatfract(Float:value);

/**
 * Rounds a floating point value to an integer value
 *
 * @note For the list of available rounding methods look at
 *		 floatround_method enumeration.
 *
 * @param value         Floating point value to be rounded
 * @param method        Rounding method
 *
 * @return              Converted value
 */
native floatround(Float:value, floatround_method:method=floatround_round);

/**
 * Compares two floating point values.
 *
 * @param fOne          First value to be compared
 * @param fTwo          Second value to be compared
 *
 * @return              If arguments are equal, returns 0.
 *                      If the first one is greater, returns 1.
 *                      If the second one is greater, returns -1.
 */
native floatcmp(Float:fOne, Float:fTwo);

/**
 * Returns the square root of a floating point value
 *
 * @note Same as floatpower(value, 0.5)
 *
 * @param value         Floating point value to get square root from
 *
 * @return              Square root of the input value
 */
native Float:floatsqroot(Float:value);

/**
 * Returns the value raised to the power of the exponent
 *
 * @param value         Floating point value to be raised
 * @param exponent      The exponent
 *
 * @return              Value raised to the power of the exponent
 */
native Float:floatpower(Float:value, Float:exponent);

/**
 * Returns the logarithm of value
 *
 * @param value         Floating point value to calculate the logarithm for
 * @param base          The optional logarithmic base to use.
 *                      Defaults to 10, or the natural logarithm
 *
 * @return              Square root of the input value
 */
native Float:floatlog(Float:value, Float:base=10.0);

/**
 * Returns the sine of a given angle
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The angle to calculate the sine from
 * @param mode          What unit of measurement is the angle specified in
 *                      Defaults to radians
 *
 * @return              The sine of a given angle
 */
native Float:floatsin(Float:value, anglemode:mode=radian);

/**
 * Returns the cosine of a given angle
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The angle to calculate the cosine from
 * @param mode          What unit of measurement is the angle specified in
 *                      Defaults to radians
 *
 * @return              The cosine of a given angle
 */
native Float:floatcos(Float:value, anglemode:mode=radian);

/**
 * Returns the tangent of a given angle
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The angle to calculate the tangent from
 * @param mode          What unit of measurement is the angle specified in
 *                      Defaults to radians
 *
 * @return              The tangent of a given angle
 */
native Float:floattan(Float:value, anglemode:mode=radian);

/**
 * Returns the hyperbolic sine of a given angle
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The angle to calculate the hyperbolic sine from
 * @param mode          What unit of measurement is the angle specified in
 *                      Defaults to radians
 *
 * @return              The hyperbolic sine of a given angle
 */
native Float:floatsinh(Float:angle, anglemode:mode=radian);

/**
 * Returns the hyperbolic cosine of a given angle
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The angle to calculate the hyperbolic cosine from
 * @param mode          What unit of measurement is the angle specified in
 *                      Defaults to radians
 *
 * @return              The hyperbolic cosine of a given angle
 */
native Float:floatcosh(Float:angle, anglemode:mode=radian);

/**
 * Returns the hyperbolic tangent of a given angle
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The angle to calculate the hyperbolic tangent from
 * @param mode          What unit of measurement is the angle specified in
 *                      Defaults to radians
 *
 * @return              The hyperbolic tangent of a given angle
 */
native Float:floattanh(Float:angle, anglemode:mode=radian);

/**
 * Returns the absolute value of a floating point value
 *
 * @param value         The floating point value to get the absolute value from
 *
 * @return              The absolute value
 */
native Float:floatabs(Float:value);

/* Return the angle of a sine, cosine or tangent.
 * The output angle may be in radians, degrees, or grades. */

/**
 * Returns the angle of the given tangent
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The tangent to calculate the angle from
 * @param mode          What unit of measurement should the output angle be in
 *
 * @return              The angle of a tangent
 */
native Float:floatatan(Float:angle, {anglemode,_}:radix);

/**
 * Returns the angle of the given cosine
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The cosine to calculate the angle from
 * @param mode          What unit of measurement should the output angle be in
 *
 * @return              The angle of a cosine
 */
native Float:floatacos(Float:angle, {anglemode,_}:radix);

/**
 * Returns the angle of the given sine
 *
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param value         The sine to calculate the angle from
 * @param mode          What unit of measurement should the output angle be in
 *
 * @return              The angle of a sine
 */
native Float:floatasin(Float:angle, {anglemode,_}:radix);

/**
 * Computes the principal value of arctangent of y/x
 *
 * @note Someone should verify this native, not sure what it actually does.
 * @note For available units of measurements(modes) look at the anglemode enum
 *
 * @param x             Value representing the proportion of the x-coordinate.
 * @param y             Value representing the proportion of the x-coordinate.
 * @param mode          What unit of measurement should the output angle be in
 *
 * @return              Arctangent of y/x
 */
native Float:floatatan2(Float:x, Float:y, {anglemode,_}:radix);



/* Multiply two floats together */
native Float:floatmul(Float:oper1, Float:oper2);

/* Divide the dividend float by the divisor float */
native Float:floatdiv(Float:dividend, Float:divisor);

/* Add two floats together */
native Float:floatadd(Float:dividend, Float:divisor);

/* Subtract oper2 float from oper1 float */
native Float:floatsub(Float:oper1, Float:oper2);

/* user defined operators */
native Float:operator*(Float:oper1, Float:oper2) = floatmul;
native Float:operator/(Float:oper1, Float:oper2) = floatdiv;
native Float:operator+(Float:oper1, Float:oper2) = floatadd;
native Float:operator-(Float:oper1, Float:oper2) = floatsub;

stock Float:operator++(Float:oper)
	return oper+1.0;

stock Float:operator--(Float:oper)
	return oper-1.0;

stock Float:operator-(Float:oper)
    return oper^Float:cellmin; /* IEEE values are sign/magnitude */

stock Float:operator*(Float:oper1, oper2)
	return floatmul(oper1, float(oper2)); /* "*" is commutative */

stock Float:operator/(Float:oper1, oper2)
	return floatdiv(oper1, float(oper2));

stock Float:operator/(oper1, Float:oper2)
	return floatdiv(float(oper1), oper2);

stock Float:operator+(Float:oper1, oper2)
	return floatadd(oper1, float(oper2)); /* "+" is commutative */

stock Float:operator-(Float:oper1, oper2)
	return floatsub(oper1, float(oper2));

stock Float:operator-(oper1, Float:oper2)
	return floatsub(float(oper1), oper2);

stock bool:operator==(Float:oper1, Float:oper2)
	return floatcmp(oper1, oper2) == 0;

stock bool:operator==(Float:oper1, oper2)
	return floatcmp(oper1, float(oper2)) == 0; /* "==" is commutative */

stock bool:operator!=(Float:oper1, Float:oper2)
	return floatcmp(oper1, oper2) != 0;

stock bool:operator!=(Float:oper1, oper2)
	return floatcmp(oper1, float(oper2)) != 0; /* "==" is commutative */

stock bool:operator>(Float:oper1, Float:oper2)
	return floatcmp(oper1, oper2) > 0;

stock bool:operator>(Float:oper1, oper2)
	return floatcmp(oper1, float(oper2)) > 0;

stock bool:operator>(oper1, Float:oper2)
	return floatcmp(float(oper1), oper2) > 0;

stock bool:operator>=(Float:oper1, Float:oper2)
	return floatcmp(oper1, oper2) >= 0;

stock bool:operator>=(Float:oper1, oper2)
	return floatcmp(oper1, float(oper2)) >= 0;

stock bool:operator>=(oper1, Float:oper2)
	return floatcmp(float(oper1), oper2) >= 0;

stock bool:operator<(Float:oper1, Float:oper2)
	return floatcmp(oper1, oper2) < 0;

stock bool:operator<(Float:oper1, oper2)
	return floatcmp(oper1, float(oper2)) < 0;

stock bool:operator<(oper1, Float:oper2)
	return floatcmp(float(oper1), oper2) < 0;

stock bool:operator<=(Float:oper1, Float:oper2)
	return floatcmp(oper1, oper2) <= 0;

stock bool:operator<=(Float:oper1, oper2)
	return floatcmp(oper1, float(oper2)) <= 0;

stock bool:operator<=(oper1, Float:oper2)
	return floatcmp(float(oper1), oper2) <= 0;

stock bool:operator!(Float:oper)
	return (_:oper & ((-1)/2)) == 0;      /* -1 = all bits to 1; /2 = remove most significant bit (sign)
                                          works on both 32bit and 64bit systems; no constant required */
/* forbidden operations */
forward operator%(Float:oper1, Float:oper2);
forward operator%(Float:oper1, oper2);
forward operator%(oper1, Float:oper2);


/**
 * Returns whichever value is the smaller one
 *
 * @param ValueA        The first value
 * @param ValueB        The second value
 *
 * @return              ValueA if it is smaller than ValueB, and vice versa
 */
stock Float:floatmin(Float:ValueA, Float:ValueB)
{
	if (ValueA<=ValueB)
	{
		return ValueA;
	}
	
	return ValueB;
}

/**
 * Returns whichever value is the greater one
 *
 * @param ValueA        The first value
 * @param ValueB        The second value
 *
 * @return              ValueA if it is greater than ValueB, and vice versa
 */
stock Float:floatmax(Float:ValueA, Float:ValueB)
{
	if (ValueA>=ValueB)
	{
		return ValueA;
	}
	
	return ValueB;
}

/**
 * Clamps a value between a minimum and a maximum floating point value
 *
 * @param Value         The value to be clamped
 * @param MinValue      Minimum value
 * @param MaxValue      Maximum value
 *
 * @return              The Value clamped between MinValue and MaxValue
 */
stock Float:floatclamp(Float:Value, Float:MinValue, Float:MaxValue)
{
	if (Value<=MinValue)
	{
		return MinValue;
	}
	if (Value>=MaxValue)
	{
		return MaxValue;
	}
	
	return Value;
}