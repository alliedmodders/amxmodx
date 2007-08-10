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

/* Different methods of rounding */
enum floatround_method {
	floatround_round = 0,
	floatround_floor,
	floatround_ceil,
	floatround_tozero
};

enum anglemode {
	radian = 0,
	degrees,
	grades
};

/* Convert an integer into a floating point value */
native Float:float(value);

/* Convert a string into a floating point value */
native Float:floatstr(const string[]);

/* Multiple two floats together */
native Float:floatmul(Float:oper1, Float:oper2);

/* Divide the dividend float by the divisor float */
native Float:floatdiv(Float:dividend, Float:divisor);

/* Add two floats together */
native Float:floatadd(Float:dividend, Float:divisor);

/* Subtract oper2 float from oper1 float */
native Float:floatsub(Float:oper1, Float:oper2);

/* Return the fractional part of a float */
native Float:floatfract(Float:value);

/* Round a float into a integer value */
native floatround(Float:value, floatround_method:method=floatround_round);

/* Compare two integers. If the two elements are equal, return 0.
* If the first argument is greater than the second argument, return 1,
* If the first argument is less than the second argument, return -1. */
native floatcmp(Float:fOne, Float:fTwo);

/* Return the square root of the input value, same as floatpower(value, 0.5) */
native Float:floatsqroot(Float:value);

/* Return the value raised to the power of the exponent */
native Float:floatpower(Float:value, Float:exponent);

/* Return the logarithm */
native Float:floatlog(Float:value, Float:base=10.0);

/* Return the sine, cosine or tangent.
 * The input angle may be in radians, degrees or grades. */
native Float:floatsin(Float:value, anglemode:mode=radian);
native Float:floatcos(Float:value, anglemode:mode=radian);
native Float:floattan(Float:value, anglemode:mode=radian);

/* Return the hyperbolic sine, cosine or tangent.
 * The input angle may be in radians, degrees or grades. */
native Float:floatsinh(Float:angle, anglemode:mode=radian);
native Float:floatcosh(Float:angle, anglemode:mode=radian);
native Float:floattanh(Float:angle, anglemode:mode=radian);

/* Return the absolute value */
native Float:floatabs(Float:value);

/* Return the angle of a sine, cosine or tangent.
 * The output angle may be in radians, degrees, or grades. */
native Float:floatatan(Float:angle, {anglemode,_}:radix);
native Float:floatacos(Float:angle, {anglemode,_}:radix);
native Float:floatasin(Float:angle, {anglemode,_}:radix);
native Float:floatatan2(Float:x, Float:y, {anglemode,_}:radix);

#pragma rational Float

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
	return (_:oper & ((-1)/2)) == 0;		/* -1 = all bits to 1; /2 = remove most significant bit (sign)
							works on both 32bit and 64bit systems; no constant required */
/* forbidden operations */
forward operator%(Float:oper1, Float:oper2);
forward operator%(Float:oper1, oper2);
forward operator%(oper1, Float:oper2);


stock Float:floatmin(Float:ValueA, Float:ValueB)
{
	if (ValueA<=ValueB)
	{
		return ValueA;
	}
	
	return ValueB;
}

stock Float:floatmax(Float:ValueA, Float:ValueB)
{
	if (ValueA>=ValueB)
	{
		return ValueA;
	}
	
	return ValueB;
}
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
