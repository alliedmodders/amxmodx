/* Float arithmetic
 *
 * (c) Copyright 1999, Artran, Inc.
 * Written by Greg Garner (gmg@artran.com)
 * Modified in March 2001 to include user defined
 * operators for the floating point functions.
 *
 * This file is provided as is (no warranties).
 */

#if defined _float_included
  #endinput
#endif
#define _float_included

native Float:float(value);
native Float:floatstr(const string[]);
native Float:floatmul(Float:oper1, Float:oper2);
native Float:floatdiv(Float:dividend, Float:divisor);
native Float:floatadd(Float:dividend, Float:divisor);
native Float:floatsub(Float:oper1, Float:oper2);
native Float:floatfract(Float:value);

enum floatround_method {
  floatround_round,
  floatround_floor,
  floatround_ceil
}

native floatround(Float:value, floatround_method:method=floatround_round);
native floatcmp(Float:fOne, Float:fTwo);

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
    return oper^Float:0x80000000;  /* IEEE values are sign/magnitude */

stock Float:operator*(Float:oper1, oper2)
    return floatmul(oper1, float(oper2));       /* "*" is commutative */

stock Float:operator/(Float:oper1, oper2)
    return floatdiv(oper1, float(oper2));

stock Float:operator/(oper1, Float:oper2)
    return floatdiv(float(oper1), oper2);

stock Float:operator+(Float:oper1, oper2)
    return floatadd(oper1, float(oper2));       /* "+" is commutative */

stock Float:operator-(Float:oper1, oper2)
    return floatsub(oper1, float(oper2));

stock Float:operator-(oper1, Float:oper2)
    return floatsub(float(oper1), oper2);

stock bool:operator==(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) == 0;

stock bool:operator==(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) == 0;  /* "==" is commutative */

stock bool:operator!=(Float:oper1, Float:oper2)
    return floatcmp(oper1, oper2) != 0;

stock bool:operator!=(Float:oper1, oper2)
    return floatcmp(oper1, float(oper2)) != 0;  /* "==" is commutative */

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
    return floatcmp(oper, 0.0) == 0;

/* forbidden operations */
forward operator%(Float:oper1, Float:oper2);
forward operator%(Float:oper1, oper2);
forward operator%(oper1, Float:oper2);