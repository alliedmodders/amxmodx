/* AMX Mod math functions backwards compatibility
 *
 * by the AMX Mod X Development Team
 *
 * This file is provided as is (no warranties).
 */

#if defined _maths_bcompat_included
  #endinput
#endif
#define _maths_bcompat_included

#if !defined _float_included
	#include <float>
#endif

stock Float:fabs(Float:value)
{
	return floatabs(value);
}

stock Float:asin(Float:value)
{
	return floatasin(value, radian);
}

stock Float:sin(Float:value)
{
	return floatsin(value, radian);
}

stock Float:sinh(Float:value)
{
	return floatsinh(value, radian);
}

stock Float:acos(Float:value)
{
	return floatacos(value, radian);
}

stock Float:cos(Float:value)
{
	return floatcos(value, radian);
}

stock Float:cosh(Float:value)
{
	return floatcosh(value, radian);
}

stock Float:atan(Float:value)
{
	return floatatan(value, radian);
}

stock Float:atan2(Float:value1, Float:value2)
{
	return floatatan2(value1, value2, radian);
}

stock Float:tan(Float:value)
{
	return floattan(value, radian);
}

stock Float:tanh(Float:value)
{
	return floattanh(value, radian);
}

stock Float:fsqroot(Float:value)
{
	return floatsqroot(value);
}

stock Float:fpower(Float:value, Float:exponent)
{
	return floatpower(value, exponent);
}

stock Float:flog(Float:value, Float:base=10.0)
{
	return floatlog(value, base);
}
