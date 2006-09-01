/*  Float arithmetic for the Small AMX engine
 *
 *  Copyright (c) Artran, Inc. 1999
 *  Written by Greg Garner (gmg@artran.com)
 *  This file may be freely used. No warranties of any kind.
 *
 * CHANGES -
 * 2002-08-27: Basic conversion of source from C++ to C by Adam D. Moss
 *             <adam@gimp.org> <aspirin@icculus.org>
 * 2003-08-29: Removal of the dynamic memory allocation and replacing two
 *             type conversion functions by macros, by Thiadmer Riemersma
 * 2003-09-22: Moved the type conversion macros to AMX.H, and simplifications
 *             of some routines, by Thiadmer Riemersma
 * 2003-11-24: A few more native functions (geometry), plus minor modifications,
 *             mostly to be compatible with dynamically loadable extension
 *             modules, by Thiadmer Riemersma
 * 2004-01-09: Adaptions for 64-bit cells (using "double precision"), by
 *             Thiadmer Riemersma
 */
#include <stdlib.h>     /* for atof() */
#include <stdio.h>      /* for NULL */
#include <assert.h>
#include <math.h>

// this file does not include amxmodx.h, so we have to include the memory manager here
#ifdef MEMORY_TEST
#include "mmgr/mmgr.h"
#endif // MEMORY_TEST

#include "amx.h"

/*
  #if defined __BORLANDC__
    #pragma resource "amxFloat.res"
  #endif
*/

#define PI  3.1415926535897932384626433832795

static REAL FromRadians(REAL angle, int radix)
{
	switch (radix)
	{
		case 1:         /* degrees, sexagesimal system (technically: degrees/minutes/seconds) */
			return (REAL)(angle / PI * 180.0);
		case 2:         /* grades, centesimal system */
			return (REAL)(angle / PI * 200.0);
		default:        /* assume already radian */
			return angle;
	} /* switch */
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_float(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = long value to convert to a float
    */
    REAL fValue;

    /* Convert to a float. Calls the compilers long to float conversion. */
    fValue = (REAL) params[1];

    /* Return the cell. */
    return amx_ftoc(fValue);
}

/******************************************************************/
#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
static cell AMX_NATIVE_CALL n_floatstr(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = virtual string address to convert to a float
    */
    char szSource[60];
    cell *pString;
    REAL fNum;
    int nLen;

    /* They should have sent us 1 cell. */
    assert(params[0]/sizeof(cell)==1);

    /* Get the real address of the string. */
    amx_GetAddr(amx,params[1],&pString);

    /* Find out how long the string is in characters. */
    amx_StrLen(pString, &nLen);
    if (nLen == 0 || (unsigned int)nLen >= sizeof szSource)
        return 0;

    /* Now convert the Small String into a C type null terminated string */
    amx_GetStringOld(szSource, pString, 0);

    /* Now convert this to a float. */
    fNum = (REAL)atof(szSource);

    return amx_ftoc(fNum);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatmul(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1
    *   params[2] = float operand 2
    */
    REAL fRes = amx_ctof(params[1]) * amx_ctof(params[2]);
    return amx_ftoc(fRes);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatdiv(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float dividend (top)
    *   params[2] = float divisor (bottom)
    */
    REAL fRes = amx_ctof(params[1]) / amx_ctof(params[2]);
    return amx_ftoc(fRes);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatadd(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1
    *   params[2] = float operand 2
    */
    REAL fRes = amx_ctof(params[1]) + amx_ctof(params[2]);
    return amx_ftoc(fRes);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatsub(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1
    *   params[2] = float operand 2
    */
    REAL fRes = amx_ctof(params[1]) - amx_ctof(params[2]);
    return amx_ftoc(fRes);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
/* Return fractional part of float */
static cell AMX_NATIVE_CALL n_floatfract(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand
    */
    REAL fA = amx_ctof(params[1]);
    fA = fA - (REAL)(floor((double)fA));
    return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
/* Return integer part of float, rounded */
static cell AMX_NATIVE_CALL n_floatround(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand
    *   params[2] = Type of rounding (long)
    */
    REAL fA = amx_ctof(params[1]);

    switch (params[2])
    {
        case 1:       /* round downwards (truncate) */
            fA = (REAL)(floor((double)fA));
            break;
        case 2:       /* round upwards */
            fA = (REAL)(ceil((double)fA));
            break;
        case 3:       /* round towards zero */
            if ( fA>=0.0 )
                fA = (REAL)(floor((double)fA));
            else
                fA = (REAL)(ceil((double)fA));
            break;
        default:      /* standard, round to nearest */
            fA = (REAL)(floor((double)fA+.5));
            break;
    }

    return (long)fA;
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatcmp(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1
    *   params[2] = float operand 2
    */
    REAL fA, fB;

    fA = amx_ctof(params[1]);
    fB = amx_ctof(params[2]);
    if (fA == fB)
        return 0;
    else if (fA>fB)
        return 1;
    else
        return -1;

}

/******************************************************************/
static cell AMX_NATIVE_CALL n_floatsqroot(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand
    */
    REAL fA = amx_ctof(params[1]);
    fA = (REAL)sqrt(fA);
    if (fA < 0)
        return amx_RaiseError(amx, AMX_ERR_DOMAIN);
    return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatpower(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1 (base)
    *   params[2] = float operand 2 (exponent)
    */
    REAL fA = amx_ctof(params[1]);
    REAL fB = amx_ctof(params[2]);
    fA = (REAL)pow(fA, fB);
    return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatlog(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1 (value)
    *   params[2] = float operand 2 (base)
    */
    REAL fValue = amx_ctof(params[1]);
    REAL fBase = amx_ctof(params[2]);
    if (fValue <= 0.0 || fBase <= 0)
        return amx_RaiseError(amx, AMX_ERR_DOMAIN);
    if (fBase == 10.0) // ??? epsilon
        fValue = (REAL)log10(fValue);
    else
        fValue = (REAL)(log(fValue) / log(fBase));
    return amx_ftoc(fValue);
}

static REAL ToRadians(REAL angle, int radix)
{
    switch (radix)
    {
        case 1:         /* degrees, sexagesimal system (technically: degrees/minutes/seconds) */
            return (REAL)(angle * PI / 180.0);
        case 2:         /* grades, centesimal system */
            return (REAL)(angle * PI / 200.0);
        default:        /* assume already radian */
            return angle;
    } /* switch */
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatsin(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1 (angle)
    *   params[2] = float operand 2 (radix)
    */
    REAL fA = amx_ctof(params[1]);
    fA = ToRadians(fA, params[2]);
    fA = sin(fA);
    return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatcos(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1 (angle)
    *   params[2] = float operand 2 (radix)
    */
    REAL fA = amx_ctof(params[1]);
    fA = ToRadians(fA, params[2]);
    fA = cos(fA);
    return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floattan(AMX *amx,cell *params)
{
    /*
    *   params[0] = number of bytes
    *   params[1] = float operand 1 (angle)
    *   params[2] = float operand 2 (radix)
    */
    REAL fA = amx_ctof(params[1]);
    fA = ToRadians(fA, params[2]);
    fA = tan(fA);
    return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by BAILOPAN */
static cell AMX_NATIVE_CALL n_floatatan(AMX *amx, cell *params)
{
	/*
	 * params[1] = angle
	 * params[2] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	fA = atan(fA);
	fA = FromRadians(fA, params[2]);
	return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by BAILOPAN */
static cell AMX_NATIVE_CALL n_floatacos(AMX *amx, cell *params)
{
	/*
	 * params[1] = angle
	 * params[2] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	fA = acos(fA);
	fA = FromRadians(fA, params[2]);
	return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by BAILOPAN */
static cell AMX_NATIVE_CALL n_floatasin(AMX *amx, cell *params)
{
	/*
	 * params[1] = angle
	 * params[2] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	fA = asin(fA);
	fA = FromRadians(fA, params[2]);
	return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by BAILOPAN */
static cell AMX_NATIVE_CALL n_floatatan2(AMX *amx, cell *params)
{
	/*
	 * params[1] = x
	 * params[2] = y
	 * params[3] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	REAL fB = amx_ctof(params[2]);
	REAL fC;
	fC = atan2(fA, fB);
	fC = FromRadians(fC, params[3]);
	return amx_ftoc(fC);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by DS */
static cell AMX_NATIVE_CALL n_floatsinh(AMX *amx, cell *params)
{
	/*
	 * params[1] = angle
	 * params[2] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	fA = ToRadians(fA, params[2]);
	fA = sinh(fA);
	return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by DS */
static cell AMX_NATIVE_CALL n_floatcosh(AMX *amx, cell *params)
{
	/*
	 * params[1] = angle
	 * params[2] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	fA = ToRadians(fA, params[2]);
	fA = cosh(fA);
	return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/* Added by DS */
static cell AMX_NATIVE_CALL n_floattanh(AMX *amx, cell *params)
{
	/*
	 * params[1] = angle
	 * params[2] = radix
	 */
	REAL fA = amx_ctof(params[1]);
	fA = ToRadians(fA, params[2]);
	fA = tanh(fA);
	return amx_ftoc(fA);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
/******************************************************************/
static cell AMX_NATIVE_CALL n_floatabs(AMX *amx,cell *params)
{
    REAL fA = amx_ctof(params[1]);
    fA = (fA >= 0) ? fA : -fA;
    return amx_ftoc(fA);
}

AMX_NATIVE_INFO float_Natives[] = {
  { "float",       n_float      },
  { "floatstr",    n_floatstr   },
  { "floatmul",    n_floatmul   },
  { "floatdiv",    n_floatdiv   },
  { "floatadd",    n_floatadd   },
  { "floatsub",    n_floatsub   },
  { "floatfract",  n_floatfract },
  { "floatround",  n_floatround },
  { "floatcmp",    n_floatcmp   },
  { "floatsqroot", n_floatsqroot},
  { "floatpower",  n_floatpower },
  { "floatlog",    n_floatlog   },
  { "floatsin",    n_floatsin   },
  { "floatcos",    n_floatcos   },
  { "floattan",    n_floattan   },
  { "floatabs",    n_floatabs   },
  { "floatasin",   n_floatasin  },
  { "floatacos",   n_floatacos  },
  { "floatatan",   n_floatatan  },
  { "floatatan2",  n_floatatan2 },
  { "floatsinh",   n_floatsinh  },
  { "floatcosh",   n_floatcosh  },
  { "floattanh",   n_floattanh  },
  { NULL, NULL }        /* terminator */
};

int AMXEXPORT amx_FloatInit(AMX *amx)
{
  return amx_Register(amx,float_Natives,-1);
}

#if defined __BORLANDC__ || defined __WATCOMC__
  #pragma argsused
#endif
int AMXEXPORT amx_FloatCleanup(AMX *amx)
{
  return AMX_ERR_NONE;
}
