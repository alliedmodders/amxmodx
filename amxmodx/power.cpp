/* This file implements two native functions. It is provided as
 * an example to show how to add native functions. See the manual
 * for more information.
 *
 *  Copyright (c) ITB CompuPhase, 1998, 1999
 *  This file may be freely used. No warranties of any kind.
 */

#include <stdlib.h>
// this file does not include amxmodx.h, so we have to include the memory manager here
#ifdef MEMORY_TEST
#include "mmgr/mmgr.h"
#endif // MEMORY_TEST

#include "amx.h"

static cell power(AMX *amx, cell *params)
{
  /* power(value, exponent);
   *   params[1] = value
   *   params[2] = exponent
   */
  cell result = 1;
  while (params[2]-- > 0)
    result *= params[1];
  return result;
}

static cell sqroot(AMX *amx, cell *params)
{
  /* sqroot(value);
   *   params[1] = value
   * This routine uses a simple successice approximation algorithm.
   */
  cell div = params[1];
  cell result = 1;
  while (div > result) {        /* end when div == result, or just below */
    div = (div + result) / 2;   /* take mean value as new divisor */
    result = params[1] / div;
  } /* while */
  return div;
}

AMX_NATIVE_INFO power_Natives[] = {
  { "power",  power },
  { "sqroot", sqroot },
  { 0, 0 }        /* terminator */
};

