/*  Float arithmetic for the Small AMX engine
 *
 *  Copyright (c) Artran, Inc. 1999
 *  Written by Greg Garner (gmg@artran.com)
 *  This file may be freely used. No warranties of any kind.
 *
 */

#include <math.h>
#include <stdlib.h>
#include <extdll.h>
#include <meta_api.h>
#include "amxmod.h"

inline cell FloatToCell(float fValue) {
  return *(cell *)((void *)&fValue);
}

inline float CellToFloat(cell cellValue){
  return *(float *)((void *)&cellValue);
}

static cell _float(AMX *,cell *params){
  return FloatToCell((float)params[1]);
}

static cell _floatstr(AMX *amx,cell *params){
  int len;
  return FloatToCell((float)atof(get_amxstring(amx,params[1],0,len)));
}

static cell _floatmul(AMX *,cell *params){
  return FloatToCell(CellToFloat(params[1]) * CellToFloat(params[2]));
}

static cell _floatdiv(AMX *,cell *params){
  return FloatToCell(CellToFloat(params[1]) / CellToFloat(params[2]));
}

static cell _floatadd(AMX *,cell *params){
  return FloatToCell(CellToFloat(params[1]) + CellToFloat(params[2]));
}

static cell _floatsub(AMX *,cell *params){
  return FloatToCell(CellToFloat(params[1]) - CellToFloat(params[2]));
}

static cell _floatfract(AMX *,cell *params){
  float fA = CellToFloat(params[1]);
  fA -= (float)(floor((double)fA));
  return FloatToCell(fA);
}

static cell _floatround(AMX *,cell *params){
  float fA = CellToFloat(params[1]);
  switch (params[2]) {
    case 1:
      fA = (float)(floor((double)fA));
      break;
    case 2:
      float fValue;
      fValue = (float)(floor((double)fA));
      if ( (fA>=0) && ((fA-fValue)!=0) )
          fValue++;
      fA = fValue;
      break;
    default:
      fA = (float)(floor((double)fA+.5));
      break;
  }
  return (long)fA;
}

static cell _floatcmp(AMX *,cell *params){
  float fA = CellToFloat(params[1]);
  float fB = CellToFloat(params[2]);
  if (fA == fB)
      return 0;
  else if (fA > fB)
      return 1;
  else
      return -1;
}

AMX_NATIVE_INFO float_Natives[] = {
  { "float",         _float },
  { "floatstr",      _floatstr },
  { "floatmul",      _floatmul },
  { "floatdiv",      _floatdiv },
  { "floatadd",      _floatadd },
  { "floatsub",      _floatsub },
  { "floatfract",    _floatfract},
  { "floatround",    _floatround},
  { "floatcmp",      _floatcmp},
  { NULL, NULL }
};