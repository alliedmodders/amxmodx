/* Core functions
*
* (c) Copyright 1998-2003, ITB CompuPhase
*
* This file is provided as is (no warranties).
*/

#if defined _core_included
  #endinput
#endif
#define _core_included

native heapspace();

native funcidx(const name[]);

native numargs();
native getarg(arg, index=0);
native setarg(arg, index=0, value);

native strlen(const string[]);

native tolower(c);
native toupper(c);
native swapchars(c);

native random(max);

native min(value1, value2);
native max(value1, value2);
native clamp(value, min=cellmin, max=cellmax);

native power(value, exponent);
native sqroot(value);

native time(&hour=0,&minute=0,&second=0);
native date(&year=0,&month=0,&day=0);

native tickcount(&granularity=0);

stock abs(x)
{
	return x > 0 ? x : -x;
}
