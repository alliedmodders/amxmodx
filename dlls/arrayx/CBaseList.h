#ifndef _BASE_ARRAYCLASS_H
#define _BASE_ARRAYCLASS_H

#include "JudyIncludes.h"

class CBaseList
{
public:
	virtual Word_t Clear() =0;
	virtual Word_t MemoryUsed() =0;

	virtual int Delete(cell Key) =0;

	virtual void Set(cell Index, Pvoid_t value, bool disable_check = false) =0;
	
	virtual Pvoid_t Get(cell Index, bool disable_check = false) =0;

	virtual cell First(cell Start = 0) =0;
	virtual cell Next(cell Start = 0) =0;
	virtual cell Prev(cell Start = -1) =0;
	virtual cell Last(cell Start = -1) =0;

	virtual cell FirstEmpty(cell Start = 0) =0;
	virtual cell NextEmpty(cell Start = 0) =0;
	virtual cell PrevEmpty(cell Start = -1) =0;
	virtual cell LastEmpty(cell Start = -1) =0;

	virtual cell ByCount(cell n, cell Start = 0) =0;
	virtual cell Count(cell Start = 0, cell Stop = -1) =0;

	virtual bool IsFilled(cell Index) =0; 
	virtual bool IsEmpty(cell Index) =0;
};

#endif
