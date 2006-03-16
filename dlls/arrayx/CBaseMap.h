#ifndef _BASE_MAPCLASS_H
#define _BASE_MAPCLASS_H

#include "JudyIncludes.h"

class CBaseMap
{
public:
	virtual Word_t Clear() =0;
	virtual Word_t MemoryUsed() =0;

	virtual int Delete(char* Key) =0;
	virtual void Remove() =0;

	virtual void Set(char* Index, Pvoid_t value, bool disable_check = false) =0;
	
	virtual Pvoid_t Get(char* Index, bool disable_check = false) =0;

	virtual char* First(char* Start = "") =0;
	virtual char* Next(char* Start) =0;
	virtual char* Prev(char* Start) =0;
	virtual char* Last(char* Start) =0;

	virtual bool IsFilled(char* Index) =0; 
	virtual bool IsEmpty(char* Index) =0;
};

#endif

