#ifndef _ARRAYCLASS_H
#define _ARRAYCLASS_H

#include "JudyIncludes.h"
#include "CBaseList.h"
#include "JudyExtra.h"
//#include <JudyL.h>

class Array: public CBaseList
{
private:
	Pvoid_t Table;
	
	void ThrowIndexError( cell index, bool disable_check = false );
	void ThrowSearchError(char* msg);

public:
	Array()					{ Table = NULL; }
	~Array()				{ Clear();		}
	void Remove()			{ delete this;	}

	Word_t Clear()			{ JudyClearList(this); return JudyLFreeArray(&Table, PJE0); }
	Word_t MemoryUsed()		{ return JudyLMemUsed(Table); }

	int Delete(cell Key)	{ return JudyLDel(&Table, Key, PJE0 ); }

	void Set(cell Index, Pvoid_t value, bool disable_check) 
	{
	     PPvoid_t PValue = JudyLIns(&Table, Index,PJE0);	
	     *PValue = value;
	} 

	Pvoid_t Get(cell Index, bool disable_check = false)
	{
		PPvoid_t PValue = JudyLGet(Table, Index, PJE0);
		if(PValue == NULL) { ThrowIndexError(Index, disable_check); return NULL; }

		return *PValue;	
	}

	template<class Type> 
	void Set(cell Index, Type value) 
	{
		PPvoid_t PValue = JudyLIns(&Table, Index,PJE0);  
		*PValue = reinterpret_cast<void*>(value); 
	}
	
	template <class Type> 
	Type Get(cell Index, Type example, bool disable_check = false)
	{
		PPvoid_t PValue = JudyLGet(Table, Index, PJE0);
		if(PValue == NULL) { ThrowIndexError(Index, disable_check); return (Type)NULL; }

		return (Type)(*PValue);
	}

	cell First(cell Start = 0);
	cell Next(cell Start = 0);
	cell Prev(cell Start = -1);
	cell Last(cell Start = -1);

	cell FirstEmpty(cell Start = 0);
	cell NextEmpty(cell Start = 0);
	cell PrevEmpty(cell Start = -1);
	cell LastEmpty(cell Start = -1);

	cell ByCount(cell n, cell Start = 0);
	cell Count(cell Start = 0, cell Stop = -1)	{ return JudyLCount(Table, Start, Stop, PJE0); }

	bool IsFilled(cell Index)			{ return ( (Get(Index, true ) != NULL) ? true : false); } 
	bool IsEmpty(cell Index)			{ return ( (Get(Index, true ) == NULL) ? true : false); }
};

#endif