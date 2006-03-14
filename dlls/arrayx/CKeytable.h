#ifndef _KEYCLASS_INCLUDED
#define _KEYCLASS_INCLUDED

#include "JudyIncludes.h"
#include "CBaseMap.h"
#include "JudyExtra.h"
//#include <JudySL.h>

class Keytable: public CBaseMap
{
private:
	Pvoid_t Table;

	void ThrowSearchError(char* type);
	void ThrowIndexError( char* index, bool disable_check);

public:
	Keytable()				{ Table = NULL; }
	~Keytable()				{ Clear(); }

	Word_t Clear()			{ JudyClearMap(this); return JudySLFreeArray(&Table, PJE0); }
	Word_t MemoryUsed()		{ return JudyLMemUsed(Table); }

	int Delete(char* Key)	{ delete Get(Key,true); return JudySLDel(&Table, Key, PJE0 ); }

	void Set(char* Index, Pvoid_t value, bool disable_check)
	{
		PPvoid_t PValue = JudySLIns(&Table, Index,PJE0);
		*PValue = value;
	}

	Pvoid_t Get(char* Index, bool disable_check = false)
	{
		PPvoid_t PValue = JudySLGet(Table, Index, PJE0);
		if(PValue == NULL) { ThrowIndexError(Index, disable_check); return NULL; }

		return *PValue;	
	}

	template <class Type> 
	void Set(char* Index, Type value)
	{
		PPvoid_t PValue = JudySLIns(&Table, Index,PJE0);
		*PValue = reinterpret_cast<void*>(value);
	}
	
	template <class Type> 
	Type Get(char* Index, Type example, bool disable_check = false)
	{
		PPvoid_t PValue = JudySLGet(Table, Index, PJE0);
		if(PValue == NULL) { ThrowIndexError(Index, disable_check); return (Type)NULL; }

		return (Type)*PValue;	
	}

	char* First(char* Start = "");
	char* Next(char* Start = "");
	char* Prev(char* Start = "");
	char* Last(char* Start = "");

	bool IsFilled(char* Index)		{ return ( (Get(Index,(PPvoid_t)(NULL), true ) != NULL) ? true : false); } 
	bool IsEmpty(char* Index)		{ return ( (Get(Index,(PPvoid_t)(NULL), true ) == NULL) ? true : false); }
};

#endif