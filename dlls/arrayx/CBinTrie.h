#ifndef _BINTRIECLASS_H
#define _BINTRIECLASS_H

#include "JudyIncludes.h"
#include "JudyExtra.h"
//#include <Judy1.h>

class BinTrie
{
private:
	Pvoid_t Table;
	
	void ThrowSearchError(char* msg);

public:
	BinTrie()				{ Table = NULL; }
	~BinTrie()				{ Clear();		}
	void Remove()			{ delete this;	}

	Word_t Clear()			{ JudyClearBinTrie(this); return Judy1FreeArray(&Table, PJE0); }
	Word_t MemoryUsed()		{ return Judy1MemUsed(Table); }

	cell Delete(cell Key)	{ return Judy1Unset(&Table, Key, PJE0 ); }

	cell Set(cell Index, bool val)
	{
		if(val == false) return Delete(Index);
		else return Judy1Set(&Table, Index,PJE0);
	}
	
	cell Get(cell Index)
	{
		cell PValue = Judy1Test(Table, Index, PJE0);
		return PValue;	
	}

	cell First(cell Start = 0);
	cell Next(cell Start = 0);
	cell Prev(cell Start = -1);
	cell Last(cell Start = -1);

	cell FirstEmpty(cell Start = 0);
	cell NextEmpty(cell Start = 0);
	cell PrevEmpty(cell Start = -1);
	cell LastEmpty(cell Start = -1);

	cell ByCount(cell n, cell Start);
	cell Count(cell Start = 0, cell Stop = -1)	{ return Judy1Count(Table, Start, Stop, PJE0); }

	bool IsFilled(cell Index)			{ return ( (Get(Index )) ? true : false); } 
	bool IsEmpty(cell Index)			{ return ( (Get(Index )) ? true : false); }
};

#endif