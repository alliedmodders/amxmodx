#ifndef _COMBOARRAY_INCLUDED
#define _COMBOARRAY_INCLUDED

#include "CBinTrie.h"
#include "CArray.h"
#include "CBaseList.h"

class ComboArray: public CBaseList
{
private:
	BinTrie MasterBin;
	Array MasterArray;
	
public:
	ComboArray()			{				}
	~ComboArray()			{ Clear();		}
	void Remove()			{ delete this;	}

	Word_t Clear()			{ return (MasterBin.Clear() + MasterArray.Clear() ); }
	Word_t MemoryUsed()		{ return (MasterBin.MemoryUsed() + MasterArray.MemoryUsed() ); }

	int Delete(cell Key)	{  return (MasterBin.Delete(Key) + MasterArray.Delete(Key) ); }

	void Set(cell Index, Pvoid_t value, bool disable_check)
	{
		MasterBin.Set(Index, true); 
		MasterArray.Set(Index, value, disable_check);
	}

	Pvoid_t Get(cell Index, bool disable_check = false)
	{
		if(MasterBin.Get(Index) == NULL) { ThrowIndexError(Index, disable_check); return NULL; }

		return MasterArray.Get(Index);
	}

	template <class Type> 
	void Set(cell Index, Type value)
	{
		MasterBin.Set(Index, true); 
		MasterArray.Set(Index, value);
	}
	
	template <class Type> 
	Type Get(cell Index, Type example, bool disable_check = false)
	{
		if(MasterBin.Get(Index) == NULL) { ThrowIndexError(Index, disable_check); return (Type)NULL; }

		return MasterArray.Get(Index,example);
	}

	cell First(cell Start = 0)				{ return MasterBin.First(Start); }
	cell Next(cell Start = 0)				{ return MasterBin.Next(Start); }
	cell Prev(cell Start = -1)				{ return MasterBin.Prev(Start); }
	cell Last(cell Start = -1)				{ return MasterBin.Last(Start); }

	cell FirstEmpty(cell Start = 0)			{ return MasterBin.FirstEmpty(Start); }
	cell NextEmpty(cell Start = 0)			{ return MasterBin.NextEmpty(Start); }
	cell PrevEmpty(cell Start = -1)			{ return MasterBin.PrevEmpty(Start); }
	cell LastEmpty(cell Start  = -1)			{ return MasterBin.LastEmpty(Start); }

	cell ByCount(cell n, cell Start = 0)	{ return MasterBin.ByCount(n, Start); }
	cell Count(cell Start = 0, cell Stop = -1)	{ return MasterBin.Count(Start, Stop); }

	bool IsFilled(cell Index)			{ return ( (MasterBin.Get(Index) != NULL) ? true : false); } 
	bool IsEmpty(cell Index)			{ return ( (MasterBin.Get(Index) == NULL) ? true : false); }

protected:
	void ThrowIndexError(cell Index, bool disable_check = false)
	{ 
		if(disable_check == true) return;

		char error[50];
		sprintf(error,"Index %i is not set.",Index);

		throw JudyEx(error,true);
	}
};

#endif