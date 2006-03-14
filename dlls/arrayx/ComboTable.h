#ifndef _COMBOTABLE_INCLUDED
#define _COMBOTABLE_INCLUDED

#include "CKeytable.h"
#include "CHashtable.h"
#include "CBaseMap.h"

class ComboTable: public CBaseMap
{
private:
	Keytable MasterKey;
	Hashtable MasterHash;

public:
	ComboTable()					{ }
	~ComboTable()				{ Clear(); }

	Word_t Clear()				{ return (MasterKey.Clear() + MasterHash.Clear() ); }
	Word_t MemoryUsed()			{ return (MasterKey.MemoryUsed() + MasterHash.MemoryUsed() ); }

	int Delete(char* Key)		{ return (MasterKey.Delete(Key) + MasterHash.Delete(Key) ); }

	bool IsFilled(char* Index)	{ return ( (MasterHash.Get(Index,(PPvoid_t)(NULL), true ) != NULL) ? true : false);} 
	bool IsEmpty(char* Index)	{ return ( (MasterHash.Get(Index,(PPvoid_t)(NULL), true ) == NULL) ? true : false);}

	void Set(char* Index, Pvoid_t value, bool disable_check)
	{
		MasterHash.Set(Index, value);
		MasterKey.Set(Index, value);
	}

	Pvoid_t Get(char* Index, bool disable_check = false)
	{
		return MasterHash.Get(Index, disable_check);
	}

	template <class Type> 
	void Set(char* Index, Type value)
	{
		MasterHash.Set(Index, value);
		MasterKey.Set(Index, value);
	}
	
	template <class Type> 
	Type Get(char* Index, Type example, bool disable_check = false)
	{
		return MasterHash.Get(Index, example, disable_check);
	}

	char* First( char* Start = "") { return MasterKey.First(Start);}
	char* Next( char* Start = "") { return MasterKey.Next(Start);}
	char* Prev( char* Start = "") { return MasterKey.Prev(Start); }
	char* Last( char* Start = "") { return MasterKey.Last(Start);}

protected:
	void ThrowIndexError( char* index, bool disable_check = false )
	{
		if(disable_check == true) return;

		char value[100];
		sprintf(value,"Function attempted to read non existant index %s", index );

		throw JudyEx(value, true);
	}
};

#endif
