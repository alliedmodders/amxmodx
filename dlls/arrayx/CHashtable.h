#ifndef _HASHCLASS_INCLUDED
#define _HASHCLASS_INCLUDED

#include "JudyIncludes.h"
#include "CBaseMap.h"
//#include <JudyHS.h>

class Hashtable: public CBaseMap
{
private:
	Pvoid_t Table;

public:
	Hashtable()					{ Table = NULL; }
	~Hashtable()				{ Clear(); }

	Word_t Clear()				{ return JudyHSFreeArray(&Table, PJE0); }
	Word_t MemoryUsed()			{ return JudyLMemUsed(Table); }

	int Delete(char* Key)		{ delete Get(Key,true); return JudyHSDel(&Table, Key, strlen(Key), PJE0 ); }

	void Set(char* Index, Pvoid_t value, bool disable_check)
	{
		int Len = strlen(Index) + 1;
		PPvoid_t PValue = JudyHSIns(&Table, Index, Len, PJE0);
		*PValue = value;
	}

	Pvoid_t Get(char* Index, bool disable_check = false)
	{
		PPvoid_t PValue = JudyHSGet(Table, Index, strlen(Index)+1);
		if(PValue == NULL) { ThrowIndexError(Index, disable_check); return NULL; }

		return *PValue;	
	}

	template <class Type> 
	void Set(char* Index, Type value)
	{
		int Len = strlen(Index) + 1;
		PPvoid_t PValue = JudyHSIns(&Table, Index, Len, PJE0);
		*PValue = reinterpret_cast<void*>(value);
	}
	
	template <class Type> 
	Type Get(char* Index, Type example, bool disable_check = false)
	{
		PPvoid_t PValue = JudyHSGet(Table, Index, strlen(Index)+1);
		if(PValue == NULL) { ThrowIndexError(Index, disable_check); return (Type)NULL; }

		return (Type)(*PValue);	
	}

	char* First( char* Start = "")	{ ThrowSearchError(); return (char*)NULL; }
	char* Next( char* Start = "")	{ ThrowSearchError(); return (char*)NULL; }
	char* Prev( char* Start)		{ ThrowSearchError(); return (char*)NULL; }
	char* Last( char* Start)		{ ThrowSearchError(); return (char*)NULL; }
	
	bool IsFilled(char* Index)	{ return ( (Get(Index,(PPvoid_t)(NULL), true ) != NULL) ? true : false);} 
	bool IsEmpty(char* Index)	{ return ( (Get(Index,(PPvoid_t)(NULL), true ) == NULL) ? true : false);}

protected:
	void ThrowIndexError( char* index, bool disable_check = false )
	{
		if(disable_check == true) return;

		char value[100];
		sprintf(value,"Function attempted to read non existant index %s", index );

		throw JudyEx(value, true);
	}
	void ThrowSearchError( void )
	{ 
		char value[50];
		sprintf(value,"Function attempted to search HashTable!: Invalid action!");

		throw JudyEx(value,true);
	}
};

#endif