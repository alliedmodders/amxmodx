#include "CKeytable.h"

void Keytable::ThrowIndexError( char* index, bool disable_check = false )
{ 
	if(disable_check == true) return;

	char error[50];
	sprintf(error,"Index %s is not set.",index);

	throw JudyEx(error,true);
}

void Keytable::ThrowSearchError(char* type)
{ 
	char value[50];
	sprintf(value,"Function attempted to search %s: Judy returned NULL value", type);

	throw JudyEx(value,false);
}

char* Keytable::First( char* Start)
{
	PPvoid_t index = JudySLFirst(Table, Start, PJE0); 
	if (index == NULL) 
	{
		sprintf(Start,"dne");
		ThrowSearchError("Type:First");
	}

	return Start;
}

char* Keytable::Next( char* Start)
{
	PPvoid_t index = JudySLNext(Table, Start, PJE0); 
	if (index == NULL) 
	{
		sprintf(Start,"dne");
		ThrowSearchError("Type:Next");
	}
	return Start;
}

char* Keytable::Prev( char* Start)
{
	PPvoid_t index = JudySLPrev(Table, Start, PJE0); 
	if (index == NULL) 
	{
		sprintf(Start,"dne");
		ThrowSearchError("Type:Prev");
	}

	return Start;
}

char* Keytable::Last( char* Start)
{
	PPvoid_t index = JudySLLast(Table, Start, PJE0); 
	if (index == NULL) 
	{
		sprintf(Start,"dne");
		ThrowSearchError("Type:Last");
	}

	return Start;
}