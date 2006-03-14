#include "CArray.h"

void Array::ThrowSearchError(char* type)
{ 
	char value[50];
	sprintf(value,"Function attempted to search %s: Judy returned NULL value", type);

	throw JudyEx(value,false);
}

void Array::ThrowIndexError( cell index, bool disable_check )
{ 
	if(disable_check == true) return;

	char error[50];
	sprintf(error,"Index %i is not set.",index);

	throw JudyEx(error,true);
}

cell Array::First( cell Start)
{
	PPvoid_t success = JudyLFirst(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:First");

	return Start;
}

cell Array::FirstEmpty( cell Start)
{
	cell success = JudyLFirstEmpty(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:FirstEmpty");
	return Start;
}

cell Array::Next( cell Start)
{
	PPvoid_t success = JudyLNext(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:Next");

	return Start;
}

cell Array::NextEmpty( cell Start)
{
	cell success = JudyLNextEmpty(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:NextEmpty");
	return Start;
}

cell Array::Prev( cell Start)
{
	PPvoid_t success = JudyLPrev(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:Prev");

	return Start;
}

cell Array::PrevEmpty( cell Start)
{
	cell success = JudyLPrevEmpty(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:PrevEmpty");
	return Start;
}

cell Array::Last( cell Start)
{
	PPvoid_t success = JudyLLast(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:Last");

	return Start;
}

cell Array::LastEmpty( cell Start)
{
	cell success = JudyLLastEmpty(Table, reinterpret_cast<Word_t *>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:LastEmpty");
	return Start;
}

cell Array::ByCount(cell n, cell Start)
{ 
	PPvoid_t success = JudyLByCount(Table, n, reinterpret_cast<Word_t *>(&Start), PJE0);
	if (success == NULL) ThrowSearchError("Type:Nth");

	return Start;
}