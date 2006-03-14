#include "CBinTrie.h"

void BinTrie::ThrowSearchError(char* type)
{ 
	char value[50];
	sprintf(value,"Function attempted to search %s: Judy returned NULL value", type);

	throw JudyEx (value,false);
}

cell BinTrie::First( cell Start)
{
	cell success = Judy1First(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:First");
	return Start;
}

cell BinTrie::FirstEmpty( cell Start)
{
	cell success = Judy1FirstEmpty(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:FirstEmpty");
	return Start;
}

cell BinTrie::Next( cell Start)
{
	cell success = Judy1Next(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:Next");

	return Start;
}

cell BinTrie::NextEmpty( cell Start)
{
	cell success = Judy1NextEmpty(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:NextEmpty");
	return Start;
}

cell BinTrie::Prev( cell Start)
{
	cell success = Judy1Prev(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:Prev");

	return Start;
}

cell BinTrie::PrevEmpty( cell Start)
{
	cell success = Judy1PrevEmpty(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:PrevEmpty");
	return Start;
}

cell BinTrie::Last( cell Start)
{
	cell success = Judy1Last(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:Last");

	return Start;
}

cell BinTrie::LastEmpty( cell Start)
{
	cell success = Judy1LastEmpty(Table, reinterpret_cast<unsigned int*>(&Start), PJE0); 
	if (success == NULL) ThrowSearchError("Type:LastEmpty");
	return Start;
}

cell BinTrie::ByCount(cell n, cell Start)
{ 
	cell success = Judy1ByCount(Table, n, reinterpret_cast<unsigned int*>(&Start), PJE0);
	if (success == NULL) ThrowSearchError("Type:Nth");

	return Start;
}