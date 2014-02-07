/* AMX Assembler
 * Copyright (C)2004 David "BAILOPAN" Anderson
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id: amx_symbol.cpp 835 2004-08-12 16:31:50Z dvander $
 */

#include "amxasm.h"

bool is_valid_symbol_marker(char c)
{
	if (c >= '0' && c <= '9')
		return true;
	if (isletter(c))
		return true;
	return false;
}

bool IsValidSymbol(std::string &text)
{
	size_t i = 0;

	if (!text.size() || !isletter(text[0]))
	{
		return false;
	}

	for (i=0; i<text.size(); i++)
	{
		if (!is_valid_symbol_marker(text[i]))
		{
			return false;
		}
	}

	return true;
}

void SymbolList::Clear()
{
	std::list<SymbolList::Symbol *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

SymbolList::~SymbolList()
{
	Clear();
}

bool SymbolList::Symbol::IsEqual(std::string &s)
{
	return (sym.compare(s)==0);
}

SymbolList::Symbol* SymbolList::AddSymbol(const char *szSym, SymbolType type, int line)
{
	SymbolList::Symbol *S = new SymbolList::Symbol;

	S->line = line;
	S->type = type;
	S->sym.assign(szSym);

	List.push_back(S);
	return S;
}

SymbolList::Symbol *SymbolList::AddSymbol(std::string &sym, SymbolType type, int line)
{
	SymbolList::Symbol *S = new SymbolList::Symbol;

	S->line = line;
	S->type = type;
	S->sym.assign(sym);

	List.push_back(S);
	return S;
}

SymbolList::Symbol* SymbolList::FindSymbol(std::string &sym)
{
	std::list<Symbol*>::iterator i;
	for (i=List.begin(); i!=List.end(); i++)
	{
		if ((*i)->IsEqual(sym))
			return (*i);
	}

	return NULL;
}

void SymbolList::PrintTable()
{
	std::list<Symbol*>::iterator i;
	for (i=List.begin(); i!=List.end(); i++)
	{
		printf("Symbol \"%s\" defined on line %d\n", (*i)->sym.c_str(), (*i)->line);
	}
}

bool SymbolList::EraseSymbol(std::string &sym)
{
	std::list<SymbolList::Symbol *>::iterator i;
	SymbolList::Symbol *S = 0;

	for (i=List.begin(); i!=List.end(); i++)
	{
		S = (*i);
		if ( S && S->IsEqual(sym) )
		{
			delete S;
			i = List.erase(i);
			S = 0;
			return true;
		}
	}

	return false;
}
