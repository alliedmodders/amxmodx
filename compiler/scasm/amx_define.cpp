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
 * Version: $Id$
 */

#include "amxasm.h"

DefineMngr::~DefineMngr()
{
	Clear();
}

void DefineMngr::Clear()
{
	std::vector<DefineMngr::Define *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

void DefineMngr::Define::Set(std::string &s, std::string &d)
{
	sym.assign(s);
	def.assign(d);
}

DefineMngr::Define *DefineMngr::AddDefine(std::string &sym, std::string &def)
{
	DefineMngr::Define *D = new DefineMngr::Define;

	D->Set(sym, def);

	List.push_back(D);

	return D;
}

DefineMngr::Define *DefineMngr::FindDefine(std::string &sym)
{
	std::vector<DefineMngr::Define*>::iterator i;
	for (i=List.begin(); i!=List.end(); i++)
	{
		if ((*i)->GetSymbol()->compare(sym)==0)
		{
			return (*i);
		}
	}
	return NULL;
}

void DefineMngr::SearchAndReplace(std::string &text)
{
	std::vector<DefineMngr::Define*>::iterator i;
	DefineMngr::Define *D = NULL;
	int pos;

	for (i=List.begin(); i!=List.end(); i++)
	{
		D = (*i);
		pos = FindSymbol(text, *(D->GetSymbol()), 0);
		if (pos != -1)
		{
			text.replace(pos, D->GetSymbol()->size(), *(D->GetDefine()));
			i = List.begin();
		}
	}
}

