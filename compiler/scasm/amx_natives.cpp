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

NativeMngr::~NativeMngr()
{
	Clear();
}

void NativeMngr::Clear()
{
	std::vector<NativeMngr::Native *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

void NativeMngr::AddNative(SymbolList::Symbol *S)
{
	NativeMngr::Native *N = new NativeMngr::Native;

	N->S = S;
	N->used = false;

	List.push_back(N);
}

int NativeMngr::GetNativeId(std::string &sym)
{
	int pos = 0;
	std::vector<NativeMngr::Native *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i)->S->IsEqual(sym) )
		{
			return pos;
		}
		pos++;
	}

	return ncip;
}

NativeMngr::Native *NativeMngr::FindNative(std::string &sym)
{
	std::vector<NativeMngr::Native *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i)->S->IsEqual(sym) )
		{
			return (*i);
		}
	}

	return NULL;
}

void NativeMngr::GetNatives(std::vector<NativeMngr::Native *> &nList)
{
	std::vector<NativeMngr::Native *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		nList.push_back( (*i) );
	}
}

