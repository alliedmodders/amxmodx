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

ProcMngr::~ProcMngr()
{
	std::vector<ProcMngr::AsmProc *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

void ProcMngr::AddProc(SymbolList::Symbol *Symbol, Asm *ASM)
{
	ProcMngr::AsmProc *a = new ProcMngr::AsmProc;

	a->ASM = ASM;
	a->Symbol = Symbol;
	a->pb = false;

	List.push_back(a);
}

bool ProcMngr::SetPublic(std::string &sym)
{
	ProcMngr::AsmProc *a = NULL;

	a = FindProc(sym);

	if (a == NULL)
		return false;

	a->pb = true;

	return true;
}

void ProcMngr::GetPublics(std::vector<ProcMngr::AsmProc *> &pbList)
{
	std::vector<ProcMngr::AsmProc *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ((*i)->pb == true)
		{
			pbList.push_back( (*i) );
		}
	}
}

ProcMngr::AsmProc *ProcMngr::FindProc(std::string &sym)
{
	std::vector<ProcMngr::AsmProc *>::iterator i;

	for (i = List.begin(); i != List.end(); i++)
	{
		if ( (*i)->Symbol->IsEqual(sym) )
		{
			return (*i);
		}
	}

	return NULL;
}

int ProcMngr::GetCip(std::string &sym)
{
	ProcMngr::AsmProc *p = NULL;

	p = FindProc(sym);
	
	if (p == NULL)
		return ncip;

	return p->ASM->cip;
}