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
 * Version: $Id: amx_proc.cpp 926 2004-08-21 18:57:47Z dvander $
 */

#include "amxasm.h"

ProcMngr::ProcMngr()
{
	CError = 0;
	printf("Instantiated without a compiler!\n");
	assert(CError);
}

ProcMngr::ProcMngr(ErrorMngr *e)
{
	CError = e;
}

ProcMngr::~ProcMngr()
{
	Clear();
}

void ProcMngr::Clear()
{
	std::vector<ProcMngr::AsmProc *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

ProcMngr::AsmProc *ProcMngr::AddProc(SymbolList::Symbol *Symbol, Asm *ASM)
{
	ProcMngr::AsmProc *a = new ProcMngr::AsmProc;

	a->ASM = ASM;
	a->Symbol = Symbol;
	a->pb = false;

	List.push_back(a);

	return a;
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
	std::vector<ProcMngr::AsmProc *>::reverse_iterator i;

	for (i=List.rbegin(); i!=List.rend(); i++)
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
	
	if (p == NULL || p->ASM == NULL)
		return ncip;

	return p->ASM->cip;
}

void ProcMngr::QueueProc(std::string &sym, Asm *ASM)
{
	std::string d(sym);
	PQ[d].push(ASM);
}

void ProcMngr::CompleteQueue()
{
	std::map<std::string,std::stack<Asm *> >::iterator i;
	std::string search;
	ProcMngr::AsmProc *p = 0;
	std::stack<Asm *> *stk = 0;

	for (i=PQ.begin(); i!=PQ.end(); i++)
	{
		search.assign( (*i).first );
		p = FindProc(search);
		stk = &((*i).second);

		if (p == NULL || p->ASM == NULL)
		{
			while (!stk->empty())
			{
				CError->SetLine(stk->top()->line);
				CError->ErrorMsg(Err_Invalid_Proc);
				stk->pop();
			}
		} else {
			while (!stk->empty())
			{
				stk->top()->cip = p->ASM->cip;
				stk->top()->params[0] = p->ASM->cip;
				stk->pop();
			}
		}
	}
}

