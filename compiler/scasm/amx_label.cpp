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

LabelMngr::~LabelMngr()
{
	Clear();
}

LabelMngr::Label::Label()
{
	cip = -1;
	sym = 0;
}

void LabelMngr::Clear()
{	
	std::vector<LabelMngr::Label *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

void LabelMngr::AddLabel(SymbolList::Symbol *sym, int cip)
{
	LabelMngr::Label *p = new LabelMngr::Label;

	p->sym = sym;
	p->cip = cip;

	List.push_back(p);
}

LabelMngr::Label *LabelMngr::FindLabel(std::string &sym)
{
	std::vector<LabelMngr::Label *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i)->sym->IsEqual(sym) )
		{
			return (*i);
		}
	}

	return NULL;
}

bool LabelMngr::SetCip(std::string &sym, int cip)
{
	LabelMngr::Label *p = NULL;

	p = FindLabel(sym);

	if (p == NULL)
		return false;

	p->cip = cip;

	return true;
}

void LabelMngr::QueueLabel(std::string &sym, Asm *ASM)
{
	std::string d(sym);
	LQ[d].push(ASM);
}

void LabelMngr::CompleteQueue(bool isLocal)
{
	std::map<std::string,std::stack<Asm *> >::iterator i;
	std::stack<Asm *> *stk = 0;
	std::string search;
	Asm *ASM = 0;
	LabelMngr::Label *p = 0;

	for (i=LQ.begin(); i!=LQ.end(); i++)
	{
		search.assign( (*i).first );
		p = FindLabel(search);
		stk = &((*i).second);
		if (p == NULL || p->cip == LabelMngr::ncip)
		{
			if ((!isLocal || (isLocal && search[0]=='_')) && CError)
			{
				while (!stk->empty())
				{
					CError->SetLine(stk->top()->line);
					CError->ErrorMsg(Err_Bad_Label);
					stk->pop();
				}
			}
		} else {
			while (!stk->empty())
			{
				ASM = stk->top();
				ASM->params[0] = p->cip;
				stk->pop();
			}
		}
	}

	LQ.clear();
}

int LabelMngr::GetCip(std::string &sym)
{
	LabelMngr::Label *p = NULL;

	p = FindLabel(sym);

	if (p == NULL)
		return ncip;

	return p->cip;
}

bool LabelMngr::EraseLabel(std::string &sym)
{
	std::vector<LabelMngr::Label *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i)->sym->IsEqual(sym) )
		{
			List.erase(i);
			return true;
		}
	}

	return false;
}

