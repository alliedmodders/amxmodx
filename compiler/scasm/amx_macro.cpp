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
 * Version: $Id: amx_macro.cpp 826 2004-08-11 10:01:56Z dvander $
 */

#include "amxasm.h"

MacroList::~MacroList()
{
	Clear();
}

void MacroList::Clear()
{
	std::vector<MacroList::Macro *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

MacroList::Macro::~Macro()
{
	std::vector<std::string *>::iterator i;

	for (i=argList.begin(); i!=argList.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	argList.clear();
}

MacroList::MacroList()
{
	printf("Not instantiated with a compiler\n");
	exit(0);
}

MacroList::MacroList(void *c)
{
	CError = ((Compiler *)c)->ErrorHandler();
	Cmp = c;
}

std::string *MacroList::BeginReplacement(MacroList::Macro *macro)
{
	std::string *mstring = new std::string(macro->macro);

	macro->arg = macro->argList.begin();
	macro->argpos = 0;

	return mstring;
}

int MacroList::ReplaceArgument(MacroList::Macro *m, std::string *macro, std::string &arg, int pos = 0)
{
	int bPos = 0;

	bPos = FindSymbol(*macro, *(*m->arg), pos);

	if (bPos != -1)
	{
		macro->replace(bPos, (*m->arg)->size(), arg);
		bPos += (int)arg.size();
	}	

	m->arg++;
	m->argpos++;

	return bPos;
}

void MacroList::EndReplacement(MacroList::Macro *m, std::string *macro)
{
	if (m->arg != m->argList.end())
	{
		CError->ErrorMsg(Err_MacroParamCount, m->symbol.c_str());
	}

	m->arg = m->argList.begin();
	m->argpos = 0;
}

MacroList::Macro *MacroList::AddMacroBegin(std::string &symbol, std::string &mac)
{
	Macro *macro = new Macro;
	macro->macro.assign(mac);
	macro->symbol.assign(symbol);
	return macro;
}

void MacroList::AddMacroArgument(MacroList::Macro *m, std::string &arg)
{
	std::string *sArg = new std::string(arg);
	m->argList.push_back(sArg);
}

void MacroList::AddMacroEnd(MacroList::Macro *m)
{
	List.push_back(m);
}

MacroList::Macro *MacroList::FindMacro(std::string &sym)
{
	std::vector<Macro *>::iterator i;
	for (i=List.begin(); i!=List.end(); i++)
	{
		if ((*i)->macro.compare(sym) == 0)
			return (*i);
	}

	return NULL;
}

void MacroList::SearchAndReplace(std::string &text)
{
	std::vector<Macro *>::iterator i;
	MacroList::Macro *m = NULL;
	int pos=0, symPos=0, bPos=0, argPos=0;

	for (i=List.begin(); i!=List.end(); i++)
	{
		m = (*i);
		pos = FindSymbol(text, m->symbol, 0);
		if (pos != -1)
		{
			/* Strip the arguments */
			std::string argstring;
			symPos = pos + (int)m->symbol.size();
			argstring.assign(text.substr(symPos+1, text.size()-symPos));
			std::vector<std::string *> argList;
			((Compiler *)Cmp)->FindArguments(argstring, argList, bPos, true);
			/* Build the macro */
			std::string *ms;
			ms = BeginReplacement(m);
			std::vector<std::string *>::iterator j;
			for (j=argList.begin(); j!=argList.end(); j++)
			{
				argPos = ReplaceArgument(m, ms, *(*j), argPos);
			}
			EndReplacement(m, ms);
			/* Search and replace */
			text.replace(pos, bPos-pos, *ms);
			/* Cleanup */
			delete ms;
			i = List.begin();
		}
	}
}

