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

#ifndef _INCLUDE_MACRO_H
#define _INCLUDE_MACRO_H

class MacroList
{
public:
	class Macro
	{
	public:
		~Macro();
		std::vector<std::string*>::iterator arg;
		std::vector<std::string*> argList;
		std::string symbol;
		std::string macro;
		int argpos;
	};
public:
	MacroList();
	MacroList(void *c);
	~MacroList();
	MacroList::Macro *AddMacroBegin(std::string &symbol, std::string &mac);
	void AddMacroArgument(MacroList::Macro *m, std::string &arg);
	void AddMacroEnd(MacroList::Macro *m);
	MacroList::Macro *FindMacro(std::string &sym);
	std::string *BeginReplacement(MacroList::Macro *macro);
	int ReplaceArgument(MacroList::Macro *m, std::string *macro, std::string &arg, int pos);
	void EndReplacement(MacroList::Macro *m, std::string *macro);
	void SearchAndReplace(std::string &text);
private:
	std::vector<Macro *> List;
	ErrorMngr *CError;
	void *Cmp;
};

#endif //_INCLUDE_MACRO_H
