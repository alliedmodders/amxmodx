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

#ifndef _INCLUDE_AMXLABEL_H
#define _INCLUDE_AMXLABEL_H

class LabelMngr
{
public:
	class Label
	{
	public:
		Label();
		SymbolList::Symbol *sym;
		int cip;
	};
public:
	~LabelMngr();
	LabelMngr() { CError = NULL; assert(CError!=NULL); }
	LabelMngr(ErrorMngr *e) { CError = e; }
	LabelMngr::Label *AddLabel(SymbolList::Symbol *sym, int cip);
	LabelMngr::Label *FindLabel(std::string &sym);
	int GetCip(std::string &sym);
	void Clear();
	bool SetCip(std::string &sym, int cip);
	void QueueLabel(std::string &sym, Asm *ASM);
	void CompleteQueue(bool isLocal = false);
	bool EraseLabel(std::string &sym);
	void PrintList();
private:
	std::list<LabelMngr::Label *> List;
	std::map<std::string, std::stack<Asm *> > LQ;
	ErrorMngr *CError;
public:
	static const int ncip = -1;
};

#endif //_INCLUDE_AMXLABEL_H
