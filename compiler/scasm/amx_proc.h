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

#ifndef _INCLUDE_AMXPROC_H
#define _INCLUDE_AMXPROC_H

class ProcMngr
{
public:
	class AsmProc
	{
	public:
		SymbolList::Symbol *Symbol;
		Asm *ASM;
		bool pb;
	};
public:
	~ProcMngr();
	ProcMngr();
	ProcMngr(ErrorMngr *e);
	ProcMngr::AsmProc *AddProc(SymbolList::Symbol *Symbol, Asm *ASM);
	ProcMngr::AsmProc *FindProc(std::string &sym);
	int GetCip(std::string &sym);
	bool SetPublic(std::string &sym);
	void GetPublics(std::vector<ProcMngr::AsmProc *> &pbList);
	void QueueProc(std::string &sym, Asm *ASM);
	void CompleteQueue();
	void Clear();
private:
	std::vector<ProcMngr::AsmProc *> List;
	std::map<std::string, std::stack<Asm *> > PQ;
	ErrorMngr *CError;
public:
	static const int ncip = -1;
};

#endif //_INCLUDE_AMXPROC_H
