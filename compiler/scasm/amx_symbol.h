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

#ifndef _INCLUDE_SYMBOL_H
#define _INCLUDE_SYMBOL_H

bool IsValidSymbol(std::string &text);

typedef enum
{
	Sym_None,
	Sym_Define,
	Sym_Macro,
	Sym_Proc,
	Sym_Dat,
	Sym_Reserved,
	Sym_Label,
	Sym_Native,
} SymbolType;

class SymbolList
{
public:
	class Symbol
	{
	public:
		Symbol(SymbolType t, const char *s, int l);
		const char *GetSymbol() { return sym.c_str(); }
		SymbolType GetType() { return type; }
		int GetLine() { return line; }
		int IsEqual(std::string &s);
	private:
		SymbolType type;
		std::string sym;
		int line;
	};

public:
	~SymbolList();
	SymbolList::Symbol* AddSymbol(const char *s, SymbolType type, int line);
	SymbolList::Symbol* FindSymbol(std::string &sym);
	void PrintTable();
private:
	std::vector<Symbol*> List;
};


#endif //_INCLUDE_SYMBOL_H
