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
 * Version: $Id: amx_compiler.h 835 2004-08-12 16:31:50Z dvander $
 */

#ifndef _INCLUDE_AMXCOMPILER_H
#define _INCLUDE_AMXCOMPILER_H

#define CHK_PARAMS(d) \
	if (paramList.size() > d) \
	{ \
		CError->ErrorMsg(Warning_Param_Count, paramList.size(), d); \
	} else if (paramList.size() < d) { \
		CError->ErrorMsg(Err_Param_Count, paramList.size(), d); \
		delete ASM; \
		ASM = 0; \
	}

#define PUSH_PARAM(n,sym) \
	if (paramList.size() >= n) \
	{ \
		ASM->params.push_back(Eval(*(paramList[n-1]), sym)); \
		lastCip+=cellsize; \
	}

class rpn
{
public:
	~rpn();
	//TODO: use linked lists, but not std::list
	std::vector<char> ops;
	std::vector<CExpr> vals;
};

class Compiler
{
public:
	Compiler();
	~Compiler();
	Compiler(std::string &f);
	void Load(std::string &f);
	bool Parse();
	bool Compile(std::string &out);
	int CurLine() { return curLine; }
	ErrorMngr *ErrorHandler() { return CError; }
	void PrintCodeList();
public:
	int FindArguments(std::string &text, std::vector<std::string*> &List, int &end, bool simple = false);
	void Clear();
	int CipCount();
	int CurCip() { return lastCip; }
	Asm *CurAsm() { return curAsm; }
	bool SetDebug();
	bool SetPack();
	bool SetDOpt();
	int DerefSymbol(std::string &str, SymbolType sym = Sym_None);
	bool IsSymbol(std::string &str);
private:
	void ProcessDirective(std::string &text);
	void Init();
	void InitOpcodes();
	int Eval(std::string &str, SymbolType sym = Sym_None);
	CExpr EvalE(std::string &str, SymbolType sym = Sym_None);
	CExpr EvalRpn(rpn *r, SymbolType sym);
	OpToken OperToken(char c);
	char OperChar(OpToken c);
	void WriteCell(FILE *fp, ucell *c, int repeat);
private:
	std::vector<Asm *> CodeList;
	std::map<std::string,int> OpCodes;
	std::stack<std::string> LabelStack;
	char *Output;
	ErrorMngr *CError;
	SymbolList *CSymbols;
	DefineMngr *CDefines;
	MacroList *CMacros;
	DataMngr *DAT;
	ProcMngr *PROC;
	LabelMngr *CLabels;
	NativeMngr *CNatives;
	std::string filename;
	int curLine;
	int lastCip;
	int cellsize;
	int stacksize;
	bool debug;
	bool pack;
	bool dopt;
	int bitsOut;
	Asm *curAsm;
};

#endif //_INCLUDE_AMXCOMPILER_H
