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

Compiler::Compiler()
{
	curLine = -1;
	cellsize = 4;
	Output = 0;
	stacksize = cellsize * 4096;
	debug = false;
	Init();
}

Compiler::~Compiler()
{
	Clear();
	delete CDefines;
	delete CError;
	delete CLabels;
	delete CMacros;
	delete CNatives;
	delete CSymbols;
	delete PROC;
	delete DAT;
}

Compiler::Compiler(std::string &f)
{
	curLine = -1;
	cellsize = 4;
	Output = 0;
	filename.assign(f);
	stacksize = cellsize * 4096;
	debug = false;
	Init();
}

bool Compiler::SetDebug()
{
	bool state = debug;

	debug = debug ? false : true;

	return state;
}

void Compiler::Load(std::string &f)
{
	filename.assign(f);
}

void Compiler::Init()
{
	Compiler *c = this;
	/* Load error handling */
	CError = new ErrorMngr(c);
	/* Load symbol management */
	CSymbols = new SymbolList;
	/* Load constant management */
	CDefines = new DefineMngr;
	/* Load macro management */
	CMacros = new MacroList(this);
	/* Load DAT management */
	DAT = new DataMngr(cellsize);
	/* Load Proc management */
	PROC = new ProcMngr;
	/* Load Label management */
	CLabels = new LabelMngr(CError);
	/* Load Native management */
	CNatives = new NativeMngr;
	/* Load the default symbols + opcodes */
	InitOpcodes();
}

int Compiler::CipCount()
{
	std::vector<Asm *>::iterator i;
	std::vector<int>::iterator j;
	int cipc = 0;

    for (i=CodeList.begin(); i!=CodeList.end(); i++)
	{
		cipc+=cellsize;
		for (j=(*i)->params.begin(); j!=(*i)->params.end(); j++)
		{
			cipc+=cellsize;
		}
	}

	return cipc;
}

bool Compiler::Compile()
{
	if (CodeList.size() < 1 || !CError || CError->GetStatus() >= Err_Error)
	{
		return false;
	}

	int32_t fileSize = 0;
	int16_t magic = (int16_t)AMX_MAGIC;
	char file_version = CUR_FILE_VERSION;
    char amx_version = MIN_AMX_VERSION;
	int16_t flags = 0;
	int16_t defsize = 8;
	int32_t cod, dat, hea, stp, cip, publics, natives, libraries;
	int32_t pubvars, tags, names;
	int hdrEnd = sizeof(AMX_HEADER);
	
	std::vector<ProcMngr::AsmProc *> ProcList;
	std::vector<ProcMngr::AsmProc *>::iterator pl;
	std::vector<NativeMngr::Native *> NativeList;
	std::vector<NativeMngr::Native *>::iterator nl;
	std::map<int,int> NameMap;

	/* Tables */
	std::vector<NameRecord> Nametbl;
	std::vector<AddrTable> PublicsTable;
	std::vector<AddrTable> NativesTable;
	std::vector<AddrTable>::iterator ai;
	std::vector<NameRecord>::iterator nt;

	PROC->GetPublics(ProcList);
	CNatives->GetNatives(NativeList);	

	/* The only way I see to do this is to build the nametable first.
	 */

	/* Public table starts right after the header */
	publics = hdrEnd;
	for (pl = ProcList.begin(); pl != ProcList.end(); pl++)
	{
		NameRecord n;
		AddrTable a;
		n.Name = (*pl)->Symbol->sym.c_str();
		a.addr = (*pl)->ASM->cip;
		a.offset = (int)Nametbl.size();
		Nametbl.push_back(n);
		PublicsTable.push_back(a);
	}

    natives = publics + (int)(PublicsTable.size() * (sizeof(int32_t) * 2));
	for (nl = NativeList.begin(); nl != NativeList.end(); nl++)
	{
		NameRecord n;
		AddrTable a;
		n.Name = (*nl)->S->sym.c_str();
		a.addr = 0;
		a.offset = (int)Nametbl.size();
		Nametbl.push_back(n);
		NativesTable.push_back(a);
	}

	libraries = natives + (int)(NativesTable.size() * (sizeof(int32_t) * 2));
	pubvars = libraries;
	tags = pubvars;
	names = tags;

	/* Fill out the tables */
	int cOffset = names + sizeof(int16_t);
	int16_t nameHdr = 0x1F;
	for (ai = PublicsTable.begin(); ai != PublicsTable.end(); ai++)
	{
		int off = (*ai).offset;
		NameMap[cOffset] = off;
		(*ai).offset = cOffset;
		cOffset += (int)strlen(Nametbl.at(off).Name) + 1;
	}
	for (ai = NativesTable.begin(); ai != NativesTable.end(); ai++)
	{
		int off = (*ai).offset;
		NameMap[cOffset] = off;
		(*ai).offset = cOffset;
		cOffset += (int)strlen(Nametbl.at(off).Name) + 1;
	}

	cod = cOffset;
	dat = cod + CipCount();
	hea = dat + DAT->GetSize();
	stp = hea + stacksize;
	int16_t cipHdr = 0x00;
	cip = -1;
	fileSize = hea;

	std::string amxname;
	amxname.assign(filename);
	int pos = amxname.find(".asm");
	if (pos != std::string::npos)
	{
		amxname.replace(pos, 4, ".amx");
	} else {
		amxname.append(".amx");
	}

	FILE *fp = fopen(amxname.c_str(), "wb");

	fwrite((void*)&fileSize, sizeof(int32_t), 1, fp);
	fwrite((void*)&magic, sizeof(int16_t), 1, fp);
	fwrite((void*)&file_version, sizeof(char), 1, fp);
	fwrite((void*)&amx_version, sizeof(char), 1, fp);
	fwrite((void*)&flags, sizeof(int16_t), 1, fp);
	fwrite((void*)&defsize, sizeof(int16_t), 1, fp);

	fwrite((void*)&cod, sizeof(int32_t), 1, fp);
	fwrite((void*)&dat, sizeof(int32_t), 1, fp);
	fwrite((void*)&hea, sizeof(int32_t), 1, fp);
	fwrite((void*)&stp, sizeof(int32_t), 1, fp);
	fwrite((void*)&cip, sizeof(int32_t), 1, fp);
	fwrite((void*)&publics, sizeof(int32_t), 1, fp);
	fwrite((void*)&natives, sizeof(int32_t), 1, fp);
	fwrite((void*)&libraries, sizeof(int32_t), 1, fp);
	fwrite((void*)&pubvars, sizeof(int32_t), 1, fp);
	fwrite((void*)&tags, sizeof(int32_t), 1, fp);
	fwrite((void*)&names, sizeof(int32_t), 1, fp);

	for (ai = PublicsTable.begin(); ai != PublicsTable.end(); ai++)
	{
		fwrite((void*)&((*ai).addr), sizeof(int32_t), 1, fp);
		fwrite((void*)&((*ai).offset), sizeof(int32_t), 1, fp);
	}

	for (ai = NativesTable.begin(); ai != NativesTable.end(); ai++)
	{
		fwrite((void*)&((*ai).addr), sizeof(int32_t), 1, fp);
		fwrite((void*)&((*ai).offset), sizeof(int32_t), 1, fp);
	}

	fwrite((void*)&(nameHdr), sizeof(int16_t), 1, fp);

	for (ai = PublicsTable.begin(); ai != PublicsTable.end(); ai++)
	{
		int off = (*ai).offset;
		int offs = NameMap[off];
		const char *s = Nametbl.at(offs).Name;
		fwrite(s, sizeof(char), strlen(s)+1, fp);
	}

	for (ai = NativesTable.begin(); ai != NativesTable.end(); ai++)
	{
		int off = (*ai).offset;
		int offs = NameMap[off];
		const char *s = Nametbl.at(offs).Name;
		fwrite(s, sizeof(char), strlen(s)+1, fp);
	}

	//fwrite((void*)&cipHdr, sizeof(int16_t), 1, fp);

	/* Write the code */

	std::vector<Asm *>::iterator ci;
	std::vector<int>::iterator di;
	int cop = 0;
	for (ci = CodeList.begin(); ci != CodeList.end(); ci++)
	{
		cop = (*ci)->op;
		fwrite((void *)&cop, sizeof(int32_t), 1, fp);
		for (di = (*ci)->params.begin(); di != (*ci)->params.end(); di++)
		{
			cop = (*di);
			fwrite((void *)&cop, sizeof(int32_t), 1, fp);
		}
	}

	std::vector<DataMngr::Datum *> dm;
	std::vector<DataMngr::Datum *>::iterator dmi;
	DAT->GetData(dm);

	int val = 0;
	const char *s = 0;
	for (dmi = dm.begin(); dmi != dm.end(); dmi++)
	{
		if ( (*dmi)->db )
		{
			if ( (*dmi)->e.GetType() == Val_Number )
			{
				val = (*dmi)->e.GetNumber();
				fwrite((void *)&val, sizeof(int32_t), 1, fp);
			} else {
				s = (*dmi)->e.GetString();
				for (int q = 0; q < (*dmi)->e.Size(); q++)
				{
					val = s[q];
					fwrite((void*)&val, sizeof(int32_t), 1, fp);
				}
			}
		} else {
			char c = (*dmi)->fill;
			for (int iter=0; iter<=(*dmi)->e.GetNumber(); iter++)
			{
				fwrite((void*)&c, sizeof(int32_t), 1, fp);
			}
		}
	}
	
	fclose(fp);

	return true;
}

void Compiler::Clear()
{
	DAT->Clear();
	CDefines->Clear();
	CMacros->Clear();
	CLabels->Clear();
	CNatives->Clear();
	PROC->Clear();
	CSymbols->Clear();
}

bool Compiler::Parse()
{
	std::ifstream fp(filename.c_str());
	char buffer[256] = {0};
	std::stack<int> DefStack;
	std::stack<std::string> LabelStack;
	curLine = 0;
	AsmSection sec = Asm_None;
	lastCip = 0-cellsize;

	if (!fp.is_open())
	{
		CError->ErrorMsg(Err_FileOpen, filename.c_str());
		return false;
	}

	while (!fp.eof())
	{
		fp.getline(buffer, 255);
		curLine+=1;

		/* Check for preprocessor directives */
		if (buffer[0] == '#')
		{
			std::string procline(buffer);
			if (procline.substr(0, 3).compare("#if") == 0)
			{
				std::string def;
				std::string temp;
				std::string comp;
				StringBreak(procline, def, temp);
				StringBreak(temp, def, comp);
				DefineMngr::Define *D = 0;
				if ((D = CDefines->FindDefine(def)) == 0)
				{
					DefStack.push(0);
				} else if (D->GetDefine()->compare(comp) == 0) {
					DefStack.push(1);
				} else {
					DefStack.push(0);
				}
			} else if (procline.substr(0, 5).compare("#else") == 0) {
				if (DefStack.size())
				{
					if (DefStack.top() == 1)
					{
						DefStack.pop();
						DefStack.push(0);
					} else if (DefStack.top() == 0) {
						DefStack.pop();
						DefStack.push(1);
					}
				} else {
					CError->ErrorMsg(Err_Misplaced_Directive);
				}
				continue;
			} else if (procline.substr(0, 6).compare("#endif") == 0) {
				if (DefStack.size())
				{
					DefStack.pop();
				} else {
					CError->ErrorMsg(Err_Misplaced_Directive);
				}
				continue;
			} else {
				/* Check for previous operations */
				if (DefStack.size())
				{
					if (DefStack.top() < 1)
					{
						continue;
					}
				}
				ProcessDirective(procline);
			}
			continue;
		}

		/* Check for previous operations */
		if (DefStack.size())
		{
			if (DefStack.top() < 1)
			{
				continue;
			}
		}

		/* Strip the line */
		std::string line(buffer);

		StripComments(line);
		Strip(line);
	
		if (line.size() < 1)
			continue;

		/* Check for section state changes */
		if (line[0] == '.')
		{
			if (line.compare(".DATA") == 0)
			{
				sec = Asm_Data;
			} else if (line.compare(".CODE") == 0) {
				sec = Asm_Code;
			} else if (line.compare(".PUBLIC") == 0) {
				sec = Asm_Public;
			} else if (line.compare(".NATIVE") == 0) {
				sec = Asm_Native;
			} else {
				sec = Asm_Invalid;
				CError->ErrorMsg(Err_Invalid_Section, buffer);
			}
			/* Update the labels */
			CLabels->CompleteQueue(true);
			while (!LabelStack.empty())
			{
				CLabels->EraseLabel(LabelStack.top());
				CSymbols->EraseSymbol(LabelStack.top());
				LabelStack.pop();
			}
		} else {
			/* Do pre-processing */
			CMacros->SearchAndReplace(line);
			CDefines->SearchAndReplace(line);

			/* See if we're not in a section */
			if (sec == Asm_None)
			{
				if (line.size() > 0)
				{
					CError->ErrorMsg(Err_Wandering_Stuff);
				}
			} else if (sec == Asm_Invalid) {
				/* Just ignore it */
			} else if (sec == Asm_Data) {
				/* Format is Symbol, [db|stat], Data */
				std::string symbol;
				std::string data;
				std::string buf;
				std::string fmt;

				/* Do two splits - line into symbol into data */
				StringBreak(line, symbol, buf);
				StringBreak(buf, fmt, data);
				
				/* Check if the symbol is already defined */
				SymbolList::Symbol *S = NULL;
				if ((S = CSymbols->FindSymbol(symbol)) != NULL)
				{
					CError->ErrorMsg(Err_Symbol_Reuse, symbol.c_str(), S->line);
					continue;
				}
				
				if (fmt.compare("db") && fmt.compare("stat"))
				{
					CError->ErrorMsg(Err_Invalid_Stor, fmt.c_str());
					continue;
				}

				if (fmt.compare("db") == 0)
				{
					/* Add and evaluate the expression */
					CExpr e(CError);
					e.Set(data);
					e.Evaluate();
					
					/* Add into the DAT section */
					DAT->Add(symbol, e, true);
				} else  if (fmt.compare("stat") == 0) {
					CExpr e(CError);
					
					if (data.find("fill") != std::string::npos)
					{
						std::string fill, amt;
						StringBreak(data, amt, buf);
						StringBreak(buf, data, fill);
						CExpr t(CError);
						t.Set(fill);
						t.Evaluate();
						e.Set(amt);
						e.Evaluate();
						DAT->Add(symbol, e, false, t.GetNumber());
					} else {
						e.Set(data);
						e.Evaluate();
						DAT->Add(symbol, e, false, 0);
					}
				}
				CSymbols->AddSymbol(symbol, Sym_Dat, CurLine());
			} else if (sec == Asm_Public) {
				if (!IsValidSymbol(line))
				{
					CError->ErrorMsg(Err_Invalid_Symbol);
					continue;
				}
				SymbolList::Symbol *S = NULL;
				if ( (S = CSymbols->FindSymbol(line)) == NULL)
				{
					CError->ErrorMsg(Err_Unknown_Symbol, line.c_str());
					continue;
				}
				if ( (S->type != Sym_Proc) )
				{
					CError->ErrorMsg(Err_Symbol_Type, Sym_Proc, S->type);
					continue;
				}
				if (!PROC->SetPublic(line))
				{
					CError->ErrorMsg(Err_Unknown_Symbol, line.c_str());
					continue;
				}
			} else if (sec == Asm_Native) {
				if (!IsValidSymbol(line))
				{
					CError->ErrorMsg(Err_Invalid_Symbol);
					continue;
				}
				SymbolList::Symbol *S = NULL;
				if ( (S = CSymbols->FindSymbol(line)) != NULL)
				{
					CError->ErrorMsg(Err_Invalid_Symbol, line.c_str(), S->line);
					continue;
				}
				S = CSymbols->AddSymbol(line, Sym_Native, CurLine());
				CNatives->AddNative(S);
			} else if (sec == Asm_Code) {
				std::string code;
				std::string params;
				SymbolList::Symbol *S;

				StringBreak(line, code, params);

				if (code.compare("PROC") == 0 || code.compare("proc") == 0)
				{
					/* Check if symbol is valid */
					if (!IsValidSymbol(params))
					{
						CError->ErrorMsg(Err_Invalid_Symbol);
						continue;
					}
					/* Check if the symbol is already used */
					if ( (S = CSymbols->FindSymbol(params)) != NULL)
					{
						CError->ErrorMsg(Err_Invalid_Symbol, params.c_str(), S->line);
						continue;
					}
					/* Create instruction */
					Asm *ASM = new Asm;
					ASM->cip = lastCip + cellsize;
					ASM->op = OpCodes["proc"];
					ASM->line = curLine;
					lastCip += cellsize;
					/* Add symbol */
					S = CSymbols->AddSymbol(params, Sym_Proc, CurLine());
					/* Add to code list */
					CodeList.push_back(ASM);
					/* Add to PROC list */
					PROC->AddProc(S, ASM);
				} else if (code.compare("ENDP") == 0) {
					/* This is, in theory, not needed
					 * Nonetheless, we check labels here
					 */
					CLabels->CompleteQueue(true);
					while (!LabelStack.empty())
					{
						CLabels->EraseLabel(LabelStack.top());
						CSymbols->EraseSymbol(LabelStack.top());
						LabelStack.pop();
					}
				} else if (params.size() < 1 && code[code.size()-1] == ':') {
					/* Label! */
					/* Check if symbol is valid */
					code.erase(code.size()-1, 1);
					if (!IsValidSymbol(code))
					{
						CError->ErrorMsg(Err_Invalid_Symbol);
						continue;
					}
					/* Check if the symbol is already used */
					if ( (S = CSymbols->FindSymbol(code)) != NULL)
					{
						if (S->type == Sym_Label)
						{
							LabelMngr::Label *p = CLabels->FindLabel(code);
							if (p == NULL)
								CError->ErrorMsg(Err_Invalid_Symbol);
							else
								p->cip = lastCip+cellsize;
							continue;
						} else {
							CError->ErrorMsg(Err_Symbol_Reuse, code.c_str(), S->line);
						}
						continue;
					}
					if (code[0] == '_')
					{
						LabelStack.push(code);
					}
					S = CSymbols->AddSymbol(code, Sym_Label, CurLine());
					CLabels->AddLabel(S, lastCip+cellsize);
				} else {
					/* Check if there is a valid opcode */
					int op = OpCodes[code];
					int argPos;
					if (op == OP_NONE)
					{
						CError->ErrorMsg(Err_Opcode);
						continue;
					}

					Asm *ASM = 0;

					if (debug)
					{
						ASM = new Asm;
						ASM->cip = lastCip+cellsize;
						ASM->op = OP_LINE;
						ASM->line = curLine;
						ASM->params.push_back(curLine);
						ASM->params.push_back(0);
						CodeList.push_back(ASM);
						lastCip+=cellsize*3;
					}

					ASM = new Asm;

					curAsm = ASM;

					std::vector<std::string *> paramList;

					if (params.size() > 0)
					{
						FindArguments(params, paramList, argPos, true);
						if (argPos != params.size()-1)
						{
							CError->ErrorMsg(Err_Unexpected_Char, params[argPos]);
							continue;
						}
					}

					ASM->cip = (lastCip+cellsize);
					ASM->op = op;
					ASM->line = curLine;
					lastCip += cellsize;

					switch (op)
					{
						case OP_LOAD_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LOAD_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LOAD_S_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LOAD_S_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LREF_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LREF_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LREF_S_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LREF_S_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LOAD_I:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_LODB_I:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_CONST_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_CONST_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ADDR_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ADDR_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_STOR_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_STOR_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_STOR_S_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_STOR_S_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SREF_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SREF_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SREF_S_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SREF_S_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_STOR_I:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_STRB_I:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LIDX:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_LIDX_B:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_IDXADDR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_IDXADDR_B:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ALIGN_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ALIGN_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_LCTRL:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SCTRL:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_MOVE_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_MOVE_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_XCHG:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_PUSH_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_PUSH_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_PUSH_R:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_PUSH_C:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_PUSH:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_PUSH_S:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_POP_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_POP_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_STACK:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_HEAP:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_PROC:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_RET:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_RETN:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_CALL:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Proc);
							break;
						}
						case OP_CALL_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_JUMP:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JREL:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JZER:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JNZ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JEQ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JNEQ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JLESS:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JLEQ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JGRTR:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JGEQ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JSLESS:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JSLEQ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JSGRTR:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_JSGEQ:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_SHL:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SHR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SSHR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SHL_C_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SHL_C_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SHR_C_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SHR_C_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SMUL:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SDIV:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SDIV_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_UMUL:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_UDIV:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_UDIV_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ADD:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SUB:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SUB_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_AND:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_OR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_XOR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_NOT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_NEG:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_INVERT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_ADD_C:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SMUL_C:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ZERO_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_ZERO_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_ZERO:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_ZERO_S:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SIGN_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SIGN_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_EQ:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_NEQ:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_LESS:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_LEQ:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_GRTR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_GEQ:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SLESS:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SLEQ:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SGRTR:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SGEQ:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_EQ_C_PRI:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_EQ_C_ALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_INC_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_INC_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_INC:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_INC_S:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_INC_I:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_DEC_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_DEC_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_DEC:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_DEC_S:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_DEC_I:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_MOVS:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_CMPS:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_FILL:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_HALT:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_BOUNDS:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SYSREQ_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SYSREQ_C:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Native);
							break;
						}
						case OP_FILE:
						{
							/* Not yet implemented */
							assert(0);
							/*CHK_PARAMS(3);
							PUSH_PARAM(1, Sym_Dat);
							PUSH_PARAM(1, Sym_Dat);
							PUSH_PARAM(1, Sym_Dat);*/
							break;
						}
						case OP_LINE:
						{
							CHK_PARAMS(2);
							PUSH_PARAM(1, Sym_Dat);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SYMBOL:
						{
							/* Not yet implemented */
							assert(0);
							/*CHK_PARAMS(3);
							PUSH_PARAM(1, Sym_Dat);
							PUSH_PARAM(1, Sym_Dat);
							PUSH_PARAM(1, Sym_Dat);*/
							break;
						}
						case OP_SRANGE:
						{
							CHK_PARAMS(2);
							PUSH_PARAM(1, Sym_Dat);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_JUMP_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SWITCH:
						{
							/* Not yet implemented */
							assert(0);
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Label);
							break;
						}
						case OP_CASETBL:
						{
							/* Not yet implemented */
							assert(0);
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Native);
							break;
						}
						case OP_SWAP_PRI:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SWAP_ALT:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_PUSHADDR:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_NOP:
						{
							CHK_PARAMS(0);
							break;
						}
						case OP_SYSREQ_D:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
						case OP_SYMTAG:
						{
							CHK_PARAMS(1);
							PUSH_PARAM(1, Sym_Dat);
							break;
						}
					} /* Switch */
					CodeList.push_back(ASM);
				} /* Asm_Code */
			} /* Section If */
		} /* Line processing */
	} /* While */

	/* We're not done! Check the label Queue */
	CLabels->CompleteQueue();

	CError->PrintReport();

	return true;
}

void Compiler::InitOpcodes()
{
	OpCodes["nop"] = 0;
	OpCodes["load.pri"] = 1;
	OpCodes["load.alt"] = 2;
	OpCodes["load.s.pri"] = 3;
	OpCodes["load.s.alt"] = 4;
	OpCodes["lref.pri"] = 5;
	OpCodes["lref.alt"] = 6;
	OpCodes["lref.s.pri"] = 7;
	OpCodes["lref.s.alt"] = 8;
	OpCodes["load.i"] = 9;
	OpCodes["lodb.i"] = 10;
	OpCodes["const.pri"] = 11;
	OpCodes["const.alt"] = 12;
	OpCodes["addr.pri"] = 13;
	OpCodes["addr.alt"] = 14;
	OpCodes["stor.pri"] = 15;
	OpCodes["stor.alt"] = 16;
	OpCodes["stor.s.pri"] = 17;
	OpCodes["stor.s.alt"] = 18;
	OpCodes["sref.pri"] = 19;
	OpCodes["sref.alt"] = 20;
	OpCodes["sref.s.pri"] = 21;
	OpCodes["sref.s.alt"] = 22;
	OpCodes["stor.i"] = 23;
	OpCodes["strb.i"] = 24;
	OpCodes["lidx"] = 25;
	OpCodes["lidx.b"] = 26;
	OpCodes["idxaddr"] = 27;
	OpCodes["idxaddr.b"] = 28;
	OpCodes["align.pri"] = 29;
	OpCodes["align.alt"] = 30;
	OpCodes["lctrl"] = 31;
	OpCodes["sctrl"] = 32;
	OpCodes["move.pri"] = 33;
	OpCodes["move.alt"] = 34;
	OpCodes["xchg"] = 35;
	OpCodes["push.pri"] = 36;
	OpCodes["push.alt"] = 37;
	OpCodes["push.r"] = 38;
	OpCodes["push.c"] = 39;
	OpCodes["push"] = 40;
	OpCodes["push.s"] = 41;
	OpCodes["pop.pri"] = 42;
	OpCodes["pop.alt"] = 43;
	OpCodes["stack"] = 44;
	OpCodes["heap"] = 45;
	OpCodes["proc"] = 46;
	OpCodes["ret"] = 47;
	OpCodes["retn"] = 48;
	OpCodes["call"] = 49;
	OpCodes["call.pri"] = 50;
	OpCodes["jump"] = 51;
	OpCodes["jrel"] = 52;
	OpCodes["jzer"] = 53;
	OpCodes["jnz"] = 54;
	OpCodes["jeq"] = 55;
	OpCodes["jneq"] = 56;
	OpCodes["jless"] = 57;
	OpCodes["jleq"] = 58;
	OpCodes["jgrtr"] = 59;
	OpCodes["jgeq"] = 60;
	OpCodes["jsless"] = 61;
	OpCodes["jsleq"] = 62;
	OpCodes["jsgrtr"] = 63;
	OpCodes["jsgeq"] = 64;
	OpCodes["shl"] = 65;
	OpCodes["shr"] = 66;
	OpCodes["sshr"] = 67;
	OpCodes["shl.c.pri"] = 68;
	OpCodes["shl.c.alt"] = 69;
	OpCodes["shr.c.pri"] = 70;
	OpCodes["shr.c.alt"] = 71;
	OpCodes["smul"] = 72;
	OpCodes["sdiv"] = 73;
	OpCodes["sdiv.alt"] = 74;
	OpCodes["umul"] = 75;
	OpCodes["udiv"] = 76;
	OpCodes["udiv.alt"] = 77;
	OpCodes["add"] = 78;
	OpCodes["sub"] = 79;
	OpCodes["sub.alt"] = 80;
	OpCodes["and"] = 81;
	OpCodes["or"] = 82;
	OpCodes["xor"] = 83;
	OpCodes["not"] = 84;
	OpCodes["neg"] = 85;
	OpCodes["invert"] = 86;
	OpCodes["add.c"] = 87;
	OpCodes["smul.c"] = 88;
	OpCodes["zero.pri"] = 89;
	OpCodes["zero.alt"] = 90;
	OpCodes["zero"] = 91;
	OpCodes["zero.s"] = 92;
	OpCodes["sign.pri"] = 93;
	OpCodes["sign.alt"] = 94;
	OpCodes["eq"] = 95;
	OpCodes["neq"] = 96;
	OpCodes["less"] = 97;
	OpCodes["leq"] = 98;
	OpCodes["grtr"] = 99;
	OpCodes["geq"] = 100;
	OpCodes["sless"] = 101;
	OpCodes["sleq"] = 102;
	OpCodes["sgrtr"] = 103;
	OpCodes["sgeq"] = 104;
	OpCodes["eq.c.pri"] = 105;
	OpCodes["eq.c.alt"] = 106;
	OpCodes["inc.pri"] = 107;
	OpCodes["inc.alt"] = 108;
	OpCodes["inc"] = 109;
	OpCodes["inc.s"] = 110;
	OpCodes["inc.i"] = 111;
	OpCodes["dec.pri"] = 112;
	OpCodes["dec.alt"] = 113;
	OpCodes["dec"] = 114;
	OpCodes["dec.s"] = 115;
	OpCodes["dec.i"] = 116;
	OpCodes["movs"] = 117;
	OpCodes["cmps"] = 118;
	OpCodes["fill"] = 119;
	OpCodes["halt"] = 120;
	OpCodes["bounds"] = 121;
	OpCodes["sysreq.pri"] = 122;
	OpCodes["sysreq.c"] = 123;
	OpCodes["file"] = 124;
	OpCodes["line"] = 125;
	OpCodes["symbol"] = 126;
	OpCodes["srange"] = 127;
	OpCodes["jump.pri"] = 128;
	OpCodes["switch"] = 129;
	OpCodes["casetbl"] = 130;
	OpCodes["swap.pri"] = 131;
	OpCodes["swap.alt"] = 132;
	OpCodes["pushaddr"] = 133;
	OpCodes["nop"] = 134;
	OpCodes["sysreq.d"] = 135;
	OpCodes["symtag"] = 136;

	std::map<std::string,int>::iterator i;
	for (i=OpCodes.begin(); i!=OpCodes.end(); i++)
	{
		CSymbols->AddSymbol((*i).first.c_str(), Sym_Reserved, 0);
	}

	CSymbols->AddSymbol("db", Sym_Reserved, 0);
	CSymbols->AddSymbol("PROC", Sym_Reserved, 0);
	CSymbols->AddSymbol("ENDP", Sym_Reserved, 0);
	CSymbols->AddSymbol("stat", Sym_Reserved, 0);

	char buf[24];
	sprintf(buf, "%d", cellsize?cellsize:4);
	std::string CellDef("CELL");
	std::string Cell(buf);
	CDefines->AddDefine(CellDef, Cell);
	CSymbols->AddSymbol("CELL", Sym_Define, 0);
}

char Compiler::OperChar(OpToken c)
{
	switch (c)
	{
	case Token_Or:
		return '|';
		break;
	case Token_Xor:
		return '^';
		break;
	case Token_And:
		return '&';
		break;
	case Token_Shr:
		return '>';
		break;
	case Token_Shl:
		return '<';
		break;
	case Token_Mod:
		return '%';
		break;
	case Token_Div:
		return '/';
		break;
	case Token_Mul:
		return '*';
		break;
	case Token_Sub:
		return '-';
		break;
	case Token_Add:
		return '+';
		break;
	case Token_Not:
		return '~';
		break;
	default:
		return Token_None;
		break;
	}
}

OpToken Compiler::OperToken(char c)
{
	switch (c)
	{
	case '|':
		return Token_Or;
		break;
	case '^':
		return Token_Xor;
		break;
	case '&':
		return Token_And;
		break;
	case '>':
		return Token_Shr;
		break;
	case '<':
		return Token_Shl;
		break;
	case '%':
		return Token_Mod;
		break;
	case '/':
		return Token_Div;
		break;
	case '*':
		return Token_Mul;
		break;
	case '-':
		return Token_Sub;
		break;
	case '+':
		return Token_Add;
		break;
	case '~':
		return Token_Not;
		break;
	default:
		return Token_None;
		break;
	}
}
	char OperToken(OpToken c);

/* Returns all the arguments in a list
 * This takes literals and expressions into account
 * In non-simple mode, It is assumed that the first ( is stripped!
 */
int Compiler::FindArguments(std::string &text, std::vector<std::string*> &List, int &end, bool simple)
{
	unsigned int i = 0, pos = 0;
	char c = 0, d = 0, l = 0, size = 0;
	std::stack<char> Stack;
	end = -1;
	bool temp = false;

	for (i=0; i<text.size(); i++)
	{
		d = text[i];

		/* Skip literal strings */
		if (l)
		{
			if (d == l)
				l = 0;
			c = d;
			continue;
		} else {
			l = literal(d);
			if (l)
			{
				c = d;
				continue;
			}
		}

		if (!Stack.empty())
		{
			/* Check if an expression is ending or starting */
			if (expr(d) == Stack.top())
			{
				Stack.pop();
				if (Stack.size() == 0 && i == text.size()-1)
				{
					goto lCheck;	//yeah yeah, I know this is bad
				}
			} else if (expr(d)) {
				Stack.push(d);
			}
		} else {
			/* Check if an expression or argument is starting */
			if (expr(d) && (i!=text.size()-1) && (text[i] != ')' && !simple))
			{
				Stack.push(d);
			} else {
				/* Check to push another argument */
				if (text[i] == ',' || (text[i] == ')' && !simple) || i==text.size()-1)
				{
lCheck:
					std::string *p = new std::string;
					if (!simple)
					{
						p->assign(text.substr(pos, 
							(i==text.size()-1 && text[i]!=')')?
							(i-pos+1):(i-pos)));
					} else {
						p->assign(text.substr(pos, i-pos+1));
					}
					Strip(*p);
					pos = i+1;
					if (p->size() < 1)
					{
						delete p;
					} else {
						List.push_back(p);
						size++;
					}
					end = i;
					if (text[i] == ')')
						break;
				}
			}
		}

		c = d;
	}

	if (!Stack.empty())
	{
		CError->ErrorMsg(Err_FatalTokenError);
	}

	return size;
}

void Compiler::ProcessDirective(std::string &text)
{
	Strip(text);
	if (text[0] == '#')
		text.erase(0, 1);
	std::string directive;
	std::string definition;
	/* Split the directive */
	StringBreak(text, directive, definition);
	if (!directive.compare("macro"))
	{
		Strip(definition);
		size_t argPos = definition.find('(', 0);
		if (argPos == std::string::npos)
		{
			CError->ErrorMsg(Err_InvalidMacro, curLine);
		} else {
			/* Store the symbol, which is read up to the occurrence of ( */
			std::string symbol;
			symbol.assign(definition.substr(0, argPos-1));
			/* Check if the symbol is already defined */
			SymbolList::Symbol *S;
			if ((S = CSymbols->FindSymbol(symbol)) != NULL)
			{
				CError->ErrorMsg(Err_SymbolRedef, S->sym.c_str(), S->line);
			}
			/* Store the argstring, which is the rest of the data */
			std::string argstring;
			argstring.assign(definition.substr(argPos+1, definition.size()-argPos+1));
			/* Parse the arg string, storing the results and the offset to the end */
			int bPos;
			std::vector<std::string *> ArgList;
			FindArguments(argstring, ArgList, bPos);
			/* Store the rest in a function string (macro definition) */
			std::string funcstring;
			funcstring.assign(argstring.substr(bPos+1, argstring.size()-bPos+1));
			/* Push the macro onto the Macro List */
			Strip(funcstring);
			Strip(symbol);
			MacroList::Macro *m = CMacros->AddMacroBegin(symbol, funcstring);
			std::vector<std::string *>::iterator i;
			for (i=ArgList.begin(); i!=ArgList.end(); i++)
			{
				CMacros->AddMacroArgument(m, *(*i));
			}
			CMacros->AddMacroEnd(m);
			/* Make sure to add the symbol */
			CSymbols->AddSymbol(symbol, Sym_Macro, curLine);
			//TODO: ClearList(ArgList);
		}
	} else if (!directive.compare("pragma")) {
		std::string pragma;
		std::string entry;
		StringBreak(definition, pragma, entry);
		if (pragma.compare("stacksize") == 0)
		{
			int stksz = atoi(entry.c_str());
			if (stksz < 100)
			{
				CError->ErrorMsg(Err_Invalid_Pragma);
			} else {
				stacksize = stksz;
			}
		} else {
			CError->ErrorMsg(Err_Invalid_Pragma);
		}
	} else if (!directive.compare("define")) {
		std::string symbol;
		std::string def;
        StringBreak(definition, symbol, def);
		SymbolList::Symbol *S;
		if ((S = CSymbols->FindSymbol(symbol)) != NULL)
		{
			CError->ErrorMsg(Err_SymbolRedef, S->sym.c_str(), S->line);
		}
		if (def.size() < 1)
			def.assign("1");
		CSymbols->AddSymbol(symbol, Sym_Define, curLine);
		CDefines->AddDefine(symbol, def);
	}
}

/* The evaluator works by storing expressions on a stack.
   Each expression is an RPN-ordered pair of lists for ops and values
   Every time the stack is popped, the expression is evaluated by searching
	for the highest operators and evaluating them.
   Note that string literals are not allowed here yet.
   */
int Compiler::Eval(std::string &str, SymbolType sym)
{
	std::stack<rpn *> Stack;
	std::string bpstr;
	int litidx = 0;
	int i = 0;
	rpn *r = new rpn;
	int pos = 0;

	Stack.push(r);

	for (i=0; i<(int)str.size(); i++)
	{
		if (OperToken(str[i]) != Token_None)
		{
			if ((i == (int)(str.size() - 1))
				|| ((OperToken(str[i]) != Token_Not && OperToken(str[i]) != Token_Sub)
					&& i == 0))
			{
				CError->ErrorMsg(Err_Unexpected_Char, str[i]);
				return 0;
			}
			if (str[i] == '<' && str[i+1] != '<')
			{
				CError->ErrorMsg(Err_Unexpected_Char, str[i]);
				return 0;
			}
			if (str[i] == '>' && str[i+1] != '>')
			{
				CError->ErrorMsg(Err_Unexpected_Char, str[i]);
				return 0;
			}
			/* Check to see if there are extra tokens */
			if (pos < i || (pos == i && (OperToken(str[i]) == Token_Sub)))
			{
				bpstr.assign(str.substr(pos, i-pos));
				Strip(bpstr);
				CExpr e(CError);
				e.Set(bpstr);
				e.Evaluate(sym);
				r->vals.push_back(e);
			}
			r->ops.push_back(str[i]);
			if (str[i] == '>' || str[i] == '<')
			{
				i++;
			}
			pos = i+1;
			continue;
		} else {
			if (str[i] == '(')
			{
				if (pos < i)
				{
					bpstr.assign(str.substr(pos, i-pos));
					Strip(bpstr);
					CExpr e(CError);
					e.Set(bpstr);
					e.Evaluate(sym);
					r->vals.push_back(e);
				}
				r = new rpn;
				Stack.push(r);
				pos = i+1;
				continue;
			} else if (str[i] == ')') {
				if (pos < i)
				{
					bpstr.assign(str.substr(pos, i-pos));
					Strip(bpstr);
					CExpr e(CError);
					e.Set(bpstr);
					e.Evaluate(sym);
					r->vals.push_back(e);
				}
				CExpr t;
				t = EvalRpn(r, sym);
				delete r;
				if (Stack.size() < 2)
				{
					while (Stack.size())
					{
						if (Stack.top())
							delete Stack.top();
						Stack.pop();
					}
					CError->ErrorMsg(Err_Unmatched_Token, str[i]);
					return 0;
				}
				Stack.pop();
				r = Stack.top();
				r->vals.push_back(t);
				pos = i + 1;
			} else if (i == (int)(str.size() - 1)) {
				if (pos < i)
				{
					bpstr.assign(str.substr(pos));
				} else if (pos == i) {
					bpstr.assign(str.substr(pos, 1));
				}
				if (pos < i || pos == i)
				{
					Strip(bpstr);
					CExpr e(CError);
					e.Set(bpstr);
					e.Evaluate(sym);
					r->vals.push_back(e);
				}
			}
		}
	}

	if (Stack.size() != 1)
	{
		while (!Stack.empty())
		{
			delete Stack.top();
			Stack.pop();
		}
		CError->ErrorMsg(Err_Unmatched_Token, '(');
		return 0;
	}
	
	rpn *r2 = Stack.top();
	Stack.pop();
	CExpr final;
    final = EvalRpn(r, sym);

	return final.GetNumber();
}

CExpr Compiler::EvalRpn(rpn *r, SymbolType sym)
{
	int i = 0, j = 0;
	char c = 0;
	CExpr er, el;
	std::vector<CExpr>::iterator Q;
	std::vector<char>::iterator R;

	while (r->ops.size())
	{
		for (i=1; i<Tokens_Total; i++)
		{
			char c = OperChar((OpToken)i);
			for (j=0; j<(int)r->ops.size(); j++)
			{
				if (r->ops[j] == c)
				{
					if ((int)r->vals.size() <= j)
						assert(0);// Can't have more ops than values
					el = r->vals[j];
					if (i != Token_Not)
					{
						if ((int)r->vals.size() <= j+1)
						{
							assert(0);
						}
						er = r->vals[j+1];
						el.Oper((OpToken)i, er);
					} else {
						el.Not();
					}
					R = r->ops.begin();
					Q = r->vals.begin();
					if (i != Token_Not)
					{
						R += j;
						Q += j;
						r->ops.erase(R);
						r->vals[j+1] = el;
						r->vals.erase(Q);
						j--;
					} else {
						R += j;
						r->ops.erase(R);
						r->vals[j] = el;
					}
				}
			}
		}
	}

	return r->vals[0];
}

void DestroyArgList(std::vector<std::string *> &List)
{
	std::vector<std::string *>::iterator i;

	for (i=List.begin(); i!=List.end(); i++)
	{
		if ( (*i) )
			delete (*i);
	}

	List.clear();
}

void Compiler::PrintCodeList()
{
	std::vector<Asm *>::iterator i;
	std::vector<int>::iterator j;

	for (i=CodeList.begin(); i!=CodeList.end(); i++)
	{
		printf("OP: %d [%d]\n", (*i)->op, (*i)->cip);
		for (j=(*i)->params.begin(); j!=(*i)->params.end(); j++)
		{
			printf("\tParameter: %d\n", (*j));
		}
	}
}

int Compiler::DerefSymbol(std::string &str, SymbolType sym)
{
	int val = 0;

	SymbolList::Symbol *S = NULL;
	if ( ((S = CSymbols->FindSymbol(str)) == NULL) )
	{
		if (sym == Sym_Label)
		{
			S = CSymbols->AddSymbol(str, Sym_Label, -1);
		} else {
			CError->ErrorMsg(Err_Unknown_Symbol, str.c_str());
			return 0;
		}
	}
	if (sym != Sym_Dat && S->type != sym)
	{
		CError->ErrorMsg(Err_Invalid_Symbol);
		return 0;
	}
	switch (S->type)
	{
	case Sym_Proc:
		{
			val = PROC->GetCip(str);
			if (val == ProcMngr::ncip)
			{
				CError->ErrorMsg(Err_Invalid_Symbol);
				return 0;
			}
			break;
			}
	case Sym_Native:
		{
			val = CNatives->GetNativeId(str);
			if (val == NativeMngr::ncip)
			{
				CError->ErrorMsg(Err_Invalid_Symbol);
				return 0;
				}
			break;
		}
	case Sym_Dat:
		{
			val = DAT->GetOffset(str);
			if (val == DataMngr::nof)
			{
				CError->ErrorMsg(Err_Invalid_Symbol);
				return 0;
				}
			break;
		}
	case Sym_Label:
		{
			val = CLabels->GetCip(str);
			if (val == LabelMngr::ncip)
			{
				/* Labels we handle differently. 
				   Add it to the label queue
			     */
				CLabels->AddLabel(S, LabelMngr::ncip);
				CLabels->QueueLabel(str, CurAsm());
			}
			break;
		}
	default:
		{
			CError->ErrorMsg(Err_Invalid_Symbol);
			return 0;
			break;
		}
	}

	return val;
}

bool Compiler::IsSymbol(std::string &str)
{
	SymbolList::Symbol *S = 0;

	if ( (S = CSymbols->FindSymbol(str)) == NULL )
		return false;

	return true;
}
