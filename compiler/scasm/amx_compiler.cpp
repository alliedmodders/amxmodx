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
	Init();
	curLine = -1;
	cellsize = 4;
}

Compiler::~Compiler()
{
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
	Init();
	filename.assign(f);
	curLine = -1;
	cellsize = 4;
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
	CLabels = new LabelMngr;
	/* Load Native management */
	CNatives = new NativeMngr;
	/* Load the default symbols + opcodes */
	InitOpcodes();
}

bool Compiler::Parse()
{
	std::ifstream fp(filename.c_str());
	char buffer[256] = {0};
	curLine = 0;
	AsmSection sec = Asm_None;
	lastCip = -1;

	if (!fp.is_open())
	{
		CError->ErrorMsg(Err_FileOpen, filename.c_str());
		return false;
	}

	while (!fp.eof())
	{
		fp.getline(buffer, 255);
		curLine++;
		
		/* Check for preprocessor directives */
		if (buffer[0] == '#')
		{
			std::string procline(buffer);
			ProcessDirective(procline);
			continue;
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
					CError->ErrorMsg(Err_Symbol_Reuse, symbol.c_str(), S->GetLine());
					continue;
				}
				
				if (fmt.compare("db") && fmt.compare("stat"))
				{
					CError->ErrorMsg(Err_Invalid_Stor, fmt.c_str());
					continue;
				}

				/* Add and evaluate the expression */
				CExpr e(CError);
				e.Set(data);
				e.Evaluate();
				
				/* Add into the DAT section */
				DAT->Add(symbol, e, (fmt.compare("db")==0)?true:false);
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
				if ( (S->GetType() != Sym_Proc) )
				{
					CError->ErrorMsg(Err_Symbol_Type, Sym_Proc, S->GetType());
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
					CError->ErrorMsg(Err_Invalid_Symbol, line.c_str(), S->GetLine());
					continue;
				}
				S = CSymbols->AddSymbol(line.c_str(), Sym_Native, CurLine());
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
						CError->ErrorMsg(Err_Invalid_Symbol, params.c_str(), S->GetLine());
						continue;
					}
					/* Create instruction */
					Asm *ASM = new Asm;
					ASM->cip = ++lastCip;
					ASM->op = OpCodes["proc"];
					/* Add symbol */
					S = CSymbols->AddSymbol(params.c_str(), Sym_Proc, CurLine());
					/* Add to code list */
					CodeList.push_back(ASM);
					/* Add to PROC list */
					PROC->AddProc(S, ASM);
				} else if (code.compare("ENDP") == 0) {
					/* This is, in theory, not needed */
				} else if (params.size() < 1 && code[code.size()-1] == ':') {
					/* Label! */
					/* Check if symbol is valid */
					if (!IsValidSymbol(code))
					{
						CError->ErrorMsg(Err_Invalid_Symbol);
						continue;
					}
					/* Check if the symbol is already used */
					if ( (S = CSymbols->FindSymbol(code)) != NULL)
					{
						CError->ErrorMsg(Err_Invalid_Symbol, code.c_str(), S->GetLine());
						continue;
					}
					S = CSymbols->AddSymbol(code.c_str(), Sym_Label, CurLine());
					CLabels->AddLabel(S, lastCip+1);
				} else {
					/* Check if there is a valid opcode */
					int op = OpCodes[code];
					int argPos;
					if (op == OP_NONE)
					{
						CError->ErrorMsg(Err_Opcode);
						continue;
					}

					Asm *ASM = new Asm;
					std::vector<std::string *> paramList;

					if (params.size() > 0)
					{
						FindArguments(params, paramList, argPos, true);
						if (argPos != params.size()-1)
						{
							printf("line: %s|%d|%d\n", line.c_str(), argPos, params.size());
							CError->ErrorMsg(Err_Unexpected_Char, params[argPos]);
							continue;
						}
					}

					switch (op)
					{
						case OP_LOAD_PRI:
						{
								break;
						}
						case OP_LOAD_ALT:
						{
								break;
						}
						case OP_LOAD_S_PRI:
						{
								break;
						}
						case OP_LOAD_S_ALT:
						{
								break;
						}
						case OP_LREF_PRI:
						{
								break;
						}
						case OP_LREF_ALT:
						{
								break;
						}
						case OP_LREF_S_PRI:
						{
								break;
						}
						case OP_LREF_S_ALT:
						{
								break;
						}
						case OP_LOAD_I:
						{
								break;
						}
						case OP_LODB_I:
						{
								break;
						}
						case OP_CONST_PRI:
						{
								break;
						}
						case OP_CONST_ALT:
						{
								break;
						}
						case OP_ADDR_PRI:
						{
								break;
						}
						case OP_ADDR_ALT:
						{
								break;
						}
						case OP_STOR_PRI:
						{
								break;
						}
						case OP_STOR_ALT:
						{
								break;
						}
						case OP_STOR_S_PRI:
						{
								break;
						}
						case OP_STOR_S_ALT:
						{
								break;
						}
						case OP_SREF_PRI:
						{
								break;
						}
						case OP_SREF_ALT:
						{
								break;
						}
						case OP_SREF_S_PRI:
						{
								break;
						}
						case OP_SREF_S_ALT:
						{
								break;
						}
						case OP_STOR_I:
						{
								break;
						}
						case OP_STRB_I:
						{
								break;
						}
						case OP_LIDX:
						{
								break;
						}
						case OP_LIDX_B:
						{
								break;
						}
						case OP_IDXADDR:
						{
								break;
						}
						case OP_IDXADDR_B:
						{
								break;
						}
						case OP_ALIGN_PRI:
						{
								break;
						}
						case OP_ALIGN_ALT:
						{
								break;
						}
						case OP_LCTRL:
						{
								break;
						}
						case OP_SCTRL:
						{
								break;
						}
						case OP_MOVE_PRI:
						{
								break;
						}
						case OP_MOVE_ALT:
						{
								break;
						}
						case OP_XCHG:
						{
								break;
						}
						case OP_PUSH_PRI:
						{
								break;
						}
						case OP_PUSH_ALT:
						{
								break;
						}
						case OP_PUSH_R:
						{
								break;
						}
						case OP_PUSH_C:
						{
								break;
						}
						case OP_PUSH:
						{
								break;
						}
						case OP_PUSH_S:
						{
								break;
						}
						case OP_POP_PRI:
						{
								break;
						}
						case OP_POP_ALT:
						{
								break;
						}
						case OP_STACK:
						{
								break;
						}
						case OP_HEAP:
						{
								break;
						}
						case OP_PROC:
						{
								break;
						}
						case OP_RET:
						{
								break;
						}
						case OP_RETN:
						{
								break;
						}
						case OP_CALL:
						{
								break;
						}
						case OP_CALL_PRI:
						{
								break;
						}
						case OP_JUMP:
						{
								break;
						}
						case OP_JREL:
						{
								break;
						}
						case OP_JZER:
						{
								break;
						}
						case OP_JNZ:
						{
								break;
						}
						case OP_JEQ:
						{
								break;
						}
						case OP_JNEQ:
						{
								break;
						}
						case OP_JLESS:
						{
								break;
						}
						case OP_JLEQ:
						{
								break;
						}
						case OP_JGRTR:
						{
								break;
						}
						case OP_JGEQ:
						{
								break;
						}
						case OP_JSLESS:
						{
								break;
						}
						case OP_JSLEQ:
						{
								break;
						}
						case OP_JSGRTR:
						{
								break;
						}
						case OP_JSGEQ:
						{
								break;
						}
						case OP_SHL:
						{
								break;
						}
						case OP_SHR:
						{
								break;
						}
						case OP_SSHR:
						{
								break;
						}
						case OP_SHL_C_PRI:
						{
								break;
						}
						case OP_SHL_C_ALT:
						{
								break;
						}
						case OP_SHR_C_PRI:
						{
								break;
						}
						case OP_SHR_C_ALT:
						{
								break;
						}
						case OP_SMUL:
						{
								break;
						}
						case OP_SDIV:
						{
								break;
						}
						case OP_SDIV_ALT:
						{
								break;
						}
						case OP_UMUL:
						{
								break;
						}
						case OP_UDIV:
						{
								break;
						}
						case OP_UDIV_ALT:
						{
								break;
						}
						case OP_ADD:
						{
								break;
						}
						case OP_SUB:
						{
								break;
						}
						case OP_SUB_ALT:
						{
								break;
						}
						case OP_AND:
						{
								break;
						}
						case OP_OR:
						{
								break;
						}
						case OP_XOR:
						{
								break;
						}
						case OP_NOT:
						{
								break;
						}
						case OP_NEG:
						{
								break;
						}
						case OP_INVERT:
						{
								break;
						}
						case OP_ADD_C:
						{
								break;
						}
						case OP_SMUL_C:
						{
								break;
						}
						case OP_ZERO_PRI:
						{
								break;
						}
						case OP_ZERO_ALT:
						{
								break;
						}
						case OP_ZERO:
						{
								break;
						}
						case OP_ZERO_S:
						{
								break;
						}
						case OP_SIGN_PRI:
						{
								break;
						}
						case OP_SIGN_ALT:
						{
								break;
						}
						case OP_EQ:
						{
								break;
						}
						case OP_NEQ:
						{
								break;
						}
						case OP_LESS:
						{
								break;
						}
						case OP_LEQ:
						{
								break;
						}
						case OP_GRTR:
						{
								break;
						}
						case OP_GEQ:
						{
								break;
						}
						case OP_SLESS:
						{
								break;
						}
						case OP_SLEQ:
						{
								break;
						}
						case OP_SGRTR:
						{
								break;
						}
						case OP_SGEQ:
						{
								break;
						}
						case OP_EQ_C_PRI:
						{
								break;
						}
						case OP_EQ_C_ALT:
						{
								break;
						}
						case OP_INC_PRI:
						{
								break;
						}
						case OP_INC_ALT:
						{
								break;
						}
						case OP_INC:
						{
								break;
						}
						case OP_INC_S:
						{
								break;
						}
						case OP_INC_I:
						{
								break;
						}
						case OP_DEC_PRI:
						{
								break;
						}
						case OP_DEC_ALT:
						{
								break;
						}
						case OP_DEC:
						{
								break;
						}
						case OP_DEC_S:
						{
								break;
						}
						case OP_DEC_I:
						{
								break;
						}
						case OP_MOVS:
						{
								break;
						}
						case OP_CMPS:
						{
								break;
						}
						case OP_FILL:
						{
								break;
						}
						case OP_HALT:
						{
								break;
						}
						case OP_BOUNDS:
						{
								break;
						}
						case OP_SYSREQ_PRI:
						{
								break;
						}
						case OP_SYSREQ_C:
						{
								break;
						}
						case OP_FILE:
						{
								break;
						}
						case OP_LINE:
						{
								break;
						}
						case OP_SYMBOL:
						{
								break;
						}
						case OP_SRANGE:
						{
								break;
						}
						case OP_JUMP_PRI:
						{
								break;
						}
						case OP_SWITCH:
						{
								break;
						}
						case OP_CASETBL:
						{
								break;
						}
						case OP_SWAP_PRI:
						{
								break;
						}
						case OP_SWAP_ALT:
						{
								break;
						}
						case OP_PUSHADDR:
						{
								break;
						}
						case OP_NOP:
						{
								break;
						}
						case OP_SYSREQ_D:
						{
								break;
						}
						case OP_SYMTAG:
						{
								break;
						}
					}
				}
			}
		}
	}

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
 * It is assumed that the first ( is stripped!
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
			if (expr(d) && (i!=text.size()-1) && (text[i] != ')'))
			{
				Stack.push(d);
			} else {
				/* Check to push another argument */
				if (text[i] == ',' || (text[i] == ')') || i==text.size()-1)
				{
lCheck:
					std::string *p = new std::string;
					p->assign(text.substr(pos, 
						(i==text.size()-1 && text[i]!=')')?
						(i-pos+1):(i-pos)));
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
				CError->ErrorMsg(Err_SymbolRedef, curLine, S->GetSymbol(), S->GetLine());
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
			CSymbols->AddSymbol(symbol.c_str(), Sym_Macro, curLine);
			//TODO: ClearList(ArgList);
		}
	} else if (!directive.compare("stacksize")) {
		
	} else if (!directive.compare("define")) {
		std::string symbol;
		std::string def;
        StringBreak(definition, symbol, def);
		SymbolList::Symbol *S;
		if ((S = CSymbols->FindSymbol(symbol)) != NULL)
		{
			CError->ErrorMsg(Err_SymbolRedef, curLine, S->GetSymbol(), S->GetLine());
		}
		if (def.size() < 1)
			def.assign("1");
		CSymbols->AddSymbol(symbol.c_str(), Sym_Define, curLine);
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
	int val = 0;
	CExpr e;

	Stack.push(r);

	for (i=0; i<(int)str.size(); i++)
	{
		if (OperToken(str[i]) != Token_None)
		{
			if (i == (int)(str.size() - 1))
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
			if (pos < i)
			{
				bpstr.assign(str.substr(pos, i-pos));
				e.Set(bpstr);
				val = 0;
				if (!e.Analyze() && IsValidSymbol(bpstr))
				{
					SymbolList::Symbol *S = NULL;
					if ( (S = CSymbols->FindSymbol(bpstr)) == NULL)
					{
						CError->ErrorMsg(Err_Unknown_Symbol, bpstr.c_str());
						return 0;
					}
					if (S->GetType() != sym)
					{
						assert(0);
						CError->ErrorMsg(Err_Invalid_Symbol);
						return 0;
					}
					switch (S->GetType())
					{
						case Sym_Proc:
							{
								val = PROC->GetCip(bpstr);
								if (val == ProcMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Native:
							{
								val = CNatives->GetNativeId(bpstr);
								if (val == NativeMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Data:
							{
								val = DAT->GetOffset(bpstr);
								if (val == DataMngr::nof)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Label:
							{
								val = CLabels->GetCip(bpstr);
								if (val == LabelMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
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
				} else if (e.Analyze() && (e.Evaluate() == Val_Number)) {
					val = e.GetNumber();
				} else {
					assert(0);
					CError->ErrorMsg(Err_Invalid_Symbol);
					return 0;
				}
				r->vals.push_back(val);
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
					e.Set(bpstr);
					val = 0;
					if (!e.Analyze() && IsValidSymbol(bpstr))
					{
						SymbolList::Symbol *S = NULL;
						if ( (S = CSymbols->FindSymbol(bpstr)) == NULL)
						{
							CError->ErrorMsg(Err_Unknown_Symbol, bpstr.c_str());
							return 0;
						}
						if (S->GetType() != sym)
						{
							assert(0);
							CError->ErrorMsg(Err_Invalid_Symbol);
							return 0;
						}
						switch (S->GetType())
						{
						case Sym_Proc:
							{
								val = PROC->GetCip(bpstr);
								if (val == ProcMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Native:
							{
								val = CNatives->GetNativeId(bpstr);
								if (val == NativeMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Data:
							{
								val = DAT->GetOffset(bpstr);
								if (val == DataMngr::nof)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Label:
							{
								val = CLabels->GetCip(bpstr);
								if (val == LabelMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
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
					} else if (e.Analyze() && (e.Evaluate() == Val_Number)) {
						val = e.GetNumber();
					} else {
						assert(0);
						CError->ErrorMsg(Err_Invalid_Symbol);
						return 0;
					}
					r->vals.push_back(val);
				}
				r = new rpn;
				Stack.push(r);
				pos = i+1;
				continue;
			} else if (str[i] == ')') {
				if (pos < i)
				{
					bpstr.assign(str.substr(pos, i-pos));
					e.Set(bpstr);
					val = 0;
					if (!e.Analyze() && IsValidSymbol(bpstr))
					{
						SymbolList::Symbol *S = NULL;
						if ( (S = CSymbols->FindSymbol(bpstr)) == NULL)
						{
							CError->ErrorMsg(Err_Unknown_Symbol, bpstr.c_str());
							return 0;
						}
						if (S->GetType() != sym)
						{
							assert(0);
							CError->ErrorMsg(Err_Invalid_Symbol);
							return 0;
						}
						switch (S->GetType())
						{
						case Sym_Proc:
							{
								val = PROC->GetCip(bpstr);
								if (val == ProcMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Native:
							{
								val = CNatives->GetNativeId(bpstr);
								if (val == NativeMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Data:
							{
								val = DAT->GetOffset(bpstr);
								if (val == DataMngr::nof)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Label:
							{
								val = CLabels->GetCip(bpstr);
								if (val == LabelMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
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
					} else if (e.Analyze() && (e.Evaluate() == Val_Number)) {
						val = e.GetNumber();
					} else {
						assert(0);
						CError->ErrorMsg(Err_Invalid_Symbol);
						return 0;
					}
					r->vals.push_back(val);
				}
				val = EvalRpn(r, sym);
				delete r;
				if (Stack.size() < 2)
				{
					//TODO: Clear memory
					CError->ErrorMsg(Err_Unmatched_Token, str[i]);
					return 0;
				}
				Stack.pop();
				r = Stack.top();
				r->vals.push_back(val);
				pos = i + 1;
			} else if (i == (int)(str.size() - 1)) {
				if (pos < i)
				{
					bpstr.assign(str.substr(pos, i-pos));
				} else if (pos == i) {
					bpstr.assign(str.substr(pos, 1));
				}
				if (pos < i || pos == i)
				{
					e.Set(bpstr);
					val = 0;
					if (!e.Analyze() && IsValidSymbol(bpstr))
					{
						SymbolList::Symbol *S = NULL;
						if ( (S = CSymbols->FindSymbol(bpstr)) == NULL)
						{
							CError->ErrorMsg(Err_Unknown_Symbol, bpstr.c_str());
							return 0;
						}
						if (S->GetType() != sym)
						{
							assert(0);
							CError->ErrorMsg(Err_Invalid_Symbol);
							return 0;
						}
						switch (S->GetType())
						{
						case Sym_Proc:
							{
								val = PROC->GetCip(bpstr);
								if (val == ProcMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Native:
							{
								val = CNatives->GetNativeId(bpstr);
								if (val == NativeMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Data:
							{
								val = DAT->GetOffset(bpstr);
								if (val == DataMngr::nof)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
								}
								break;
							}
						case Sym_Label:
							{
								val = CLabels->GetCip(bpstr);
								if (val == LabelMngr::ncip)
								{
									CError->ErrorMsg(Err_Invalid_Symbol);
									return 0;
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
					} else if (e.Analyze() && (e.Evaluate() == Val_Number)) {
						val = e.GetNumber();
					} else {
						CError->ErrorMsg(Err_Invalid_Symbol);
						return 0;
					}
					r->vals.push_back(val);
				}
			}
		}
	}

	if (Stack.size() != 1)
	{
		//TODO: Clear memory
		CError->ErrorMsg(Err_Unmatched_Token, '(');
		return 0;
	}
	
	rpn *r2 = Stack.top();
	Stack.pop();
    val = EvalRpn(r, sym);

	return val;
}

int Compiler::EvalRpn(rpn *r, SymbolType sym)
{
	int i = 0, j = 0;
	char c = 0;
	int lval = 0;
	int rval = 0;
	int nval = 0;
	std::vector<int>::iterator Q;
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
					lval = r->vals[j];
					if (i != Token_Not)
					{
						if ((int)r->vals.size() <= j+1)
							assert(0);
						rval = r->vals[j+1];
					}
					switch (i)
					{
					case Token_Xor:
						{
							nval = lval ^ rval;
							break;
						}
					case Token_Shr:
						{
							nval = lval >> rval;
							break;
						}
					case Token_Sub:
						{
							nval = lval - rval;
							break;
						}
					case Token_Mod:
						{
							nval = lval % rval;
							break;
						}
					case Token_Mul:
						{
							nval = lval * rval;
							break;
						}
					case Token_Div:
						{
							nval = (int)(lval / rval);
							break;
						}
					case Token_Shl:
						{
							nval = lval << rval;
							break;
						}
					case Token_And:
						{
							nval = lval & rval;
							break;
						}
					case Token_Or:
						{
							nval = lval | rval;
							break;
						}
					case Token_Add:
						{
							nval = lval + rval;
							break;
						}
					case Token_Not:
						{
							nval = ~lval;
							break;
						}
					default:
						{
							nval = 0;
							break;
						}
					}
					R = r->ops.begin();
					Q = r->vals.begin();
					if (i != Token_Not)
					{
						R += j;
						Q += j;
						r->ops.erase(R);
						r->vals[j+1] = nval;
						r->vals.erase(Q);
						j--;
					} else {
						R += j;
						r->ops.erase(R);
						r->vals[j] = nval;
					}
				}
			}
		}
	}

	return r->vals[0];
}