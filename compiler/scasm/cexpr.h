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

#ifndef _INCLUDE_CEXPR_H
#define _INCLUDE_CEXPR_H

/* This class is used for a single expression element
 * It reads in a symbol and evaluates it.
 */

typedef enum
{
	Token_None,
	Token_Or,
	Token_Xor,
	Token_And,
	Token_Shr,
	Token_Shl,
	Token_Mod,
	Token_Div,
	Token_Mul,
	Token_Sub,
	Token_Add,
	Token_Not,
	/* End */
	Tokens_Total,
} OpToken;

typedef enum
{
	Val_None,
	Val_Error,
	Val_String,
	Val_String_Hex,
	Val_String_Number,
	Val_Number_Hex,
	Val_Number,
	Val_Block,
	Val_Hex,
	Val_Float,
} cExprType;

class CExpr
{
public:
	CExpr();
	CExpr(ErrorMngr *e);
	CExpr(ErrorMngr *e, std::string &text);
	void Set(std::string &text);
	const char *GetString(int *d=NULL);
	int Analyze();
	cExprType Evaluate(int symNum = 0);
	cExprType GetType();
	int GetNumber();
	float GetFloat() { return fData; }
	int Size();
	void Clear();
	void Not();
	void Oper(OpToken op, CExpr &e);
	~CExpr();
private:
	void Update();
	char IsHex(char c);
	char IsLiteral(char c);
	int DeHex(std::string blk);
private:
	char *block;
	std::string data;
	cExprType type;
	float fData;
	int numVal;
	bool bDone;
private:
	ErrorMngr *CError;
};

#endif //_INCLUDE_CEXPR_H
