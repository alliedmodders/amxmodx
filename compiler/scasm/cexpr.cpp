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

#include <string>
#include "amxasm.h"

CExpr::CExpr()
{
	numVal = 0;
	type = Val_None;
	block = 0;
	CError = NULL;
}

CExpr::CExpr(ErrorMngr *e)
{
	numVal = 0;
	type = Val_None;
	block = 0;
	CError = e;
}

CExpr::CExpr(ErrorMngr *e, std::string &text)
{
	numVal = 0;
	type = Val_None;
	block = 0;
	CError = e;
	data.assign(text);
}

void CExpr::Clear()
{
	if (type == Val_Block && block)
		delete [] block;
	numVal = 0;
	type = Val_None;
	block = 0;
}

CExpr::~CExpr()
{
	if (block)
		delete [] block;
}

char CExpr::IsLiteral(char c)
{
	if (c == '"')
		return '"';
	if (c == '\'')
		return '\'';
	return 0;
}

char CExpr::IsHex(char c)
{
	if (c >= '0' && c <= '9')
		return c;
	if (c >= 'A' && c <= 'F')
		return c;
	if (c >= 'a' && c <= 'f')
		return c;
	return 0;
}

cExprType CExpr::GetType()
{
	return type;
}

void CExpr::Set(std::string &text)
{
	data.assign(text);
}

int CExpr::DeHex(std::string blk)
{
	size_t pos = 0, xc = 0, xpos = 0;
	/* run through the characters */
	for (pos = 0; pos < blk.size(); pos++)
	{
		if (blk[pos] == 'x')
		{
			xc++;
			if (xc > 1)
				break;
			xpos = pos;
		} else if (blk[pos] != ' ' 
			&& (blk[pos] < '0' || blk[pos] > '9')
			&& (!xc || (xc && !IsHex(blk[pos])))) {
			CError->ErrorMsg(Err_Unexpected_Char, blk[pos]);
			return 0;
		}
	}
	if (xc > 1)
	{
		CError->ErrorMsg(Err_Unexpected_Char, 'x');
		return 0;
	}
	if (xc)
	{
		if (xpos == 0 || blk[xpos-1] != '0')
		{
			if (CError)
				CError->ErrorMsg(Warning_Hex_Start);
		}
		blk.erase(0, xpos+1);
		pos = 0;
		int j = 0;
		while (blk[pos])
		{
			blk[pos] -= 48;
			if (blk[pos] > 16)
				blk[pos] -= 7;
			if (blk[pos] >= 16)
				blk[pos] -= 32;
			if (blk[pos] >= 16 || blk[pos] < 0)
			{
				if (CError)
					CError->ErrorMsg(Err_Unexpected_Char, blk[pos]);
				return 0;
			}
			j *= 16;
			j += blk[pos];
			pos++;
		}

		return j;
	}

	return atoi(blk.c_str());
}

int CExpr::Size()
{
	if (type == Val_String || type == Val_Block)
		return numVal;
	if (type == Val_Number)
		return (SMALL_CELL_SIZE/8);
	return 0;
}

const char *CExpr::GetString(int *d)
{
	const char *ret = 0;
	
	if (type == Val_String)
	{
		ret = data.c_str();
		if (d != NULL)
			*d = numVal;
	} else if (type == Val_Block) {
		ret = block;
		if (d != NULL)
			*d = numVal;
	}

	return ret;
}

int CExpr::GetNumber()
{
	return numVal;
}

/* Returns true if the expr can be evaluated */
int CExpr::Analyze()
{
	size_t pos = 0, xc = 0, xpos = 0;
	/* run through the characters */
	for (pos = 0; pos < data.size(); pos++)
	{
		if (data[pos] == 'x')
		{
			xc++;
			if (xc > 1)
				return 0;
			xpos = pos;
		} else if (data[pos] != ' ' 
			&& (data[pos] < '0' || data[pos] > '9')
			&& (!xc || (xc && !IsHex(data[pos])))) {
			return 0;
		}
	}

	return 1;
}

cExprType CExpr::Evaluate()
{
	size_t i = 0, blk = 0;
	char litc = 0, c = 0, csave = 0;
	cExprType t = Val_None;
	std::string num;

	block = new char[2];

	if (data.find('\'', 0) != std::string::npos || data.find('"', 0) != std::string::npos)
	{
		/* STRESS TEST */
		for (i=0; i<data.size(); i++)
		{
			c = data[i];
			if (IsLiteral(c) != 0)
			{
				if (litc == IsLiteral(c))
				{
					litc = 0;
					/* The literal has terminated.  Expect no more. */
					if (i != data.size() - 1)
					{
						t = Val_Error;
						if (CError)
							CError->ErrorMsg(Err_String_Extra, data[i+1]);
					} else {
						/* STRING DISCOVERED */
						t = Val_String;
						numVal = (int)(data.size()+1);
						break;
					}
				} else {
					litc = IsLiteral(c);
				}
			}
		}
	} else if (data.find(' ', 0) != std::string::npos) {
		/* Build a mem block from values, store length in numVal */
		t = Val_Block;
		size_t pos = 0, npos = 0;
		numVal = 0;
		pos = data.find(' ', 0);
		block[numVal++] = DeHex(data.substr(0, pos));
		while ((pos = data.find(' ', pos)) != std::string::npos)
		{
			npos = data.find(' ', pos+1);
			block = (char *)realloc(block, numVal+2);
			if (npos != std::string::npos)
			{
				block[numVal] = (char)DeHex(data.substr(pos, npos-pos));
			} else {
				block[numVal] = (char)DeHex(data.substr(pos));
			}
			pos++;
			numVal++;
		}
	} else {
		/* Just get the number */
		t = Val_Number;
		numVal = DeHex(data);
		char buf[32];
		sprintf(buf, "%d", numVal);
		data.assign(buf);
	}

	if (litc)
	{
		if (CError)
			CError->ErrorMsg(Err_String_Terminate);
	}

	type = t;

	return t;
}
