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

ErrorMngr::ErrorMngr()
{
	printf("Not instantiated with a compiler.");
	Cmp = NULL;
	line = -1;
	assert(Cmp);
}

void ErrorMngr::Clear()
{
	Totals[0] = 0;
	Totals[1] = 0;
	Totals[2] = 0;
	Totals[3] = 0;
	HighestError = Err_None;	
}

int ErrorMngr::CurLine()
{
	return ((Compiler *)Cmp)->CurLine();
}

int ErrorMngr::CurCip()
{
	return ((Compiler *)Cmp)->CurCip();
}

ErrorMngr::ErrorMngr(void *c)
{
	Cmp = c;
	DefineErrors();
	Totals[0] = 0;
	Totals[1] = 0;
	Totals[2] = 0;
	Totals[3] = 0;
	line = -1;
	HighestError = Err_None;
}

ErrorType ErrorMngr::GetErrorType(ErrorCode id)
{
	if (id > fatals_start && id < fatals_end)
		return Err_Fatal;
	if (id > warnings_start && id < warnings_end)
		return Err_Warning;
	if (id > notices_start && id < notices_end)
		return Err_Notice;
	if (id > errors_start && id < errors_end)
		return Err_Error;
	return Err_None;
}

void ErrorMngr::DefineErrors()
{
	List.resize(fatals_end+1);

	List.at(Warning_Hex_Start) = "Hexadecimal notation is 0xN, 0 missing";
	List.at(Warning_Null_Expression) = "Bad expression will evaluate to 0";
	List.at(Warning_Param_Count) = "Expected %d parameters, found %d";

	List.at(Err_String_Terminate) = "String not terminated properly";
	List.at(Err_String_Extra) = "Unexpected characters after string end (character '%c')";
	List.at(Err_Unexpected_Char) = "Unexpected character found (character '%c')";
	List.at(Err_Wandering_Stuff) = "Unknown directive placed outside of a section";
	List.at(Err_Symbol_Reuse) = "Symbol \"%s\" already defined on line %d";
	List.at(Err_Invalid_Stor) = "Invalid DAT storage identifier \"%s\"";
	List.at(Err_Unknown_Symbol) = "Unknown symbol \"%s\"";
	List.at(Err_Symbol_Type) = "Expected symbol type %d, got %d (bad symbol)";
	List.at(Err_Invalid_Symbol) = "Invalid symbol";
	List.at(Err_Opcode) = "Invalid or unrecognized opcode";
	List.at(Err_Unmatched_Token) = "Unmatched token '%c'";
	List.at(Err_Param_Count) = "Expected %d parameters, found %d";
	List.at(Err_Unknown_Define) = "Unknown define referenced";
	List.at(Err_Misplaced_Directive) = "Misplaced preprocessor directive";
	List.at(Err_Bad_Label) = "Label referenced without being created";
	List.at(Err_Bad_Not) = "Wrong type argument to bit-complement";
	List.at(Err_Invalid_Operator) = "Operator used on bad type";
	List.at(Err_Invalid_Pragma) = "Invalid pragma";
	List.at(Err_Invalid_Proc) = "Procedure referenced that does not exist";

	List.at(Err_FileNone) = "No file specified";
	List.at(Err_FileOpen) = "Could not open file \"%s\"";
	List.at(Err_NoMemory) = "Ran out of memory";
	List.at(Err_PragmaStacksize) = "Invalid stacksize on #pragma stacksize";
	List.at(Err_InvalidMacro) = "Invalid or empty macro definition";
	List.at(Err_SymbolRedef) = "Symbol \"%s\" already defined on line %d";
	List.at(Err_Reserved) = "Symbol assigned to a reserved token";
	List.at(Err_MacroParamCount) = "Parameter count for macro \"%s\" incorrect";
	List.at(Err_FatalTokenError) = "Fatal token error encountered"; 
	List.at(Err_Invalid_Section) = "Section identifier \"%s\" is not valid, ignoring section.";
}

void ErrorMngr::PrintReport()
{
	static char *ErrorSwi[4] = {"Notice", "Warning", "Error", "Fatal Error"};
	int i = 0;

	printf("+---------------------------+\n");

	for (i=0; i<4; i++)
	{
		printf("| %ss: %s%d   |\n", ErrorSwi[i], (i!=3)?"\t\t":"\t", Totals[i]);
	}

	printf("+---------------------------+\n");
}

void ErrorMngr::ErrorMsg(ErrorCode error, ...)
{
	static char *ErrorSwi[4] = {"Notice", "Warning", "Error", "Fatal Error"};
	static char errbuf[1024];
	ErrorType type = GetErrorType(error);
	if (type == -1)
		return;

	int curLine = 0;

	if (line == -1)
	{
		curLine = ((Compiler *)Cmp)->CurLine();
	} else {
		curLine = line;
		line = -1;
	}

	va_list argptr;
	va_start(argptr, error);
	
	if (((Compiler *)Cmp)->CurLine() == -1)
		sprintf(errbuf, "%s(%d): %s\n", ErrorSwi[type], error, GetError(error));
	else 
		sprintf(errbuf, "%s(%d) on line %d: %s\n", ErrorSwi[type], error, curLine, GetError(error));
	vprintf(errbuf, argptr);

	va_end(argptr);

	Totals[type]++;

	if (type == Err_Fatal)
		exit(0);
	if (type > HighestError)
		HighestError = type;
}

const char *ErrorMngr::GetError(ErrorCode id)
{
	if (id == fatals_start || id == fatals_end)
		return NULL;
	if (id == warnings_start || id == warnings_end)
		return NULL;
	if (id == notices_start || id == notices_end)
		return NULL;
	if (id == errors_start || id == errors_end)
		return NULL;
	if (id < notices_start || id > fatals_end)
		return NULL;
	return List.at(id);
}

int ErrorMngr::DerefSymbol(std::string &str, int sym)
{
	return ((Compiler *)Cmp)->DerefSymbol(str, (SymbolType)sym);
}

bool ErrorMngr::IsSymbol(std::string &str)
{
	return ((Compiler *)Cmp)->IsSymbol(str);
}

void ErrorMngr::SetLine(int ln)
{
	line = ln;
}

