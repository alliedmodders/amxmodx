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

#ifndef _INCLUDE_AMX_ERROR
#define _INCLUDE_AMX_ERROR

typedef enum
{
	Err_None=-1,
	Err_Notice,
	Err_Warning,
	Err_Error,
	Err_Fatal,
} ErrorType;

typedef enum
{
	notices_start,
	notices_end,

	warnings_start,
	Warning_Hex_Start,
	Warning_Null_Expression,
	Warning_Param_Count,
	warnings_end,

	errors_start,
	Err_String_Terminate,
	Err_String_Extra,
	Err_Unexpected_Char,
	Err_Invalid_Section,
	Err_Wandering_Stuff,
	Err_Symbol_Reuse, /* Non-fatal version of Redef */
	Err_Invalid_Stor,
	Err_Unknown_Symbol,
	Err_Symbol_Type,
	Err_Invalid_Symbol,
	Err_Opcode,
	Err_Unmatched_Token,
	Err_Param_Count,
	errors_end,

	fatals_start,
	Err_FileNone,
	Err_FileOpen,
	Err_NoMemory,
	Err_PragmaStacksize,
	Err_InvalidMacro,
	Err_SymbolRedef,
	Err_Reserved,
	Err_MacroParamCount,
	Err_FatalTokenError,
	fatals_end,

} ErrorCode;

class ErrorMngr
{
private:
	void DefineErrors();
	const char *GetError(ErrorCode id);
	ErrorType GetErrorType(ErrorCode id);
private:
	std::vector<const char *> List;
	ErrorType HighestError;
	void *Cmp;
	int Totals[4];
public:
	ErrorMngr();
	ErrorMngr(void *c);
	void Clear();
	void ErrorMsg(ErrorCode error, ...);
	ErrorType GetStatus() { return HighestError; }
	void PrintReport();
};

#endif //_INCLUDE_AMX_ERROR
