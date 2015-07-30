// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// SMC Parser Functions
//

#if defined _textparse_smc_included
	#endinput
#endif
#define _textparse_smc_included

/**
 * Everything below describes the SMC Parse, or "SourceMod Configuration" format.
 * This parser is entirely event based.  You must hook events to receive data.
 * The file format itself is nearly identical to Valve's KeyValues format (also known as VDF).
 */

/**
 * The SMC file format is defined as:
 *    WHITESPACE: 0x20, \n, \t, \r
 *    IDENTIFIER: Any ASCII character EXCLUDING ", {, }, ;, //, / *, or WHITESPACE.
 *    STRING    : Any set of symbols enclosed in quotes.
 *
 *    Note: if a STRING does not have quotes, it is parsed as an IDENTIFIER.
 *
 * Basic syntax is comprised of SECTIONBLOCKs.
 *    A SECTIONBLOCK defined as:
 *
 *    SECTIONNAME
 *    {
 *        OPTION
 *    }
 *
 * OPTION can be repeated any number of times inside a SECTIONBLOCK.
 * A new line will terminate an OPTION, but there can be more than one OPTION per line.
 * OPTION is defined any of:
 *    "KEY"  "VALUE"
 *    SECTIONBLOCK
 *
 * SECTIONNAME, KEY, VALUE, and SINGLEKEY are strings
 * SECTIONNAME cannot have trailing characters if quoted, but the quotes can be optionally removed.
 * If SECTIONNAME is not enclosed in quotes, the entire sectionname string is used (minus surrounding whitespace).
 * If KEY is not enclosed in quotes, the key is terminated at first whitespace.
 * If VALUE is not properly enclosed in quotes, the entire value string is used (minus surrounding whitespace).
 * The VALUE may have inner quotes, but the key string may not.
 *
 * For an example, see scripting/testsuite/textparse_test.cfg
 *
 * WHITESPACE should be ignored.
 * Comments are text occurring inside the following tokens, and should be stripped
 * unless they are inside literal strings:
 *    ;<TEXT>
 *    //<TEXT>
 *    / *<TEXT> * /
 */

/**
 * Parser invalid code.
 */
enum SMCParser
{
	Invalid_SMCParser = 0
};

/**
 * Parse result directive.
 */
enum SMCResult
{
	SMCParse_Continue,          /* Continue parsing */
	SMCParse_Halt,              /* Stop parsing here */
	SMCParse_HaltFail           /* Stop parsing and return failure */
};

/**
 * Parse error codes.
 */
enum SMCError
{
	SMCError_Okay = 0,          /* No error */
	SMCError_StreamOpen,        /* Stream failed to open */
	SMCError_StreamError,       /* The stream died... somehow */
	SMCError_Custom,            /* A custom handler threw an error */
	SMCError_InvalidSection1,   /* A section was declared without quotes, and had extra tokens */
	SMCError_InvalidSection2,   /* A section was declared without any header */
	SMCError_InvalidSection3,   /* A section ending was declared with too many unknown tokens */
	SMCError_InvalidSection4,   /* A section ending has no matching beginning */
	SMCError_InvalidSection5,   /* A section beginning has no matching ending */
	SMCError_InvalidTokens,     /* There were too many unidentifiable strings on one line */
	SMCError_TokenOverflow,     /* The token buffer overflowed */
	SMCError_InvalidProperty1,  /* A property was declared outside of any section */
};

/**
 * Creates a new SMC parser.
 * This is used to set parse hooks.
 *
 * @return              A new handle to an SMC Parse structure.
 */
native SMCParser:SMC_CreateParser();

/**
 * Disposes of an SMC parser.
 *
 * @param handle        Handle to an SMC Parse structure.
 *
 * @return              True if disposed, false otherwise.
 */
native SMC_DestroyParser(&SMCParser:handle);

/**
 * Parses a config file.
 *
 * @param handle        A handle to an SMC Parse structure.
 * @param file          A string containing the file path.
 * @param line          An optional by reference cell to store the last line number read.
 * @param col           An optional by reference cell to store the last column number read.
 * @param data          An optional handle or value to pass through to callback functions
 *
 * @return              An SMCParseError result.
 * @error               Invalid or corrupt handle.
 */
native SMCError:SMC_ParseFile(SMCParser:handle, const file[], &line = 0, &col = 0, any:data = 0);

/**
 * Sets the SMC_ParseStart function of a parse handle.
 *
 * @note  Below is the prototype of callback:
 *        -
 *          Called when parsing is started.
 *
 *          @param handle        Handle to an SMC Parse structure.
 *          @param data          Handle or value passed in SMC_ParseFile
 *
 *          @noreturn
 *
 *          public OnParseStart(SMCParser:handle, any:data)
 *        -
 * @param handle        Handle to an SMC Parse structure.
 * @param func          A ParseStart callback.
 *
 * @noreturn
 * @error               Invalid or corrupt handle.
 */
native SMC_SetParseStart(SMCParser:handle, const func[]);

/**
 * Sets the SMC_ParseEnd function of a parse handle.
 *
 * @note  Below is the prototype of callback:
 *        -
 *          Called when parsing is halted.
 *
 *          @param handle        Handle to an SMC Parse structure.
 *          @param halted        True if abnormally halted, false otherwise.
 *          @param failed        True if parsing failed, false otherwise.
 *          @param data          Handle or value passed in SMC_ParseFile
 *
 *          @noreturn
 *
 *          public OnParseEnd(SMCParser:handle, bool:halted, bool:failed, any:data)
 *        -
 * @param handle        Handle to an SMC Parse structure.
 * @param func          A ParseEnd callback.
 *
 * @noreturn
 * @error               Invalid or corrupt handle.
 */
native SMC_SetParseEnd(SMCParser:handle, const func[]);

/**
 * Sets the three main reader functions.
 *
 * @note  Enclosing quotes are always stripped.
 * @note  Below is the prototype of callbacks:
 *        -
 *          NewSection:
 *              Called when the parser finds a new section or sub-section.
 *
 *              @param handle           Handle to an SMC Parse structure.
 *              @param name             String containing section name.
 *              @param data             Handle or value passed in SMC_ParseFile
 *
 *              @return                 An SMCResult action to take.
 *
 *              public SMCResult:OnNewSection(SMCParser:handle, const name[], any:data)
 *
 *          KeyValue:
 *              Called when the parser finds a new key/value pair.
 *
 *              @param handle        Handle to an SMC Parse structure.
 *              @param key           String containing key name.
 *              @param value         String containing value name.
 *              @param data          Handle or value passed in SMC_ParseFile
 *
 *              @return              An SMCResult action to take.
 *
 *              public SMCResult:OnKeyValue(SMCParser:handle, const key[], const value[], any:data)
 *
 *          EndSection:
 *              Called when the parser finds the end of the current section.
 *
 *              @param handle        Handle to an SMC Parse structure.
 *              @param data          Handle or value passed in SMC_ParseFile
 *
 *              @return              An SMCResult action to take.
 *
 *              public SMCResult:OnEndSection(SMCParser:handle, any:data)
 * -
 * @param handle        Handle to an SMC Parse structure.
 * @param kv            A KeyValue callback.
 * @param ns            An optional NewSection callback.
 * @param es            An optional EndSection callback.
 *
 * @noreturn
 */
native SMC_SetReaders(SMCParser:smc, const kvFunc[], const nsFunc[] = "", const esFunc[] = "");

/**
 * Sets a raw line reader on an text parser handle.
 *
 * @note  Below is the prototype of callbacks:
 *        -
 *          Called whenever a raw line is read.
 *
 *          @param handle        Handle to an SMC Parse structure.
 *          @param line          A string containing the raw line from the file.
 *          @param lineno        The line number it occurs on.
 *          @param data          Handle or value passed in SMC_ParseFile
 *
 *          @return              An SMCResult action to take.
 *
 *          public SMCResult:SMC_RawLine(SMCParser:handle, const line[], lineno, any:data)
 *        -
 * @param handle        Handle to an SMC Parse structure.
 * @param func          A RawLine callback.
 *
 * @noreturn
 */
native SMC_SetRawLine(SMCParser:handle, const func[]);

/**
 * Gets an error string for an SMCError code.
 *
 * @note  SMCError_Okay returns false.
 * @note  SMCError_Custom (which is thrown on SMCParse_HaltFail) returns false.
 *
 * @param error         The SMCParseError code.
 * @param buffer        A string buffer for the error (contents undefined on failure).
 * @param buf_max       The maximum size of the buffer.
 *
 * @return              True on success, false otherwise.
 */
native bool:SMC_GetErrorString(SMCError:error, buffer[], buf_max);
