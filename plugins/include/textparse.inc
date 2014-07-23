
#if defined _textparse_included
	#endinput
#endif
#define _textparse_included

/**
 * This parser API is entirely event based.  You must hook events to receive data.
 * The file format can ben either INI or SMC (also known as "SourceMod Configuration".
 * SMC format is nearly identical to  VDF (also known as "Valve's KeyValues format").
 * Also please note INI format is handled differently. Because more "simple" to parse, some 
 * event doesn't exist and callback prototype can be different.
 */

/**
 * The INI file format is defined as:
 *    WHITESPACE: 0x20, \n, \t, \r
 *    IDENTIFIER: A-Z a-z 0-9 _ - , + . $ ? / 
 *    STRING    : Any set of symbols
 * 
 * Basic syntax is comprised of SECTIONs.
 *    A SECTION is defined as:
 *    [SECTIONNAME]
 *    OPTION
 *    OPTION
 *    OPTION...
 *
 * SECTIONNAME is an IDENTIFIER.
 *    OPTION can be repeated any number of times, once per line.
 *    OPTION is defined as one of:
 *      KEY = "VALUE"
 *      KEY = VALUE
 *      KEY
 *    Where KEY is an IDENTIFIER and VALUE is a STRING.
 * 
 * WHITESPACE should always be omitted.
 *    COMMENTS should be stripped, and are defined as text occurring in:
 *    ;<TEXT>
 * 
 * Example file below.  Note that the second line is technically invalid.  
 * The event handler must decide whether this should be allowed.
 *    --FILE BELOW--
 *    [gaben]
 *    hi = clams
 *    bye = "NO CLAMS"
 *
 *    [valve]
 *    cannot
 *    maintain
 *    products
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
 
enum TextParser
{
	Invalid_TextParser = 0
};

/**
 * Parse result directive.
 */
enum SMCResult
{
	SMCParse_Continue,          /**< Continue parsing */
	SMCParse_Halt,              /**< Stop parsing here */
	SMCParse_HaltFail           /**< Stop parsing and return failure */
};

/**
 * Parse error codes.
 */
enum SMCError
{
	SMCError_Okay = 0,          /**< No error */
	SMCError_StreamOpen,        /**< Stream failed to open */
	SMCError_StreamError,       /**< The stream died... somehow */
	SMCError_Custom,            /**< A custom handler threw an error */
	SMCError_InvalidSection1,   /**< A section was declared without quotes, and had extra tokens */
	SMCError_InvalidSection2,   /**< A section was declared without any header */
	SMCError_InvalidSection3,   /**< A section ending was declared with too many unknown tokens */
	SMCError_InvalidSection4,   /**< A section ending has no matching beginning */
	SMCError_InvalidSection5,   /**< A section beginning has no matching ending */
	SMCError_InvalidTokens,     /**< There were too many unidentifiable strings on one line */
	SMCError_TokenOverflow,     /**< The token buffer overflowed */
	SMCError_InvalidProperty1,  /**< A property was declared outside of any section */
};


/**
 * Creates a new text parser.  
 * This is used to set parse hooks.
 *
 * @param ini_format    Sets whether INI format or SMC should be used.
 *
 * @return              A new handle to an Text Parse structure.
 */
native TextParser:SMC_CreateParser(bool:ini_format = false);

/**
 * Disposes of a text parser.
 *
 * @param handle        Handle to a Text Parse.
 * @return              True if disposed, false otherwise.
 */
native SMC_DestroyParser(&TextParser:handle);

/**
 * Parses a config file.
 *
 * @note  If using INI format, you can expect only the following SMCError_* constants: 
 *        SMCError_Okay, SMCError_StreamOpen and SMCError_Custom.
 *
 * @param handle        A handle to an Text Parse structure.
 * @param file          A string containing the file path.
 * @param line          An optional by reference cell to store the last line number read.
 * @param col           An optional by reference cell to store the last column number read.
 
 * @return              A SMCParseError result.
 * @error               Invalid or corrupt handle. 
 */
native SMCError:SMC_ParseFile(TextParser:handle, const file[], &line = 0, &col = 0);

/**
 * Sets the SMC_ParseStart function of a parse handle.
 * Below is the prototype of callback.:
 *
 * -  ParseStart: 
 *      Called when parsing is started.
 *
 *      @param handle        The Text Parse handle.
 *      @noreturn
 *
 *      public SMC_ParseStart(TextParser:handle)
 *
 * @param handle        Handle to a Text Parse.
 * @param func          A ParseStart callback.
 *
 * @noreturn
 * @error               Invalid or corrupt handle. 
 */
native SMC_SetParseStart(TextParser:handle, const func[]);

/**
 * Sets the SMC_ParseEnd of a parse handle.
 * Below is the prototype of callback.:
 *
 * -  ParseEnd: 
 *      Called when parsing is halted.
 *
 *      @param handle        The Text Parse handle.
 *      @param halted        True if abnormally halted, false otherwise.
 *      @param failed        True if parsing failed, false otherwise.
 *                           If using INI format, this is only true when
 *                           a callback returns SMCResult_HaltFail.
 *      @noreturn
 *
 *      public SMC_ParseEnd(TextParser:handle, bool:halted, bool:failed)
 *
 * @param handle        Handle to a Text Parse.
 * @param func          A ParseEnd callback..
 *
 * @noreturn
 * @error               Invalid or corrupt handle. 
 */
native SMC_SetParseEnd(TextParser:handle, const func[]);

/**
 * Sets the three main reader functions.
 * Below are the different prototypes of callbacks:
 *
 * -  NewSection: 
 *      Called when the parser finds the end of the current section.
 *
 *      INI:
 *          @param section          Name of section in between the [ and ] characters.
 *          @param invalid_tokens   True if invalid tokens were detected in the name.
 *          @param close_bracket    True if a closing bracket was detected, false otherwise.
 *          @param extra_tokens     True if extra tokens were detected on the line.
 *          @param curtok           Contains current token in the line where the section name starts.
 *                                  can add to this offset when failing to point to a token.
 *          @return                 An SMCResult action to take.
 *
 *          public SMCResult:SMC_NewSection(TextParser:handle, const section[], bool:invalid_tokens, bool:close_bracket, bool:extra_tokens, curtok)
 *      
 *      SMC:
 *          @note  Enclosing quotes are always stripped.
 *          @param handle           The Text Parse handle.
 *          @param name             String containing section name.
 *          @return                 An SMCResult action to take.
 *
 *         public SMCResult:SMC_NewSection(TextParser:handle, const name[])
 *
 * - KeyValue: 
 *      Called when the parser finds a new key/value pair.
 *
 *      INI:
 *          @param handle           The Text Parse handle.
 *          @param key              Name of key.
 *          @param value            String containing value (with quotes stripped, if any).
 *          @param invalid_tokens   Whether or not the key contained invalid tokens.
 *          @param equal_token      There was an '=' sign present (in case the value is missing).
 *          @param quotes           Whether value was enclosed in quotes.
 *          @param curtok           Contains the token index of the start of the value string.  
 *                                  This can be changed when returning false.
 *          @return                 SMCResult directive.
 * 
 *          public SMCResult:SMC_KeyValue(TextParser:handle, const key[], const value[], bool:invalid_tokens, bool:equal_token, bool:quotes, curtok)
 *
 *      SMC:
 *          @note  Enclosing quotes are always stripped.
 *
 *          @param handle        The Text Parse handle.
 *          @param key           String containing key name.
 *          @param value         String containing value name.
 *          @return              A SMCResult action to take.
 *
 *          public SMCResult:SMC_KeyValue(TextParser:handle, const key[], const value[])
 *
 * - EndSection:
 *       Called when the parser finds the end of the current section.
 *       Only with SMC format.
 
 *       @param handle        The Text Parse handle.
 * 
 *       public SMCResult:SMC_EndSection(TextParser:handle)
 * -
 * @param handle        The Text parse handle.
 * @param ns            A NewSection callback. 
 * @param kv            A KeyValue callback. 
 * @param es            An EndSection callback. Only available for SMC config format.
 *
 * @noreturn
 */
native SMC_SetReaders(TextParser:smc, const nsFunc[], const kvFunc[], const esFunc[] = "");

/**
 * Sets a raw line reader on an text parser handle.
 * Below is the prototype of callback.:
 *
 * -  RawLine: 
 *      Called whenever a raw line is read.
 *
 *      INI:
 *          @param handle       The Text Parse handle.
 *          @param line         Contents of line.
 *          @param lineno       The line number it occurs on.
 *          @param curtok       Pointer to optionally store failed position in string.
 *          @return             SMCResult directive.
 *
 *          public SMCResult:SMC_RawLine(TextParser:smc, const line[], lineno, curtok)
 *
 *      SMC:
 *          @param handle        The Text Parse handle.
 *          @param line          A string containing the raw line from the file.
 *          @param lineno        The line number it occurs on.
 *          @return              An SMCResult action to take.
 *
 *          public SMCResult:SMC_RawLine(TextParser:handle, const line[], lineno)
 *
 * @param handle        Handle to an Text Parse.
 * @param func          A RawLine callback.
 * @noreturn
 */
native SMC_SetRawLine(TextParser:handle, const func[]);

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