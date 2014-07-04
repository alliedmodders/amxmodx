/* Regular Expression API
 * (C)2004 by David "BAILOPAN" Anderson
 * Licensed under the GNU General Public License.
 * No warranties of any kind.
 */

#if defined _regex_included
  #endinput
#endif
#define _regex_included

#if AMXX_VERSION_NUM >= 175
 #pragma reqlib regex
 #if !defined AMXMODX_NOAUTOLOAD
  #pragma loadlib regex
 #endif
#else
 #pragma library regex
#endif


enum Regex
{
	REGEX_MATCH_FAIL = -2,
	REGEX_PATTERN_FAIL,
	REGEX_NO_MATCH,
	REGEX_OK
};


/**
 * @section Flags for compiling regex expressions.  
 *          These come directly from the pcre library and can be used in MatchRegex and CompileRegex.
 *
 * @note    To be used with regex_compile_ex. 
 *          Only available in 1.8.3 and above.
 */ 
#define PCRE_CASELESS           0x00000001  /* Ignore Case */
#define PCRE_MULTILINE          0x00000002  /* Multilines (affects ^ and $ so that they match the start/end of a line rather than matching the start/end of the string). */
#define PCRE_DOTALL             0x00000004  /* Single line (affects . so that it matches any character, even new line characters). */
#define PCRE_EXTENDED           0x00000008  /* Pattern extension (ignore whitespace and # comments). */
#define PCRE_ANCHORED           0x00000010  /* Force pattern anchoring. */
#define PCRE_DOLLAR_ENDONLY     0x00000020  /* $ not to match newline at end. */
#define PCRE_UNGREEDY           0x00000200  /* Invert greediness of quantifiers */
#define PCRE_NOTEMPTY           0x00000400  /* An empty string is not a valid match. */
#define PCRE_UTF8               0x00000800  /* Use UTF-8 Chars */
#define PCRE_NO_UTF8_CHECK      0x00002000  /* Do not check the pattern for UTF-8 validity (only relevant if PCRE_UTF8 is set) */
#define PCRE_UCP                0x20000000  /* Use Unicode properties for \ed, \ew, etc. */

/**
 * Regex expression error codes.
 *
 * @note    To be used with regex_compile_ex and regex_match_ex natives. 
 *          Only available in 1.8.3 and above.
 */
enum RegexError
{
	REGEX_ERROR_NONE           =  0,    /* No error */
	REGEX_ERROR_NOMATCH        = -1,    /* No match was found */
	REGEX_ERROR_NULL           = -2,
	REGEX_ERROR_BADOPTION      = -3,
	REGEX_ERROR_BADMAGIC       = -4,
	REGEX_ERROR_UNKNOWN_OPCODE = -5,
	REGEX_ERROR_NOMEMORY       = -6,
	REGEX_ERROR_NOSUBSTRING    = -7,
	REGEX_ERROR_MATCHLIMIT     = -8,
	REGEX_ERROR_CALLOUT        = -9,    /* Never used by PCRE itself */
	REGEX_ERROR_BADUTF8        = -10,
	REGEX_ERROR_BADUTF8_OFFSET = -11,
	REGEX_ERROR_PARTIAL        = -12,
	REGEX_ERROR_BADPARTIAL     = -13,
	REGEX_ERROR_INTERNAL       = -14,
	REGEX_ERROR_BADCOUNT       = -15,
	REGEX_ERROR_DFA_UITEM      = -16,
	REGEX_ERROR_DFA_UCOND      = -17,
	REGEX_ERROR_DFA_UMLIMIT    = -18,
	REGEX_ERROR_DFA_WSSIZE     = -19,
	REGEX_ERROR_DFA_RECURSE    = -20,
	REGEX_ERROR_RECURSIONLIMIT = -21,
	REGEX_ERROR_NULLWSLIMIT    = -22,   /* No longer actually used */
	REGEX_ERROR_BADNEWLINE     = -23,
	REGEX_ERROR_BADOFFSET      = -24,
	REGEX_ERROR_SHORTUTF8      = -25,
	REGEX_ERROR_RECURSELOOP    = -26,
	REGEX_ERROR_JIT_STACKLIMIT = -27,
	REGEX_ERROR_BADMODE        = -28,
	REGEX_ERROR_BADENDIANNESS  = -29,
	REGEX_ERROR_DFA_BADRESTART = -30,
	REGEX_ERROR_JIT_BADOPTION  = -31,
	REGEX_ERROR_BADLENGTH      = -32,
};


/**
 * Precompile a regular expression.  Use this if you intend on using the 
 * same expression multiple times.  Pass the regex handle returned here to
 * regex_match_c to check for matches.
 *
 * @param pattern		The regular expression pattern.
 * @param errcode		Error code encountered, if applicable.
 * @param error		Error message encountered, if applicable.
 * @param maxLen		Maximum string length of the error buffer.
 * @param flags		General flags for the regular expression.
 *						i = Ignore case
 *						m = Multilines (affects ^ and $ so that they match 
 *							the start/end of a line rather than matching the 
 *							start/end of the string).
 *						s = Single line (affects . so that it matches any character, 
 *							even new line characters).
 *						x = Pattern extension (ignore whitespace and # comments).
 *
 * @return				-1 on error in the pattern, > valid regex handle (> 0) on success.
 *
 * @note				This handle is automatically freed on map change.  However,
 *						if you are completely done with it before then, you should
 *						call regex_free on this handle.
 */
native Regex:regex_compile(const pattern[], &ret, error[], maxLen, const flags[]="");

/**
 * Matches a string against a pre-compiled regular expression pattern.
 *
 *
 * @param pattern		The regular expression pattern.
 * @param string		The string to check.
 * @param ret			Error code, if applicable, or number of results on success.
 *
 * @return				-2 = Matching error (error code is stored in ret)
 *						 0 = No match.
 *						>1 = Number of results.
 *
 * @note				You should free the returned handle (with regex_free())
 *						when you are done with this pattern.
 *
 * @note				Use the regex handle passed to this function to extract
 *						matches with regex_substr().
 */
native regex_match_c(const string[], Regex:pattern, &ret);

/**
 * Precompile a regular expression.  
 *
 * @note  Use this if you intend on using the ame expression multiple times.  
 *        Pass the regex handle returned here to regex_match_ex to check for matches.
 *
 * @note  Unlike regex_compile, this allows you to use directly PCRE flags, and
 *        to get a more complete set of regular expression error codes. 
 *        Only available in 1.8.3 and above.
 *
 * @param pattern		The regular expression pattern.
 * @param flags			General flags for the regular expression, see PCRE_* defines.
 * @param error			Error message encountered, if applicable.
 * @param maxLen		Maximum string length of the error buffer.
 * @param errcode		Regex type error code encountered, if applicable.
 *
 * @return				Valid regex handle (> 0) on success, or -1 on failure.
 */
native Regex:regex_compile_ex(const pattern[], flags = 0, error[]= "", maxLen = 0, &RegexError:errcode = REGEX_ERROR_NONE);

/**
 * Matches a string against a pre-compiled regular expression pattern.
 *
 * @note  Use the regex handle passed to this function to extract
 *        matches with regex_substr().
 *
 * @note  You should free the returned handle with regex_free()
 *        when you are done with this pattern.
 *
 * @note  Unlike regex_match_c(), this allows you to get a more complete 
 *        set of regular expression error codes and parameter is optional.
 *        Only available in 1.8.3 and above.
 *
 * @param str			The string to check.
 * @param regex			Regex Handle from regex_compile_ex()
 * @param ret			Error code, if applicable.
 *
 * @return				-2 = Matching error (error code is stored in ret)
 *						 0 = No match.
 *						>1 = Number of results.
 */
native regex_match_ex(Handle:regex, const str[], &RegexError:ret = REGEX_ERROR_NONE);

/**
 * Matches a string against a regular expression pattern.
 *
 * @note				If you intend on using the same regular expression pattern
 *						multiple times, consider using regex_compile and regex_match_c 
 *						instead of making this function reparse the expression each time.
 *
 * @param string		The string to check.
 * @param pattern		The regular expression pattern.
 * @param ret			Error code, or result state of the match.
 * @param error		Error message, if applicable.
 * @param maxLen		Maximum length of the error buffer.
 * @param flags		General flags for the regular expression.
 *						i = Ignore case
 *						m = Multilines (affects ^ and $ so that they match 
 *							the start/end of a line rather than matching the 
 *							start/end of the string).
 *						s = Single line (affects . so that it matches any character, 
 *							even new line characters).
 *						x = Pattern extension (ignore whitespace and # comments).
 *
 * @return				-2 = Matching error (error code is stored in ret)
 *						-1 = Error in pattern (error message and offset # in error and ret)
 *						 0 = No match.
 *						>1 = Handle for getting more information (via regex_substr)
 *
 * @note				Flags only exist in amxmodx 1.8 and later.
 * @note				You should free the returned handle (with regex_free())
 *						when you are done extracting all of the substrings.
 */
native Regex:regex_match(const string[], const pattern[], &ret, error[], maxLen, const flags[] = "");

/**
 * Returns a matched substring from a regex handle.
 * 
 * @note  Substring ids start at 0 and end at ret - 1, where ret is from the corresponding
 *        regex_match, regex_match_c or regex_match_ex function call.
 *
 * @param id			The regex handle to extract data from.
 * @param str_id		The index of the expression to get - starts at 0, and ends at ret - 1.
 * @param buffer		The buffer to set to the matching substring.
 * @param maxLen		The maximum string length of the buffer.
 *
 * @return				1 on success, otherwise 0 on failure.
 */
native regex_substr(Regex:id, str_id, buffer[], maxLen);

/**
 * Frees the memory associated with a regex result, and sets the handle to 0.
 * 
 * @note  This must be called on all results from regex_match() when you are done extracting
 *        the results with regex_substr().
 *
 * @note  The results of regex_compile() or regex_compile_ex() (and subsequently, regex_match_c() or regex_match_ex()) 
 *        only need to be freed when you are done using the pattern.
 *
 * @note  Do not use the handle again after freeing it!
 *
 * @param id			The regex handle to free.
 * @noreturn
 */ 
native regex_free(&Regex:id);
