// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// Regular Expressions API
//

#if defined _regex_included
	#endinput
#endif
#define _regex_included

#pragma reqlib regex
#if !defined AMXMODX_NOAUTOLOAD
	#pragma loadlib regex
#endif


enum Regex
{
	REGEX_MATCH_FAIL   = -2,
	REGEX_PATTERN_FAIL = -1,
	REGEX_NO_MATCH     =  0,
	REGEX_OK           =  1
};

/**
 * Flags for compiling regex expressions.
 * These come directly from the pcre library and can be used in regex_compile_ex.
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
#define PCRE_NEVER_UTF          0x00010000  /* Lock out interpretation of the pattern as UTF-8 */
#define PCRE_FIRSTLINE          0x00040000  /* Force matching to be before newline */
#define PCRE_DUPNAMES           0x00080000  /* Allow duplicate names for subpattern */
#define PCRE_NEWLINE_CR         0x00100000  /* Specify that a newline is indicated by a single character CR           )                            */
#define PCRE_NEWLINE_CRLF       0x00300000  /* specify that a newline is indicated by the two-character CRLF sequence )  Overrides the default     */
#define PCRE_NEWLINE_ANY        0x00400000  /* Specify that any Unicode newline sequence should be recognized.        )  newline definition (LF)   */
#define PCRE_NEWLINE_ANYCRLF    0x00500000  /* Specify that any of CR, LF and CRLF sequences should be recognized     )                            */
#define PCRE_UCP                0x20000000  /* Change the way PCRE processes \B, \b, \D, \d, \S, \s, \W, \w etc. to use Unicode properties */

/**
 * Regex expression error codes.
 * This can be used with regex_compile_ex and regex_match_ex.
 */
enum /*RegexError*/
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
	REGEX_ERROR_UNSET          = -33
};

/**
 * Precompile a regular expression.
 *
 * @note  Use this if you intend on using the same expression multiple times.
 *        Pass the regex handle returned here to regex_match_c to check for matches.
 *
 * @note  This handle is automatically freed on map change.  However,
 *        if you are completely done with it before then, you should
 *        call regex_free on this handle.
 *
 * @note  Consider using regex_compile_ex instead if you want to use PCRE_* flags.
 *
 * @param pattern       The regular expression pattern.
 * @param ret           Error code encountered, if applicable.
 * @param error         Error message encountered, if applicable.
 * @param maxLen        Maximum string length of the error buffer.
 * @param flags         General flags for the regular expression.
 *                      i = Ignore case
 *                      m = Multilines (affects ^ and $ so that they match
 *                          the start/end of a line rather than matching the
 *                          start/end of the string).
 *                      s = Single line (affects . so that it matches any character,
 *                          even new line characters).
 *                      x = Pattern extension (ignore whitespace and # comments).
 *
 * @return              -1 on error in the pattern, > valid regex handle (> 0) on success.
 */
native Regex:regex_compile(const pattern[], &ret = 0, error[] = "", maxLen = 0, const flags[]="");

/**
 * Matches a string against a pre-compiled regular expression pattern.
 *
 * @note  You should free the returned handle with regex_free()
 *        when you are done with this pattern.
 *
 * @note  Use the regex handle passed to this function to extract
 *        matches with regex_substr().
 *
 * @param string        The string to check.
 * @param pattern       The regular expression pattern.
 * @param ret           Error code, if applicable, or number of results on success. See REGEX_ERROR_* defines.
 *
 * @return              -2 = Matching error (error code is stored in ret)
 *                       0 = No match.
 *                      >1 = Number of results.
 */
native regex_match_c(const string[], Regex:pattern, &ret = 0);

/**
 * Matches a string against a regular expression pattern.
 *
 * @note  If you intend on using the same regular expression pattern
 *        multiple times, consider using regex_compile and regex_match_ex
 *        instead of making this function reparse the expression each time.
 *
 * @note  Flags only exist in amxmodx 1.8 and later.
 *
 * @note  You should free the returned handle with regex_free()
 *        when you are done extracting all of the substrings.
 *
 * @param string        The string to check.
 * @param pattern       The regular expression pattern.
 * @param ret           Error code, or result state of the match.
 * @param error         Error message, if applicable.
 * @param maxLen        Maximum length of the error buffer.
 * @param flags         General flags for the regular expression.
 *                      i = Ignore case
 *                      m = Multilines (affects ^ and $ so that they match
 *                          the start/end of a line rather than matching the
 *                          start/end of the string).
 *                      s = Single line (affects . so that it matches any character,
 *                          even new line characters).
 *                      x = Pattern extension (ignore whitespace and # comments).
 *
 * @return              -2 = Matching error (error code is stored in ret)
 *                      -1 = Error in pattern (error message and offset # in error and ret)
 *                       0 = No match.
 *                      >1 = Handle for getting more information (via regex_substr)
 */
native Regex:regex_match(const string[], const pattern[], &ret = 0, error[] = "", maxLen = 0, const flags[] = "");

/**
 * Returns a matched substring from a regex handle.
 *
 * @note  Substring ids start at 0 and end at ret - 1, where ret is from the corresponding
 *        regex_match* function call.
 *
 * @param id            The regex handle to extract data from.
 * @param str_id        The index of the expression to get - starts at 0, and ends at ret - 1.
 * @param buffer        The buffer to set to the matching substring.
 * @param maxLen        The maximum string length of the buffer.
 *
 * @return              1 on success, otherwise 0 on failure.
 */
native regex_substr(Regex:id, str_id, buffer[], maxLen);

/**
 * Frees the memory associated with a regex result, and sets the handle to 0.
 *
 * @note  This must be called on all results from regex_match() when you are done extracting
 *        the results with regex_substr().
 *
 * @note  The results of regex_compile() or regex_compile_ex() (and subsequently, regex_match_c())
 *        only need to be freed when you are done using the pattern.
 *
 * @note  Do not use the handle again after freeing it!
 *
 * @param id            The regex handle to free.
 * @noreturn
 */
native regex_free(&Regex:id);


/**
 * The following natives are only available in 1.8.3 and above.
 */

/**
 * Precompile a regular expression.
 *
 * @note  Use this if you intend on using the same expression multiple times.
 *        Pass the regex handle returned here to regex_match_c() to check for matches.
 *
 * @note  Unlike regex_compile(), this allows you to use PCRE flags directly.
 *
 * @param pattern       The regular expression pattern.
 * @param flags         General flags for the regular expression, see PCRE_* defines.
 * @param error         Error message encountered, if applicable.
 * @param maxLen        Maximum string length of the error buffer.
 * @param errcode       Regex type error code encountered, if applicable. See REGEX_ERROR_* defines.
 *
 * @return              Valid regex handle (> 0) on success, or -1 on failure.
 */
native Regex:regex_compile_ex(const pattern[], flags = 0, error[]= "", maxLen = 0, &errcode = 0);

/**
 * Matches a string against a pre-compiled regular expression pattern, matching all
 * occurrences of the pattern inside the string. This is similar to using the "g" flag
 * in perl regex.
 *
 * @note  You should free the returned handle (with regex_free())
 *        when you are done with this pattern.
 *
 * @note  Use the regex handle passed to this function to extract
 *        matches with regex_substr().
 *
 * @param pattern       The regular expression pattern.
 * @param string        The string to check.
 * @param ret           Error code, if applicable, or number of results on success.
 *                      See REGEX_ERROR_* defines.
 *
 * @return              -2 = Matching error (error code is stored in ret)
 *                       0 = No match.
 *                      >1 = Number of results.
 */
native regex_match_all_c(const string[], Regex:pattern, &ret = 0);

/**
 * Matches a string against a regular expression pattern, matching all occurrences of the
 * pattern inside the string. This is similar to using the "g" flag in perl regex.
 *
 * @note  If you intend on using the same regular expression pattern
 *        multiple times, consider using regex_compile and regex_match_ex
 *        instead of making this function reparse the expression each time.
 *
 * @note  Flags only exist in amxmodx 1.8 and later.
 *
 * @note  You should free the returned handle with regex_free()
 *        when you are done extracting all of the substrings.
 *
 * @param string        The string to check.
 * @param pattern       The regular expression pattern.
 * @param flags         General flags for the regular expression, see PCRE_* defines.
 * @param error         Error message encountered, if applicable.
 * @param maxLen        Maximum string length of the error buffer.
 * @param errcode       Regex type error code encountered, if applicable. See REGEX_ERROR_* defines.
 *
 * @return              -2 = Matching error (error code is stored in ret)
 *                      -1 = Error in pattern (error message and offset # in error and ret)
 *                       0 = No match.
 *                      >1 = Handle for getting more information (via regex_substr)
 */
native Regex:regex_match_all(const string[], const pattern[], flags = 0, error[]= "", maxLen = 0, &errcode = 0);

/**
 * Matches a string against a regular expression pattern.
 *
 * @note  If you intend on using the same regular expression pattern
 *        multiple times, consider using compile regex_compile_ex and regex_match*
 *        instead of making this function reparse the expression each time.
 *
 * @param str           The string to check.
 * @param pattern       The regular expression pattern.
 * @param flags         General flags for the regular expression.
 * @param error         Error message, if applicable.
 * @param maxLen        Maximum length of the error buffer.
 * @param errcode       Regex type error code encountered, if applicable. See REGEX_ERROR_* defines.
 *
 * @return              -2 = Matching error (error code is stored in ret)
 *                      -1 = Pattern error (error code is stored in ret)
 *                       0 = No match.
 *                      >1 = Number of results.
 */
stock regex_match_simple(const str[], const pattern[], flags = 0, error[]= "", maxLen = 0, &errcode = 0)
{
	new Regex:regex = regex_compile_ex(pattern, flags, error, maxLen, errcode);
	if (regex < REGEX_OK)
	{
		return -1;
	}
	new substrings = regex_match_c(str, regex);
	regex_free(regex);
	return substrings;
}

/**
 * Flags used with regex_replace to control the replacement behavior.
 */
#define REGEX_FORMAT_DEFAULT   0       /* Uses the standard formatting rules to replace matches */
#define REGEX_FORMAT_NOCOPY    (1<<0)  /* The sections that do not match the regular expression are not copied when replacing matches. */
#define REGEX_FORMAT_FIRSTONLY (1<<1)  /* Only the first occurrence of a regular expression is replaced. */

/**
 * Perform a regular expression search and replace.
 *
 * An optional parameter, flags, allows you to specify options on how the replacement is performed.
 * Supported format specifiers for replace parameter:
 *   $number  : Substitutes the substring matched by group number.
 *              n must be an integer value designating a valid backreference, greater than 0, and of two digits at most.
 *   ${name}  : Substitutes the substring matched by the named group name (a maximum of 32 characters).
 *   $&       : Substitutes a copy of the whole match.
 *   $`       : Substitutes all the text of the input string before the match.
 *   $'       : Substitutes all the text of the input string after the match.
 *   $+       : Substitutes the last group that was captured.
 *   $_       : Substitutes the entire input string.
 *   $$       : Substitutes a literal "$".
 * As note, the character \ can be also used with format specifier, this is same hehavior as $.
 *
 * @param pattern       The regular expression pattern.
 * @param string        The string to check.
 * @param error         Error message, if applicable.
 * @param maxLen        Maximum length of the error buffer.
 * @param replace       The string will be used to replace any matches. See above for format specifiers.
 * @param flags         General flags to control how the string is replaced. See REGEX_FORMAT_* defines.
 * @param errcode       Regex type error code encountered, if applicable. See REGEX_ERROR_* defines.
 *
 * @return              -2 = Matching error (error code is stored in ret)
 *                       0 = No match.
 *                      >1 = Number of matches.
 */
native regex_replace(Regex:pattern, string[], maxLen, const replace[], flags = REGEX_FORMAT_DEFAULT, &errcode = 0);
