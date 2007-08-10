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
 * Substring ids start at 0 and end at ret-1, where ret is from the corresponding
 * regex_match or regex_match_c function call.
 *
 * @param id			The regex handle to extract data from.
 * @param str_id		The index of the expression to get - starts at 0, and ends at ret - 1.
 * @param buffer		The buffer to set to the matching substring.
 * @param maxLen		The maximum string length of the buffer.
 *
 */
native regex_substr(Regex:id, str_id, buffer[], maxLen);

/**
 * Frees the memory associated with a regex result, and sets the handle to 0.
 * This must be called on all results from regex_match() when you are done extracting
 * the results with regex_substr().
 * The results of regex_compile() (and subsequently, regex_match_c()) only need to be freed
 * when you are done using the pattern.
 *
 *
 * @param id			The regex handle to free.
 *
 * @noreturn
 *
 * @note				Do not use the handle again after freeing it!
 */ 
native regex_free(&Regex:id);
