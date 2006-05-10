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

/* Return values:
   -2 = Matching error (error code stored in ret)
   -1 = Error in pattern (error message and offset # in error[] and ret)
    0 = No match
   >1 = Id for getting more info (you must call regex_free() later on)
        (also note that ret will contain the number of substrings found) 
 */
 
native Regex:regex_match(const string[], const pattern[], &ret, error[], maxLen);

/* Returns a matched substring from a regex handle
 * substring ids start at 0 and end at ret-1, where ret is from the above function
 */
native regex_substr(Regex:id, str_id, buffer[], maxLen);

/* Frees the memory associated with a regex results and sets the handle to 0.
 * You must do this if the handle >=1, once you're done.
 */
native regex_free(&Regex:id);