/* Strings manipulation
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
* This file is provided as is (no warranties).
*/

#if defined _string_included
  #endinput
#endif
#define _string_included

/* Checks if source contains string. On success function
* returns position in source, on failure returns -1. */
native contain(const source[],const string[]);

/* Checks if source contains string with case ignoring. On success function
* returns position in source, on failure returns -1. */
native containi(const source[],const string[]);

/* Replaces given string to another in given text. */
native replace(text[],len,const what[],const with[]);

/* Adds one string to another. Last parameter different from 0, specifies
* how many chars we want to add. Function returns number of all merged chars. */
native add(dest[],len,const src[],max=0);

/* Fills string with given format and parameters.
* Function returns number of copied chars.
* Example: format(dest,"Hello %s. You are %d years old","Tom",17). */
native format(output[] ,len ,const format[] , {Float,_}:...);

/* Gets parameters from function as formated string. */
native format_args(output[] ,len ,pos = 0);

/* Converts number to string. */
native num_to_str(num,string[],len);

/* Returns converted string to number. */
native str_to_num(const string[]);

/* Checks if two strings equal. If len var is set
* then there are only c chars comapred. */
native equal(const a[],const b[],c=0);

/* Checks if two strings equal with case ignoring. 
* If len var is set then there are only c chars comapred. */
native equali(const a[],const b[],c=0);

/* Copies one string to another. By len var
*  you may specify max. number of chars to copy. */
native copy(dest[],len,const src[]);

/* Copies one string to another until char ch is found. 
*  By len var you may specify max. number of chars to copy. */
native copyc(dest[],len,const src[],ch);

/* Sets string with given character. */
native setc(src[],len,ch);

/* Gets parameters from text.
* Example: to split text: "^"This is^" the best year",
* call function like this: parse(text,arg1,len1,arg2,len2,arg3,len3,arg4,len4)
* and you will get: "This is", "the", "best", "year"
* Function returns number of parsed parameters. */
native parse(const text[], ... );

/* Converts all chars in string to lower case. */
native strtolower(string[]);

/* Converts all chars in string to upper case. */
native strtoupper(string[]);

/* Returns true when value is digit. */
native isdigit(ch);

/* Returns true when value is letter. */
native isalpha(ch);

/* Returns true when value is space. */
native isspace(ch);

/* Returns true when value is letter or digit. */
native isalnum(ch);