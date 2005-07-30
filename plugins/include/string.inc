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
native format(output[] ,len ,const format[] , {Float,Sql,Result,_}:...);

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

/* Breaks a string into two halves, by token.
   See strbreak() for doing this with parameters.
   Example:
   str1[] = This *is*some text
   strtok(str1, left, 24, right, 24, '*')
   left will be "This "
   Right will be "is*some text"
   If you use trimSpaces, all spaces are trimmed from Left.
*/
native strtok(const text[], Left[], leftLen, Right[], rightLen, token=' ', trimSpaces=0);
   

/* Gets parameters from text one at a time
   It breaks a string into the first parameter and the rest of the parameters
   (A left side and right side of the string)
   Example: to split text: "^"This is^" the best year",
   strbreak(text, arg1, len1, arg2, len2)
   arg1="This is", arg2=the best year
   This is more useful than parse() because you can keep breaking
   any number of arguments */
native strbreak(const text[], Left[], leftLen, Right[], rightLen);

/* Strips spaces from the beginning and end of a string. */
native trim(text[]);

/* Converts all chars in string to lower case. */
native strtolower(string[]);

/* Converts all chars in string to upper case. */
native strtoupper(string[]);

/* Make a string's first character uppercase */
native ucfirst(string[]);

/* Returns true when value is digit. */
native isdigit(ch);

/* Returns true when value is letter. */
native isalpha(ch);

/* Returns true when value is space. */
native isspace(ch);

/* Returns true when value is letter or digit. */
native isalnum(ch);

/* Concatenates a string.  Maxlength is the total buffer of the destination. */
native strcat(dest[], const source[], maxlength);

/* Finds a string in another string.  Returns -1 if not found. */
native strfind(const string[], const sub[], bool:ignorecase=false, pos=0);
