// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

//
// String Manipulation Constants
//

#if defined _string_const_included
  #endinput
#endif
#define _string_const_included

#define charsmax(%1) (sizeof(%1)-1)

/**
 * @global  Unless otherwise noted, all string functions which take in a
 *          writable buffer and maximum length should NOT have the null terminator INCLUDED
 *          in the length.  This means that this is valid:
 *          copy(string, charsmax(string), ...)
 */

/**
 * Buffer size used by fmt().
 */
#define MAX_FMT_LENGTH 256

/**
 * Below are the trim flags for strtok2
 *
 * You can specify how the left and right buffers will
 * be trimmed by strtok2. LTRIM trims spaces from the
 * left side. RTRIM trims from the right side.
 *
 * The defines TRIM_INNER, TRIM_OUTER and TRIM_FULL are
 * shorthands for commonly used flag combinations.
 *
 * When the initial string is trimmed, using TRIM_INNER
 * for all subsequent strtok2 calls will ensure that left
 * and right are always trimmed from both sides.
 *
 * Examples:
 * str1[] = "  This is  *  some text  "
 * strtok2(str1, left, 24, right, 24, '*', TRIM_FULL)
 *  left will be "This is", right will be "some text"
 *
 * str2[] = "  Here is  |  an  | example  "
 * trim(str2)
 * strtok2(str2, left, 24, right, 24, '|', TRIM_INNER)
 *  left will be "Here is", right will be "an  | example"
 * strtok2(right, left, 24, right, 24, '|', TRIM_INNER)
 *  left will be "an", right will be "example"
 *
 * str3[] = "  One  -  more  "
 * strtok2(str3, left, 24, right, 24, '-', TRIM_OUTER)
 *  left will be "One  ", right will be "  more"
 *
 * str4[] = "  Final  .  example  "
 * strtok2(str4, left, 24, right, 24, '.', LTRIM_LEFT|LTRIM_RIGHT)
 *  left will be "Final  ", right will be "example  "
*/
#define LTRIM_LEFT (1<<0)
#define RTRIM_LEFT (1<<1)
#define LTRIM_RIGHT (1<<2)
#define RTRIM_RIGHT (1<<3)

#define TRIM_INNER RTRIM_LEFT|LTRIM_RIGHT
#define TRIM_OUTER LTRIM_LEFT|RTRIM_RIGHT
#define TRIM_FULL TRIM_OUTER|TRIM_INNER

/**
 * Category flags to be used with is_string_category(), to check whether code points in a
 * string are part of that category.
 */
#define UTF8C_LETTER_UPPERCASE          0x00000001    // Uppercase letter code points, Lu in the Unicode database.
#define UTF8C_LETTER_LOWERCASE          0x00000002    // Lowercase letter code points, Ll in the Unicode database.
#define UTF8C_LETTER_TITLECASE          0x00000004    // Titlecase letter code points, Lt in the Unicode database.
#define UTF8C_LETTER_MODIFIER           0x00000008    // Modifier letter code points, Lm in the Unicode database.
#define UTF8C_LETTER_OTHER              0x00000010    // Other letter code points, Lo in the Unicode database.

// Combined flag for all letter categories with case mapping
// Combined flag for all letter categories
const UTF8C_LETTER      = (UTF8C_LETTER_UPPERCASE | UTF8C_LETTER_LOWERCASE | UTF8C_LETTER_TITLECASE | UTF8C_LETTER_MODIFIER  | UTF8C_LETTER_OTHER);
const UTF8C_CASE_MAPPED = (UTF8C_LETTER_UPPERCASE | UTF8C_LETTER_LOWERCASE | UTF8C_LETTER_TITLECASE);

#define UTF8C_MARK_NON_SPACING          0x00000020    // Non-spacing mark code points, Mn in the Unicode database.
#define UTF8C_MARK_SPACING              0x00000040    // Spacing mark code points, Mc in the Unicode database.
#define UTF8C_MARK_ENCLOSING            0x00000080    // Enclosing mark code points, Me in the Unicode database.

// Combined flag for all mark categories.
const UTF8C_MARK = (UTF8C_MARK_NON_SPACING | UTF8C_MARK_SPACING | UTF8C_MARK_ENCLOSING);

#define UTF8C_NUMBER_DECIMAL            0x00000100    // Decimal number code points, Nd in the Unicode database.
#define UTF8C_NUMBER_LETTER             0x00000200    // Letter number code points, Nl in the Unicode database.
#define UTF8C_NUMBER_OTHER              0x00000400    // Other number code points, No in the Unicode database.

// Combined flag for all number categories.
const UTF8C_NUMBER = (UTF8C_NUMBER_DECIMAL | UTF8C_NUMBER_LETTER | UTF8C_NUMBER_OTHER);

#define UTF8C_PUNCTUATION_CONNECTOR     0x00000800    // Connector punctuation category, Pc in the Unicode database.
#define UTF8C_PUNCTUATION_DASH          0x00001000    // Dash punctuation category, Pd in the Unicode database.
#define UTF8C_PUNCTUATION_OPEN          0x00002000    // Open punctuation category, Ps in the Unicode database.
#define UTF8C_PUNCTUATION_CLOSE         0x00004000    // Close punctuation category, Pe in the Unicode database.
#define UTF8C_PUNCTUATION_INITIAL       0x00008000    // Initial punctuation category, Pi in the Unicode database.
#define UTF8C_PUNCTUATION_FINAL         0x00010000    // Final punctuation category, Pf in the Unicode database.
#define UTF8C_PUNCTUATION_OTHER         0x00020000    // Other punctuation category, Po in the Unicode database.

// Combined flag for all punctuation categories.
const UTF8C_PUNCTUATION  = (UTF8C_PUNCTUATION_CONNECTOR | UTF8C_PUNCTUATION_DASH    | UTF8C_PUNCTUATION_OPEN  | \
							UTF8C_PUNCTUATION_CLOSE     | UTF8C_PUNCTUATION_INITIAL | UTF8C_PUNCTUATION_FINAL | \
							UTF8C_PUNCTUATION_OTHER);

#define UTF8C_SYMBOL_MATH               0x00040000    // Math symbol category, Sm in the Unicode database.
#define UTF8C_SYMBOL_CURRENCY           0x00080000    // Currency symbol category, Sc in the Unicode database.
#define UTF8C_SYMBOL_MODIFIER           0x00100000    // Modifier symbol category, Sk in the Unicode database.
#define UTF8C_SYMBOL_OTHER              0x00200000    // Other symbol category, So in the Unicode database.

// Combined flag for all symbol categories.
const UTF8C_SYMBOL = (UTF8C_SYMBOL_MATH | UTF8C_SYMBOL_CURRENCY | UTF8C_SYMBOL_MODIFIER | UTF8C_SYMBOL_OTHER);

#define UTF8C_SEPARATOR_SPACE           0x00400000    // Space separator category, Zs in the Unicode database.
#define UTF8C_SEPARATOR_LINE            0x00800000    // Line separator category, Zl in the Unicode database.
#define UTF8C_SEPARATOR_PARAGRAPH       0x01000000    // Paragraph separator category, Zp in the Unicode database.

// Combined flag for all separator categories.
const UTF8C_SEPARATOR = (UTF8C_SEPARATOR_SPACE | UTF8C_SEPARATOR_LINE | UTF8C_SEPARATOR_PARAGRAPH);

#define UTF8C_CONTROL                   0x02000000    // Control category, Cc in the Unicode database.
#define UTF8C_FORMAT                    0x04000000    // Format category, Cf in the Unicode database.
#define UTF8C_SURROGATE                 0x08000000    // Surrogate category, Cs in the Unicode database.
#define UTF8C_PRIVATE_USE               0x10000000    // Private use category, Co in the Unicode database.
#define UTF8C_UNASSIGNED                0x20000000    // Unassigned category, Cn in the Unicode database.
#define UTF8C_COMPATIBILITY             0x40000000    // Flag used for maintaining backwards compatibility with POSIX
#define UTF8C_IGNORE_GRAPHEME_CLUSTER   0x80000000    // Flag used for checking only the general category of code points at the start of a grapheme cluster.

// Flag used for maintaining backwards compatibility with POSIX function
const UTF8C_ISCNTRL  = (UTF8C_COMPATIBILITY | UTF8C_CONTROL);
const UTF8C_ISPRINT  = (UTF8C_COMPATIBILITY | UTF8C_LETTER | UTF8C_NUMBER | UTF8C_PUNCTUATION | UTF8C_SYMBOL | UTF8C_SEPARATOR);
const UTF8C_ISSPACE  = (UTF8C_COMPATIBILITY | UTF8C_SEPARATOR_SPACE);
const UTF8C_ISBLANK  = (UTF8C_COMPATIBILITY | UTF8C_SEPARATOR_SPACE | UTF8C_PRIVATE_USE);
const UTF8C_ISGRAPH  = (UTF8C_COMPATIBILITY | UTF8C_LETTER | UTF8C_NUMBER | UTF8C_PUNCTUATION | UTF8C_SYMBOL);
const UTF8C_ISPUNCT  = (UTF8C_COMPATIBILITY | UTF8C_PUNCTUATION | UTF8C_SYMBOL);
const UTF8C_ISALNUM  = (UTF8C_COMPATIBILITY | UTF8C_LETTER | UTF8C_NUMBER);
const UTF8C_ISALPHA  = (UTF8C_COMPATIBILITY | UTF8C_LETTER);
const UTF8C_ISUPPER  = (UTF8C_COMPATIBILITY | UTF8C_LETTER_UPPERCASE);
const UTF8C_ISLOWER  = (UTF8C_COMPATIBILITY | UTF8C_LETTER_LOWERCASE);
const UTF8C_ISDIGIT  = (UTF8C_COMPATIBILITY | UTF8C_NUMBER);
const UTF8C_ISXDIGIT = (UTF8C_COMPATIBILITY | UTF8C_NUMBER | UTF8C_PRIVATE_USE);

// All flags.
const UTF8C_ALL = 0xFFFFFFFF & (~UTF8C_COMPATIBILITY);
