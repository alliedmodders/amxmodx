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
