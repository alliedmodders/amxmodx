/*
	Copyright (C) 2014-2016 Quinten Lansu

	Permission is hereby granted, free of charge, to any person
	obtaining a copy of this software and associated documentation
	files (the "Software"), to deal in the Software without
	restriction, including without limitation the rights to use,
	copy, modify, merge, publish, distribute, sublicense, and/or
	sell copies of the Software, and to permit persons to whom the
	Software is furnished to do so, subject to the following
	conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
	OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
	NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
	WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
	FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
	OTHER DEALINGS IN THE SOFTWARE.
*/

#ifndef _UTF8REWIND_INTERNAL_CODEPOINT_H_
#define _UTF8REWIND_INTERNAL_CODEPOINT_H_

/*!
	\file
	\brief Codepoint interface.

	\cond INTERNAL
*/

#include "utf8rewind.h"

/*!
	\addtogroup internal Internal functions and definitions
	\{
*/

/*!
	\def MAX_BASIC_LATIN
	\brief The last codepoint part of Basic Latin (U+0000 - U+007F).
*/
#define MAX_BASIC_LATIN                      0x007F

/*!
	\def MAX_LATIN_1
	\brief The last codepoint part of Latin-1 Supplement (U+0080 - U+00FF).
*/
#define MAX_LATIN_1                          0x00FF

/*!
	\def MAX_BASIC_MULTILINGUAL_PLANE
	\brief The last legal codepoint in the Basic Multilingual Plane (BMP).
*/
#define MAX_BASIC_MULTILINGUAL_PLANE         0xFFFF

/*!
	\def MAX_LEGAL_UNICODE
	\brief The last legal codepoint in Unicode.
*/
#define MAX_LEGAL_UNICODE                    0x10FFFF

/*!
	\def REPLACEMENT_CHARACTER
	\brief The codepoint used to replace illegal codepoints.
*/
#define REPLACEMENT_CHARACTER                0xFFFD

/*!
	\def REPLACEMENT_CHARACTER_STRING
	\brief The replacement character as a UTF-8 encoded string.
*/
#define REPLACEMENT_CHARACTER_STRING         "\xEF\xBF\xBD"

/*!
	\def REPLACEMENT_CHARACTER_STRING_LENGTH
	\brief Length of the UTF-8 encoded string of the replacment character.
*/
#define REPLACEMENT_CHARACTER_STRING_LENGTH  3

/*!
	\def SURROGATE_HIGH_START
	\brief The minimum codepoint for the high member of a surrogate pair.
*/
#define SURROGATE_HIGH_START                 0xD800

/*!
	\def SURROGATE_HIGH_END
	\brief The maximum codepoint for the high member of a surrogate pair.
*/
#define SURROGATE_HIGH_END                   0xDBFF

/*!
	\def SURROGATE_LOW_START
	\brief The minimum codepoint for the low member of a surrogate pair.
*/
#define SURROGATE_LOW_START                  0xDC00

/*!
	\def SURROGATE_LOW_END
	\brief The maximum codepoint for the low member of a surrogate pair.
*/
#define SURROGATE_LOW_END                    0xDFFF

/*!
	\def HANGUL_JAMO_FIRST
	\brief The first codepoint part of the Hangul Jamo block.
*/
#define HANGUL_JAMO_FIRST                    0x1100

/*!
	\def HANGUL_JAMO_LAST
	\brief The last codepoint part of the Hangul Jamo block.
*/
#define HANGUL_JAMO_LAST                     0x11FF

/*!
	\def HANGUL_L_FIRST
	\brief The first codepoint part of the Hangul Jamo L section used for
	normalization.
*/
#define HANGUL_L_FIRST                       0x1100

/*!
	\def HANGUL_L_LAST
	\brief The last codepoint part of the Hangul Jamo L section used for
	normalization.
*/
#define HANGUL_L_LAST                        0x1112

/*!
	\def HANGUL_L_COUNT
	\brief The number of codepoints in the Hangul Jamo L section.
*/
#define HANGUL_L_COUNT                       19

/*!
	\def HANGUL_V_FIRST
	\brief The first codepoint part of the Hangul Jamo V section used for
	normalization.
*/
#define HANGUL_V_FIRST                       0x1161

/*!
	\def HANGUL_V_LAST
	\brief The last codepoint part of the Hangul Jamo V section used for
	normalization.
*/
#define HANGUL_V_LAST                        0x1175

/*!
	\def HANGUL_V_COUNT
	\brief The number of codepoints in the Hangul Jamo V section.
*/
#define HANGUL_V_COUNT                       21

/*!
	\def HANGUL_T_FIRST
	\brief The first codepoint part of the Hangul Jamo T section used for
	normalization.
*/
#define HANGUL_T_FIRST                       0x11A7

/*!
	\def HANGUL_T_LAST
	\brief The last codepoint part of the Hangul Jamo V section used for
	normalization.
*/
#define HANGUL_T_LAST                        0x11C2

/*!
	\def HANGUL_T_COUNT
	\brief The number of codepoints in the Hangul Jamo T section.
*/
#define HANGUL_T_COUNT                       28

/*!
	\def HANGUL_N_COUNT
	\brief Number of codepoints part of the Hangul Jamo V and T sections.
*/
#define HANGUL_N_COUNT                       588 /* VCount * TCount */

/*!
	\def HANGUL_S_FIRST
	\brief The first codepoint in the Hangul Syllables block.
*/
#define HANGUL_S_FIRST                       0xAC00

/*!
	\def HANGUL_S_LAST
	\brief The last codepoint in the Hangul Syllables block.
*/
#define HANGUL_S_LAST                        0xD7A3

/*!
	\def HANGUL_S_COUNT
	\brief The number of codepoints in the Hangul Syllables block.
*/
#define HANGUL_S_COUNT                       11172 /* LCount * NCount */

#define CP_LATIN_CAPITAL_LETTER_I                 0x0049
#define CP_LATIN_CAPITAL_LETTER_J                 0x004A
#define CP_LATIN_SMALL_LETTER_I                   0x0069
#define CP_LATIN_SMALL_LETTER_J                   0x006A
#define CP_LATIN_CAPITAL_LETTER_I_WITH_GRAVE      0x00CC
#define CP_LATIN_CAPITAL_LETTER_I_WITH_ACUTE      0x00CD
#define CP_LATIN_CAPITAL_LETTER_I_WITH_TILDE      0x0128
#define CP_LATIN_CAPITAL_LETTER_I_WITH_OGONEK     0x012E
#define CP_LATIN_SMALL_LETTER_I_WITH_OGONEK       0x012F
#define CP_LATIN_CAPITAL_LETTER_I_WITH_DOT_ABOVE  0x0130
#define CP_LATIN_SMALL_LETTER_DOTLESS_I           0x0131
#define CP_COMBINING_GRAVE_ACCENT                 0x0300
#define CP_COMBINING_ACUTE_ACCENT                 0x0301
#define CP_COMBINING_TILDE_ACCENT                 0x0303
#define CP_COMBINING_DOT_ABOVE                    0x0307
#define CP_COMBINING_GREEK_YPOGEGRAMMENI          0x0345
#define CP_COMBINING_GRAPHEME_JOINER              0x034F
#define CP_GREEK_CAPITAL_LETTER_SIGMA             0x03A3

#define CCC_NOT_REORDERED                         0
#define CCC_OVERLAY                               1
#define CCC_NUKTA                                 7
#define CCC_KANA_VOICING                          8
#define CCC_VIRAMA                                9
#define CCC_FIXED_POSITION_START                  10
#define CCC_FIXED_POSITION_END                    199
#define CCC_ATTACHED_BELOW_LEFT                   200
#define CCC_ATTACHED_BELOW                        202
#define CCC_ATTACHED_BOTTOM_RIGHT                 204
#define CCC_ATTACHED_LEFT                         208
#define CCC_ATTACHED_RIGHT                        210
#define CCC_ATTACHED_TOP_LEFT                     212
#define CCC_ATTACHED_ABOVE                        214
#define CCC_ATTACHED_ABOVE_RIGHT                  216
#define CCC_BELOW_LEFT                            218
#define CCC_BELOW                                 220
#define CCC_BELOW_RIGHT                           222
#define CCC_LEFT                                  224
#define CCC_RIGHT                                 226
#define CCC_ABOVE_LEFT                            228
#define CCC_ABOVE                                 230
#define CCC_ABOVE_RIGHT                           232
#define CCC_DOUBLE_BELOW                          233
#define CCC_DOUBLE_ABOVE                          234
#define CCC_IOTA_SUBSCRIPT                        240
#define CCC_INVALID                               255

/*!
	\brief Get the number of bytes used for encoding a code point.

	\param[in]  byte  Encoded byte

	\return Number of bytes needed for decoding or 0 if input is illegal.
*/
extern const uint8_t codepoint_decoded_length[256];

/*!
	\brief Write Unicode code point to UTF-8 encoded string.

	Target buffer and size is modified by encoded size.

	\param[in]      encoded     Unicode code point
	\param[in,out]  target      Target buffer
	\param[in,out]  targetSize  Size of output buffer in bytes

	\return Bytes needed for encoding or 0 on error.
*/
uint8_t codepoint_write(unicode_t encoded, char** target, size_t* targetSize);

/*!
	\brief Read Unicode code point from UTF-8 encoded string.

	\param[in]   input      Input buffer
	\param[in]   inputSize  Size of input buffer in bytes
	\param[out]  decoded    Unicode codepoint

	\return Bytes read from string or 0 on error.
*/
uint8_t codepoint_read(const char* input, size_t inputSize, unicode_t* decoded);

/*!
	\}
*/

/*! \endcond */

#endif /* _UTF8REWIND_INTERNAL_CODEPOINT_H_ */