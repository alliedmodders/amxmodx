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

#include "codepoint.h"

const uint8_t codepoint_decoded_length[256] = {
	/* Basic Latin */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x00 - 0x07 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x08 - 0x0F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x10 - 0x17 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x18 - 0x1F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x20 - 0x27 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x28 - 0x2F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x30 - 0x37 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x38 - 0x3F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x40 - 0x47 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x48 - 0x4F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x50 - 0x57 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x58 - 0x5F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x60 - 0x67 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x68 - 0x6F */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x70 - 0x77 */
	1, 1, 1, 1, 1, 1, 1, 1, /* 0x78 - 0x7F */

	/* Malformed continuation byte */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0x80 - 0x87 */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0x88 - 0x8F */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0x90 - 0x97 */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0x98 - 0x9F */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0xA0 - 0xA7 */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0xA8 - 0xAF */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0xB0 - 0xB7 */
	0, 0, 0, 0, 0, 0, 0, 0, /* 0xB8 - 0xBF */

	/* Two bytes */
	2, 2, 2, 2, 2, 2, 2, 2, /* 0xC0 - 0xC7 */
	2, 2, 2, 2, 2, 2, 2, 2, /* 0xC8 - 0xCF */
	2, 2, 2, 2, 2, 2, 2, 2, /* 0xD0 - 0xD7 */
	2, 2, 2, 2, 2, 2, 2, 2, /* 0xD8 - 0xDF */

	/* Three bytes */
	3, 3, 3, 3, 3, 3, 3, 3, /* 0xE0 - 0xE7 */
	3, 3, 3, 3, 3, 3, 3, 3, /* 0xE8 - 0xEF */

	/* Four bytes */
	4, 4, 4, 4, 4, 4, 4, 4, /* 0xF0 - 0xF7 */

	/* Five bytes */
	5, 5, 5, 5,             /* 0xF8 - 0xFB */

	/* Six bytes */
	6, 6,                   /* 0xFC - 0xFD */

	/* Invalid */
	7, 7                    /* 0xFE - 0xFF */
};

uint8_t codepoint_write(unicode_t encoded, char** target, size_t* targetSize)
{
	uint8_t encoded_length;

	/* Determine encoded length of code point */

	if (encoded <= MAX_BASIC_LATIN)
	{
		encoded_length = 1;
	}
	else if (
		encoded <= 0x7FF)
	{
		encoded_length = 2;
	}
	else if (
		encoded <= MAX_BASIC_MULTILINGUAL_PLANE)
	{
		encoded_length = 3;
	}
	else if (
		encoded > MAX_LEGAL_UNICODE)
	{
		encoded = REPLACEMENT_CHARACTER;
		encoded_length = REPLACEMENT_CHARACTER_STRING_LENGTH;
	}
	else
	{
		encoded_length = 4;
	}

	/* Write to target */

	if (*target != 0)
	{
		char* dst;

		if (*targetSize < encoded_length)
		{
			return 0;
		}

		dst = *target;

		switch (encoded_length)
		{

		case 1:
			*dst++ = (char)encoded;
			break;

		case 2:
			*dst++ = (char)(encoded >>   6)         | 0xC0;
			*dst++ = (char)(encoded         & 0x3F) | 0x80;
			break;

		case 3:
			*dst++ = (char)(encoded  >> 12)         | 0xE0;
			*dst++ = (char)((encoded >>  6) & 0x3F) | 0x80;
			*dst++ = (char)(encoded         & 0x3F) | 0x80;
			break;

		case 4:
			*dst++ = (char)(encoded  >> 18)         | 0xF0;
			*dst++ = (char)((encoded >> 12) & 0x3F) | 0x80;
			*dst++ = (char)((encoded >>  6) & 0x3F) | 0x80;
			*dst++ = (char)(encoded         & 0x3F) | 0x80;
			break;

		default:
			break;

		}

		*target += encoded_length;
		*targetSize -= encoded_length;
	}

	return encoded_length;
}

uint8_t codepoint_read(const char* input, size_t inputSize, unicode_t* decoded)
{
	const uint8_t* src = (const uint8_t*)input;

	if (input == 0 ||
		inputSize == 0)
	{
		/* Invalid data */

		return 0;
	}

	if (*src <= MAX_BASIC_LATIN)
	{
		/* Basic Latin */

		*decoded = (unicode_t)*src;

		return 1;
	}
	else
	{
		/* Multi-byte sequence */

		static const uint8_t SequenceMask[7] = {
			0x00, 0x7F, 0x1F, 0x0F,
			0x07, 0x03, 0x01
		};
		static const unicode_t SequenceMinimum[7] = {
			0x0000, 0x0000, 0x0080, 0x0800,
			0x10000, MAX_LEGAL_UNICODE, MAX_LEGAL_UNICODE
		};

		size_t src_size = inputSize;
		uint8_t src_index;

		/* Length of sequence is determined by first byte */

		uint8_t decoded_length = codepoint_decoded_length[*src];
		if (decoded_length < 1 ||
			decoded_length > 6)
		{
			/* Not a multi-byte sequence starter */

			*decoded = REPLACEMENT_CHARACTER;
			decoded_length = 1;
		}
		else if (decoded_length > 4)
		{
			/* Always an overlong sequence */

			*decoded = REPLACEMENT_CHARACTER;

			/* All bytes in the sequence must be processed */

			for (src_index = 1; src_index < decoded_length; ++src_index)
			{
				src++;

				/* Check if next byte is valid */

				if (src_size == 0 ||               /* Not enough data */
					(*src < 0x80 || *src > 0xBF))  /* Not a continuation byte */
				{
					return src_index;
				}

				src_size--;
			}
		}
		else
		{
			/* Use mask to strip value from first byte */

			*decoded = (unicode_t)(*src & SequenceMask[decoded_length]);

			/* All bytes in the sequence must be processed */

			for (src_index = 1; src_index < decoded_length; ++src_index)
			{
				src++;

				/* Check if next byte is valid */

				if (src_size == 0 ||               /* Not enough data */
					(*src < 0x80 || *src > 0xBF))  /* Not a continuation byte */
				{
					*decoded = REPLACEMENT_CHARACTER;

					return src_index;
				}

				src_size--;

				/* Add value of continuation byte to codepoint */

				*decoded = (*decoded << 6) | (*src & 0x3F);
			}

			/* Check for overlong sequences and surrogate pairs */

			if (*decoded < SequenceMinimum[decoded_length] ||
				*decoded > MAX_LEGAL_UNICODE ||
				(*decoded >= SURROGATE_HIGH_START && *decoded <= SURROGATE_LOW_END))
			{
				*decoded = REPLACEMENT_CHARACTER;
			}
		}

		return decoded_length;
	}
}