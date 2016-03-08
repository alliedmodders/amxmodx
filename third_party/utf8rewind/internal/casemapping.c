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

#include "casemapping.h"

#include "base.h"
#include "codepoint.h"
#include "database.h"
#include "streaming.h"

static const char basic_latin_lowercase_table[58] = {
	/* LATIN CAPITAL LETTER A - LATIN CAPITAL LETTER Z */
	0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C,
	0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
	0x79, 0x7A,

	0x5B, /* LEFT SQUARE BRACKET */
	0x5C, /* REVERSE SOLIDUS */
	0x5D, /* RIGHT SQUARE BRACKET */
	0x5E, /* CIRCUMFLEX ACCENT */
	0x5F, /* LOW LINE */
	0x60, /* GRAVE ACCENT */

	/* LATIN SMALL LETTER A - LATIN SMALL LETTER Z */
	0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C,
	0x6D, 0x6E, 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
	0x79, 0x7A
};

static const char basic_latin_uppercase_table[58] = {
	/* LATIN CAPITAL LETTER A - LATIN CAPITAL LETTER Z */
	0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C,
	0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
	0x59, 0x5A,

	0x5B, /* LEFT SQUARE BRACKET */
	0x5C, /* REVERSE SOLIDUS */
	0x5D, /* RIGHT SQUARE BRACKET */
	0x5E, /* CIRCUMFLEX ACCENT */
	0x5F, /* LOW LINE */
	0x60, /* GRAVE ACCENT */

	/* LATIN SMALL LETTER A - LATIN SMALL LETTER Z */
	0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C,
	0x4D, 0x4E, 0x4F, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
	0x59, 0x5A
};

uint8_t casemapping_initialize(
	CaseMappingState* state,
	const char* input, size_t inputSize,
	char* target, size_t targetSize,
	const uint32_t* propertyIndex1, const uint32_t* propertyIndex2, const uint32_t* propertyData,
	uint8_t quickCheck, size_t locale,
	int32_t* errors)
{
	memset(state, 0, sizeof(CaseMappingState));

	if (locale >= UTF8_LOCALE_MAXIMUM)
	{
		UTF8_SET_ERROR(INVALID_LOCALE);

		return 0;
	}

	state->src = input;
	state->src_size = inputSize;
	state->dst = target;
	state->dst_size = targetSize;
	state->property_index1 = propertyIndex1;
	state->property_index2 = propertyIndex2;
	state->property_data = propertyData;
	state->quickcheck_flags = quickCheck;
	state->locale = locale;

	return 1;
}

size_t casemapping_execute(CaseMappingState* state, int32_t* errors)
{
	uint8_t qc_casemapped = 0;
	uint8_t bytes_needed = 0;
	const char* resolved = 0;
	StreamState stream;
	uint8_t i;

	/* Read next code point */

	state->last_code_point_size = codepoint_read(state->src, state->src_size, &state->last_code_point);
	if (state->last_code_point_size == 0)
	{
		goto invaliddata;
	}

	/* Check for invalid characters */

	if (state->last_code_point == REPLACEMENT_CHARACTER)
	{
		/* Get code point properties */

		state->last_canonical_combining_class = CCC_NOT_REORDERED;
		state->last_general_category = UTF8_CATEGORY_SYMBOL_OTHER;

		resolved = REPLACEMENT_CHARACTER_STRING;
		bytes_needed = REPLACEMENT_CHARACTER_STRING_LENGTH;

		goto writeresolved;
	}

	if (state->locale == UTF8_LOCALE_TURKISH_AND_AZERI_LATIN)
	{
		/*
			Code point General Category does not need to be modified, because
			all mappings result in the same General Category
		*/

		if (state->property_data == LowercaseDataPtr)
		{
			if (state->last_code_point == CP_LATIN_CAPITAL_LETTER_I_WITH_DOT_ABOVE)
			{
				state->last_code_point = CP_LATIN_SMALL_LETTER_I;

				resolved = "i";
				bytes_needed = 1;
			}
			else if (
				state->last_code_point == CP_LATIN_CAPITAL_LETTER_I)
			{
				if (state->src_size == 0)
				{
					/* Early-out for easy case */

					state->last_code_point = CP_LATIN_SMALL_LETTER_DOTLESS_I;

					resolved = "\xC4\xB1";
					bytes_needed = 2;
				}
				else
				{
					uint8_t found = 0;

					/* Initialize stream and read the next sequence */

					if (!stream_initialize(&stream, state->src, state->src_size) ||
						!stream_read(&stream, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr))
					{
						goto writeregular;
					}

					/* Erase COMBINING DOT ABOVE from sequence */

					for (i = stream.current - 1; i > 0; --i)
					{
						if (stream.codepoint[i] == CP_COMBINING_DOT_ABOVE)
						{
							stream.canonical_combining_class[i] = CCC_INVALID;

							found++;
						}
					}

					/* Stabilize sequence and write to output */

					if (!stream.stable ||
						found > 0)
					{
						stream_reorder(&stream);

						stream.current -= found;
					}

					stream.codepoint[0] = (found > 0) ? CP_LATIN_SMALL_LETTER_I : CP_LATIN_SMALL_LETTER_DOTLESS_I;

					goto writestream;
				}
			}
		}
		else
		{
			if (state->last_code_point == CP_LATIN_SMALL_LETTER_I)
			{
				state->last_code_point = CP_LATIN_CAPITAL_LETTER_I_WITH_DOT_ABOVE;

				resolved = "\xC4\xB0";
				bytes_needed = 2;
			}
			else if (
				state->last_code_point == CP_LATIN_SMALL_LETTER_DOTLESS_I)
			{
				state->last_code_point = CP_LATIN_CAPITAL_LETTER_I;

				resolved = "I";
				bytes_needed = 1;
			}
		}

		/* Check if mapping succeeded */

		if (resolved != 0)
		{
			/* Code point properties */

			state->last_general_category = UTF8_CATEGORY_LETTER;

			goto writeresolved;
		}
	}
	else if (
		state->locale == UTF8_LOCALE_LITHUANIAN)
	{
		if (state->property_data == LowercaseDataPtr)
		{
			unicode_t cp_additional_accent = 0;
			uint8_t write_soft_dot = 1;

			switch (state->last_code_point)
			{

			case CP_LATIN_CAPITAL_LETTER_I:
				state->last_code_point = CP_LATIN_SMALL_LETTER_I;
				break;

			case CP_LATIN_CAPITAL_LETTER_J:
				state->last_code_point = CP_LATIN_SMALL_LETTER_J;
				break;

			case CP_LATIN_CAPITAL_LETTER_I_WITH_OGONEK:
				state->last_code_point = CP_LATIN_SMALL_LETTER_I_WITH_OGONEK;
				break;

			case CP_LATIN_CAPITAL_LETTER_I_WITH_GRAVE:
				state->last_code_point = CP_LATIN_SMALL_LETTER_I;
				cp_additional_accent = CP_COMBINING_GRAVE_ACCENT;
				break;

			case CP_LATIN_CAPITAL_LETTER_I_WITH_ACUTE:
				state->last_code_point = CP_LATIN_SMALL_LETTER_I;
				cp_additional_accent = CP_COMBINING_ACUTE_ACCENT;
				break;

			case CP_LATIN_CAPITAL_LETTER_I_WITH_TILDE:
				state->last_code_point = CP_LATIN_SMALL_LETTER_I;
				cp_additional_accent = CP_COMBINING_TILDE_ACCENT;
				break;

			default:
				goto writeregular;

			}

			/* Initialize stream and read the next sequence */

			if (!stream_initialize(&stream, state->src, state->src_size) ||
				!stream_read(&stream, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr))
			{
				goto writeregular;
			}

			/* Assign the lowercase code point to the start of the stream */

			stream.codepoint[0] = state->last_code_point;

			/* Check if COMBINING DOT ABOVE is not yet present */ 

			for (i = stream.current - 1; i > 0; --i)
			{
				if (stream.codepoint[i] == CP_COMBINING_DOT_ABOVE)
				{
					write_soft_dot = 0;

					break;
				}
			}

			/* Stabilize the sequence */

			if (!stream.stable)
			{
				stream_reorder(&stream);

				stream.stable = 1;
			}

			/* Write COMBINING DOT ABOVE */

			if (write_soft_dot &&
				stream.current < STREAM_BUFFER_MAX)
			{
				/* Ensure the COMBINING DOT ABOVE comes before other accents with the same CCC */

				if (stream.canonical_combining_class[stream.current - 1] == CCC_ABOVE)
				{
					unicode_t cp_swap = stream.codepoint[stream.current - 1];
					stream.codepoint[stream.current - 1] = CP_COMBINING_DOT_ABOVE;
					stream.codepoint[stream.current] = cp_swap;
				}
				else
				{
					stream.codepoint[stream.current] = CP_COMBINING_DOT_ABOVE;
				}

				stream.canonical_combining_class[stream.current] = CCC_ABOVE;

				/* Check if sequence has become unstable */

				stream.stable = stream.canonical_combining_class[stream.current - 1] <= CCC_ABOVE;

				stream.current++;
			}

			/* Write additional accent */

			if (cp_additional_accent != 0 &&
				stream.current < STREAM_BUFFER_MAX)
			{
				/* Additional accents are always of the upper variety */

				stream.codepoint[stream.current] = cp_additional_accent;
				stream.canonical_combining_class[stream.current] = CCC_ABOVE;

				/* Check if sequence has become unstable */

				if (stream.stable &&
					stream.canonical_combining_class[stream.current] < stream.canonical_combining_class[stream.current - 1])
				{
					stream.stable = 0;
				}

				stream.current++;
			}

			/* Stabilize the sequence */

			if (!stream.stable)
			{
				stream_reorder(&stream);
			}
		}
		else
		{
			uint8_t erase_count = 0;

			switch (state->last_code_point)
			{

			case CP_LATIN_SMALL_LETTER_I:
				state->last_code_point = CP_LATIN_CAPITAL_LETTER_I;
				break;

			case CP_LATIN_SMALL_LETTER_J:
				state->last_code_point = CP_LATIN_CAPITAL_LETTER_J;
				break;

			case CP_LATIN_SMALL_LETTER_I_WITH_OGONEK:
				state->last_code_point = CP_LATIN_CAPITAL_LETTER_I_WITH_OGONEK;
				break;

			default:
				goto writeregular;

			}

			/* Initialize stream and read the next sequence */

			if (!stream_initialize(&stream, state->src, state->src_size) ||
				!stream_read(&stream, QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr))
			{
				goto writeregular;
			}

			/* Assign the uppercase code point to the start of the stream */

			stream.codepoint[0] = state->last_code_point;

			/* Remove COMBINING DOT ABOVE from sequence */

			for (i = 1; i < stream.current; ++i)
			{
				if (stream.codepoint[i] == CP_COMBINING_DOT_ABOVE)
				{
					stream.canonical_combining_class[i] = CCC_INVALID;
					erase_count++;
				}
			}

			/* Stabilize the sequence */

			if (!stream.stable ||
				erase_count > 0)
			{
				stream_reorder(&stream);

				stream.current -= erase_count;
			}
		}

		goto writestream;
	}

writeregular:
	/* Get code point properties */

	state->last_canonical_combining_class = PROPERTY_GET_CCC(state->last_code_point);
	state->last_general_category = PROPERTY_GET_GC(state->last_code_point);

	/* Move source cursor */

	if (state->src_size >= state->last_code_point_size)
	{
		state->src += state->last_code_point_size;
		state->src_size -= state->last_code_point_size;
	}
	else
	{
		state->src_size = 0;
	}

	/* Write to output */

	if (state->last_code_point_size == 1)
	{
		/* Write Basic Latin to output buffer*/

		if (state->dst != 0)
		{
			if (state->dst_size < 1)
			{
				goto outofspace;
			}

			/*
				Uppercase letters are U+0041 ('A') to U+005A ('Z')
				Lowercase letters are U+0061 ('a') to U+007A ('z')
			*/

			if (state->last_code_point >= 0x41 &&
				state->last_code_point <= 0x7A)
			{
				if (state->property_data == LowercaseDataPtr)
				{
					*state->dst = basic_latin_lowercase_table[state->last_code_point - 0x41];
				}
				else
				{
					*state->dst = basic_latin_uppercase_table[state->last_code_point - 0x41];
				}
			}
			else
			{
				/* All other code points in Basic Latin are unaffected by case mapping */

				*state->dst = (char)state->last_code_point;
			}

			state->dst++;
			state->dst_size--;
		}

		bytes_needed = 1;
	}
	else
	{
		if (state->property_data == LowercaseDataPtr &&
			state->last_code_point == CP_GREEK_CAPITAL_LETTER_SIGMA)
		{
			/*
				If the final letter of a word (defined as "a collection of code
				points with the General Category 'Letter'") is a GREEK CAPITAL
				LETTER SIGMA and more than one code point was processed, the
				lowercase version is U+03C2 GREEK SMALL LETTER FINAL SIGMA
				instead of U+03C3 GREEK SMALL LETTER SIGMA.
			*/

			/* At least one code point should have been read */

			uint8_t should_convert = state->total_bytes_needed > 0;

			if (state->src_size > 0)
			{
				unicode_t peeked = 0;
				const char* peeked_src = state->src;
				size_t peeked_src_size = state->src_size;

				while (1)
				{
					uint8_t peeked_read = 0;

					/* Peek next code point */

					if ((peeked_read = codepoint_read(peeked_src, peeked_src_size, &peeked)) == 0 ||
						peeked_src_size < peeked_read)
					{
						should_convert = 1;

						break;
					}

					/* Convert if the "word" has ended */

					if (PROPERTY_GET_CCC(peeked) == CCC_NOT_REORDERED)
					{
						should_convert = (PROPERTY_GET_GC(peeked) & UTF8_CATEGORY_LETTER) == 0;

						break;
					}

					peeked_src += peeked_read;
					peeked_src_size -= peeked_read;
				}
			}

			/* Write the converted code point to the output buffer */

			bytes_needed = 2;

			if (state->dst != 0)
			{
				if (state->dst_size < bytes_needed)
				{
					goto outofspace;
				}

				memcpy(state->dst, should_convert ? "\xCF\x82" : "\xCF\x83", bytes_needed);

				state->dst += bytes_needed;
				state->dst_size -= bytes_needed;
			}

			return bytes_needed;
		}

		/* Check if the code point is case mapped */

		qc_casemapped = PROPERTY_GET_CM(state->last_code_point);
		if ((qc_casemapped & state->quickcheck_flags) != 0)
		{
			/* Attempt to resolve the case mapping */

			resolved = database_querydecomposition(state->last_code_point, state->property_index1, state->property_index2, state->property_data, &bytes_needed);
			if (resolved != 0)
			{
				/* Code point properties */

				state->last_general_category = UTF8_CATEGORY_LETTER;

				goto writeresolvedonly;
			}
		}

		/* Write code point unchanged to output */

		bytes_needed = codepoint_write(state->last_code_point, &state->dst, &state->dst_size);
		if (bytes_needed == 0)
		{
			goto outofspace;
		}
	}

	return bytes_needed;

writeresolved:
	/* Move source cursor */

	if (state->src_size >= state->last_code_point_size)
	{
		state->src += state->last_code_point_size;
		state->src_size -= state->last_code_point_size;
	}
	else
	{
		state->src_size = 0;
	}

writeresolvedonly:
	/* Write resolved string to output */

	if (state->dst != 0)
	{
		if (state->dst_size < bytes_needed)
		{
			goto outofspace;
		}

		memcpy(state->dst, resolved, bytes_needed);

		state->dst += bytes_needed;
		state->dst_size -= bytes_needed;
	}

	return bytes_needed;

writestream:
	/* Get code point properties */

	state->last_code_point = stream.codepoint[stream.current - 1];
	state->last_canonical_combining_class = stream.canonical_combining_class[stream.current - 1];
	state->last_general_category = PROPERTY_GET_GC(stream.codepoint[0]);

	/* Move source cursor */

	state->src = stream.src;
	state->src_size = stream.src_size;

	/* Write result to the output buffer */

	if (!stream_write(&stream, &state->dst, &state->dst_size, &bytes_needed))
	{
		goto outofspace;
	}

	return bytes_needed;

invaliddata:
	UTF8_SET_ERROR(INVALID_DATA);

	state->src_size = 0;

	return 0;

outofspace:
	UTF8_SET_ERROR(NOT_ENOUGH_SPACE);

	state->src_size = 0;

	return 0;
}