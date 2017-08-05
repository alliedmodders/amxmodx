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

#include "streaming.h"

#include "codepoint.h"
#include "database.h"

uint8_t stream_initialize(StreamState* state, const char* input, size_t inputSize)
{
	memset(state, 0, sizeof(StreamState));

	if (input == 0 ||
		inputSize == 0)
	{
		return 0;
	}

	state->src = input;
	state->src_size = inputSize;

	state->stable = 1;

	return 1;
}

uint8_t stream_read(StreamState* state, const size_t* propertyIndex, const uint8_t* propertyData)
{
	/* Ensure input is available */

	if (state->src_size == 0 ||
		propertyIndex == 0 ||
		propertyData == 0)
	{
		return 0;
	}

	/* Reset sequence after the first pass */

	if (state->filled > 0)
	{
		/* Check for end of data */

		if (state->filled == state->current &&
			state->src_size <= state->last_length)
		{
			state->src_size = 0;

			state->index = 0;
			state->current = 0;
			state->filled = 0;

			return 0;
		}

		/* Copy last peeked codepoint to new sequence */

		state->codepoint[0]                  = state->codepoint[state->filled - 1];
		state->canonical_combining_class[0]  = state->canonical_combining_class[state->filled - 1];
		state->quick_check[0]                = state->quick_check[state->filled - 1];

		/* New sequence always starts as stable */

		state->stable = 1;

		/* Reset buffer members */

		state->index = 0;
		state->current = 1;
		state->filled = 1;
	}

	/* Read codepoints */

	while (state->filled < STREAM_SAFE_MAX)
	{
		/* Move the input cursor after peeking */

		if (state->last_length > 0)
		{
			if (state->src_size <= state->last_length)
			{
				state->src += state->src_size;
				state->src_size = 0;

				break;
			}

			state->src += state->last_length;
			state->src_size -= state->last_length;
		}

		/* Peek the next codepoint */

		state->last_length = codepoint_read(state->src, state->src_size, &state->codepoint[state->filled]);
		state->quick_check[state->filled] = PROPERTY_GET(propertyIndex, propertyData, state->codepoint[state->filled]);
		state->canonical_combining_class[state->filled] = PROPERTY_GET_CCC(state->codepoint[state->filled]);

		state->filled++;

		if (state->current > 0)
		{
			/* Sequences end on the next starter and can consist of only non-starters */

			if (state->canonical_combining_class[state->current] == 0)
			{
				break;
			}

			/* Check if sequence is unstable by comparing canonical combining classes */

			if (state->stable &&
				state->canonical_combining_class[state->current] < state->canonical_combining_class[state->current - 1])
			{
				state->stable = 0;
			}
		}

		state->current++;
	}

	if (state->filled == STREAM_SAFE_MAX)
	{
		/* Insert COMBINING GRAPHEME JOINER into output */

		state->codepoint[state->filled]                  = CP_COMBINING_GRAPHEME_JOINER;
		state->quick_check[state->filled]                = QuickCheckResult_Yes;
		state->canonical_combining_class[state->filled]  = CCC_NOT_REORDERED;

		state->filled++;
	}

	return 1;
}

uint8_t stream_write(StreamState* state, char** output, size_t* outputSize, uint8_t* bytesWritten)
{
	uint8_t i;

	if (state->current == 0)
	{
		/* Nothing to write */

		*bytesWritten = 0;

		return 1;
	}

	/* Encode code points as UTF-8 */

	for (i = 0; i < state->current; ++i)
	{
		uint8_t encoded_size = codepoint_write(state->codepoint[i], output, outputSize);
		if (encoded_size == 0)
		{
			/* Not enough space */

			return 0;
		}

		*bytesWritten += encoded_size;
	}

	return 1;
}

uint8_t stream_reorder(StreamState* state)
{
	uint8_t i;
	uint8_t dirty = 1;

	if (state->current == 0)
	{
		/* Nothing to do */

		return 0;
	}

	/* Reorder codepoints until the entire sequence is table */

	do
	{
		dirty = 0;

		for (i = 1; i < state->current; i++)
		{
			/* Sort codepoints by canonical combining class, smallest to largest */

			if (state->canonical_combining_class[i] < state->canonical_combining_class[i - 1])
			{
				unicode_t swap_cp;
				uint8_t swap_qc;
				uint8_t swap_ccc;

				swap_cp = state->codepoint[i];
				state->codepoint[i] = state->codepoint[i - 1];
				state->codepoint[i - 1] = swap_cp;

				swap_qc = state->quick_check[i];
				state->quick_check[i] = state->quick_check[i - 1];
				state->quick_check[i - 1] = swap_qc;

				swap_ccc = state->canonical_combining_class[i];
				state->canonical_combining_class[i] = state->canonical_combining_class[i - 1];
				state->canonical_combining_class[i - 1] = swap_ccc;

				dirty = 1;
			}
		}
	}
	while (dirty == 1);

	return 1;
}