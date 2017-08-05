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

#include "composition.h"

#include "codepoint.h"
#include "database.h"

uint8_t compose_initialize(ComposeState* state, StreamState* input, StreamState* output, uint8_t compatibility)
{
	memset(state, 0, sizeof(ComposeState));

	/* Ensure streams are valid */

	if (input == 0 ||
		output == 0)
	{
		return 0;
	}

	/* Set up streams */

	state->input = input;

	state->output = output;
	memset(state->output, 0, sizeof(StreamState));

	/* Set up codepoint quickcheck property */

	if (compatibility == 1)
	{
		state->qc_index = QuickCheckNFKCIndexPtr;
		state->qc_data = QuickCheckNFKCDataPtr;
	}
	else
	{
		state->qc_index = QuickCheckNFCIndexPtr;
		state->qc_data = QuickCheckNFCDataPtr;
	}

	return 1;
}

uint8_t compose_readcodepoint(ComposeState* state, uint8_t index)
{
	if (state->input->index == state->input->current &&
		!stream_read(state->input, state->qc_index, state->qc_data))
	{
		/* End of data */

		return 0;
	}

	/* Get next codepoint from sequence */

	state->output->codepoint[index]                  = state->input->codepoint[state->input->index];
	state->output->quick_check[index]                = state->input->quick_check[state->input->index];
	state->output->canonical_combining_class[index]  = state->input->canonical_combining_class[state->input->index];

	state->input->index++;
	state->output->current++;

	return 1;
}

uint8_t compose_execute(ComposeState* state)
{
	uint8_t output_index;
	uint8_t cursor_current;
	uint8_t cursor_next;

	/* Check if input is available */

	if (state->input == 0)
	{
		return 0;
	}

	/* Reset output */

	state->output->current = 0;

	/* Read first codepoint */

	if (!compose_readcodepoint(state, 0))
	{
		return 0;
	}

	for (output_index = 0; output_index < state->output->current; ++output_index)
	{
		/* Ensure current codepoint is a starter */

		cursor_current = output_index;

		while (state->output->canonical_combining_class[cursor_current] != CCC_NOT_REORDERED)
		{
			cursor_current++;

			if (cursor_current == state->output->current &&
				!compose_readcodepoint(state, cursor_current))
			{
				/* Only non-starters left */

				return 1;
			}
		}

		/* Get next codepoint */

		cursor_next = cursor_current + 1;

		while (
			cursor_next < state->output->current ||
			compose_readcodepoint(state, cursor_next))
		{
			/*
				Two codepoints can be composed if the current codepoint is a starter
				and the next codepoint isn't blocked by a previous codepoint.
			*/

			if (state->output->canonical_combining_class[cursor_next] > state->output->canonical_combining_class[cursor_next - 1] || /* Can be composed based on CCC */
				/* Quick check value can override composition block by previous codepoint */
				(state->output->quick_check[cursor_next] != QuickCheckResult_Yes && state->output->canonical_combining_class[cursor_next - 1] == CCC_NOT_REORDERED))
			{
				unicode_t composed = 0;

				/*
					Hangul composition

					Algorithm adapted from Unicode Technical Report #15:
					http://www.unicode.org/reports/tr15/tr15-18.html#Hangul
				*/

				if (state->output->codepoint[cursor_current] >= HANGUL_L_FIRST &&
					state->output->codepoint[cursor_current] <= HANGUL_L_LAST)
				{
					/* Check for Hangul LV pair */ 

					if (state->output->codepoint[cursor_next] >= HANGUL_V_FIRST &&
						state->output->codepoint[cursor_next] <= HANGUL_V_LAST)
					{
						unicode_t l_index = state->output->codepoint[cursor_current] - HANGUL_L_FIRST;
						unicode_t v_index = state->output->codepoint[cursor_next] - HANGUL_V_FIRST;

						composed = HANGUL_S_FIRST + (((l_index * HANGUL_V_COUNT) + v_index) * HANGUL_T_COUNT);
					}
				}
				else if (
					state->output->codepoint[cursor_current] >= HANGUL_S_FIRST &&
					state->output->codepoint[cursor_current] <= HANGUL_S_LAST)
				{
					/* Check for Hangul LV and T pair */ 

					if (state->output->codepoint[cursor_next] >= HANGUL_T_FIRST &&
						state->output->codepoint[cursor_next] <= HANGUL_T_LAST)
					{
						unicode_t t_index = state->output->codepoint[cursor_next] - HANGUL_T_FIRST;

						composed = state->output->codepoint[cursor_current] + t_index;
					}
				}
				else
				{
					/* Attempt to compose codepoints using the database */

					composed = database_querycomposition(
						state->output->codepoint[cursor_current],
						state->output->codepoint[cursor_next]);
				}

				/* Check if composition succeeded */

				if (composed != 0)
				{
					/*
						When we successfully compose two codepoints, the second must be removed
						from the sequence. The way this is accomplished is by marking the cell
						empty with a NUL codepoint.

						Decomposed:

						codepoint   U+0044 U+0307 U+0031
						    index        0      1      2

						Composed:

						codepoint   U+1E0A U+0000 U+0031
						    index        0      1      2

						If the second codepoint was at the end of the sequence, the output 
						sequence is shortened by one.
					*/

					/* Add composition to output */

					state->output->codepoint[cursor_current]                  = composed;
					state->output->quick_check[cursor_current]                = PROPERTY_GET(state->qc_index, state->qc_data, composed);
					state->output->canonical_combining_class[cursor_current]  = PROPERTY_GET_CCC(composed);

					/* Clear next codepoint from output */

					state->output->codepoint[cursor_next]                  = 0;
					state->output->quick_check[cursor_next]                = QuickCheckResult_Yes;
					state->output->canonical_combining_class[cursor_next]  = CCC_NOT_REORDERED;

					if (cursor_next == state->output->current - 1)
					{
						/* Next codepoint was at end of output */

						state->output->current--;
					}

					/* Reset cursor to current output index */

					cursor_current = output_index;
					cursor_next = output_index;
				}
			}
			else if (
				state->output->canonical_combining_class[cursor_next] == CCC_NOT_REORDERED)
			{
				/* Attempt to compose starters, but do not read from the next sequence */

				break;
			}

			/* Evaluate next codepoint */

			cursor_next++;
		}

		/* Fill up "holes" left by composing codepoints not at the end of the sequence */

		if (state->output->current > 1)
		{
			uint8_t write_index = 0;
			uint8_t read_index = 1;

			/*
				We want to move valid codepoints to the left as much as possible in order to fill up
				holes left by the composition process. 

				Note that the process does not clear unused codepoints at the end, this is a small
				optimization in order to avoid unnecessary clears. The length member is adjusted to
				the new size.
				
				Before reordering:

				codepoint   A  B  0  0  0  D
				    index   0  1  2  3  4  5
				   length                  6

				After reordering:

				codepoint   A  B  D  0  0  D
				    index   0  1  2  3  4  5
				   length         3
			*/

			/* Evaluate all codepoints in output sequence */

			while (write_index < state->output->current)
			{
				/* Check if read cursor is on an empty cell */

				if (read_index < state->output->current &&
					state->output->codepoint[read_index] == 0)
				{
					/* Skip all empty cells */

					while (
						read_index < state->output->current &&
						state->output->codepoint[read_index] == 0)
					{
						read_index++;
					}

					if (read_index == state->output->current)
					{
						/* Reached end of data */

						break;
					}

					/* Copy cell at read cursor to write cursor */

					state->output->codepoint[write_index]                  = state->output->codepoint[read_index];
					state->output->quick_check[write_index]                = state->output->quick_check[read_index];
					state->output->canonical_combining_class[write_index]  = state->output->canonical_combining_class[read_index];
				}

				/* Move cursors */

				write_index++;
				read_index++;
			}

			/* Adjust length of output sequence */

			state->output->current = write_index;
		}
		else
		{
			/* Evaluated all sequences in output */

			state->input = 0;

			break;
		}
	}

	return 1;
}