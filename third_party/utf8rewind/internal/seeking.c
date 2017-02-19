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

#include "seeking.h"

#include "codepoint.h"

const char* seeking_forward(const char* input, const char* inputEnd, size_t inputSize, off_t offset)
{
	if (inputEnd <= input ||  /* Swapped parameters */
		offset <= 0 ||        /* Invalid offset */
		inputSize == 0)       /* Nothing to do */
	{
		return input;
	}
	else if (
		offset >= (off_t)inputSize)  /* Out of bounds */
	{
		return inputEnd;
	}

	do
	{
		/* Get decoded length of next sequence */

		uint8_t codepoint_length = codepoint_decoded_length[(uint8_t)*input];

		if (codepoint_length > 1 &&
			codepoint_length < 7)
		{
			/* Check all bytes of multi-byte sequence */

			uint8_t i;

			for (i = 0; i < codepoint_length; ++i)
			{
				/* Next byte of sequence */

				input++;

				if (input == inputEnd ||                             /* End of data */
					codepoint_decoded_length[(uint8_t)*input] != 0)  /* Not a continuation byte */
				{
					break;
				}
			}
		}
		else
		{
			/* Skip to next sequence */

			input++;
		}
	}
	while (input < inputEnd &&
		--offset > 0);

	return input;
}

const char* seeking_rewind(const char* inputStart, const char* input, size_t inputSize, off_t offset)
{
	const char* marker;
	const char* marker_valid;

	if (inputStart >= input ||  /* Swapped parameters */
		offset >= 0)            /* Invalid offset */
	{
		return input;
	}
	else if (
		-offset >= (off_t)inputSize)  /* Out of bounds */
	{
		return inputStart;
	}

	/* Set up the marker */

	marker = input - 1;
	marker_valid = marker;

	do
	{
		/* Move the cursor */

		input--;

		/* Move the marker until we encounter a valid sequence */

		while (marker_valid == input)
		{
			uint8_t codepoint_length = codepoint_decoded_length[(uint8_t)*marker];

			if (codepoint_length == 1 ||  /* Basic Latin */
				codepoint_length == 7)    /* Illegal byte */
			{
				marker_valid = marker;

				break;
			}
			else if (
				codepoint_length > 1)
			{
				if (marker == inputStart &&
					/* Not overlong */
					marker_valid - inputStart == codepoint_length - 1)
				{
					/* Last sequence */

					return marker;
				}
				else
				{
					/* Multi-byte sequence */

					marker_valid = marker + codepoint_length - 1;

					break;
				}
			}
			else if (
				marker <= inputStart)
			{
				/* Continuation bytes only */

				marker_valid = marker;

				break;
			}
			else
			{
				/* Move marker to next byte */

				marker--;
			}
		}

		/* Read the next part of a sequence */

		if (input <= marker_valid)
		{
			if (marker == inputStart)
			{
				/* Last sequence */

				return marker;
			}
			else
			{
				/* Move the cursor to the start of the sequence */

				input = marker;

				/* Reset the marker on the next byte */

				marker--;
				marker_valid = marker;
			}
		}
	}
	while (input >= inputStart &&
		++offset < 0);

	return input;
}