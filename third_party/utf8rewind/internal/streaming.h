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

#ifndef _UTF8REWIND_INTERNAL_STREAMING_H_
#define _UTF8REWIND_INTERNAL_STREAMING_H_

/*!
	\file
	\brief Streaming interface.

	\cond INTERNAL
*/

#include "utf8rewind.h"

/*
	UAX15-D4. Stream-Safe Text Process
		
	This is the process of producing a Unicode string in Stream-Safe Text Format by processing that string
	from start to finish, inserting U+034F COMBINING GRAPHEME JOINER (CGJ) within long sequences of
	non-starters. The exact position of the inserted CGJs are determined according to the following algorithm,
	which describes the generation of an output string from an input string:

	* If the input string is empty, return an empty output string.
	* Set nonStarterCount to zero.
	* For each code point C in the input string:
		* Produce the NFKD decomposition S.
		* If nonStarterCount plus the number of initial non-starters in S is greater than 30, append a CGJ to
			the output string and set the nonStarterCount to zero.
		* Append C to the output string.
		* If there are no starters in S, increment nonStarterCount by the number of code points in S; otherwise,
			set nonStarterCount to the number of trailing non-starters in S (which may be zero).
	* Return the output string.
*/

#define STREAM_SAFE_MAX 30
#define STREAM_BUFFER_MAX 32

typedef struct {
	const char* src;
	size_t src_size;
	uint8_t index;
	uint8_t current;
	uint8_t filled;
	uint8_t stable;
	uint8_t last_length;
	unicode_t codepoint[STREAM_BUFFER_MAX];
	uint8_t quick_check[STREAM_BUFFER_MAX];
	uint8_t canonical_combining_class[STREAM_BUFFER_MAX];
} StreamState;

uint8_t stream_initialize(StreamState* state, const char* input, size_t inputSize);

uint8_t stream_read(StreamState* state, const size_t* propertyIndex, const uint8_t* propertyData);

uint8_t stream_write(StreamState* state, char** output, size_t* outputSize, uint8_t* bytesWritten);

uint8_t stream_reorder(StreamState* state);

/*! \endcond */

#endif /* _UTF8REWIND_INTERNAL_STREAMING_H_ */