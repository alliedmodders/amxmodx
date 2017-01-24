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

#ifndef _UTF8REWIND_INTERNAL_COMPOSITION_H_
#define _UTF8REWIND_INTERNAL_COMPOSITION_H_

/*!
	\file
	\brief Composition interface.

	\cond INTERNAL
*/

#include "utf8rewind.h"
#include "streaming.h"

typedef struct {
	StreamState* input;
	StreamState* output;
	const size_t* qc_index;
	const uint8_t* qc_data;
} ComposeState;

uint8_t compose_initialize(ComposeState* state, StreamState* input, StreamState* output, uint8_t compatibility);

uint8_t compose_readcodepoint(ComposeState* state, uint8_t index);

uint8_t compose_execute(ComposeState* state);

/*! \endcond */

#endif /* _UTF8REWIND_INTERNAL_COMPOSITION_H_ */