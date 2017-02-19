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

#ifndef _UTF8REWIND_INTERNAL_CASEMAPPING_H_
#define _UTF8REWIND_INTERNAL_CASEMAPPING_H_

/*!
	\file
	\brief Case mapping interface.

	\cond INTERNAL
*/

#include "utf8rewind.h"

typedef struct {
	const char* src;
	char* dst;
	size_t src_size;
	size_t dst_size;
	size_t total_bytes_needed;
	unicode_t last_code_point;
	size_t locale;
	const uint32_t* property_index1;
	const uint32_t* property_index2;
	const uint32_t* property_data;
	uint32_t last_general_category;
	uint8_t last_code_point_size;
	uint8_t last_canonical_combining_class;
	uint8_t quickcheck_flags;
} CaseMappingState;

uint8_t casemapping_initialize(
	CaseMappingState* state,
	const char* input, size_t inputSize,
	char* target, size_t targetSize,
	const uint32_t* propertyIndex1, const uint32_t* propertyIndex2, const uint32_t* propertyData,
	uint8_t quickCheck, size_t locale,
	int32_t* errors);

size_t casemapping_execute(CaseMappingState* state, int32_t* errors);

/*! \endcond */

#endif /* _UTF8REWIND_INTERNAL_CASEMAPPING_H_ */