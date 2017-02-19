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

#ifndef _UTF8REWIND_UNICODEDATABASE_H_
#define _UTF8REWIND_UNICODEDATABASE_H_

/*!
	\file
	\brief Unicode property database.

	\cond INTERNAL
*/

#include "utf8rewind.h"

typedef struct {
	unicode_t codepoint;
	uint32_t length_and_offset;
} DecompositionRecord;

typedef struct {
	uint64_t key;
	unicode_t value;
} CompositionRecord;

extern const size_t* GeneralCategoryIndexPtr;
extern const uint32_t* GeneralCategoryDataPtr;

extern const size_t* CanonicalCombiningClassIndexPtr;
extern const uint8_t* CanonicalCombiningClassDataPtr;

extern const size_t* QuickCheckCaseMappedIndexPtr;
extern const uint8_t* QuickCheckCaseMappedDataPtr;

extern const size_t* QuickCheckNFCIndexPtr;
extern const uint8_t* QuickCheckNFCDataPtr;

extern const size_t* QuickCheckNFDIndexPtr;
extern const uint8_t* QuickCheckNFDDataPtr;

extern const size_t* QuickCheckNFKCIndexPtr;
extern const uint8_t* QuickCheckNFKCDataPtr;

extern const size_t* QuickCheckNFKDIndexPtr;
extern const uint8_t* QuickCheckNFKDDataPtr;

extern const size_t UnicodeNFDRecordCount;
extern const DecompositionRecord* UnicodeNFDRecordPtr;

extern const size_t UnicodeNFKDRecordCount;
extern const DecompositionRecord* UnicodeNFKDRecordPtr;

extern const size_t UnicodeUppercaseRecordCount;
extern const DecompositionRecord* UnicodeUppercaseRecordPtr;

extern const size_t UnicodeLowercaseRecordCount;
extern const DecompositionRecord* UnicodeLowercaseRecordPtr;

extern const size_t UnicodeTitlecaseRecordCount;
extern const DecompositionRecord* UnicodeTitlecaseRecordPtr;

extern const size_t UnicodeCompositionRecordCount;
extern const CompositionRecord* UnicodeCompositionRecordPtr;

extern const uint32_t* NFDIndex1Ptr;
extern const uint32_t* NFDIndex2Ptr;
extern const uint32_t* NFDDataPtr;

extern const uint32_t* NFKDIndex1Ptr;
extern const uint32_t* NFKDIndex2Ptr;
extern const uint32_t* NFKDDataPtr;

extern const uint32_t* UppercaseIndex1Ptr;
extern const uint32_t* UppercaseIndex2Ptr;
extern const uint32_t* UppercaseDataPtr;

extern const uint32_t* LowercaseIndex1Ptr;
extern const uint32_t* LowercaseIndex2Ptr;
extern const uint32_t* LowercaseDataPtr;

extern const uint32_t* TitlecaseIndex1Ptr;
extern const uint32_t* TitlecaseIndex2Ptr;
extern const uint32_t* TitlecaseDataPtr;

extern const uint32_t* CaseFoldingIndex1Ptr;
extern const uint32_t* CaseFoldingIndex2Ptr;
extern const uint32_t* CaseFoldingDataPtr;

extern const char* CompressedStringData;
extern const size_t CompressedStringDataLength;

extern const char* DecompositionData;
extern const size_t DecompositionDataLength;

/*! \endcond */

#endif /* _UTF8REWIND_UNICODEDATABASE_H_ */