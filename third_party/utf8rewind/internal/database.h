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

#ifndef _UTF8REWIND_INTERNAL_DATABASE_H_
#define _UTF8REWIND_INTERNAL_DATABASE_H_

/*!
	\file
	\brief Database interface.

	\cond INTERNAL
*/

#include "utf8rewind.h"

#include "../unicodedatabase.h"

enum QuickCheckCaseMapped
{
	QuickCheckCaseMapped_Uppercase = 0x01,
	QuickCheckCaseMapped_Lowercase = 0x02,
	QuickCheckCaseMapped_Titlecase = 0x04,
	QuickCheckCaseMapped_Casefolded = 0x08,
};

enum QuickCheckResult
{
	QuickCheckResult_Yes,
	QuickCheckResult_Maybe,
	QuickCheckResult_No,
};

#define PROPERTY_INDEX_SHIFT (5)

static const unicode_t PROPERTY_DATA_MASK = (1 << PROPERTY_INDEX_SHIFT) - 1;

#define PROPERTY_GET(_indexArray, _dataArray, _cp) \
	(_dataArray)[ \
		(_indexArray)[(_cp) >> PROPERTY_INDEX_SHIFT] + \
		((_cp) & PROPERTY_DATA_MASK)]

#define PROPERTY_GET_GC(_cp) \
	PROPERTY_GET(GeneralCategoryIndexPtr, GeneralCategoryDataPtr, _cp)

#define PROPERTY_GET_CCC(_cp) \
	PROPERTY_GET(CanonicalCombiningClassIndexPtr, CanonicalCombiningClassDataPtr, _cp)

#define PROPERTY_GET_CM(_cp) \
	PROPERTY_GET(QuickCheckCaseMappedIndexPtr, QuickCheckCaseMappedDataPtr, _cp)

#define PROPERTY_GET_NFC(_cp) \
	PROPERTY_GET(QuickCheckNFCIndexPtr, QuickCheckNFCDataPtr, _cp)

#define PROPERTY_GET_NFD(_cp) \
	PROPERTY_GET(QuickCheckNFDIndexPtr, QuickCheckNFDDataPtr, _cp)

#define PROPERTY_GET_NFKC(_cp) \
	PROPERTY_GET(QuickCheckNFKCIndexPtr, QuickCheckNFKCDataPtr, _cp)

#define PROPERTY_GET_NFKD(_cp) \
	PROPERTY_GET(QuickCheckNFKDIndexPtr, QuickCheckNFKDDataPtr, _cp)

const char* database_querydecomposition(unicode_t codepoint, const uint32_t* index1Array, const uint32_t* index2Array, const uint32_t* dataArray, uint8_t* length);

unicode_t database_querycomposition(unicode_t left, unicode_t right);

/*! \endcond */

#endif /* _UTF8REWIND_INTERNAL_DATABASE_H_ */