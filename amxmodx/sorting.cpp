// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include <stdlib.h>
#include <time.h>
#include "datastructs.h"

/***********************************
 *   About the double array hack   *
     ***************************   

 Double arrays in Pawn are vectors offset by the current offset.  For example:

 new array[2][2]

   In this array, index 0 contains the offset from the current offset which
 results in the final vector [2] (at [0][2]).  Meaning, to dereference [1][2], 
 it is equivalent to:

 address = &array[1] + array[1] + 2 * sizeof(cell)

   The fact that each offset is from the _current_ position rather than the _base_
 position is very important.  It means that if you to try to swap vector positions,
 the offsets will no longer match, because their current position has changed.  A 
 simple and ingenious way around this is to back up the positions in a separate array,
 then to overwrite each position in the old array with absolute indices.  Pseudo C++ code:

 cell *array; //assumed to be set to the 2+D array
 cell *old_offsets = new cell[2];
 for (int i=0; i<2; i++)
 {
    old_offsets = array[i];
	array[i] = i;
 }

   Now, you can swap the array indices with no problem, and do a reverse-lookup to find the original addresses.
 After sorting/modification is done, you must relocate the new indices.  For example, if the two vectors in our
 demo array were swapped, array[0] would be 1 and array[1] would be 0.  This is invalid to the virtual machine.
 Luckily, this is also simple -- all the information is there.

 for (int i=0; i<2; i++)
 {
	//get the # of the vector we want to relocate in
	cell vector_index = array[i];			
	//get the real address of this vector
	char *real_address = (char *)array + (vector_index * sizeof(cell)) + old_offsets[vector_index];
	//calc and store the new distance offset
	array[i] = real_address - ( (char *)array + (vector_index + sizeof(cell)) )
 }

 Note that the inner expression can be heavily reduced; it is expanded for readability.
 **********************************/

enum SortOrder
{
	Sort_Ascending = 0,
	Sort_Descending = 1,
	Sort_Random = 2,
};

int sort_ints_asc(const void *int1, const void *int2)
{
	return (*(int *)int1) - (*(int *)int2);
}

int sort_ints_desc(const void *int1, const void *int2)
{
	return (*(int *)int2) - (*(int *)int1);
}

void sort_random(cell *array, cell size)
{
	srand((unsigned int)time(NULL));

	for (int i = size-1; i > 0; i--)
	{
		int n = rand() % (i + 1);

		if (array[i] != array[n]) 
		{
			array[i] ^= array[n];
			array[n] ^= array[i];
			array[i] ^= array[n];
		}
	}
}

static cell AMX_NATIVE_CALL SortIntegers(AMX *amx, cell *params)
{
	cell *array = get_amxaddr(amx, params[1]);
	cell array_size = params[2];
	cell type = params[3];

	if (type == Sort_Ascending) 
	{
		qsort(array, array_size, sizeof(cell), sort_ints_asc);
	} 
	else if (type == Sort_Descending) 
	{
		qsort(array, array_size, sizeof(cell), sort_ints_desc);
	} 
	else
	{
		sort_random(array, array_size);
	}

	return 1;
}

int sort_floats_asc(const void *float1, const void *float2)
{
	REAL r1 = *(REAL *)float1;
	REAL r2 = *(REAL *)float2;
	
	if (r1 < r2)
	{
		return -1;
	} else if (r2 < r1) {
		return 1;
	} else {
		return 0;
	}
}

int sort_floats_desc(const void *float1, const void *float2)
{
	REAL r1 = *(REAL *)float1;
	REAL r2 = *(REAL *)float2;

	if (r1 < r2)
	{
		return 1;
	} else if (r2 < r1) {
		return -1;
	} else {
		return 0;
	}
}

static cell AMX_NATIVE_CALL SortFloats(AMX *amx, cell *params)
{
	cell *array = get_amxaddr(amx, params[1]);
	cell array_size = params[2];
	cell type = params[3];

	if (type == Sort_Ascending)
	{
		qsort(array, array_size, sizeof(cell), sort_floats_asc);
	} 
	else if (type == Sort_Descending)
	{
		qsort(array, array_size, sizeof(cell), sort_floats_desc);
	}
	else
	{
		sort_random(array, array_size);
	}

	return 1;
}

static cell *g_CurStringArray = NULL;
static cell *g_CurRebaseMap = NULL;

int sort_strings_asc(const void *blk1, const void *blk2)
{
	cell reloc1 = *(cell *)blk1;
	cell reloc2 = *(cell *)blk2;
	
	register cell *str1 = (cell *)((char *)(&g_CurStringArray[reloc1]) + g_CurRebaseMap[reloc1]);
	register cell *str2 = (cell *)((char *)(&g_CurStringArray[reloc2]) + g_CurRebaseMap[reloc2]);

	while (*str1 == *str2++)
	{
		if (*str1++ == 0)
		{
			return 0;
		}
	}

	return (*str1 - *(str2 - 1));
}

int sort_strings_desc(const void *blk1, const void *blk2)
{
	cell reloc1 = *(cell *)blk1;
	cell reloc2 = *(cell *)blk2;

	register cell *str1 = (cell *)((char *)(&g_CurStringArray[reloc1]) + g_CurRebaseMap[reloc1]);
	register cell *str2 = (cell *)((char *)(&g_CurStringArray[reloc2]) + g_CurRebaseMap[reloc2]);

	while (*str1 == *str2++)
	{
		if (*str1++ == 0)
		{
			return 0;
		}
	}

	return (*(str2 - 1) - *str1);
}

static cell AMX_NATIVE_CALL SortStrings(AMX *amx, cell *params)
{
	cell *array = get_amxaddr(amx, params[1]);
	cell array_size = params[2];
	cell type = params[3];

	/** HACKHACK - back up the old indices, replace the indices with something easier */
	cell amx_addr, *phys_addr;
	int err;
	if ((err=amx_Allot(amx, array_size, &amx_addr, &phys_addr)) != AMX_ERR_NONE)
	{
		LogError(amx, err, "Ran out of memory");
		return 0;
	}

	g_CurStringArray = array;
	g_CurRebaseMap = phys_addr;

	for (int i=0; i<array_size; i++)
	{
		phys_addr[i] = array[i];
		array[i] = i;
	}

	if (type == Sort_Ascending)
	{
		qsort(array, array_size, sizeof(cell), sort_strings_asc);
	} 
	else if (type == Sort_Descending)
	{
		qsort(array, array_size, sizeof(cell), sort_strings_desc);
	}
	else
	{
		sort_random(array, array_size);
	}

	/* END HACKHACK - restore what we damaged so Pawn doesn't throw up.
	 * We'll browse through each index of the array and patch up the distance.
	 */
	for (int i=0; i<array_size; i++)
	{
		/* Compute the final address of the old array and subtract the new location.
		 * This is the fixed up distance.
		 */
		array[i] = ((char *)&array[array[i]] + phys_addr[array[i]]) - (char *)&array[i];
	}

	amx_Release(amx, amx_addr);

	g_CurStringArray = NULL;
	g_CurRebaseMap = NULL;

	return 1;
}

struct sort_info
{
	int pfn;
	cell data_addr;
	cell data_size;
	cell array_addr;
	cell *array_base;
	cell *array_remap;
	AMX *amx;
};

static CStack<sort_info *> g_AMXSortStack;

int sort1d_amx_custom(const void *elem1, const void *elem2)
{
	cell c1 = *(cell *)elem1;
	cell c2 = *(cell *)elem2;
	sort_info *pInfo = g_AMXSortStack.front();

	return executeForwards(pInfo->pfn, c1, c2, pInfo->array_addr, pInfo->data_addr, pInfo->data_size);
}

static cell AMX_NATIVE_CALL SortCustom1D(AMX *amx, cell *params)
{
	cell *array = get_amxaddr(amx, params[1]);
	cell array_size = params[2];
	int len;
	const char *funcname = get_amxstring(amx, params[3], 0, len);

	int pfn = registerSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	if (pfn < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "The public function \"%s\" was not found.", funcname);
		return 0;
	}

	sort_info *pInfo = new sort_info;

	pInfo->pfn = pfn;
	pInfo->data_addr = params[4];
	pInfo->data_size = params[5];
	pInfo->array_addr = params[1];
	pInfo->array_remap = NULL;
	pInfo->array_base = NULL;

	g_AMXSortStack.push(pInfo);
	qsort(array, array_size, sizeof(cell), sort1d_amx_custom);
	g_AMXSortStack.pop();

	unregisterSPForward(pfn);
	delete pInfo;

	return 1;
}

int sort2d_amx_custom(const void *elem1, const void *elem2)
{
	cell c1 = *(cell *)elem1;
	cell c2 = *(cell *)elem2;
	sort_info *pInfo = g_AMXSortStack.front();

	cell c1_addr = pInfo->array_addr + (c1 * sizeof(cell)) + pInfo->array_remap[c1];
	cell c2_addr = pInfo->array_addr + (c2 * sizeof(cell)) + pInfo->array_remap[c2];

	//cell *c1_r = get_amxaddr(pInfo->amx, c1_addr);
	//cell *c2_r = get_amxaddr(pInfo->amx, c2_addr);

	return executeForwards(pInfo->pfn, c1_addr, c2_addr, pInfo->array_addr, pInfo->data_addr, pInfo->data_size);
}

static cell AMX_NATIVE_CALL SortCustom2D(AMX *amx, cell *params)
{
	cell *array = get_amxaddr(amx, params[1]);
	cell array_size = params[2];
	int len;
	const char *funcname = get_amxstring(amx, params[3], 0, len);

	/** back up the old indices, replace the indices with something easier */
	cell amx_addr, *phys_addr;
	int err;
	if ((err=amx_Allot(amx, array_size, &amx_addr, &phys_addr)) != AMX_ERR_NONE)
	{
		LogError(amx, err, "Ran out of memory");
		return 0;
	}

	int pfn = registerSPForwardByName(amx, funcname, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	if (pfn < 0)
	{
		amx_Release(amx, amx_addr);
		LogError(amx, AMX_ERR_NATIVE, "The public function \"%s\" was not found.", funcname);
		return 0;
	}

	sort_info *pInfo = new sort_info;

	pInfo->pfn = pfn;
	pInfo->data_addr = params[4];
	pInfo->data_size = params[5];
	pInfo->array_addr = params[1];
	pInfo->amx = amx;

	/** Same process as in strings, back up the old indices for later fixup */
	pInfo->array_base = array;
	pInfo->array_remap = phys_addr;
	
	for (int i=0; i<array_size; i++)
	{
		phys_addr[i] = array[i];
		array[i] = i;
	}

	g_AMXSortStack.push(pInfo);
	qsort(array, array_size, sizeof(cell), sort2d_amx_custom);
	g_AMXSortStack.pop();

	/** Fixup process! */
	for (int i=0; i<array_size; i++)
	{
		/* Compute the final address of the old array and subtract the new location.
		 * This is the fixed up distance.
		 */
		array[i] = ((char *)&array[array[i]] + phys_addr[array[i]]) - (char *)&array[i];
	}

	amx_Release(amx, amx_addr);
	unregisterSPForward(pInfo->pfn);
	delete pInfo;

	return 1;
}

enum SortType
{
	Sort_Integer = 0,
	Sort_Float,
	Sort_String,
};

int strcellcmp(cell *s1, cell *s2)
{
	for (; *s1 == *s2; s1++, s2++)
	{
		if (*s1 == '\0')
		{
			return 0;
		}
	}

	return (*(byte *)s1 < *(byte *)s2) ? -1 : +1;
}

int sort_adtarray_strings_asc(const void *str1, const void *str2)
{
	return strcellcmp((cell *)str1, (cell *)str2);
}

int sort_adtarray_strings_desc(const void *str1, const void *str2)
{
	return strcellcmp((cell *)str2, (cell *)str1);
}

void sort_adt_random(CellArray *cArray)
{
	size_t arraysize = cArray->size();

	srand((unsigned int)time(NULL));

	for (int i = arraysize-1; i > 0; i--)
	{
		int n = rand() % (i + 1);

		cArray->swap(i, n);
	}
}

static cell AMX_NATIVE_CALL SortADTArray(AMX *amx, cell *params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	cell order = params[2];

	if (order == Sort_Random)
	{
		sort_adt_random(vec);

		return 1;
	}

	cell type = params[3];
	size_t arraysize = vec->size();
	size_t blocksize = vec->blocksize();
	cell *array = vec->base();

	if (type == Sort_Integer)
	{
		if (order == Sort_Ascending)
		{
			qsort(array, arraysize, blocksize * sizeof(cell), sort_ints_asc);
		}
		else
		{
			qsort(array, arraysize, blocksize * sizeof(cell), sort_ints_desc);
		}
	}
	else if (type == Sort_Float)
	{
		if (order == Sort_Ascending)
		{
			qsort(array, arraysize, blocksize * sizeof(cell), sort_floats_asc);
		}
		else 
		{
			qsort(array, arraysize, blocksize * sizeof(cell), sort_floats_desc);
		}
	}
	else if (type == Sort_String)
	{
		if (order == Sort_Ascending)
		{
			qsort(array, arraysize, blocksize * sizeof(cell), sort_adtarray_strings_asc);
		}
		else 
		{
			qsort(array, arraysize, blocksize * sizeof(cell), sort_adtarray_strings_desc);
		}
	}

	return 1;
}


AMX_NATIVE_INFO g_SortNatives[] = 
{
	{"SortIntegers",			SortIntegers},
	{"SortFloats",				SortFloats},
	{"SortStrings",				SortStrings},
	{"SortCustom1D",			SortCustom1D},
	{"SortCustom2D",			SortCustom2D},
	{"SortADTArray",			SortADTArray},

	{NULL,						NULL},
};
