// vim: set ts=4 sw=4 tw=99 noet:
//
// AMX Mod X, based on AMX Mod by Aleksander Naszko ("OLO").
// Copyright (C) The AMX Mod X Development Team.
//
// This software is licensed under the GNU General Public License, version 3 or higher.
// Additional exceptions apply. For full license details, see LICENSE.txt or visit:
//     https://alliedmods.net/amxmodx-license

#include "amxmodx.h"
#include "datastructs.h"
#include <amtl/am-utility.h>

NativeHandle<CellArray> ArrayHandles;

// Array:ArrayCreate(cellsize=1, reserved=32);
static cell AMX_NATIVE_CALL ArrayCreate(AMX* amx, cell* params)
{
	// params[1] (cellsize) is how big in cells each element is.
	// this MUST be greater than 0!
	int cellsize = params[1];

	// params[2] (reserved) is how many elements to allocate
	// immediately when the list is created.
	int reserved = params[2];

	if (cellsize <= 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array size (%d)", cellsize);
		return -1;
	}

	if (reserved < 0)
	{
		reserved = 0;
	}

	return ArrayHandles.create(cellsize, reserved);
}

// native ArrayClear(Array:which);
static cell AMX_NATIVE_CALL ArrayClear(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	vec->clear();

	return 1;
}

// native ArraySize(Array:which);
static cell AMX_NATIVE_CALL ArraySize(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	return vec->size();
}

// native bool:ArrayResize(Array:which, newsize);
static cell AMX_NATIVE_CALL ArrayResize(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	if (!vec->resize(params[2]))
	{
		LogError(amx, AMX_ERR_NATIVE, "Unable to resize array to \"%u\"", params[2]);
		return 0;
	}

	return 1;
}

// native Array:ArrayClone(Array:which);
static cell AMX_NATIVE_CALL ArrayClone(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	return ArrayHandles.clone(vec->clone());
}

// native ArrayGetArray(Array:which, item, any:output[], size = -1);
static cell AMX_NATIVE_CALL ArrayGetArray(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell *blk = vec->at(idx);
	size_t indexes = vec->blocksize();

	if (*params / sizeof(cell) == 4)
	{
		if (params[4] != -1 && (size_t)params[4] <= vec->blocksize())
		{
			indexes = params[4];
		}
	}

	cell *addr = get_amxaddr(amx, params[3]);

	memcpy(addr, blk, sizeof(cell) * indexes);

	return indexes;
}

// native any:ArrayGetCell(Array:which, item, block = 0, bool:asChar = false);
static cell AMX_NATIVE_CALL ArrayGetCell(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell *blk = vec->at(idx);

	if (*params / sizeof(cell) <= 2)
	{
		return *blk;
	}

	idx = (size_t)params[3];

	if (!params[4])
	{
		if (idx >= vec->blocksize())
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid block %d (blocksize: %d)", idx, vec->blocksize());
			return 0;
		}

		return blk[idx];
	}
	else 
	{
		if (idx >= vec->blocksize() * 4)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid byte %d (blocksize: %d bytes)", idx, vec->blocksize() * 4);
			return 0;
		}

		return (cell)*((char *)blk + idx);
	}

	return 0;
}

// native ArrayGetString(Array:which, item, output[], size);
static cell AMX_NATIVE_CALL ArrayGetString(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell *blk = vec->at(idx);
	return set_amxstring_utf8(amx, params[3], blk, amxstring_len(blk), params[4]);
}

// native ArraySetArray(Array:which, item, const any:input[], size =-1);
static cell AMX_NATIVE_CALL ArraySetArray(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell *blk = vec->at(idx);
	size_t indexes = vec->blocksize();

	if (*params / sizeof(cell) == 4)
	{
		if (params[4] != -1 && (size_t)params[4] <= vec->blocksize())
		{
			indexes = params[4];
		}
	}

	cell *addr = get_amxaddr(amx, params[3]);

	memcpy(blk, addr, sizeof(cell) * indexes);

	return indexes;
}

// native ArraySetCell(Array:which, item, any:input, block = 0, bool:asChar = false);
static cell AMX_NATIVE_CALL ArraySetCell(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell *blk = vec->at(idx);
	idx = (size_t)params[4];

	if (*params / sizeof(cell) <= 3)
	{
		*blk = params[3];
		return 1;
	}

	if (params[5] == 0)
	{
		if (idx >= vec->blocksize())
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid block %d (blocksize: %d)", idx, vec->blocksize());
			return 0;
		}
		blk[idx] = params[3];
	}
	else 
	{
		if (idx >= vec->blocksize() * 4)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid byte %d (blocksize: %d bytes)", idx, vec->blocksize() * 4);
			return 0;
		}
		*((char *)blk + idx) = (char)params[3];
	}

	return 1;
}

// native ArraySetString(Array:which, item, const input[]);
static cell AMX_NATIVE_CALL ArraySetString(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell *blk = vec->at(idx);

	int len;
	char *str = get_amxstring(amx, params[3], 0, len);

	return strncopy(blk, str, ke::Min((size_t)len + 1, vec->blocksize()));
}

// native ArrayPushArray(Array:which, const any:input[], size = -1);
static cell AMX_NATIVE_CALL ArrayPushArray(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	cell *blk = vec->push();

	if (!blk)
	{
		LogError(amx, AMX_ERR_NATIVE, "Failed to grow array");
		return 0;
	}

	cell *addr = get_amxaddr(amx, params[2]);
	size_t indexes = vec->blocksize();

	if (*params / sizeof(cell) == 3)
	{
		if (params[3] != -1 && (size_t)params[3] <= vec->blocksize())
		{
			indexes = params[3];
		}
	}

	memcpy(blk, addr, sizeof(cell) * indexes);

	return static_cast<cell>((vec->size() - 1));
}

// native ArrayPushCell(Array:which, any:input);
static cell AMX_NATIVE_CALL ArrayPushCell(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	cell *blk = vec->push();

	if (!blk)
	{
		LogError(amx, AMX_ERR_NATIVE, "Failed to grow array");
		return 0;
	}

	*blk = params[2];

	return static_cast<cell>((vec->size() - 1));
}

// native ArrayPushString(Array:which, const input[]);
static cell AMX_NATIVE_CALL ArrayPushString(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	cell *blk = vec->push();
	if (!blk)
	{
		LogError(amx, AMX_ERR_NATIVE, "Failed to grow array");
		return 0;
	}

	strncopy(blk, get_amxaddr(amx, params[2]), vec->blocksize());

	return static_cast<cell>((vec->size() - 1));
}

// native DoNotUse : ArrayGetStringHandle(Array : which, item);
static cell AMX_NATIVE_CALL ArrayGetStringHandle(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	cell* ptr = vec->at(idx);

	if (ptr == NULL)
	{
		return 0;
	}

	return reinterpret_cast<cell>(ptr);
}

// native ArrayInsertArrayAfter(Array:which, item, const any:input[]);
static cell AMX_NATIVE_CALL ArrayInsertArrayAfter(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = params[2] + 1;

	if (idx > vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertArrayAfter (%d:%d)", idx, vec->size());
		return 0;
	}

	cell *addr = get_amxaddr(amx, params[3]);

	memcpy(vec->insert_at(idx), addr, sizeof(cell) * vec->blocksize());

	return 1;
}

// native ArrayInsertCellAfter(Array:which, item, any:input);
static cell AMX_NATIVE_CALL ArrayInsertCellAfter(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = params[2] + 1;

	if (idx > vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertCellAfter (%d:%d)", idx, vec->size());
		return 0;
	}

	*vec->insert_at(idx) = params[3];

	return 1;
}

// native ArrayInsertStringAfter(Array:which, item, const input[]);
static cell AMX_NATIVE_CALL ArrayInsertStringAfter(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = params[2] + 1;

	if (idx > vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertStringAfter (%d:%d)", idx, vec->size());
		return 0;
	}

	int len;
	const char *str = get_amxstring(amx, params[3], 0, len);

	return strncopy(vec->insert_at(idx), str, ke::Min((size_t)len + 1, vec->blocksize()));
}

// native ArrayInsertArrayBefore(Array:which, item, const any:input[]);
static cell AMX_NATIVE_CALL ArrayInsertArrayBefore(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertArrayBefore (%d:%d)", idx, vec->size());
		return 0;
	}

	cell *addr = get_amxaddr(amx, params[3]);

	memcpy(vec->insert_at(idx), addr, vec->blocksize() * sizeof(cell));

	return 1;
}

// native ArrayInsertCellBefore(Array:which, item, const any:input);
static cell AMX_NATIVE_CALL ArrayInsertCellBefore(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertCellBefore (%d:%d)", idx, vec->size());
		return 0;
	}

	*vec->insert_at(idx) = params[3];

	return 1;
}
// native ArrayInsertStringBefore(Array:which, item, const input[]);
static cell AMX_NATIVE_CALL ArrayInsertStringBefore(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertStringBefore (%d:%d)", idx, vec->size());
		return 0;
	}

	int len;
	const char *str = get_amxstring(amx, params[3], 0, len);

	return strncopy(vec->insert_at(idx), str, ke::Min((size_t)len + 1, vec->blocksize()));
}

// native ArraySwap(Array:which, item1, item2);
static cell AMX_NATIVE_CALL ArraySwap(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx1 = (size_t)params[2];
	size_t idx2 = (size_t)params[3];

	if (idx1 >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx1, vec->size());
		return 0;
	}

	if (idx2 >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx2, vec->size());
		return 0;
	}

	vec->swap(idx1, idx2);

	return 1;
}

// native ArrayDeleteItem(Array:which, item);
static cell AMX_NATIVE_CALL ArrayDeleteItem(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	size_t idx = (size_t)params[2];

	if (idx >= vec->size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid index %d (count: %d)", idx, vec->size());
		return 0;
	}

	vec->remove(idx);

	return 1;
}

// native ArrayDestroy(&Array:which);
static cell AMX_NATIVE_CALL ArrayDestroy(AMX* amx, cell* params)
{
	cell *handle = get_amxaddr(amx, params[1]);

	CellArray* vec = ArrayHandles.lookup(*handle);

	if (!vec)
	{
		return 0;
	}

	if (ArrayHandles.destroy(*handle))
	{
		*handle = 0;
		return 1;
	}

	return 1;
}


struct ArraySort_s
{
	int   func;
	cell  array_hndl;
	cell* array_base;
	cell  array_bsize;
	cell  data;
	cell  size;
	cell  addr1;
	cell  addr2;
	AMX  *amx;
};

ArraySort_s SortInfo;

int SortArrayList(const void *elem1, const void *elem2)
{
	return executeForwards(
		SortInfo.func,
		SortInfo.array_hndl,
		((cell)((cell *)elem1 - SortInfo.array_base)) / SortInfo.array_bsize,
		((cell)((cell *)elem2 - SortInfo.array_base)) / SortInfo.array_bsize,
		SortInfo.data,
		SortInfo.size
	);
}

// native ArraySort(Array:array, const comparefunc[], data[]="", data_size=0);
static cell AMX_NATIVE_CALL ArraySort(AMX* amx, cell* params)
{
	cell handle = params[1];
	CellArray* vec = ArrayHandles.lookup(handle);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", handle);
		return 0;
	}

	int len;
	char* funcName = get_amxstring(amx, params[2], 0, len);
	
	// MySortFunc(Array:array, item1, item2, const data[], data_size)
	int func = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	if (func < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "The public function \"%s\" was not found.", funcName);
		return 0;
	}

	size_t arraysize = vec->size();
	size_t blocksize = vec->blocksize();
	cell *array = vec->base();

	ArraySort_s oldinfo = SortInfo;

	SortInfo.func        = func;
	SortInfo.array_base  = array;
	SortInfo.array_bsize = static_cast<cell>(blocksize);
	SortInfo.array_hndl  = handle;
	SortInfo.data        = params[3];
	SortInfo.size        = params[4];

	qsort(array, arraysize, blocksize * sizeof(cell), SortArrayList);

	SortInfo = oldinfo;

	unregisterSPForward(func);

	return 1;
}


int SortArrayListExCell(const void *elem1, const void *elem2)
{
	size_t index1 = ((cell)((cell *)elem1 - SortInfo.array_base)) / SortInfo.array_bsize;
	size_t index2 = ((cell)((cell *)elem2 - SortInfo.array_base)) / SortInfo.array_bsize;

	return executeForwards(
		SortInfo.func,
		SortInfo.array_hndl,
		*&SortInfo.array_base[index1 * SortInfo.array_bsize], 
		*&SortInfo.array_base[index2 * SortInfo.array_bsize],
		SortInfo.data,
		SortInfo.size
	);
}

int SortArrayListExArray(const void *elem1, const void *elem2)
{
	size_t index1 = ((cell)((cell *)elem1 - SortInfo.array_base)) / SortInfo.array_bsize;
	size_t index2 = ((cell)((cell *)elem2 - SortInfo.array_base)) / SortInfo.array_bsize;

	cell *addr1 = get_amxaddr(SortInfo.amx, SortInfo.addr1);
	cell *addr2 = get_amxaddr(SortInfo.amx, SortInfo.addr2);

	memcpy(addr1, &SortInfo.array_base[index1 * SortInfo.array_bsize], SortInfo.array_bsize * sizeof(cell));
	memcpy(addr2, &SortInfo.array_base[index2 * SortInfo.array_bsize], SortInfo.array_bsize * sizeof(cell));

	return executeForwards(
		SortInfo.func,
		SortInfo.array_hndl,
		SortInfo.addr1,
		SortInfo.addr2,
		SortInfo.data,
		SortInfo.size
	);
}

// native ArraySortEx(Array:array, const comparefunc[], data[]="", data_size=0);
static cell AMX_NATIVE_CALL ArraySortEx(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	int len;
	char* funcName = get_amxstring(amx, params[2], 0, len);

	int func = registerSPForwardByName(amx, funcName, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);

	if (!func)
	{
		LogError(amx, AMX_ERR_NATIVE, "The public function \"%s\" was not found.", funcName);
		return 0;
	}

	size_t arraysize = vec->size();
	size_t blocksize = vec->blocksize();
	cell *array = vec->base();
	cell amx_addr1 = 0, amx_addr2 = 0, *phys_addr = NULL;

	if (blocksize > 1)
	{
		int err;
		if ((err = amx_Allot(amx, blocksize, &amx_addr1, &phys_addr)) != AMX_ERR_NONE
		|| ( err = amx_Allot(amx, blocksize, &amx_addr2, &phys_addr)) != AMX_ERR_NONE)
		{
			LogError(amx, err, "Ran out of memory");
			return 0;
		}
	}

	ArraySort_s oldinfo = SortInfo;

	SortInfo.func        = func;
	SortInfo.array_base  = array;
	SortInfo.array_bsize = static_cast<cell>(blocksize);
	SortInfo.array_hndl  = params[1];
	SortInfo.data        = params[3];
	SortInfo.size        = params[4];
	SortInfo.amx         = amx;
	SortInfo.addr1       = amx_addr1;
	SortInfo.addr2       = amx_addr2;

	qsort(array, arraysize, blocksize * sizeof(cell), blocksize > 1 ? SortArrayListExArray : SortArrayListExCell);

	SortInfo = oldinfo;

	if (blocksize > 1)
	{
		amx_Release(amx, amx_addr1);
		amx_Release(amx, amx_addr2);
	}

	unregisterSPForward(func);

	return 1;
}

extern bool fastcellcmp(cell *a, cell *b, cell len);
extern int amxstring_len(cell* a);

// native ArrayFindString(Array:which, const item[]);
static cell AMX_NATIVE_CALL ArrayFindString(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return -1;
	}

	cell *b, *a = get_amxaddr(amx, params[2]);
	size_t cellcount = vec->blocksize();
	size_t a_len = ke::Max(1, amxstring_len(a));
	size_t len = a_len > cellcount ? cellcount : a_len;

	for (size_t i = 0; i < vec->size(); i++)
	{	
		b = vec->at(i);

		if (fastcellcmp(a, b, len))
		{
			return static_cast<cell>(i);
		}
	}

	return -1;
}

// native ArrayFindValue(Array:which, any:item); 
static cell AMX_NATIVE_CALL ArrayFindValue(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return -1;
	}

	for (size_t i = 0; i < vec->size(); i++)
	{
		if (params[2] == *vec->at(i))
		{
			return static_cast<cell>(i);
		}
	}

	return -1;
}

AMX_NATIVE_INFO g_DataStructNatives[] = 
{
	{ "ArrayCreate"            , ArrayCreate },
	{ "ArrayClear"             , ArrayClear },
	{ "ArrayClone"             , ArrayClone },
	{ "ArraySize"              , ArraySize },
	{ "ArrayResize"            , ArrayResize },
	{ "ArrayGetArray"          , ArrayGetArray },
	{ "ArrayGetCell"           , ArrayGetCell },
	{ "ArrayGetString"         , ArrayGetString },
	{ "ArraySetArray"          , ArraySetArray },
	{ "ArraySetCell"           , ArraySetCell },
	{ "ArraySetString"         , ArraySetString },
	{ "ArrayPushArray"         , ArrayPushArray },
	{ "ArrayPushCell"          , ArrayPushCell },
	{ "ArrayPushString"        , ArrayPushString },
	{ "ArrayInsertArrayAfter"  , ArrayInsertArrayAfter },
	{ "ArrayInsertCellAfter"   , ArrayInsertCellAfter },
	{ "ArrayInsertStringAfter" , ArrayInsertStringAfter },
	{ "ArrayInsertArrayBefore" , ArrayInsertArrayBefore },
	{ "ArrayInsertCellBefore"  , ArrayInsertCellBefore },
	{ "ArrayInsertStringBefore", ArrayInsertStringBefore },
	{ "ArraySwap"              , ArraySwap },
	{ "ArrayDeleteItem"        , ArrayDeleteItem },
	{ "ArrayGetStringHandle"   , ArrayGetStringHandle },
	{ "ArrayDestroy"           , ArrayDestroy },
	{ "ArraySort"              , ArraySort },
	{ "ArraySortEx"            , ArraySortEx },
	{ "ArrayFindString"        , ArrayFindString },
	{ "ArrayFindValue"         , ArrayFindValue },
	{ nullptr                  , nullptr }
};
