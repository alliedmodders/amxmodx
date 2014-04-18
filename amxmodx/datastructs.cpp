/* AMX Mod X 
*
* by the AMX Mod X Development Team
*  originally developed by OLO
*
*  This program is free software; you can redistribute it and/or modify it
*  under the terms of the GNU General Public License as published by the
*  Free Software Foundation; either version 2 of the License, or (at
*  your option) any later version.
*
*  This program is distributed in the hope that it will be useful, but
*  WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*  General Public License for more details.
*
*  You should have received a copy of the GNU General Public License
*  along with this program; if not, write to the Free Software Foundation,
*  Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
*
*  In addition, as a special exception, the author gives permission to
*  link the code of this program with the Half-Life Game Engine ("HL
*  Engine") and Modified Game Libraries ("MODs") developed by Valve,
*  L.L.C ("Valve"). You must obey the GNU General Public License in all
*  respects for all of the code used other than the HL Engine and MODs
*  from Valve. If you modify this file, you may extend this exception
*  to your version of the file, but you are not obligated to do so. If
*  you do not wish to do so, delete this exception statement from your
*  version.
*/

#include "amxmodx.h"
#include "datastructs.h"


// Note: All handles start at 1. 0 and below are invalid handles.
//       This way, a plugin that doesn't initialize a vector or
//       string will not be able to modify another plugin's data
//       on accident.
CVector<CellVector*> VectorHolder;


// Array:ArrayCreate(cellsize=1, reserved=32);
static cell AMX_NATIVE_CALL ArrayCreate(AMX* amx, cell* params)
{
	// params[1] (cellsize) is how big in cells each element is.
	// this MUST be greater than 0!
	int cellsize=params[1];

	// params[2] (reserved) is how many elements to allocate
	// immediately when the list is created.
	// this MUST be greater than 0!
	int reserved=params[2];

	if (cellsize<=0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array size (%d)", cellsize);
		return -1;
	}
	if (reserved<=0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid reserved size (%d)", reserved);
		return -1;
	}

	// Scan through the vector list to see if any are NULL.
	// NULL means the vector was previously destroyed.
	for (unsigned int i=0; i < VectorHolder.size(); ++i)
	{
		if (VectorHolder[i]==NULL)
		{
			VectorHolder[i]=new CellVector(cellsize);
			VectorHolder[i]->Grow(reserved);
			return i + 1;
		}
	}

	// None are NULL, create a new vector
	CellVector* NewVector=new CellVector(cellsize);
	NewVector->Grow(reserved);

	VectorHolder.push_back(NewVector);

	return VectorHolder.size();
}

// ArrayClear(Array:which)
static cell AMX_NATIVE_CALL ArrayClear(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	vec->Clear();

	return 1;
}

// ArraySize(Array:which)
static cell AMX_NATIVE_CALL ArraySize(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	return vec->Size();
}

// ArrayResize(Array:which, newsize);
static cell AMX_NATIVE_CALL ArrayResize(AMX* amx, cell* params)
{
	CellVector* vec = HandleToVector(amx, params[1]);

	if (vec == NULL)
	{
		return 0;
	}

	if (!vec->Resize(params[2]))
	{
		LogError(amx, AMX_ERR_NATIVE, "Unable to resize array to \"%u\"", params[2]);
		return 0;
	}

	return 1;
}

// ArrayClone(Array:which)
static cell AMX_NATIVE_CALL ArrayClone(AMX* amx, cell* params)
{
	CellVector* vec = HandleToVector(amx, params[1]);

	if (vec == NULL)
	{
		return 0;
	}

	CellVector *clonevec = vec->Clone();

	// Scan through the vector list to see if any are NULL.
	// NULL means the vector was previously destroyed.
	for (unsigned int i = 0; i < VectorHolder.size(); ++i)
	{
		if (VectorHolder[i] == NULL)
		{
			VectorHolder[i] = clonevec;
			return i + 1;
		}
	}

	VectorHolder.push_back(clonevec);

	return VectorHolder.size();
}

// ArrayGetArray(Array:which, item, any:output[]);
static cell AMX_NATIVE_CALL ArrayGetArray(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	if (vec->GetArray(params[2],get_amxaddr(amx, params[3]))!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cellvector handle provided (%d:%d:%d)", params[1], params[2], vec->Size());
		return 0;
	}

	return 1;
}
// ArrayGetCell(Array:which, item, any:&output);
static cell AMX_NATIVE_CALL ArrayGetCell(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	cell ret;
	if (vec->GetCell(params[2],&ret)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cellvector handle provided (%d:%d:%d)", params[1], params[2], vec->Size());
		return 0;
	}

	return ret;
}
// ArrayGetString(Array:which, item, any:output[], size);
static cell AMX_NATIVE_CALL ArrayGetString(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	if (vec->GetString(params[2],get_amxaddr(amx, params[3]),params[4])!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cellvector handle provided (%d:%d:%d)", params[1], params[2], vec->Size());
		return 0;
	}

	return 1;
}
// ArraySetArray(Array:which, item, any:output[]);
static cell AMX_NATIVE_CALL ArraySetArray(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	if (vec->SetArray(params[2],get_amxaddr(amx, params[3]))!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cellvector handle provided (%d:%d:%d)", params[1], params[2], vec->Size());
		return 0;
	}

	return 1;
}
// ArraySetCell(Array:which, item, any:&output);
static cell AMX_NATIVE_CALL ArraySetCell(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	if (vec->SetCell(params[2], params[3])!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cellvector handle provided (%d:%d:%d)", params[1], params[2], vec->Size());
		return 0;
	}

	return 1;
}
// ArraySetString(Array:which, item, any:output[]);
static cell AMX_NATIVE_CALL ArraySetString(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	if (vec->SetString(params[2],get_amxaddr(amx, params[3]))!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid cellvector handle provided (%d:%d:%d)", params[1], params[2], vec->Size());
		return 0;
	}

	return 1;
}
// ArrayPushArray(Array:which, any:output[]);
static cell AMX_NATIVE_CALL ArrayPushArray(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	vec->SetArray(vec->Push(),get_amxaddr(amx, params[2]));

	return 1;
}
// ArrayPushCell(Array:which, &any:output);
static cell AMX_NATIVE_CALL ArrayPushCell(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	vec->SetCell(vec->Push(), params[2]);

	return 1;
}
// ArrayPushString(Array:which, any:output[]);
static cell AMX_NATIVE_CALL ArrayPushString(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec==NULL)
	{
		return 0;
	}

	vec->SetString(vec->Push(),get_amxaddr(amx, params[2]));

	return 1;
}
static cell AMX_NATIVE_CALL ArrayGetStringHandle(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (vec == NULL)
	{
		return 0;
	}

	cell* ptr=vec->GetCellPointer(params[2]);

	if (ptr == NULL)
	{
		return 0;
	}

	return reinterpret_cast<cell>(ptr);

}
// ArrayInsertArrayAfter(Array:which, item, const value[])
static cell AMX_NATIVE_CALL ArrayInsertArrayAfter(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	int item=params[2]+1;

	if (vec->ShiftUpFrom(item)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertArrayAfter (%d:%d)", params[1], vec->Size());
		return 0;
	}

	vec->SetArray(item, get_amxaddr(amx, params[3]));

	return 1;
}
// ArrayInsertCellAfter(Array:which, item, value[])
static cell AMX_NATIVE_CALL ArrayInsertCellAfter(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	int item=params[2]+1;

	if (vec->ShiftUpFrom(item)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertCellAfter (%d:%d)", params[1], vec->Size());
		return 0;
	}

	vec->SetCell(item, params[3]);

	return 1;
}
// ArrayInsertStringAfter(Array:which, item, const value[])
static cell AMX_NATIVE_CALL ArrayInsertStringAfter(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	int item=params[2]+1;

	if (vec->ShiftUpFrom(item)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertStringAfter (%d:%d)", params[1], vec->Size());
		return 0;
	}

	vec->SetString(item, get_amxaddr(amx, params[3]));

	return 1;
}
// ArrayInsertArrayBefore(Array:which, item, const value[])
static cell AMX_NATIVE_CALL ArrayInsertArrayBefore(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	int item=params[2];

	if (item==vec->Size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertArrayBefore (%d:%d)", params[2], vec->Size());
		return 0;
	}
	if (vec->ShiftUpFrom(item)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertArrayBefore (%d:%d)", params[2], vec->Size());
		return 0;
	}

	vec->SetArray(item, get_amxaddr(amx, params[3]));

	return 1;
}
// ArrayInsertCellBefore(Array:which, item, const value)
static cell AMX_NATIVE_CALL ArrayInsertCellBefore(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	int item=params[2];

	if (item==vec->Size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertCellBefore (%d:%d)", params[2], vec->Size());
		return 0;
	}
	if (vec->ShiftUpFrom(item)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertCellBefore (%d:%d)", params[2], vec->Size());
		return 0;
	}

	vec->SetCell(item, params[3]);

	return 1;
}
// ArrayInsertStringBefore(Array:which, item, const value[])
static cell AMX_NATIVE_CALL ArrayInsertStringBefore(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	int item=params[2];

	if (item==vec->Size())
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertStringBefore (%d:%d)", params[2], vec->Size());
		return 0;
	}
	if (vec->ShiftUpFrom(item)!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayInsertStringBefore (%d:%d)", params[2], vec->Size());
		return 0;
	}

	vec->SetString(item, get_amxaddr(amx, params[3]));

	return 1;
}

// ArraySwap(Array:which, item1, item2)
static cell AMX_NATIVE_CALL ArraySwap(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}
	if (vec->Swap(params[2], params[3])!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArraySwap (%d , %d:%d)",params[2], params[3], vec->Size());
		return 0;
	}

	return 1;
}

// ArrayDeleteItem(Array:which, item);
static cell AMX_NATIVE_CALL ArrayDeleteItem(AMX* amx, cell* params)
{
	CellVector* vec=HandleToVector(amx, params[1]);

	if (!vec)
	{
		return 0;
	}

	if (vec->Delete(params[2])!=1)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid item specified in ArrayDeleteItem (%d:%d)", params[2], vec->Size());
		return 0;
	}
	return 1;
}


// ArrayDestroy(Array:&which)
static cell AMX_NATIVE_CALL ArrayDestroy(AMX* amx, cell* params)
{
	// byref the handle here so we can zero it out after destroying
	// this way they cannot accidentally reuse it
	cell* handle=get_amxaddr(amx,params[1]);
	CellVector* vec=HandleToVector(amx, *handle);

	if (!vec)
	{
		return 0;
	}

	delete vec;

	VectorHolder[*handle-1]=NULL;

	*handle=0;

	return 1;
}

typedef struct ArraySort_s
{
	int    handle;
	int    forward;
	cell   data;
	cell   size;
	CellVector *vec;
	AMX*   amx;
	cell   addr1;
	cell   addr2;

} ArraySort_t;

static CStack<ArraySort_t *> ArraySortStack;

int SortArrayList(const void *itema, const void *itemb)
{
	ArraySort_t *Info = ArraySortStack.front();

	return executeForwards(Info->forward, Info->handle, *((int *)itema), *((int *)itemb), Info->data, Info->size);

}
// native ArraySort(Array:array, const comparefunc[], data[]="", data_size=0);
static cell AMX_NATIVE_CALL ArraySort(AMX* amx, cell* params)
{
	int handle=params[1];
	CellVector* vec=HandleToVector(amx, handle);

	if (!vec)
	{
		return 0;
	}
	
	// This is kind of a cheating way to go about this but...
	// Create an array of integers as big as however many elements are in the vector.
	// Pass that array to qsort
	// After the array is sorted out, then create a NEW cellvector
	// and copy in the old data in the order of what was sorted
	int len;
	char* FuncName=get_amxstring(amx, params[2], 0, len);
	// MySortFunc(Array:array, item1, item2, const data[], data_size)
	int Forward = registerSPForwardByName(amx, FuncName, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	if (Forward < 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "The public function \"%s\" was not found.", FuncName);
		return 0;
	}

	int *IntList=new int[vec->Size()];

	for (int i=0; i< vec->Size(); i++)
	{
		IntList[i]=i;
	}

	ArraySort_t *Info=new ArraySort_t;

	Info->handle=handle;
	Info->forward=Forward;
	Info->data=params[3];
	Info->size=params[4];

	ArraySortStack.push(Info);
	qsort(IntList, vec->Size(), sizeof(int), SortArrayList);
	ArraySortStack.pop();

	CellVector* newvec=new CellVector(vec->GetCellCount());

	// Set the new vector's values
	for (int i=0; i< vec->Size(); i++)
	{
		if (newvec->SetArray(newvec->Push(), vec->GetCellPointer(IntList[i]))!=1)
		{
			// This should never happen..
			LogError(amx, AMX_ERR_NATIVE, "Failed to SetArray in ArraySort (i=%d, IntList=%d)",i,IntList[i]);

			return 0;
		}
	}

	// Delete the old vector
	delete vec;

	// Now save the new vector in its handle location
	VectorHolder[handle-1]=newvec;

	// Cleanup
	delete Info;
	delete IntList;

	unregisterSPForward(Forward);

	return 1;
}


int SortArrayListExCell(const void *itema, const void *itemb)
{
	ArraySort_t *Info = ArraySortStack.front();
	cell vala = 0, valb = 0;

	Info->vec->GetCell(*((int *)itema), &vala);
	Info->vec->GetCell(*((int *)itemb), &valb);

	return executeForwards(Info->forward, Info->handle, vala, valb, Info->data, Info->size);
}

int SortArrayListExArray(const void *itema, const void *itemb)
{
	ArraySort_t *Info = ArraySortStack.front();

	Info->vec->GetArray(*((int *)itema), get_amxaddr(Info->amx, Info->addr1));
	Info->vec->GetArray(*((int *)itemb), get_amxaddr(Info->amx, Info->addr2));

	return executeForwards(Info->forward, Info->handle, Info->addr1, Info->addr2, Info->data, Info->size);
}

// native ArraySortEx(Array:array, const comparefunc[], data[]="", data_size=0);
static cell AMX_NATIVE_CALL ArraySortEx(AMX* amx, cell* params)
{
	int handle=params[1];
	CellVector* vec=HandleToVector(amx, handle);

	if (!vec)
	{
		return 0;
	}

	cell amx_addr1 = 0, amx_addr2 = 0, *phys_addr = NULL;
	size_t cellcount = vec->GetCellCount();

	if (cellcount > 1)
	{
		int err;
		if ((err=amx_Allot(amx, cellcount, &amx_addr1, &phys_addr)) != AMX_ERR_NONE
			|| (err=amx_Allot(amx, cellcount, &amx_addr2, &phys_addr)) != AMX_ERR_NONE)
		{
			LogError(amx, err, "Ran out of memory");
			return 0;
		}
	}

	// This is kind of a cheating way to go about this but...
	// Create an array of integers as big as however many elements are in the vector.
	// Pass that array to qsort
	// After the array is sorted out, then create a NEW cellvector
	// and copy in the old data in the order of what was sorted
	int len;
	char* FuncName=get_amxstring(amx, params[2], 0, len);
	// MySortFunc(Array:array, item1, item2, const data[], data_size)
	int Forward = registerSPForwardByName(amx, FuncName, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_CELL, FP_DONE);
	if (Forward < 0)
	{
		if (cellcount > 1)
		{
			amx_Release(amx, amx_addr1);
			amx_Release(amx, amx_addr2);
		}

		LogError(amx, AMX_ERR_NATIVE, "The public function \"%s\" was not found.", FuncName);
		return 0;
	}

	int *IntList=new int[vec->Size()];

	for (int i=0; i< vec->Size(); i++)
	{
		IntList[i]=i;
	}

	ArraySort_t *Info=new ArraySort_t;

	Info->handle=handle;
	Info->forward=Forward;
	Info->data=params[3];
	Info->size=params[4];
	Info->vec = vec;
	Info->amx = amx;
	Info->addr1 = amx_addr1;
	Info->addr2 = amx_addr2;

	ArraySortStack.push(Info);
	qsort(IntList, vec->Size(), sizeof(int), cellcount > 1 ? SortArrayListExArray : SortArrayListExCell);
	ArraySortStack.pop();

	CellVector* newvec=new CellVector(vec->GetCellCount());

	// Set the new vector's values
	for (int i=0; i< vec->Size(); i++)
	{
		if (newvec->SetArray(newvec->Push(), vec->GetCellPointer(IntList[i]))!=1)
		{
			// This should never happen..
			LogError(amx, AMX_ERR_NATIVE, "Failed to SetArray in ArraySort (i=%d, IntList=%d)",i,IntList[i]);

			if (cellcount > 1)
			{
				amx_Release(amx, amx_addr1);
				amx_Release(amx, amx_addr2);
			}

			return 0;
		}
	}

	// Delete the old vector
	delete vec;

	// Now save the new vector in its handle location
	VectorHolder[handle-1]=newvec;

	// Cleanup
	delete Info;
	delete IntList;

	unregisterSPForward(Forward);

	if (cellcount > 1)
	{
		amx_Release(amx, amx_addr1);
		amx_Release(amx, amx_addr2);
	}

	return 1;
}

extern bool fastcellcmp(cell *a, cell *b, cell len);
extern int amxstring_len(cell* a);

// ArrayFindString(Array:which, const item[])
static cell AMX_NATIVE_CALL ArrayFindString(AMX* amx, cell* params)
{
	int handle = params[1];
	CellVector* vec = HandleToVector(amx, handle);

	if (!vec)
	{
		return -1;
	}

	cell *b, *a = get_amxaddr(amx, params[2]);
	size_t cellcount = vec->GetCellCount();
	size_t a_len = amxstring_len(a);
	if (a_len < 1)
		a_len = 1;
	size_t len = a_len > cellcount ? cellcount : a_len;

	for (int i = 0; i < vec->Size(); i++)
	{	
		b = vec->GetCellPointer(i);

		if (fastcellcmp(a, b, len))
		{
			return (cell)i;
		}
	}

	return -1;
}

// ArrayFindValue(Array:which, any:item);
static cell AMX_NATIVE_CALL ArrayFindValue(AMX* amx, cell* params)
{
	int handle = params[1];
	CellVector* vec = HandleToVector(amx, handle);

	if (!vec)
	{
		return -1;
	}

	for (int i = 0; i < vec->Size(); i++)
	{
		if (params[2] == *vec->GetCellPointer(i))
		{
			return (cell)i;
		}
	}

	return -1;
}

AMX_NATIVE_INFO g_DataStructNatives[] = 
{
	{ "ArrayCreate",				ArrayCreate },
	{ "ArrayClear",					ArrayClear },
	{ "ArrayClone",					ArrayClone },
	{ "ArraySize",					ArraySize },
	{ "ArrayResize",				ArrayResize },
	{ "ArrayGetArray",				ArrayGetArray },
	{ "ArrayGetCell",				ArrayGetCell },
	{ "ArrayGetString",				ArrayGetString },
	{ "ArraySetArray",				ArraySetArray },
	{ "ArraySetCell",				ArraySetCell },
	{ "ArraySetString",				ArraySetString },
	{ "ArrayPushArray",				ArrayPushArray },
	{ "ArrayPushCell",				ArrayPushCell },
	{ "ArrayPushString",			ArrayPushString },
	{ "ArrayInsertArrayAfter",		ArrayInsertArrayAfter },
	{ "ArrayInsertCellAfter",		ArrayInsertCellAfter },
	{ "ArrayInsertStringAfter",		ArrayInsertStringAfter },
	{ "ArrayInsertArrayBefore",		ArrayInsertArrayBefore },
	{ "ArrayInsertCellBefore",		ArrayInsertCellBefore },
	{ "ArrayInsertStringBefore",	ArrayInsertStringBefore },
	{ "ArraySwap",					ArraySwap },
	{ "ArrayDeleteItem",			ArrayDeleteItem },
	{ "ArrayGetStringHandle",		ArrayGetStringHandle },
	{ "ArrayDestroy",				ArrayDestroy },
	{ "ArraySort",					ArraySort },
	{ "ArraySortEx",				ArraySortEx },
	{ "ArrayFindString",			ArrayFindString },
	{ "ArrayFindValue",				ArrayFindValue },

	{ NULL,							NULL }
};
