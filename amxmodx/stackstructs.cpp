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

// native Stack:CreateStack(blocksize = 1);
static cell AMX_NATIVE_CALL CreateStack(AMX* amx, cell* params)
{
	int cellsize = params[1];

	if (cellsize <= 0)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid block size (must be > 0)", cellsize);
		return -1;
	}

	return ArrayHandles.create(cellsize);
}

// native PushStackCell(Stack:handle, any:value);
static cell AMX_NATIVE_CALL PushStackCell(AMX* amx, cell* params)
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
		LogError(amx, AMX_ERR_NATIVE, "Failed to grow stack");
		return 0;
	}

	*blk = params[2];

	return 1;
}

// native PushStackString(Stack:handle, const value[]);
static cell AMX_NATIVE_CALL PushStackString(AMX* amx, cell* params)
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
		LogError(amx, AMX_ERR_NATIVE, "Failed to grow stack");
		return 0;
	}

	int len;
	const char *value = get_amxstring(amx, params[2], 0, len);

	strncopy(blk, value, vec->blocksize());

	return 1;
}

// native PushStackArray(Stack:handle, const any:values[], size= -1);
static cell AMX_NATIVE_CALL PushStackArray(AMX* amx, cell* params)
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
		LogError(amx, AMX_ERR_NATIVE, "Failed to grow stack");
		return 0;
	}

	cell *addr = get_amxaddr(amx, params[2]);
	size_t indexes = vec->blocksize();

	if (params[3] != -1 && (size_t)params[3] <= vec->blocksize())
	{
		indexes = params[3];
	}

	memcpy(blk, addr, indexes * sizeof(cell));

	return 1;
}

// native bool:PopStackCell(Stack:handle, &any:value, block = 0, bool:asChar = false);
static cell AMX_NATIVE_CALL PopStackCell(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	if (vec->size() == 0)
	{
		return 0;
	}

	cell *buffer = get_amxaddr(amx, params[2]);
	size_t index = params[3];

	cell *blk = vec->at(vec->size() - 1);
	size_t idx = (size_t)params[3];

	if (params[4] == 0)
	{
		if (idx >= vec->blocksize())
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid block %d (blocksize: %d)", idx, vec->blocksize());
			return 0;
		}

		*buffer = blk[idx];
	}
	else
	{
		if (idx >= vec->blocksize() * 4)
		{
			LogError(amx, AMX_ERR_NATIVE, "Invalid byte %d (blocksize: %d bytes)", idx, vec->blocksize() * 4);
			return 0;
		}

		*buffer = (cell)*((char *)blk + idx);
	}

	vec->remove(vec->size() - 1);

	return 1;
}

// native bool:PopStackString(Stack:handle, buffer[], maxlength, &written = 0);
static cell AMX_NATIVE_CALL PopStackString(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	if (vec->size() == 0)
	{
		return 0;
	}

	size_t idx = vec->size() - 1;
	cell *blk = vec->at(idx);

	int numWritten = set_amxstring_utf8(amx, params[2], blk, amxstring_len(blk), params[3]);
	*get_amxaddr(amx, params[4]) = numWritten;

	vec->remove(idx);

	return 1;
}

// native bool:PopStackArray(Stack:handle, any:buffer[], size=-1);
static cell AMX_NATIVE_CALL PopStackArray(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	if (vec->size() == 0)
	{
		return 0;
	}

	size_t idx = vec->size() - 1;
	cell *blk = vec->at(idx);
	size_t indexes = vec->blocksize();

	if (params[3] != -1 && (size_t)params[3] <= vec->blocksize())
	{
		indexes = params[3];
	}

	cell *addr = get_amxaddr(amx, params[2]);
	memcpy(addr, blk, indexes * sizeof(cell));

	vec->remove(idx);

	return 1;
}

// native bool:IsStackEmpty(Stack:handle);
static cell AMX_NATIVE_CALL IsStackEmpty(AMX* amx, cell* params)
{
	CellArray* vec = ArrayHandles.lookup(params[1]);

	if (!vec)
	{
		LogError(amx, AMX_ERR_NATIVE, "Invalid array handle provided (%d)", params[1]);
		return 0;
	}

	return vec->size() == 0;
}

// native DestroyStack(&Stack:which);
static cell AMX_NATIVE_CALL DestroyStack(AMX* amx, cell* params)
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

AMX_NATIVE_INFO g_StackNatives[] =
{
	{ "CreateStack",     CreateStack     },
	{ "IsStackEmpty",    IsStackEmpty    },
	{ "PopStackArray",   PopStackArray   },
	{ "PopStackCell",    PopStackCell    },
	{ "PopStackString",  PopStackString  },
	{ "PushStackArray",  PushStackArray  },
	{ "PushStackCell",   PushStackCell   },
	{ "PushStackString", PushStackString },
	{ "DestroyStack",    DestroyStack    },
	{ nullptr,           nullptr         },
};
